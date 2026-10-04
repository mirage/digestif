module By = Digestif_by
module Bi = Digestif_bi

let nist_padding = 0x06L
let keccak_padding = 0x01L

module Int64 = struct
  include Int64

  external ( lsl ) : int64 -> int -> int64 = "%int64_lsl"
  external ( lsr ) : int64 -> int -> int64 = "%int64_lsr"
  external ( asr ) : int64 -> int -> int64 = "%int64_asr"
  external ( lor ) : int64 -> int64 -> int64 = "%int64_or"
  external ( land ) : int64 -> int64 -> int64 = "%int64_and"
  external ( lxor ) : int64 -> int64 -> int64 = "%int64_xor"
  external ( + ) : int64 -> int64 -> int64 = "%int64_add"

  let[@inline] lnot a = a lxor (-1L)
  let[@inline] ror64 a n = (a lsr n) lor (a lsl (64 - n))
  let[@inline] rol64 a n = (a lsl n) lor (a lsr (64 - n))
end

module Unsafe (P : sig
  val padding : int64
end) =
struct
  type ctx = {
    q : Bytes.t;
    rsize : int;
    (* block size *)
    mdlen : int;
    (* output size *)
    mutable pt : int;
  }

  let dup ctx =
    { q = By.copy ctx.q; rsize = ctx.rsize; mdlen = ctx.mdlen; pt = ctx.pt }

  let init mdlen =
    let rsize = 200 - (2 * mdlen) in
    { q = By.make (25 * 8) '\x00'; rsize; mdlen; pt = 0 }

  let of_array a =
    let b = By.create (8 * Array.length a) in
    Array.iteri (fun i x -> By.unsafe_set_64 b (i * 8) x) a ;
    b

  let keccakf_rounds = 24

  let keccaft_rndc : Bytes.t =
    of_array
      [|
        0x0000000000000001L; 0x0000000000008082L; 0x800000000000808aL;
        0x8000000080008000L; 0x000000000000808bL; 0x0000000080000001L;
        0x8000000080008081L; 0x8000000000008009L; 0x000000000000008aL;
        0x0000000000000088L; 0x0000000080008009L; 0x000000008000000aL;
        0x000000008000808bL; 0x800000000000008bL; 0x8000000000008089L;
        0x8000000000008003L; 0x8000000000008002L; 0x8000000000000080L;
        0x000000000000800aL; 0x800000008000000aL; 0x8000000080008081L;
        0x8000000000008080L; 0x0000000080000001L; 0x8000000080008008L;
      |]

  let keccaft_rotc : int array =
    [|
      1; 3; 6; 10; 15; 21; 28; 36; 45; 55; 2; 14; 27; 41; 56; 8; 25; 43; 62; 18;
      39; 61; 20; 44;
    |]

  let keccakf_piln : int array =
    [|
      10; 7; 11; 17; 18; 3; 5; 16; 8; 21; 24; 4; 15; 23; 19; 13; 12; 2; 20; 14;
      22; 9; 6; 1;
    |]

  let sha3_keccakf (q : Bytes.t) =
    let bc = By.create (5 * 8) in
    for r = 0 to keccakf_rounds - 1 do
      let ( lxor ) = Int64.( lxor ) in
      let lnot = Int64.lnot in
      let ( land ) = Int64.( land ) in
      (* Theta *)
      for i = 0 to 4 do
        By.unsafe_set_64 bc (i * 8)
          (By.unsafe_get_64 q (i * 8)
          lxor By.unsafe_get_64 q ((i + 5) * 8)
          lxor By.unsafe_get_64 q ((i + 10) * 8)
          lxor By.unsafe_get_64 q ((i + 15) * 8)
          lxor By.unsafe_get_64 q ((i + 20) * 8))
      done ;
      for i = 0 to 4 do
        let t =
          By.unsafe_get_64 bc ((i + 4) mod 5 * 8)
          lxor Int64.rol64 (By.unsafe_get_64 bc ((i + 1) mod 5 * 8)) 1 in
        for k = 0 to 4 do
          let j = k * 5 in
          By.unsafe_set_64 q ((j + i) * 8)
            (By.unsafe_get_64 q ((j + i) * 8) lxor t)
        done
      done ;

      (* Rho Pi *)
      let t = ref (By.unsafe_get_64 q 8) in
      for i = 0 to 23 do
        let j = keccakf_piln.(i) in
        let v = By.unsafe_get_64 q (j * 8) in
        By.unsafe_set_64 q (j * 8) (Int64.rol64 !t keccaft_rotc.(i)) ;
        t := v
      done ;

      (* Chi *)
      for k = 0 to 4 do
        let j = k * 5 in
        for i = 0 to 4 do
          By.unsafe_set_64 bc (i * 8) (By.unsafe_get_64 q ((j + i) * 8))
        done ;
        for i = 0 to 4 do
          By.unsafe_set_64 q ((j + i) * 8)
            (By.unsafe_get_64 q ((j + i) * 8)
            lxor (lnot (By.unsafe_get_64 bc ((i + 1) mod 5 * 8))
                 land By.unsafe_get_64 bc ((i + 2) mod 5 * 8)))
        done
      done ;

      (* Iota *)
      By.unsafe_set_64 q 0
        (By.unsafe_get_64 q 0 lxor By.unsafe_get_64 keccaft_rndc (r * 8))
    done

  let masks =
    [|
      0xffffffffffffff00L; 0xffffffffffff00ffL; 0xffffffffff00ffffL;
      0xffffffff00ffffffL; 0xffffff00ffffffffL; 0xffff00ffffffffffL;
      0xff00ffffffffffffL; 0x00ffffffffffffffL;
    |]

  let feed : type a.
      get_uint8:(a -> int -> int) -> ctx -> a -> int -> int -> unit =
   fun ~get_uint8 ctx buf off len ->
    let ( && ) = ( land ) in

    let ( lxor ) = Int64.( lxor ) in
    let ( land ) = Int64.( land ) in
    let ( lor ) = Int64.( lor ) in
    let ( lsr ) = Int64.( lsr ) in
    let ( lsl ) = Int64.( lsl ) in

    let j = ref ctx.pt in

    for i = 0 to len - 1 do
      let q = By.unsafe_get_64 ctx.q (!j / 8 * 8) in
      let v = (q land (0xffL lsl ((!j && 0x7) * 8))) lsr ((!j && 0x7) * 8) in
      let v = v lxor Int64.of_int (get_uint8 buf (off + i)) in
      By.unsafe_set_64 ctx.q
        (!j / 8 * 8)
        (q land masks.(!j && 0x7) lor (v lsl ((!j && 0x7) * 8))) ;
      incr j ;
      if !j >= ctx.rsize
      then (
        sha3_keccakf ctx.q ;
        j := 0)
    done ;

    ctx.pt <- !j

  let unsafe_feed_bytes ctx buf off len =
    let get_uint8 buf off = Char.code (By.get buf off) in
    feed ~get_uint8 ctx buf off len

  let unsafe_feed_bigstring : ctx -> Bi.t -> int -> int -> unit =
   fun ctx buf off len ->
    let get_uint8 buf off = Char.code (Bi.get buf off) in
    feed ~get_uint8 ctx buf off len

  let unsafe_get ctx =
    let ( && ) = ( land ) in

    let ( lxor ) = Int64.( lxor ) in
    let ( lsl ) = Int64.( lsl ) in

    let v = By.unsafe_get_64 ctx.q (ctx.pt / 8 * 8) in
    let v = v lxor (P.padding lsl ((ctx.pt && 0x7) * 8)) in
    By.unsafe_set_64 ctx.q (ctx.pt / 8 * 8) v ;

    let v = By.unsafe_get_64 ctx.q ((ctx.rsize - 1) / 8 * 8) in
    let v = v lxor (0x80L lsl (((ctx.rsize - 1) && 0x7) * 8)) in
    By.unsafe_set_64 ctx.q ((ctx.rsize - 1) / 8 * 8) v ;

    sha3_keccakf ctx.q ;

    (* Get hash *)
    (* if the hash size in bytes is not a multiple of 8 (meaning it is
       not composed of whole int64 words, like for sha3_224), we
       extract the whole last int64 word from the state [ctx.st] and
       cut the hash at the right size after conversion to bytes. *)
    let n =
      let r = ctx.mdlen mod 8 in
      ctx.mdlen + if r = 0 then 0 else 8 - r in

    let hash = By.create n in
    for i = 0 to (n / 8) - 1 do
      let v = By.unsafe_get_64 ctx.q (i * 8) in
      By.unsafe_set_64 hash (i * 8) (if Sys.big_endian then By.swap64 v else v)
    done ;

    By.sub hash 0 ctx.mdlen
end
