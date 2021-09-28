module By = Digestif_by
module Bi = Digestif_bi

let nist_padding = 0x06L

let keccak_padding = 0x01L

module Int64 = struct
  include Int64

  let ( lsl ) = Int64.shift_left

  let ( lsr ) = Int64.shift_right_logical

  let ( asr ) = Int64.shift_right

  let ( lor ) = Int64.logor

  let ( land ) = Int64.logand

  let ( lxor ) = Int64.logxor

  let ( + ) = Int64.add

  let ror64 a n = (a lsr n) lor (a lsl (64 - n))

  let rol64 a n = (a lsl n) lor (a lsr (64 - n))
end

module Unsafe (P : sig
  val padding : int64
end) =
struct
  type ctx = {
    q : int64 array;
    rsize : int;
    (* block size *)
    mdlen : int;
    (* output size *)
    mutable pt : int;
  }

  let dup ctx =
    { q = Array.copy ctx.q; rsize = ctx.rsize; mdlen = ctx.mdlen; pt = ctx.pt }

  let init mdlen =
    let rsize = 200 - (2 * mdlen) in
    { q = Array.make 25 0L; rsize; mdlen; pt = 0 }

  let keccakf_rounds = 24

  let keccaft_rndc : int64 array =
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

  let sha3_keccakf (q : int64 array) =
    for r = 0 to keccakf_rounds - 1 do
      let ( lxor ) = Int64.( lxor ) in
      let lnot = Int64.lognot in
      let ( land ) = Int64.( land ) in
      (* Theta *)
      let bc =
        Array.init 5 (fun i ->
            q.(i) lxor q.(i + 5) lxor q.(i + 10) lxor q.(i + 15) lxor q.(i + 20))
      in
      for i = 0 to 4 do
        let t = bc.((i + 4) mod 5) lxor Int64.rol64 bc.((i + 1) mod 5) 1 in
        for k = 0 to 4 do
          let j = k * 5 in
          q.(j + i) <- q.(j + i) lxor t
        done
      done ;

      (* Rho Pi *)
      let t = ref q.(1) in
      let _ =
        Array.iteri
          (fun i rotc ->
            let j = keccakf_piln.(i) in
            bc.(0) <- q.(j) ;
            q.(j) <- Int64.rol64 !t rotc ;
            t := bc.(0))
          keccaft_rotc in

      (* Chi *)
      for k = 0 to 4 do
        let j = k * 5 in
        let bc = Array.init 5 (fun i -> q.(j + i)) in
        for i = 0 to 4 do
          q.(j + i) <-
            q.(j + i) lxor (lnot bc.((i + 1) mod 5) land bc.((i + 2) mod 5))
        done
      done ;

      (* Iota *)
      q.(0) <- q.(0) lxor keccaft_rndc.(r)
    done

  let masks =
    [|
      0xffffffffffffff00L; 0xffffffffffff00ffL; 0xffffffffff00ffffL;
      0xffffffff00ffffffL; 0xffffff00ffffffffL; 0xffff00ffffffffffL;
      0xff00ffffffffffffL; 0x00ffffffffffffffL;
    |]

  let feed :
      type a. get_uint8:(a -> int -> int) -> ctx -> a -> int -> int -> unit =
   fun ~get_uint8 ctx buf off len ->
    let ( && ) = ( land ) in

    let ( lxor ) = Int64.( lxor ) in
    let ( land ) = Int64.( land ) in
    let ( lor ) = Int64.( lor ) in
    let ( lsr ) = Int64.( lsr ) in
    let ( lsl ) = Int64.( lsl ) in

    let j = ref ctx.pt in

    for i = 0 to len - 1 do
      let v =
        (ctx.q.(!j / 8) land (0xffL lsl ((!j && 0x7) * 8))) lsr ((!j && 0x7) * 8)
      in
      let v = v lxor Int64.of_int (get_uint8 buf (off + i)) in
      ctx.q.(!j / 8) <-
        ctx.q.(!j / 8) land masks.(!j && 0x7) lor (v lsl ((!j && 0x7) * 8)) ;
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

    let v = ctx.q.(ctx.pt / 8) in
    let v = v lxor (P.padding lsl ((ctx.pt && 0x7) * 8)) in
    ctx.q.(ctx.pt / 8) <- v ;

    let v = ctx.q.((ctx.rsize - 1) / 8) in
    let v = v lxor (0x80L lsl (((ctx.rsize - 1) && 0x7) * 8)) in
    ctx.q.((ctx.rsize - 1) / 8) <- v ;

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
      By.unsafe_set_64 hash (i * 8)
        (if Sys.big_endian then By.swap64 ctx.q.(i) else ctx.q.(i))
    done ;

    By.sub hash 0 ctx.mdlen
end
