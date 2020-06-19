module By = Digestif_by
module Bi = Digestif_bi

module Int64 = struct
  include Int64

  let ( lsl ) = Int64.shift_left
  let ( lsr ) = Int64.shift_right
  let ( asr ) = Int64.shift_right_logical
  let ( lor ) = Int64.logor
  let ( land ) = Int64.logand
  let ( lxor ) = Int64.logxor
  let ( + ) = Int64.add
  let ror64 a n = (a asr n) lor (a lsl (64 - n))
  let rol64 a n = (a lsl n) lor (a asr (64 - n))
end

module type S = sig
  type kind = [ `SHA3 ]

  type ctx = {
    st : int64 array;
    rsize : int;
    mdlen : int;
    mutable pt : int;
    data : Bytes.t;
  }

  val init : unit -> ctx
  val unsafe_feed_bytes : ctx -> By.t -> int -> int -> unit
  val unsafe_feed_bigstring : ctx -> Bi.t -> int -> int -> unit
  val unsafe_get : ctx -> By.t
  val dup : ctx -> ctx
end

module Unsafe : S = struct
  type kind = [ `SHA3 ]

  type ctx = {
    st : int64 array;
    rsize : int;
    mdlen : int;
    mutable pt : int;
    data : Bytes.t;
  }

  let dup ctx =
    {
      st = Array.copy ctx.st;
      rsize = ctx.rsize;
      mdlen = ctx.mdlen;
      pt = ctx.pt;
      data = Bytes.copy ctx.data;
    }

  let init () =
    (* mdlen should be a parameter *)
    let mdlen = 64 (* 512 bits = 64 bytes *) in
    (* bloc size *)
    let rsize = 200 - (2 * mdlen) (* 1600 bits = 200 bytes *) in
    {
      st = Array.make 25 0L;
      rsize;
      mdlen;
      pt = 0;
      data = Bytes.make rsize '\x00';
    }

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

  let swap64 = if Sys.big_endian then By.swap64 else fun x -> x

  let sha3_keccakf (st : int64 array) =
    if Sys.big_endian then Array.iteri (fun i sti -> st.(i) <- swap64 sti) st;

    for r = 0 to keccakf_rounds - 1 do
      let ( lxor ) = Int64.( lxor ) in
      let lnot = Int64.lognot in
      let ( land ) = Int64.( land ) in
      (* Theta *)
      let bc =
        Array.init 5 (fun i ->
            st.(i)
            lxor st.(i + 5)
            lxor st.(i + 10)
            lxor st.(i + 15)
            lxor st.(i + 20))
      in
      for i = 0 to 4 do
        let t = bc.((i + 4) mod 5) lxor Int64.rol64 bc.((i + 1) mod 5) 1 in
        for k = 0 to 4 do
          let j = k * 5 in
          st.(j + i) <- st.(j + i) lxor t
        done
      done;

      (* Rho Pi*)
      let t = ref st.(1) in
      let _ =
        Array.iteri
          (fun i rotc ->
            let j = keccakf_piln.(i) in
            bc.(0) <- st.(j);
            st.(j) <- Int64.rol64 !t rotc;
            t := bc.(0))
          keccaft_rotc
      in

      (* Chi *)
      for k = 0 to 4 do
        let j = k * 5 in
        let bc = Array.init 5 (fun i -> st.(j + i)) in
        for i = 0 to 4 do
          st.(j + i) <-
            st.(j + i) lxor (lnot bc.((i + 1) mod 5) land bc.((i + 2) mod 5))
        done
      done;

      (* Iota *)
      st.(0) <- st.(0) lxor keccaft_rndc.(r)
    done;

    if Sys.big_endian then Array.iteri (fun i sti -> st.(i) <- swap64 sti) st

  let feed :
      type a.
      blit:(a -> int -> By.t -> int -> int -> unit) ->
      ctx ->
      a ->
      int ->
      int ->
      unit =
   fun ~blit ctx buf off len ->
    let ( lxor ) = Int64.( lxor ) in
    let len = ref len in
    let off = ref off in

    try
      while true do
        let to_fill = ctx.rsize - ctx.pt in
        if to_fill > !len then (
          blit buf !off ctx.data ctx.pt !len;
          ctx.pt <- ctx.pt + !len;
          raise Exit )
        else (
          blit buf !off ctx.data ctx.pt to_fill;
          for i = 0 to (ctx.rsize / 8) - 1 do
            ctx.st.(i) <- ctx.st.(i) lxor By.unsafe_get_64 ctx.data (i * 8)
          done;
          sha3_keccakf ctx.st;
          ctx.pt <- 0;
          off := !off + to_fill;
          len := !len - to_fill )
      done
    with Exit -> ()

  let unsafe_feed_bytes = feed ~blit:By.blit
  let unsafe_feed_bigstring = feed ~blit:By.blit_from_bigstring

  let unsafe_get ctx =
    let ( lxor ) = Int64.( lxor ) in

    (* Padding *)
    Bytes.set ctx.data ctx.pt '\x06';
    for i = ctx.pt + 1 to ctx.rsize - 2 do
      Bytes.set ctx.data i '\x00'
    done;
    Bytes.set ctx.data (ctx.rsize - 1) '\x80';

    (* Call sha3_keccaft on the last block *)
    for i = 0 to (ctx.rsize / 8) - 1 do
      ctx.st.(i) <- ctx.st.(i) lxor By.unsafe_get_64 ctx.data (i * 8)
    done;

    sha3_keccakf ctx.st;

    (* Get hash *)
    let hash = By.create ctx.mdlen in
    for i = 0 to (ctx.mdlen / 8) - 1 do
      By.unsafe_set_64 hash (i * 8) ctx.st.(i)
    done;

    hash
end
