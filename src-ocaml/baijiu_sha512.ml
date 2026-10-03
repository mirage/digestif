module By = Digestif_by
module Bi = Digestif_bi

module Int64 = struct
  include Int64

  external ( lsl ) : int64 -> int -> int64 = "%int64_lsl"
  external ( lsr ) : int64 -> int -> int64 = "%int64_lsr"
  external ( asr ) : int64 -> int -> int64 = "%int64_asr"
  external ( lor ) : int64 -> int64 -> int64 = "%int64_or"
  external ( land ) : int64 -> int64 -> int64 = "%int64_and"
  external ( lxor ) : int64 -> int64 -> int64 = "%int64_xor"
  external ( + ) : int64 -> int64 -> int64 = "%int64_add"

  let[@inline] ror64 a n = (a lsr n) lor (a lsl (64 - n))
  let[@inline] rol64 a n = (a lsl n) lor (a lsr (64 - n))
end

module type S = sig
  type kind = [ `SHA512 ]
  type ctx = { mutable size : int64 array; b : Bytes.t; h : Bytes.t }

  val of_array : int64 array -> Bytes.t
  val init : unit -> ctx
  val unsafe_feed_bytes : ctx -> By.t -> int -> int -> unit
  val unsafe_feed_bigstring : ctx -> Bi.t -> int -> int -> unit
  val unsafe_get : ctx -> By.t
  val dup : ctx -> ctx
end

module Unsafe : S = struct
  type kind = [ `SHA512 ]
  type ctx = { mutable size : int64 array; b : Bytes.t; h : Bytes.t }

  let of_array a =
    let b = By.create (8 * Array.length a) in
    Array.iteri (fun i x -> By.unsafe_set_64 b (i * 8) x) a ;
    b

  let dup ctx =
    { size = Array.copy ctx.size; b = By.copy ctx.b; h = By.copy ctx.h }

  let init () =
    let b = By.make 128 '\x00' in
    {
      size = [| 0L; 0L |];
      b;
      h =
        of_array
          [|
            0x6a09e667f3bcc908L; 0xbb67ae8584caa73bL; 0x3c6ef372fe94f82bL;
            0xa54ff53a5f1d36f1L; 0x510e527fade682d1L; 0x9b05688c2b3e6c1fL;
            0x1f83d9abfb41bd6bL; 0x5be0cd19137e2179L;
          |];
    }

  let k =
    of_array
      [|
        0x428a2f98d728ae22L; 0x7137449123ef65cdL; 0xb5c0fbcfec4d3b2fL;
        0xe9b5dba58189dbbcL; 0x3956c25bf348b538L; 0x59f111f1b605d019L;
        0x923f82a4af194f9bL; 0xab1c5ed5da6d8118L; 0xd807aa98a3030242L;
        0x12835b0145706fbeL; 0x243185be4ee4b28cL; 0x550c7dc3d5ffb4e2L;
        0x72be5d74f27b896fL; 0x80deb1fe3b1696b1L; 0x9bdc06a725c71235L;
        0xc19bf174cf692694L; 0xe49b69c19ef14ad2L; 0xefbe4786384f25e3L;
        0x0fc19dc68b8cd5b5L; 0x240ca1cc77ac9c65L; 0x2de92c6f592b0275L;
        0x4a7484aa6ea6e483L; 0x5cb0a9dcbd41fbd4L; 0x76f988da831153b5L;
        0x983e5152ee66dfabL; 0xa831c66d2db43210L; 0xb00327c898fb213fL;
        0xbf597fc7beef0ee4L; 0xc6e00bf33da88fc2L; 0xd5a79147930aa725L;
        0x06ca6351e003826fL; 0x142929670a0e6e70L; 0x27b70a8546d22ffcL;
        0x2e1b21385c26c926L; 0x4d2c6dfc5ac42aedL; 0x53380d139d95b3dfL;
        0x650a73548baf63deL; 0x766a0abb3c77b2a8L; 0x81c2c92e47edaee6L;
        0x92722c851482353bL; 0xa2bfe8a14cf10364L; 0xa81a664bbc423001L;
        0xc24b8b70d0f89791L; 0xc76c51a30654be30L; 0xd192e819d6ef5218L;
        0xd69906245565a910L; 0xf40e35855771202aL; 0x106aa07032bbd1b8L;
        0x19a4c116b8d2d0c8L; 0x1e376c085141ab53L; 0x2748774cdf8eeb99L;
        0x34b0bcb5e19b48a8L; 0x391c0cb3c5c95a63L; 0x4ed8aa4ae3418acbL;
        0x5b9cca4f7763e373L; 0x682e6ff3d6b2b8a3L; 0x748f82ee5defb2fcL;
        0x78a5636f43172f60L; 0x84c87814a1f0ab72L; 0x8cc702081a6439ecL;
        0x90befffa23631e28L; 0xa4506cebde82bde9L; 0xbef9a3f7b2c67915L;
        0xc67178f2e372532bL; 0xca273eceea26619cL; 0xd186b8c721c0c207L;
        0xeada7dd6cde0eb1eL; 0xf57d4f7fee6ed178L; 0x06f067aa72176fbaL;
        0x0a637dc5a2c898a6L; 0x113f9804bef90daeL; 0x1b710b35131c471bL;
        0x28db77f523047d84L; 0x32caab7b40c72493L; 0x3c9ebe0a15c9bebcL;
        0x431d67c49c100d4cL; 0x4cc5d4becb3e42b6L; 0x597f299cfc657e2aL;
        0x5fcb6fab3ad6faecL; 0x6c44198c4a475817L;
      |]

  let[@inline] e0 x = Int64.(ror64 x 28 lxor ror64 x 34 lxor ror64 x 39)
  let[@inline] e1 x = Int64.(ror64 x 14 lxor ror64 x 18 lxor ror64 x 41)
  let[@inline] s0 x = Int64.(ror64 x 1 lxor ror64 x 8 lxor (x lsr 7))
  let[@inline] s1 x = Int64.(ror64 x 19 lxor ror64 x 61 lxor (x lsr 6))

  let sha512_do_chunk : type a.
      be64_to_cpu:(a -> int -> int64) -> ctx -> a -> int -> unit =
   fun ~be64_to_cpu ctx buf off ->
    let a = ref (By.unsafe_get_64 ctx.h 0) in
    let b = ref (By.unsafe_get_64 ctx.h 8) in
    let c = ref (By.unsafe_get_64 ctx.h 16) in
    let d = ref (By.unsafe_get_64 ctx.h 24) in
    let e = ref (By.unsafe_get_64 ctx.h 32) in
    let f = ref (By.unsafe_get_64 ctx.h 40) in
    let g = ref (By.unsafe_get_64 ctx.h 48) in
    let h = ref (By.unsafe_get_64 ctx.h 56) in
    let w = By.create (80 * 8) in
    for i = 0 to 15 do
      By.unsafe_set_64 w (i * 8) (be64_to_cpu buf (off + (i * 8)))
    done ;
    for i = 16 to 79 do
      By.unsafe_set_64 w (i * 8)
        Int64.(
          s1 (By.unsafe_get_64 w ((i - 2) * 8))
          + By.unsafe_get_64 w ((i - 7) * 8)
          + s0 (By.unsafe_get_64 w ((i - 15) * 8))
          + By.unsafe_get_64 w ((i - 16) * 8))
    done ;
    for i = 0 to 79 do
      let open Int64 in
      let t1 =
        !h
        + e1 !e
        + (!g lxor (!e land (!f lxor !g)))
        + By.unsafe_get_64 k (i * 8)
        + By.unsafe_get_64 w (i * 8) in
      let t2 = e0 !a + (!a land !b lor (!c land (!a lor !b))) in
      h := !g ;
      g := !f ;
      f := !e ;
      e := !d + t1 ;
      d := !c ;
      c := !b ;
      b := !a ;
      a := t1 + t2
    done ;
    let open Int64 in
    By.unsafe_set_64 ctx.h 0 (By.unsafe_get_64 ctx.h 0 + !a) ;
    By.unsafe_set_64 ctx.h 8 (By.unsafe_get_64 ctx.h 8 + !b) ;
    By.unsafe_set_64 ctx.h 16 (By.unsafe_get_64 ctx.h 16 + !c) ;
    By.unsafe_set_64 ctx.h 24 (By.unsafe_get_64 ctx.h 24 + !d) ;
    By.unsafe_set_64 ctx.h 32 (By.unsafe_get_64 ctx.h 32 + !e) ;
    By.unsafe_set_64 ctx.h 40 (By.unsafe_get_64 ctx.h 40 + !f) ;
    By.unsafe_set_64 ctx.h 48 (By.unsafe_get_64 ctx.h 48 + !g) ;
    By.unsafe_set_64 ctx.h 56 (By.unsafe_get_64 ctx.h 56 + !h) ;
    ()

  let feed : type a.
      blit:(a -> int -> By.t -> int -> int -> unit) ->
      be64_to_cpu:(a -> int -> int64) ->
      ctx ->
      a ->
      int ->
      int ->
      unit =
   fun ~blit ~be64_to_cpu ctx buf off len ->
    let idx = ref Int64.(to_int (ctx.size.(0) land 0x7FL)) in
    let len = ref len in
    let off = ref off in
    let to_fill = 128 - !idx in
    ctx.size.(0) <- Int64.add ctx.size.(0) (Int64.of_int !len) ;
    if ctx.size.(0) < Int64.of_int !len
    then ctx.size.(1) <- Int64.succ ctx.size.(1) ;
    if !idx <> 0 && !len >= to_fill
    then (
      blit buf !off ctx.b !idx to_fill ;
      sha512_do_chunk ~be64_to_cpu:By.be64_to_cpu ctx ctx.b 0 ;
      len := !len - to_fill ;
      off := !off + to_fill ;
      idx := 0) ;
    while !len >= 128 do
      sha512_do_chunk ~be64_to_cpu ctx buf !off ;
      len := !len - 128 ;
      off := !off + 128
    done ;
    if !len <> 0 then blit buf !off ctx.b !idx !len ;
    ()

  let unsafe_feed_bytes = feed ~blit:By.blit ~be64_to_cpu:By.be64_to_cpu

  let unsafe_feed_bigstring =
    feed ~blit:By.blit_from_bigstring ~be64_to_cpu:Bi.be64_to_cpu

  let unsafe_get ctx =
    let index = Int64.(to_int (ctx.size.(0) land 0x7FL)) in
    let padlen = if index < 112 then 112 - index else 128 + 112 - index in
    let padding = By.init padlen (function 0 -> '\x80' | _ -> '\x00') in
    let bits = By.create 16 in
    By.cpu_to_be64 bits 0 Int64.((ctx.size.(1) lsl 3) lor (ctx.size.(0) lsr 61)) ;
    By.cpu_to_be64 bits 8 Int64.(ctx.size.(0) lsl 3) ;
    unsafe_feed_bytes ctx padding 0 padlen ;
    unsafe_feed_bytes ctx bits 0 16 ;
    let res = By.create (8 * 8) in
    for i = 0 to 7 do
      By.cpu_to_be64 res (i * 8) (By.unsafe_get_64 ctx.h (i * 8))
    done ;
    res
end
