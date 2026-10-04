module By = Digestif_by
module Bi = Digestif_bi

module Int32 = struct
  include Int32

  external ( lsl ) : int32 -> int -> int32 = "%int32_lsl"
  external ( lsr ) : int32 -> int -> int32 = "%int32_lsr"
  external ( asr ) : int32 -> int -> int32 = "%int32_asr"
  external ( lor ) : int32 -> int32 -> int32 = "%int32_or"
  external ( lxor ) : int32 -> int32 -> int32 = "%int32_xor"
  external ( land ) : int32 -> int32 -> int32 = "%int32_and"
  external ( + ) : int32 -> int32 -> int32 = "%int32_add"

  let[@inline] lnot x = x lxor (-1l)
  let[@inline] rol32 a n = (a lsl n) lor (a lsr (32 - n))
  let[@inline] ror32 a n = (a lsr n) lor (a lsl (32 - n))
end

module Int64 = struct
  include Int64

  external ( land ) : int64 -> int64 -> int64 = "%int64_and"
  external ( lsl ) : int64 -> int -> int64 = "%int64_lsl"
end

module type S = sig
  type kind = [ `MD5 ]
  type ctx = { mutable size : int64; b : Bytes.t; h : Bytes.t }

  val init : unit -> ctx
  val unsafe_feed_bytes : ctx -> By.t -> int -> int -> unit
  val unsafe_feed_bigstring : ctx -> Bi.t -> int -> int -> unit
  val unsafe_get : ctx -> By.t
  val dup : ctx -> ctx
end

module Unsafe : S = struct
  type kind = [ `MD5 ]
  type ctx = { mutable size : int64; b : Bytes.t; h : Bytes.t }

  let dup ctx = { size = ctx.size; b = By.copy ctx.b; h = By.copy ctx.h }

  let init () =
    let b = By.make 64 '\x00' in
    let h = By.create (4 * 4) in
    By.unsafe_set_32 h 0 0x67452301l ;
    By.unsafe_set_32 h 4 0xefcdab89l ;
    By.unsafe_set_32 h 8 0x98badcfel ;
    By.unsafe_set_32 h 12 0x10325476l ;
    { size = 0L; b; h }

  let[@inline] f1 x y z = Int32.(z lxor (x land (y lxor z)))
  let[@inline] f2 x y z = f1 z x y
  let[@inline] f3 x y z = Int32.(x lxor y lxor z)
  let[@inline] f4 x y z = Int32.(y lxor (x lor lnot z))

  let of_array a =
    let b = By.create (4 * Array.length a) in
    Array.iteri (fun i x -> By.unsafe_set_32 b (i * 4) x) a ;
    b

  let k =
    of_array
      [|
        0xd76aa478l; 0xe8c7b756l; 0x242070dbl; 0xc1bdceeel; 0xf57c0fafl;
        0x4787c62al; 0xa8304613l; 0xfd469501l; 0x698098d8l; 0x8b44f7afl;
        0xffff5bb1l; 0x895cd7bel; 0x6b901122l; 0xfd987193l; 0xa679438el;
        0x49b40821l; 0xf61e2562l; 0xc040b340l; 0x265e5a51l; 0xe9b6c7aal;
        0xd62f105dl; 0x02441453l; 0xd8a1e681l; 0xe7d3fbc8l; 0x21e1cde6l;
        0xc33707d6l; 0xf4d50d87l; 0x455a14edl; 0xa9e3e905l; 0xfcefa3f8l;
        0x676f02d9l; 0x8d2a4c8al; 0xfffa3942l; 0x8771f681l; 0x6d9d6122l;
        0xfde5380cl; 0xa4beea44l; 0x4bdecfa9l; 0xf6bb4b60l; 0xbebfbc70l;
        0x289b7ec6l; 0xeaa127fal; 0xd4ef3085l; 0x04881d05l; 0xd9d4d039l;
        0xe6db99e5l; 0x1fa27cf8l; 0xc4ac5665l; 0xf4292244l; 0x432aff97l;
        0xab9423a7l; 0xfc93a039l; 0x655b59c3l; 0x8f0ccc92l; 0xffeff47dl;
        0x85845dd1l; 0x6fa87e4fl; 0xfe2ce6e0l; 0xa3014314l; 0x4e0811a1l;
        0xf7537e82l; 0xbd3af235l; 0x2ad7d2bbl; 0xeb86d391l;
      |]

  let md5_do_chunk : type a.
      le32_to_cpu:(a -> int -> int32) -> ctx -> a -> int -> unit =
   fun ~le32_to_cpu ctx buf off ->
    let a = ref (By.unsafe_get_32 ctx.h 0) in
    let b = ref (By.unsafe_get_32 ctx.h 4) in
    let c = ref (By.unsafe_get_32 ctx.h 8) in
    let d = ref (By.unsafe_get_32 ctx.h 12) in
    let w = By.create (16 * 4) in
    for i = 0 to 15 do
      By.unsafe_set_32 w (i * 4) (le32_to_cpu buf (off + (i * 4)))
    done ;
    for i = 0 to 3 do
      let w0 = By.unsafe_get_32 w (i * 4 * 4) in
      let k0 = By.unsafe_get_32 k (i * 4 * 4) in
      let w1 = By.unsafe_get_32 w (((i * 4) + 1) * 4) in
      let k1 = By.unsafe_get_32 k (((i * 4) + 1) * 4) in
      let w2 = By.unsafe_get_32 w (((i * 4) + 2) * 4) in
      let k2 = By.unsafe_get_32 k (((i * 4) + 2) * 4) in
      let w3 = By.unsafe_get_32 w (((i * 4) + 3) * 4) in
      let k3 = By.unsafe_get_32 k (((i * 4) + 3) * 4) in
      let open Int32 in
      a := rol32 (!a + f1 !b !c !d + w0 + k0) 7 + !b ;
      d := rol32 (!d + f1 !a !b !c + w1 + k1) 12 + !a ;
      c := rol32 (!c + f1 !d !a !b + w2 + k2) 17 + !d ;
      b := rol32 (!b + f1 !c !d !a + w3 + k3) 22 + !c
    done ;
    for i = 0 to 3 do
      let w0 = By.unsafe_get_32 w ((((i * 4) + 1) land 15) * 4) in
      let k0 = By.unsafe_get_32 k (((i * 4) + 16) * 4) in
      let w1 = By.unsafe_get_32 w ((((i * 4) + 6) land 15) * 4) in
      let k1 = By.unsafe_get_32 k (((i * 4) + 17) * 4) in
      let w2 = By.unsafe_get_32 w ((((i * 4) + 11) land 15) * 4) in
      let k2 = By.unsafe_get_32 k (((i * 4) + 18) * 4) in
      let w3 = By.unsafe_get_32 w ((((i * 4) + 16) land 15) * 4) in
      let k3 = By.unsafe_get_32 k (((i * 4) + 19) * 4) in
      let open Int32 in
      a := rol32 (!a + f2 !b !c !d + w0 + k0) 5 + !b ;
      d := rol32 (!d + f2 !a !b !c + w1 + k1) 9 + !a ;
      c := rol32 (!c + f2 !d !a !b + w2 + k2) 14 + !d ;
      b := rol32 (!b + f2 !c !d !a + w3 + k3) 20 + !c
    done ;
    for i = 0 to 3 do
      let w0 = By.unsafe_get_32 w ((((i * 12) + 5) land 15) * 4) in
      let k0 = By.unsafe_get_32 k (((i * 4) + 32) * 4) in
      let w1 = By.unsafe_get_32 w ((((i * 12) + 8) land 15) * 4) in
      let k1 = By.unsafe_get_32 k (((i * 4) + 33) * 4) in
      let w2 = By.unsafe_get_32 w ((((i * 12) + 11) land 15) * 4) in
      let k2 = By.unsafe_get_32 k (((i * 4) + 34) * 4) in
      let w3 = By.unsafe_get_32 w ((((i * 12) + 14) land 15) * 4) in
      let k3 = By.unsafe_get_32 k (((i * 4) + 35) * 4) in
      let open Int32 in
      a := rol32 (!a + f3 !b !c !d + w0 + k0) 4 + !b ;
      d := rol32 (!d + f3 !a !b !c + w1 + k1) 11 + !a ;
      c := rol32 (!c + f3 !d !a !b + w2 + k2) 16 + !d ;
      b := rol32 (!b + f3 !c !d !a + w3 + k3) 23 + !c
    done ;
    for i = 0 to 3 do
      let w0 = By.unsafe_get_32 w (((i * 12) land 15) * 4) in
      let k0 = By.unsafe_get_32 k (((i * 4) + 48) * 4) in
      let w1 = By.unsafe_get_32 w ((((i * 12) + 7) land 15) * 4) in
      let k1 = By.unsafe_get_32 k (((i * 4) + 49) * 4) in
      let w2 = By.unsafe_get_32 w ((((i * 12) + 14) land 15) * 4) in
      let k2 = By.unsafe_get_32 k (((i * 4) + 50) * 4) in
      let w3 = By.unsafe_get_32 w ((((i * 12) + 21) land 15) * 4) in
      let k3 = By.unsafe_get_32 k (((i * 4) + 51) * 4) in
      let open Int32 in
      a := rol32 (!a + f4 !b !c !d + w0 + k0) 6 + !b ;
      d := rol32 (!d + f4 !a !b !c + w1 + k1) 10 + !a ;
      c := rol32 (!c + f4 !d !a !b + w2 + k2) 15 + !d ;
      b := rol32 (!b + f4 !c !d !a + w3 + k3) 21 + !c
    done ;
    let open Int32 in
    By.unsafe_set_32 ctx.h 0 (By.unsafe_get_32 ctx.h 0 + !a) ;
    By.unsafe_set_32 ctx.h 4 (By.unsafe_get_32 ctx.h 4 + !b) ;
    By.unsafe_set_32 ctx.h 8 (By.unsafe_get_32 ctx.h 8 + !c) ;
    By.unsafe_set_32 ctx.h 12 (By.unsafe_get_32 ctx.h 12 + !d) ;
    ()

  let feed : type a.
      blit:(a -> int -> By.t -> int -> int -> unit) ->
      le32_to_cpu:(a -> int -> int32) ->
      ctx ->
      a ->
      int ->
      int ->
      unit =
   fun ~blit ~le32_to_cpu ctx buf off len ->
    let idx = ref Int64.(to_int (ctx.size land 0x3FL)) in
    let len = ref len in
    let off = ref off in
    let to_fill = 64 - !idx in
    ctx.size <- Int64.add ctx.size (Int64.of_int !len) ;
    if !idx <> 0 && !len >= to_fill
    then (
      blit buf !off ctx.b !idx to_fill ;
      md5_do_chunk ~le32_to_cpu:By.le32_to_cpu ctx ctx.b 0 ;
      len := !len - to_fill ;
      off := !off + to_fill ;
      idx := 0) ;
    while !len >= 64 do
      md5_do_chunk ~le32_to_cpu ctx buf !off ;
      len := !len - 64 ;
      off := !off + 64
    done ;
    if !len <> 0 then blit buf !off ctx.b !idx !len ;
    ()

  let unsafe_feed_bytes = feed ~blit:By.blit ~le32_to_cpu:By.le32_to_cpu

  let unsafe_feed_bigstring =
    feed ~blit:By.blit_from_bigstring ~le32_to_cpu:Bi.le32_to_cpu

  let unsafe_get ctx =
    let index = Int64.(to_int (ctx.size land 0x3FL)) in
    let padlen = if index < 56 then 56 - index else 64 + 56 - index in
    let padding = By.init padlen (function 0 -> '\x80' | _ -> '\x00') in
    let bits = By.create 8 in
    By.cpu_to_le64 bits 0 Int64.(ctx.size lsl 3) ;
    unsafe_feed_bytes ctx padding 0 padlen ;
    unsafe_feed_bytes ctx bits 0 8 ;
    let res = By.create (4 * 4) in
    for i = 0 to 3 do
      By.cpu_to_le32 res (i * 4) (By.unsafe_get_32 ctx.h (i * 4))
    done ;
    res
end
