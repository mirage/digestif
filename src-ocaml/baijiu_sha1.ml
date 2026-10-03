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

  let[@inline] rol32 a n = (a lsl n) lor (a lsr (32 - n))
end

module Int64 = struct
  include Int64

  external ( land ) : int64 -> int64 -> int64 = "%int64_and"
  external ( lsl ) : int64 -> int -> int64 = "%int64_lsl"
end

module type S = sig
  type ctx
  type kind = [ `SHA1 ]

  val init : unit -> ctx
  val unsafe_feed_bytes : ctx -> By.t -> int -> int -> unit
  val unsafe_feed_bigstring : ctx -> Bi.t -> int -> int -> unit
  val unsafe_get : ctx -> By.t
  val dup : ctx -> ctx
end

module Unsafe : S = struct
  type kind = [ `SHA1 ]
  type ctx = { mutable size : int64; b : Bytes.t; h : Bytes.t }

  let dup ctx = { size = ctx.size; b = By.copy ctx.b; h = By.copy ctx.h }

  let init () =
    let b = By.make 64 '\x00' in
    let h = By.create (5 * 4) in
    By.unsafe_set_32 h 0 0x67452301l ;
    By.unsafe_set_32 h 4 0xefcdab89l ;
    By.unsafe_set_32 h 8 0x98badcfel ;
    By.unsafe_set_32 h 12 0x10325476l ;
    By.unsafe_set_32 h 16 0xc3d2e1f0l ;
    { size = 0L; b; h }

  let[@inline] f1 x y z = Int32.(z lxor (x land (y lxor z)))
  let[@inline] f2 x y z = Int32.(x lxor y lxor z)
  let[@inline] f3 x y z = Int32.((x land y) + (z land (x lxor y)))
  let f4 = f2
  let k1 = 0x5a827999l
  let k2 = 0x6ed9eba1l
  let k3 = 0x8f1bbcdcl
  let k4 = 0xca62c1d6l

  let sha1_do_chunk : type a.
      be32_to_cpu:(a -> int -> int32) -> ctx -> a -> int -> unit =
   fun ~be32_to_cpu ctx buf off ->
    let a = ref (By.unsafe_get_32 ctx.h 0) in
    let b = ref (By.unsafe_get_32 ctx.h 4) in
    let c = ref (By.unsafe_get_32 ctx.h 8) in
    let d = ref (By.unsafe_get_32 ctx.h 12) in
    let e = ref (By.unsafe_get_32 ctx.h 16) in
    let w = By.create (80 * 4) in
    for i = 0 to 15 do
      By.unsafe_set_32 w (i * 4) (be32_to_cpu buf (off + (i * 4)))
    done ;
    for i = 16 to 79 do
      By.unsafe_set_32 w (i * 4)
        Int32.(
          rol32
            (By.unsafe_get_32 w ((i - 3) * 4)
            lxor By.unsafe_get_32 w ((i - 8) * 4)
            lxor By.unsafe_get_32 w ((i - 14) * 4)
            lxor By.unsafe_get_32 w ((i - 16) * 4))
            1)
    done ;
    for i = 0 to 19 do
      let t =
        Int32.(
          !e + rol32 !a 5 + f1 !b !c !d + k1 + By.unsafe_get_32 w (i * 4)) in
      e := !d ;
      d := !c ;
      c := Int32.rol32 !b 30 ;
      b := !a ;
      a := t
    done ;
    for i = 20 to 39 do
      let t =
        Int32.(
          !e + rol32 !a 5 + f2 !b !c !d + k2 + By.unsafe_get_32 w (i * 4)) in
      e := !d ;
      d := !c ;
      c := Int32.rol32 !b 30 ;
      b := !a ;
      a := t
    done ;
    for i = 40 to 59 do
      let t =
        Int32.(
          !e + rol32 !a 5 + f3 !b !c !d + k3 + By.unsafe_get_32 w (i * 4)) in
      e := !d ;
      d := !c ;
      c := Int32.rol32 !b 30 ;
      b := !a ;
      a := t
    done ;
    for i = 60 to 79 do
      let t =
        Int32.(
          !e + rol32 !a 5 + f4 !b !c !d + k4 + By.unsafe_get_32 w (i * 4)) in
      e := !d ;
      d := !c ;
      c := Int32.rol32 !b 30 ;
      b := !a ;
      a := t
    done ;
    By.unsafe_set_32 ctx.h 0 (Int32.add (By.unsafe_get_32 ctx.h 0) !a) ;
    By.unsafe_set_32 ctx.h 4 (Int32.add (By.unsafe_get_32 ctx.h 4) !b) ;
    By.unsafe_set_32 ctx.h 8 (Int32.add (By.unsafe_get_32 ctx.h 8) !c) ;
    By.unsafe_set_32 ctx.h 12 (Int32.add (By.unsafe_get_32 ctx.h 12) !d) ;
    By.unsafe_set_32 ctx.h 16 (Int32.add (By.unsafe_get_32 ctx.h 16) !e) ;
    ()

  let feed : type a.
      blit:(a -> int -> By.t -> int -> int -> unit) ->
      be32_to_cpu:(a -> int -> int32) ->
      ctx ->
      a ->
      int ->
      int ->
      unit =
   fun ~blit ~be32_to_cpu ctx buf off len ->
    let idx = ref Int64.(to_int (ctx.size land 0x3FL)) in
    let len = ref len in
    let off = ref off in
    let to_fill = 64 - !idx in
    ctx.size <- Int64.add ctx.size (Int64.of_int !len) ;
    if !idx <> 0 && !len >= to_fill
    then (
      blit buf !off ctx.b !idx to_fill ;
      sha1_do_chunk ~be32_to_cpu:By.be32_to_cpu ctx ctx.b 0 ;
      len := !len - to_fill ;
      off := !off + to_fill ;
      idx := 0) ;
    while !len >= 64 do
      sha1_do_chunk ~be32_to_cpu ctx buf !off ;
      len := !len - 64 ;
      off := !off + 64
    done ;
    if !len <> 0 then blit buf !off ctx.b !idx !len ;
    ()

  let unsafe_feed_bytes = feed ~blit:By.blit ~be32_to_cpu:By.be32_to_cpu

  let unsafe_feed_bigstring =
    feed ~blit:By.blit_from_bigstring ~be32_to_cpu:Bi.be32_to_cpu

  let unsafe_get ctx =
    let index = Int64.(to_int (ctx.size land 0x3FL)) in
    let padlen = if index < 56 then 56 - index else 64 + 56 - index in
    let padding = By.init padlen (function 0 -> '\x80' | _ -> '\x00') in
    let bits = By.create 8 in
    By.cpu_to_be64 bits 0 Int64.(ctx.size lsl 3) ;
    unsafe_feed_bytes ctx padding 0 padlen ;
    unsafe_feed_bytes ctx bits 0 8 ;
    let res = By.create (5 * 4) in
    for i = 0 to 4 do
      By.cpu_to_be32 res (i * 4) (By.unsafe_get_32 ctx.h (i * 4))
    done ;
    res
end
