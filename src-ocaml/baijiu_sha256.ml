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
  let[@inline] ror32 a n = (a lsr n) lor (a lsl (32 - n))
end

module Int64 = struct
  include Int64

  external ( land ) : int64 -> int64 -> int64 = "%int64_and"
  external ( lsl ) : int64 -> int -> int64 = "%int64_lsl"
end

module type S = sig
  type kind = [ `SHA256 ]
  type ctx = { mutable size : int64; b : Bytes.t; h : Bytes.t }

  val of_array : int32 array -> Bytes.t
  val init : unit -> ctx
  val unsafe_feed_bytes : ctx -> By.t -> int -> int -> unit
  val unsafe_feed_bigstring : ctx -> Bi.t -> int -> int -> unit
  val unsafe_get : ctx -> By.t
  val dup : ctx -> ctx
end

module Unsafe : S = struct
  type kind = [ `SHA256 ]
  type ctx = { mutable size : int64; b : Bytes.t; h : Bytes.t }

  let of_array a =
    let b = By.create (4 * Array.length a) in
    Array.iteri (fun i x -> By.unsafe_set_32 b (i * 4) x) a ;
    b

  let dup ctx = { size = ctx.size; b = By.copy ctx.b; h = By.copy ctx.h }

  let init () =
    let b = By.make 128 '\x00' in
    {
      size = 0L;
      b;
      h =
        of_array
          [|
            0x6a09e667l; 0xbb67ae85l; 0x3c6ef372l; 0xa54ff53al; 0x510e527fl;
            0x9b05688cl; 0x1f83d9abl; 0x5be0cd19l;
          |];
    }

  let k =
    of_array
      [|
        0x428a2f98l; 0x71374491l; 0xb5c0fbcfl; 0xe9b5dba5l; 0x3956c25bl;
        0x59f111f1l; 0x923f82a4l; 0xab1c5ed5l; 0xd807aa98l; 0x12835b01l;
        0x243185bel; 0x550c7dc3l; 0x72be5d74l; 0x80deb1fel; 0x9bdc06a7l;
        0xc19bf174l; 0xe49b69c1l; 0xefbe4786l; 0x0fc19dc6l; 0x240ca1ccl;
        0x2de92c6fl; 0x4a7484aal; 0x5cb0a9dcl; 0x76f988dal; 0x983e5152l;
        0xa831c66dl; 0xb00327c8l; 0xbf597fc7l; 0xc6e00bf3l; 0xd5a79147l;
        0x06ca6351l; 0x14292967l; 0x27b70a85l; 0x2e1b2138l; 0x4d2c6dfcl;
        0x53380d13l; 0x650a7354l; 0x766a0abbl; 0x81c2c92el; 0x92722c85l;
        0xa2bfe8a1l; 0xa81a664bl; 0xc24b8b70l; 0xc76c51a3l; 0xd192e819l;
        0xd6990624l; 0xf40e3585l; 0x106aa070l; 0x19a4c116l; 0x1e376c08l;
        0x2748774cl; 0x34b0bcb5l; 0x391c0cb3l; 0x4ed8aa4al; 0x5b9cca4fl;
        0x682e6ff3l; 0x748f82eel; 0x78a5636fl; 0x84c87814l; 0x8cc70208l;
        0x90befffal; 0xa4506cebl; 0xbef9a3f7l; 0xc67178f2l;
      |]

  let[@inline] e0 x = Int32.(ror32 x 2 lxor ror32 x 13 lxor ror32 x 22)
  let[@inline] e1 x = Int32.(ror32 x 6 lxor ror32 x 11 lxor ror32 x 25)
  let[@inline] s0 x = Int32.(ror32 x 7 lxor ror32 x 18 lxor (x lsr 3))
  let[@inline] s1 x = Int32.(ror32 x 17 lxor ror32 x 19 lxor (x lsr 10))

  let sha256_do_chunk : type a.
      be32_to_cpu:(a -> int -> int32) -> ctx -> a -> int -> unit =
   fun ~be32_to_cpu ctx buf off ->
    let a = ref (By.unsafe_get_32 ctx.h 0) in
    let b = ref (By.unsafe_get_32 ctx.h 4) in
    let c = ref (By.unsafe_get_32 ctx.h 8) in
    let d = ref (By.unsafe_get_32 ctx.h 12) in
    let e = ref (By.unsafe_get_32 ctx.h 16) in
    let f = ref (By.unsafe_get_32 ctx.h 20) in
    let g = ref (By.unsafe_get_32 ctx.h 24) in
    let h = ref (By.unsafe_get_32 ctx.h 28) in
    let w = By.create (64 * 4) in
    for i = 0 to 15 do
      By.unsafe_set_32 w (i * 4) (be32_to_cpu buf (off + (i * 4)))
    done ;
    for i = 16 to 63 do
      By.unsafe_set_32 w (i * 4)
        Int32.(
          s1 (By.unsafe_get_32 w ((i - 2) * 4))
          + By.unsafe_get_32 w ((i - 7) * 4)
          + s0 (By.unsafe_get_32 w ((i - 15) * 4))
          + By.unsafe_get_32 w ((i - 16) * 4))
    done ;
    for i = 0 to 63 do
      let open Int32 in
      let t1 =
        !h
        + e1 !e
        + (!g lxor (!e land (!f lxor !g)))
        + By.unsafe_get_32 k (i * 4)
        + By.unsafe_get_32 w (i * 4) in
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
    let open Int32 in
    By.unsafe_set_32 ctx.h 0 (By.unsafe_get_32 ctx.h 0 + !a) ;
    By.unsafe_set_32 ctx.h 4 (By.unsafe_get_32 ctx.h 4 + !b) ;
    By.unsafe_set_32 ctx.h 8 (By.unsafe_get_32 ctx.h 8 + !c) ;
    By.unsafe_set_32 ctx.h 12 (By.unsafe_get_32 ctx.h 12 + !d) ;
    By.unsafe_set_32 ctx.h 16 (By.unsafe_get_32 ctx.h 16 + !e) ;
    By.unsafe_set_32 ctx.h 20 (By.unsafe_get_32 ctx.h 20 + !f) ;
    By.unsafe_set_32 ctx.h 24 (By.unsafe_get_32 ctx.h 24 + !g) ;
    By.unsafe_set_32 ctx.h 28 (By.unsafe_get_32 ctx.h 28 + !h) ;
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
      sha256_do_chunk ~be32_to_cpu:By.be32_to_cpu ctx ctx.b 0 ;
      len := !len - to_fill ;
      off := !off + to_fill ;
      idx := 0) ;
    while !len >= 64 do
      sha256_do_chunk ~be32_to_cpu ctx buf !off ;
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
    let res = By.create (8 * 4) in
    for i = 0 to 7 do
      By.cpu_to_be32 res (i * 4) (By.unsafe_get_32 ctx.h (i * 4))
    done ;
    res
end
