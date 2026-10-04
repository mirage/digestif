module By = Digestif_by
module Bi = Digestif_bi

module type S = sig
  type ctx
  type kind = [ `RMD160 ]

  val init : unit -> ctx
  val unsafe_feed_bytes : ctx -> By.t -> int -> int -> unit
  val unsafe_feed_bigstring : ctx -> Bi.t -> int -> int -> unit
  val unsafe_get : ctx -> By.t
  val dup : ctx -> ctx
end

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

module Unsafe : S = struct
  type kind = [ `RMD160 ]
  type ctx = { s : int32 array; mutable n : int; h : Bytes.t; b : Bytes.t }

  let dup ctx =
    { s = Array.copy ctx.s; n = ctx.n; h = By.copy ctx.h; b = By.copy ctx.b }

  let init () =
    let b = By.make 64 '\x00' in
    let h = By.create (5 * 4) in
    By.unsafe_set_32 h 0 0x67452301l ;
    By.unsafe_set_32 h 4 0xefcdab89l ;
    By.unsafe_set_32 h 8 0x98badcfel ;
    By.unsafe_set_32 h 12 0x10325476l ;
    By.unsafe_set_32 h 16 0xc3d2e1f0l ;
    { s = [| 0l; 0l |]; n = 0; b; h }

  let[@inline] f x y z = Int32.(x lxor y lxor z)
  let[@inline] g x y z = Int32.(x land y lor (lnot x land z))
  let[@inline] h x y z = Int32.(x lor lnot y lxor z)
  let[@inline] i x y z = Int32.(x land z lor (y land lnot z))
  let[@inline] j x y z = Int32.(x lxor (y lor lnot z))

  let rl =
    [|
      0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 7; 4; 13; 1; 10; 6;
      15; 3; 12; 0; 9; 5; 2; 14; 11; 8; 3; 10; 14; 4; 9; 15; 8; 1; 2; 7; 0; 6;
      13; 11; 5; 12; 1; 9; 11; 10; 0; 8; 12; 4; 13; 3; 7; 15; 14; 5; 6; 2; 4; 0;
      5; 9; 7; 12; 2; 10; 14; 1; 3; 8; 11; 6; 15; 13;
    |]

  let sl =
    [|
      11; 14; 15; 12; 5; 8; 7; 9; 11; 13; 14; 15; 6; 7; 9; 8; 7; 6; 8; 13; 11;
      9; 7; 15; 7; 12; 15; 9; 11; 7; 13; 12; 11; 13; 6; 7; 14; 9; 13; 15; 14; 8;
      13; 6; 5; 12; 7; 5; 11; 12; 14; 15; 14; 15; 9; 8; 9; 14; 5; 6; 8; 6; 5;
      12; 9; 15; 5; 11; 6; 8; 13; 12; 5; 12; 13; 14; 11; 8; 5; 6;
    |]

  let rr =
    [|
      5; 14; 7; 0; 9; 2; 11; 4; 13; 6; 15; 8; 1; 10; 3; 12; 6; 11; 3; 7; 0; 13;
      5; 10; 14; 15; 8; 12; 4; 9; 1; 2; 15; 5; 1; 3; 7; 14; 6; 9; 11; 8; 12; 2;
      10; 0; 4; 13; 8; 6; 4; 1; 3; 11; 15; 0; 5; 12; 2; 13; 9; 7; 10; 14; 12;
      15; 10; 4; 1; 5; 8; 7; 6; 2; 13; 14; 0; 3; 9; 11;
    |]

  let sr =
    [|
      8; 9; 9; 11; 13; 15; 15; 5; 7; 7; 8; 11; 14; 14; 12; 6; 9; 13; 15; 7; 12;
      8; 9; 11; 7; 7; 12; 7; 6; 15; 13; 11; 9; 7; 15; 11; 8; 6; 6; 14; 12; 13;
      5; 14; 13; 13; 7; 5; 15; 5; 8; 11; 14; 14; 6; 14; 6; 9; 12; 9; 12; 5; 15;
      8; 8; 5; 12; 9; 12; 5; 14; 6; 8; 13; 6; 5; 15; 13; 11; 11;
    |]

  let rmd160_do_chunk : type a.
      le32_to_cpu:(a -> int -> int32) -> ctx -> a -> int -> unit =
   fun ~le32_to_cpu ctx buff off ->
    let aa = ref (By.unsafe_get_32 ctx.h 0) in
    let bb = ref (By.unsafe_get_32 ctx.h 4) in
    let cc = ref (By.unsafe_get_32 ctx.h 8) in
    let dd = ref (By.unsafe_get_32 ctx.h 12) in
    let ee = ref (By.unsafe_get_32 ctx.h 16) in
    let aaa = ref (By.unsafe_get_32 ctx.h 0) in
    let bbb = ref (By.unsafe_get_32 ctx.h 4) in
    let ccc = ref (By.unsafe_get_32 ctx.h 8) in
    let ddd = ref (By.unsafe_get_32 ctx.h 12) in
    let eee = ref (By.unsafe_get_32 ctx.h 16) in
    let w = By.create (16 * 4) in
    for i = 0 to 15 do
      By.unsafe_set_32 w (i * 4) (le32_to_cpu buff (off + (i * 4)))
    done ;
    for n = 0 to 15 do
      let x = By.unsafe_get_32 w (rl.(n) * 4) in
      let xxx = By.unsafe_get_32 w (rr.(n) * 4) in
      let open Int32 in
      let t = rol32 (!aa + f !bb !cc !dd + x) sl.(n) + !ee in
      aa := !ee ;
      ee := !dd ;
      dd := rol32 !cc 10 ;
      cc := !bb ;
      bb := t ;
      let ttt =
        rol32 (!aaa + j !bbb !ccc !ddd + xxx + 0x50a28be6l) sr.(n) + !eee in
      aaa := !eee ;
      eee := !ddd ;
      ddd := rol32 !ccc 10 ;
      ccc := !bbb ;
      bbb := ttt
    done ;
    for n = 16 to 31 do
      let x = By.unsafe_get_32 w (rl.(n) * 4) in
      let xxx = By.unsafe_get_32 w (rr.(n) * 4) in
      let open Int32 in
      let t = rol32 (!aa + g !bb !cc !dd + x + 0x5a827999l) sl.(n) + !ee in
      aa := !ee ;
      ee := !dd ;
      dd := rol32 !cc 10 ;
      cc := !bb ;
      bb := t ;
      let ttt =
        rol32 (!aaa + i !bbb !ccc !ddd + xxx + 0x5c4dd124l) sr.(n) + !eee in
      aaa := !eee ;
      eee := !ddd ;
      ddd := rol32 !ccc 10 ;
      ccc := !bbb ;
      bbb := ttt
    done ;
    for n = 32 to 47 do
      let x = By.unsafe_get_32 w (rl.(n) * 4) in
      let xxx = By.unsafe_get_32 w (rr.(n) * 4) in
      let open Int32 in
      let t = rol32 (!aa + h !bb !cc !dd + x + 0x6ed9eba1l) sl.(n) + !ee in
      aa := !ee ;
      ee := !dd ;
      dd := rol32 !cc 10 ;
      cc := !bb ;
      bb := t ;
      let ttt =
        rol32 (!aaa + h !bbb !ccc !ddd + xxx + 0x6d703ef3l) sr.(n) + !eee in
      aaa := !eee ;
      eee := !ddd ;
      ddd := rol32 !ccc 10 ;
      ccc := !bbb ;
      bbb := ttt
    done ;
    for n = 48 to 63 do
      let x = By.unsafe_get_32 w (rl.(n) * 4) in
      let xxx = By.unsafe_get_32 w (rr.(n) * 4) in
      let open Int32 in
      let t = rol32 (!aa + i !bb !cc !dd + x + 0x8f1bbcdcl) sl.(n) + !ee in
      aa := !ee ;
      ee := !dd ;
      dd := rol32 !cc 10 ;
      cc := !bb ;
      bb := t ;
      let ttt =
        rol32 (!aaa + g !bbb !ccc !ddd + xxx + 0x7a6d76e9l) sr.(n) + !eee in
      aaa := !eee ;
      eee := !ddd ;
      ddd := rol32 !ccc 10 ;
      ccc := !bbb ;
      bbb := ttt
    done ;
    for n = 64 to 79 do
      let x = By.unsafe_get_32 w (rl.(n) * 4) in
      let xxx = By.unsafe_get_32 w (rr.(n) * 4) in
      let open Int32 in
      let t = rol32 (!aa + j !bb !cc !dd + x + 0xa953fd4el) sl.(n) + !ee in
      aa := !ee ;
      ee := !dd ;
      dd := rol32 !cc 10 ;
      cc := !bb ;
      bb := t ;
      let ttt = rol32 (!aaa + f !bbb !ccc !ddd + xxx) sr.(n) + !eee in
      aaa := !eee ;
      eee := !ddd ;
      ddd := rol32 !ccc 10 ;
      ccc := !bbb ;
      bbb := ttt
    done ;
    let open Int32 in
    ddd := !ddd + !cc + By.unsafe_get_32 ctx.h 4 ;
    (* final result for h[0]. *)
    By.unsafe_set_32 ctx.h 4 (By.unsafe_get_32 ctx.h 8 + !dd + !eee) ;
    By.unsafe_set_32 ctx.h 8 (By.unsafe_get_32 ctx.h 12 + !ee + !aaa) ;
    By.unsafe_set_32 ctx.h 12 (By.unsafe_get_32 ctx.h 16 + !aa + !bbb) ;
    By.unsafe_set_32 ctx.h 16 (By.unsafe_get_32 ctx.h 0 + !bb + !ccc) ;
    By.unsafe_set_32 ctx.h 0 !ddd ;
    ()

  exception Leave

  let feed : type a.
      le32_to_cpu:(a -> int -> int32) ->
      blit:(a -> int -> By.t -> int -> int -> unit) ->
      ctx ->
      a ->
      int ->
      int ->
      unit =
   fun ~le32_to_cpu ~blit ctx buf off len ->
    let t = ref ctx.s.(0) in
    let off = ref off in
    let len = ref len in
    ctx.s.(0) <- Int32.add !t (Int32.of_int (!len lsl 3)) ;
    if Int32.unsigned_compare ctx.s.(0) !t < 0
    then ctx.s.(1) <- Int32.(ctx.s.(1) + 1l) ;
    ctx.s.(1) <- Int32.add ctx.s.(1) (Int32.of_int (!len lsr 29)) ;
    try
      if ctx.n <> 0
      then (
        let t = 64 - ctx.n in
        if !len < t
        then (
          blit buf !off ctx.b ctx.n !len ;
          ctx.n <- ctx.n + !len ;
          raise Leave) ;
        blit buf !off ctx.b ctx.n t ;
        rmd160_do_chunk ~le32_to_cpu:By.le32_to_cpu ctx ctx.b 0 ;
        off := !off + t ;
        len := !len - t) ;
      while !len >= 64 do
        rmd160_do_chunk ~le32_to_cpu ctx buf !off ;
        off := !off + 64 ;
        len := !len - 64
      done ;
      blit buf !off ctx.b 0 !len ;
      ctx.n <- !len
    with Leave -> ()

  let unsafe_feed_bytes ctx buf off len =
    feed ~blit:By.blit ~le32_to_cpu:By.le32_to_cpu ctx buf off len

  let unsafe_feed_bigstring ctx buf off len =
    feed ~blit:By.blit_from_bigstring ~le32_to_cpu:Bi.le32_to_cpu ctx buf off
      len

  let unsafe_get ctx =
    let i = ref (ctx.n + 1) in
    let res = By.create (5 * 4) in
    By.set ctx.b ctx.n '\x80' ;
    if !i > 56
    then (
      By.fill ctx.b !i (64 - !i) '\x00' ;
      rmd160_do_chunk ~le32_to_cpu:By.le32_to_cpu ctx ctx.b 0 ;
      i := 0) ;
    By.fill ctx.b !i (56 - !i) '\x00' ;
    By.cpu_to_le32 ctx.b 56 ctx.s.(0) ;
    By.cpu_to_le32 ctx.b 60 ctx.s.(1) ;
    rmd160_do_chunk ~le32_to_cpu:By.le32_to_cpu ctx ctx.b 0 ;
    for i = 0 to 4 do
      By.cpu_to_le32 res (i * 4) (By.unsafe_get_32 ctx.h (i * 4))
    done ;
    res
end
