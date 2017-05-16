module Int32 =
struct
  include Int32

  let ( lsl ) = Int32.shift_left
  let ( lsr ) = Int32.shift_right
  let ( srl ) = Int32.shift_right_logical
  let ( lor ) = Int32.logor
  let ( lxor ) = Int32.logxor
  let ( land ) = Int32.logand
  let ( + ) = Int32.add
  let rol32 a n =
    (a lsl n) lor (srl a (32 - n))
end

module Int64 =
struct
  include Int64

  let ( land ) = Int64.logand
  let ( lsl ) = Int64.shift_left
end

module type S =
sig
  type t
  type ctx
  type buffer

  val init : unit -> ctx
  val feed : ctx -> t -> int -> int -> unit
  val get  : ctx -> t
end

module Make (B : Baijiu_buffer.S)
  : S with type buffer = B.buffer and type t = B.buffer
  = struct
  type t = B.buffer
  type buffer = B.buffer
  type ctx =
    { mutable size : int64
    ; b            : buffer
    ; h            : int32 array }

  let init () =
    let b = B.create 64 in

    B.fill b 0 64 '\x00';

    { size   = 0L
    ; b
    ; h = [| 0x67452301l
           ; 0xefcdab89l
           ; 0x98badcfel
           ; 0x10325476l
           ; 0xc3d2e1f0l |] }

  let f1 x y z = Int32.(z lxor (x land (y lxor z)))
  let f2 x y z = Int32.(x lxor y lxor z)
  let f3 x y z = Int32.((x land y) + (z land (x lxor y)))
  let f4 = f2

  let k1 = 0x5a827999l
  let k2 = 0x6ed9eba1l
  let k3 = 0x8f1bbcdcl
  let k4 = 0xca62c1d6l

  let sha1_do_chunk ctx buf off =
    let a = ref ctx.h.(0) in
    let b = ref ctx.h.(1) in
    let c = ref ctx.h.(2) in
    let d = ref ctx.h.(3) in
    let e = ref ctx.h.(4) in
    let w = Array.make 16 0l in

    let m i =
      let ( && ) a b = a land b in
      let ( -- ) a b = a - b in

      let v = Int32.(rol32 (w.(i && 0x0F) lxor w.((i -- 14) && 0x0F)
                            lxor w.((i -- 8) && 0x0F) lxor w.((i -- 3) && 0x0F)) 1)
      in

      w.(i land 0x0F) <- v;
      w.(i land 0x0F)
    in

    let round a b c d e f k w =
      e := Int32.(!e + rol32 !a 5 + (f !b !c !d) + k + w);
      b := Int32.(rol32 !b 30)
    in

    for i = 0 to 15
    do w.(i) <- B.be32_to_cpu buf (off + (i * 4)) done;

    round a b c d e f1 k1 w.(0);
    round e a b c d f1 k1 w.(1);
    round d e a b c f1 k1 w.(2);
    round c d e a b f1 k1 w.(3);
    round b c d e a f1 k1 w.(4);
    round a b c d e f1 k1 w.(5);
    round e a b c d f1 k1 w.(6);
    round d e a b c f1 k1 w.(7);
    round c d e a b f1 k1 w.(8);
    round b c d e a f1 k1 w.(9);
    round a b c d e f1 k1 w.(10);
    round e a b c d f1 k1 w.(11);
    round d e a b c f1 k1 w.(12);
    round c d e a b f1 k1 w.(13);
    round b c d e a f1 k1 w.(14);
    round a b c d e f1 k1 w.(15);
    round e a b c d f1 k1 (m 16);
    round d e a b c f1 k1 (m 17);
    round c d e a b f1 k1 (m 18);
    round b c d e a f1 k1 (m 19);

    round a b c d e f2 k2 (m 20);
    round e a b c d f2 k2 (m 21);
    round d e a b c f2 k2 (m 22);
    round c d e a b f2 k2 (m 23);
    round b c d e a f2 k2 (m 24);
    round a b c d e f2 k2 (m 25);
    round e a b c d f2 k2 (m 26);
    round d e a b c f2 k2 (m 27);
    round c d e a b f2 k2 (m 28);
    round b c d e a f2 k2 (m 29);
    round a b c d e f2 k2 (m 30);
    round e a b c d f2 k2 (m 31);
    round d e a b c f2 k2 (m 32);
    round c d e a b f2 k2 (m 33);
    round b c d e a f2 k2 (m 34);
    round a b c d e f2 k2 (m 35);
    round e a b c d f2 k2 (m 36);
    round d e a b c f2 k2 (m 37);
    round c d e a b f2 k2 (m 38);
    round b c d e a f2 k2 (m 39);

    round a b c d e f3 k3 (m 40);
    round e a b c d f3 k3 (m 41);
    round d e a b c f3 k3 (m 42);
    round c d e a b f3 k3 (m 43);
    round b c d e a f3 k3 (m 44);
    round a b c d e f3 k3 (m 45);
    round e a b c d f3 k3 (m 46);
    round d e a b c f3 k3 (m 47);
    round c d e a b f3 k3 (m 48);
    round b c d e a f3 k3 (m 49);
    round a b c d e f3 k3 (m 50);
    round e a b c d f3 k3 (m 51);
    round d e a b c f3 k3 (m 52);
    round c d e a b f3 k3 (m 53);
    round b c d e a f3 k3 (m 54);
    round a b c d e f3 k3 (m 55);
    round e a b c d f3 k3 (m 56);
    round d e a b c f3 k3 (m 57);
    round c d e a b f3 k3 (m 58);
    round b c d e a f3 k3 (m 59);

    round a b c d e f4 k4 (m 60);
    round e a b c d f4 k4 (m 61);
    round d e a b c f4 k4 (m 62);
    round c d e a b f4 k4 (m 63);
    round b c d e a f4 k4 (m 64);
    round a b c d e f4 k4 (m 65);
    round e a b c d f4 k4 (m 66);
    round d e a b c f4 k4 (m 67);
    round c d e a b f4 k4 (m 68);
    round b c d e a f4 k4 (m 69);
    round a b c d e f4 k4 (m 70);
    round e a b c d f4 k4 (m 71);
    round d e a b c f4 k4 (m 72);
    round c d e a b f4 k4 (m 73);
    round b c d e a f4 k4 (m 74);
    round a b c d e f4 k4 (m 75);
    round e a b c d f4 k4 (m 76);
    round d e a b c f4 k4 (m 77);
    round c d e a b f4 k4 (m 78);
    round b c d e a f4 k4 (m 79);

    ctx.h.(0) <- Int32.add ctx.h.(0) !a;
    ctx.h.(1) <- Int32.add ctx.h.(1) !b;
    ctx.h.(2) <- Int32.add ctx.h.(2) !c;
    ctx.h.(3) <- Int32.add ctx.h.(3) !d;
    ctx.h.(4) <- Int32.add ctx.h.(4) !e;

    ()

  let feed ctx buf off len =
    let idx = ref Int64.(to_int (ctx.size land 0x3FL)) in
    let len = ref len in
    let off = ref off in

    let to_fill = (64 - !idx) in

    ctx.size <- Int64.add ctx.size (Int64.of_int !len);

    if !idx <> 0 && !len >= to_fill
    then begin
      B.blit buf !off ctx.b !idx to_fill;
      sha1_do_chunk ctx ctx.b 0;
      len := !len - to_fill;
      off := !off + to_fill;
      idx := 0;
    end;

    while !len >= 64
    do sha1_do_chunk ctx buf !off;
      len := !len - 64;
      off := !off + 64;
    done;

    if !len <> 0
    then B.blit buf !off ctx.b !idx !len;

    ()

  let get ctx =
    let padding = B.create 64 in
    let bits = B.create 8 in
    let res = B.create (5 * 4) in

    B.set padding 0 '\x80';
    B.fill padding 1 63 '\x00';
    B.cpu_to_be64 bits 0 Int64.(ctx.size lsl 3);

    let index = Int64.(to_int (ctx.size land 0x3FL)) in
    let padlen = if index < 56 then 56 - index else (64 + 56) - index in

    feed ctx padding 0 padlen;
    feed ctx bits 0 8;

    for i = 0 to 4
    do B.cpu_to_be32 res (i * 4) ctx.h.(i) done;

    res
end
