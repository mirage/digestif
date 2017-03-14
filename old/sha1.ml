open Common

let hash512 array h0 h1 h2 h3 h4 msg =
  let genwords () =
    for i = 16 to 79 do
      array.(i) <- 
	array.(i - 3) lxor array.(i - 8)
	lxor 
	  array.(i - 14) lxor array.(i - 16) <<< 1
    done
  in
  let a = ref !h0 in
  let b = ref !h1 in
  let c = ref !h2 in
  let d = ref !h3 in
  let e = ref !h4 in
  let k = ref 0l  in
  let r = ref 0l  in
  let setk t =
    if t >= 0 && t <= 19 then
      k := 0x5A827999l
    else if t >= 20 && t <= 39 then
      k := 0x6ED9EBA1l
    else if t >= 40 && t <= 59 then
      k := 0x8F1BBCDCl
    else if t >= 60 && t <= 79 then
      k := 0xCA62C1D6l
  in
  let parity x y z = x lxor y lxor z in
  let ch x y z = (x land y) lor ((lnot x) land z) in
  let maj x y z = (x land y) lor (x land z) lor (y land z) in
  let shafun t x y z =
    if t >= 0 && t <= 19 then
      r := ch x y z
    else if (t >= 20 && t <= 39) || (t >= 60 && t <= 79) then
      r := parity x y z
    else if t >= 40 && t <= 59 then
      r := maj x y z
  in
  let t = ref 0l in
  let loop () =
    for i = 0 to 79 do
      setk i;
      shafun i !b !c !d;
      t := (!a <<< 5) +| !r +| !e +| !k +| array.(i);
      e := !d;
      d := !c;
      c := !b <<< 30;
      b := !a;
      a := !t
    done
  in
  init16 array msg;
  genwords ();
  loop ();
  h0 := !a +| !h0;
  h1 := !b +| !h1;
  h2 := !c +| !h2;
  h3 := !d +| !h3;
  h4 := !e +| !h4

let encrypt m =
  let m = padding (Bytes.of_string m) in
  let array = Array.make 80 0l in
  let h0 = ref 0x67452301l in
  let h1 = ref 0xEFCDAB89l in
  let h2 = ref 0x98BADCFEl in
  let h3 = ref 0x10325476l in
  let h4 = ref 0xC3D2E1F0l in
  for i = 0 to Bytes.length m / 64 - 1 do
    hash512 array h0 h1 h2 h3 h4 (Bytes.sub m (i * 64) 64);
    Array.fill array 0 80 0l
  done;
  let h = Bytes.create 20 in
  bitstringify h 0 !h0;
  bitstringify h 4 !h1;
  bitstringify h 8 !h2;
  bitstringify h 12 !h3;
  bitstringify h 16 !h4;
  Bytes.to_string h

let sha1_to_hexstring = digest_to_hexstring 20 "SHA-1"

