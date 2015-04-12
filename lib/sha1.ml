(* Hello I'm a really ugly OCaml library *)

let ( +| ) = Int32.add
let ( land ) = Int32.logand
let ( lor ) = Int32.logor
let ( lnot ) = Int32.lognot
let ( lxor ) = Int32.logxor
let ( lsr ) = Int32.shift_right_logical
let ( lsl ) = Int32.shift_left
let ( <<< ) a b = (a lsl b) lor (a lsr (32 - b)) (* rotate left *)

let hash512 array h0 h1 h2 h3 h4 msg =
  let init16 () =
    for i = 0 to 15 do
      (Int32.of_int (Char.code msg.[i * 4 + 3])) lor
      ((Int32.of_int (Char.code msg.[i * 4 + 2])) lsl 8) lor
      ((Int32.of_int (Char.code msg.[i * 4 + 1])) lsl 16) lor
      ((Int32.of_int (Char.code msg.[i * 4])) lsl 24)
      |> Array.set array i
    done
  in
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
  let ch x y z = (x land y) lor ((lnot x) land z)
  in
  let maj x y z = (x land y) lor (x land z) lor (y land z)
  in
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
  init16 ();
  genwords ();
  loop ();
  h0 := !a +| !h0;
  h1 := !b +| !h1;
  h2 := !c +| !h2;
  h3 := !d +| !h3;
  h4 := !e +| !h4

let bitstring_of_int64 n =
  let buff = Bytes.create 8 in
  let ( lsr ) = Int64.shift_right_logical in
  let ( land ) = Int64.logand in
  let extract i =
    buff.[i] <- char_of_int (Int64.to_int ((n lsr ((7 - i) * 8)) land 0xFFL))
  in
  extract 7;
  extract 6;
  extract 5;
  extract 4;
  extract 3;
  extract 2;
  extract 1;
  extract 0;
  buff

let padding m =
  let size = Bytes.length m in
  let extsize = 
    if (size mod 64) * 8 >= 448 then 128 - size mod 64
    else 64 - size mod 64 
  in
  let m = Bytes.extend m 0 extsize in
  m.[size] <- '\x80';
  for i = size + 1 to Bytes.length m - 9 do
    m.[i] <- '\x00'
  done;
  Bytes.blit 
    (bitstring_of_int64 (Int64.of_int (size * 8))) 0 m (Bytes.length m - 8) 8;
  m

let encrypt m =
  let bitstringify b i n =
    let extract_byte k =
      char_of_int (Int32.to_int ((n lsr (k * 8)) land 0xFFl))
    in
    b.[i]     <- extract_byte 3;
    b.[i + 1] <- extract_byte 2;
    b.[i + 2] <- extract_byte 1;
    b.[i + 3] <- extract_byte 0
  in
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

module Utils = struct
  let sha1_to_hexstring ?(case=`Lower) s =
    if String.length s <> 20 then invalid_arg "Sha-1 digest expected."
    else
    let sprint = match case with
      | `Lower -> Printf.sprintf "%08lx%08lx%08lx%08lx%08lx"
      | `Upper -> Printf.sprintf "%08lX%08lX%08lX%08lX%08lX"
    in
    let extract_int32 i =
      let b1 = int_of_char s.[i] in
      let b2 = int_of_char s.[i + 1] in
      let b3 = int_of_char s.[i + 2] in
      let b4 = int_of_char s.[i + 3] in
      Int32.of_int 
	Pervasives.(((b1 land 0xFF) lsl 24) lor (((b2 land 0xFF) lsl 16))
		    lor (((b3 land 0xFF) lsl 8)) lor (b4 land 0xFF))
    in
    let n1 = extract_int32 0 in
    let n2 = extract_int32 4 in
    let n3 = extract_int32 8 in
    let n4 = extract_int32 12 in
    let n5 = extract_int32 16 in
    sprint n1 n2 n3 n4 n5
end
