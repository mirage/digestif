let ( +| ) = Int32.add
let ( land ) = Int32.logand
let ( lor ) = Int32.logor
let lnot = Int32.lognot
let ( lxor ) = Int32.logxor
let ( lsr ) = Int32.shift_right_logical
let ( lsl ) = Int32.shift_left
let ( <<< ) a b = (a lsl b) lor (a lsr (32 - b)) (* rotate left *)
let ( >>> ) a b = (a lsr b) lor (a lsl (32 - b)) (* rotate right *)

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

let init16 array msg =
  for i = 0 to 15 do
    (Int32.of_int (Char.code msg.[i * 4 + 3])) lor
      ((Int32.of_int (Char.code msg.[i * 4 + 2])) lsl 8) lor
	((Int32.of_int (Char.code msg.[i * 4 + 1])) lsl 16) lor
	  ((Int32.of_int (Char.code msg.[i * 4])) lsl 24)
    |> Array.set array i
  done

let bitstringify b i n =
  let extract_byte k =
    char_of_int (Int32.to_int ((n lsr (k * 8)) land 0xFFl))
  in
  b.[i]     <- extract_byte 3;
  b.[i + 1] <- extract_byte 2;
  b.[i + 2] <- extract_byte 1;
  b.[i + 3] <- extract_byte 0

let digest_to_hexstring  len name ?(case=`Lower) s =
  let buff = Buffer.create (len / 4) in
  if String.length s <> len then invalid_arg (name ^ " digest expected.")
  else
    let bprint = match case with
      | `Lower -> Printf.bprintf buff "%08lx"
      | `Upper -> Printf.bprintf buff "%08lX"
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
    let rec extract_hexstring i =
      if i * 4 = len then
	Buffer.contents buff
      else
        (bprint (extract_int32 (i * 4));
         extract_hexstring (succ i))
    in extract_hexstring 0
