open Bigarray

(* XXX(dinosaure): don't use any external stub in this module. *)

type t = (char, int8_unsigned_elt, c_layout) Array1.t
type v = { off : int
         ; len : int
         ; buf : t }

let length = Array1.dim
let create = Array1.create Char c_layout
let get    = Array1.get
let set    = Array1.set
let sub    = Array1.sub
let init l f =
  let v = create l in
  for i = 0 to l - 1 do set v i (f i) done;
  v
let fill x off len v =
  let x' = sub x off len in
  Array1.fill x' v
let copy v =
  let v' = create (length v) in
  Array1.blit v v'; v'

let get_u8 : t -> int -> int = fun s i -> Char.code @@ get s i
external get_u16 : t -> int -> int   = "%caml_bigstring_get16u"
external get_u32 : t -> int -> int32 = "%caml_bigstring_get32u"
external get_u64 : t -> int -> int64 = "%caml_bigstring_get64u"
external by_get_u16 : Bytes.t -> int -> int   = "%caml_string_get16u"
external by_get_u32 : Bytes.t -> int -> int32 = "%caml_string_get32u"
external by_get_u64 : Bytes.t -> int -> int64 = "%caml_string_get64u"
let get_nat : t -> int -> nativeint = fun s i ->
  if Sys.word_size = 32
  then Nativeint.of_int32 @@ get_u32 s i
  else Int64.to_nativeint @@ get_u64 s i
let set_u8 : t -> int -> int -> unit = fun s i v -> set s i (Char.unsafe_chr v)
external set_u16 : t -> int -> int -> unit   = "%caml_bigstring_set16u"
external set_u32 : t -> int -> int32 -> unit = "%caml_bigstring_set32u"
external set_u64 : t -> int -> int64 -> unit = "%caml_bigstring_set64u"
let set_nat : t -> int -> nativeint -> unit = fun s i v ->
  if Sys.word_size = 32
  then set_u32 s i (Nativeint.to_int32 v)
  else set_u64 s i (Int64.of_nativeint v)

let to_string v =
  let buf = Bytes.create (length v) in
  for i = 0 to length v - 1
  do Bytes.set buf i (get v i) done;
  Bytes.unsafe_to_string buf

let blit src src_off dst dst_off len =
  let a = sub src src_off len in
  let b = sub dst dst_off len in

  Array1.blit a b

let blit_from_bytes src src_off dst dst_off len =
  for i = 0 to len - 1
  do set dst (dst_off + i) (Bytes.get src (src_off + i)) done

let blit_from_bigstring = blit

let rpad a size x =
  let l = length a
  and b = create size in
  blit a 0 b 0 l;
  fill b l (size - l) x;
  b

exception Break of int

let compare a b =
  if length a <> length b
  then (if length a < length b then 0 - (Char.code (get b (length a))) else (Char.code (get a (length b))))
  else
    try
      for i = 0 to length a - 1
      do if get a i <> get b i then raise (Break ((Char.code (get a i)) - (Char.code (get b i)))) done;
      0
    with Break n -> n

let eq a b = compare a b = 0
let neq a b = not (eq a b)

let iter f x =
  let l = length x in

  for i = 0 to l - 1 do f (get x i) done

let empty = create 0

let pp fmt ba =
  for i = 0 to length ba - 1
  do Format.fprintf fmt "%c" (get ba i) done

external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"
external swapnat : nativeint -> nativeint = "%bswap_native"

let cpu_to_be32 s i v =
  if Sys.big_endian
  then set_u32 s i v
  else set_u32 s i (swap32 v)

let cpu_to_le32 s i v =
  if Sys.big_endian
  then set_u32 s i (swap32 v)
  else set_u32 s i v

let cpu_to_be64 s i v =
  if Sys.big_endian
  then set_u64 s i v
  else set_u64 s i (swap64 v)

let cpu_to_le64 s i v =
  if Sys.big_endian
  then set_u64 s i (swap64 v)
  else set_u64 s i v

let be32_to_cpu s i =
  if Sys.big_endian
  then get_u32 s i
  else swap32 @@ get_u32 s i

let be32_from_bigstring_to_cpu = be32_to_cpu

let be32_from_bytes_to_cpu s i =
  if Sys.big_endian
  then by_get_u32 s i
  else swap32 @@ by_get_u32 s i

let le32_to_cpu s i =
  if Sys.big_endian
  then swap32 @@ get_u32 s i
  else get_u32 s i

let le32_from_bigstring_to_cpu = le32_to_cpu

let le32_from_bytes_to_cpu s i =
  if Sys.big_endian
  then swap32 @@ by_get_u32 s i
  else by_get_u32 s i

let be64_to_cpu s i =
  if Sys.big_endian
  then get_u64 s i
  else swap64 @@ get_u64 s i

let be64_from_bigstring_to_cpu = be64_to_cpu

let be64_from_bytes_to_cpu s i =
  if Sys.big_endian
  then by_get_u64 s i
  else swap64 @@ by_get_u64 s i

let le64_to_cpu s i =
  if Sys.big_endian
  then swap64 @@ get_u64 s i
  else get_u64 s i

let le64_from_bigstring_to_cpu = le64_to_cpu

let le64_from_bytes_to_cpu s i =
  if Sys.big_endian
  then swap64 @@ by_get_u64 s i
  else by_get_u64 s i

let benat_to_cpu s i =
  if Sys.big_endian
  then get_nat s i
  else swapnat @@ get_nat s i

let cpu_to_benat s i v =
  if Sys.big_endian
  then set_nat s i v
  else set_nat s i (swapnat v)
