module B = Digestif_bigstring

(* XXX(dinosaure): don't use any external stub in this module. *)

include Bytes

type v = { off : int
          ; len : int
          ; buf : t }

let get_u8 : t -> int -> int = fun s i -> Char.code @@ get s i
external get_u16 : t -> int -> int   = "%caml_string_get16u"
external get_u32 : t -> int -> int32 = "%caml_string_get32u"
external get_u64 : t -> int -> int64 = "%caml_string_get64u"
external bi_get_u16 : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t -> int -> int   = "%caml_bigstring_get16u"
external bi_get_u32 : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t -> int -> int32 = "%caml_bigstring_get32u"
external bi_get_u64 : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t -> int -> int64 = "%caml_bigstring_get64u"
let get_nat : t -> int -> nativeint = fun s i ->
  if Sys.word_size = 32
  then Nativeint.of_int32 @@ get_u32 s i
  else Int64.to_nativeint @@ get_u64 s i
let set_u8 : t -> int -> int -> unit = fun s i v -> set s i (Char.unsafe_chr v)
external set_u16 : t -> int -> int -> unit   = "%caml_string_set16u"
external set_u32 : t -> int -> int32 -> unit = "%caml_string_set32u"
external set_u64 : t -> int -> int64 -> unit = "%caml_string_set64u"
let set_nat : t -> int -> nativeint -> unit = fun s i v ->
  if Sys.word_size = 32
  then set_u32 s i (Nativeint.to_int32 v)
  else set_u64 s i (Int64.of_nativeint v)

let blit_from_bigstring src src_off dst dst_off len =
  for i = 0 to len - 1
  do set dst (dst_off + i) (B.get src (src_off + i)) done

let blit_from_bytes = blit

let rpad a size x =
  let l = length a
  and b = create size in
  blit a 0 b 0 l;
  fill b l (size - l) x;
  b

let eq a b = Bytes.compare a b = 0
let neq a b = not (eq a b)

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

let be32_from_bytes_to_cpu = be32_to_cpu

let be32_from_bigstring_to_cpu s i =
  if Sys.big_endian
  then bi_get_u32 s i
  else swap32 @@ bi_get_u32 s i

let le32_to_cpu s i =
  if Sys.big_endian
  then swap32 @@ get_u32 s i
  else get_u32 s i

let le32_from_bytes_to_cpu = le32_to_cpu

let le32_from_bigstring_to_cpu s i =
  if Sys.big_endian
  then swap32 @@ bi_get_u32 s i
  else bi_get_u32 s i

let be64_to_cpu s i =
  if Sys.big_endian
  then get_u64 s i
  else swap64 @@ get_u64 s i

let be64_from_bytes_to_cpu = be64_to_cpu

let be64_from_bigstring_to_cpu s i =
  if Sys.big_endian
  then bi_get_u64 s i
  else swap64 @@ bi_get_u64 s i

let le64_to_cpu s i =
  if Sys.big_endian
  then swap64 @@ get_u64 s i
  else get_u64 s i

let le64_from_bytes_to_cpu = le64_to_cpu

let le64_from_bigstring_to_cpu s i =
  if Sys.big_endian
  then swap64 @@ bi_get_u64 s i
  else bi_get_u64 s i

let benat_to_cpu s i =
  if Sys.big_endian
  then get_nat s i
  else swapnat @@ get_nat s i

let cpu_to_benat s i v =
  if Sys.big_endian
  then set_nat s i v
  else set_nat s i (swapnat v)
