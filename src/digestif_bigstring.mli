open Bigarray
type t = (char, int8_unsigned_elt, c_layout) Array1.t
val length: t -> int
val create: int -> t
val get: t -> int -> char
val set: t -> int -> char -> unit
val sub: t -> int -> int -> t
val init: int -> (int -> char) -> t
val fill: t -> int -> int -> char -> unit
val copy: t  -> t
val get_u8: t -> int -> int
external get_u16: t -> int -> int = "%caml_bigstring_get16u"
external get_u32: t -> int -> int32 = "%caml_bigstring_get32u"
external get_u64: t -> int -> int64 = "%caml_bigstring_get64u"
val get_nat: t -> int -> nativeint
val set_u8: t -> int -> int -> unit
external set_u16: t -> int -> int -> unit = "%caml_bigstring_set16u"
external set_u32: t -> int -> int32 -> unit = "%caml_bigstring_set32u"
external set_u64: t -> int -> int64 -> unit = "%caml_bigstring_set64u"
val set_nat: t -> int -> nativeint -> unit
val to_string: t -> string
val blit: t -> int -> t -> int -> int -> unit
val blit_from_bytes: bytes -> int -> t -> int -> int -> unit
val blit_from_bigstring: t -> int -> t -> int -> int -> unit
val rpad: t -> int -> char -> t
val compare: t -> t -> int
val eq: t -> t -> bool
val neq: t -> t -> bool
val iter: (char -> unit) -> t -> unit
val empty: t
external swap32: int32 -> int32 = "%bswap_int32"
external swap64: int64 -> int64 = "%bswap_int64"
external swapnat: nativeint -> nativeint = "%bswap_native"
val cpu_to_be32: t -> int -> int32 -> unit
val cpu_to_le32: t -> int -> int32 -> unit
val cpu_to_be64: t -> int -> int64 -> unit
val cpu_to_le64: t -> int -> int64 -> unit
val be32_to_cpu: t -> int -> int32
val be32_from_bigstring_to_cpu: t -> int -> int32
val be32_from_bytes_to_cpu: Bytes.t -> int -> int32
val le32_to_cpu: t -> int -> int32
val le32_from_bigstring_to_cpu: t -> int -> int32
val le32_from_bytes_to_cpu: Bytes.t -> int -> int32
val be64_to_cpu: t -> int -> int64
val be64_from_bigstring_to_cpu: t -> int -> int64
val be64_from_bytes_to_cpu: Bytes.t -> int -> int64
val le64_to_cpu: t -> int -> int64
val le64_from_bigstring_to_cpu: t -> int -> int64
val le64_from_bytes_to_cpu: Bytes.t -> int -> int64
val benat_to_cpu: t -> int -> nativeint
val cpu_to_benat: t -> int -> nativeint -> unit
