type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val create: int -> t
val length: t -> int
val sub: t -> int -> int -> t
val empty: t
val init: int -> (int -> char) -> t
val to_string: t -> string
val copy: t -> t

val blit_from_bytes: Bytes.t -> int -> t -> int -> int -> unit

val cpu_to_be32: t -> int -> int32 -> unit
val cpu_to_le32: t -> int -> int32 -> unit
val cpu_to_be64: t -> int -> int64 -> unit
val cpu_to_le64: t -> int -> int64 -> unit
val cpu_to_benat: t -> int -> nativeint -> unit

val be32_to_cpu: t -> int -> int32
val le32_to_cpu: t -> int -> int32
val be64_to_cpu: t -> int -> int64
val le64_to_cpu: t -> int -> int64
val benat_to_cpu: t -> int -> nativeint
