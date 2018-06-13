include module type of Bytes

val blit_from_bigstring: (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t -> int -> t -> int -> int -> unit
val rpad: t -> int -> char -> t

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
