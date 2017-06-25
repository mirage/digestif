module type S =
sig
  type buffer

  val create : int -> buffer
  val set : buffer -> int -> char -> unit
  val be32_to_cpu : buffer -> int -> int32
  val le32_to_cpu : buffer -> int -> int32
  val be64_to_cpu : buffer -> int -> int64
  val le64_to_cpu : buffer -> int -> int64
  val cpu_to_be32 : buffer -> int -> int32 -> unit
  val cpu_to_be64 : buffer -> int -> int64 -> unit
  val cpu_to_le32 : buffer -> int -> int32 -> unit
  val cpu_to_le64 : buffer -> int -> int64 -> unit
  val cpu_to_benat : buffer -> int -> nativeint -> unit
  val benat_to_cpu : buffer -> int -> nativeint
  val fill : buffer -> int -> int -> char -> unit
  val blit : buffer -> int -> buffer -> int -> int -> unit
  val length : buffer -> int
  val copy : buffer -> buffer
  val sub : buffer -> int -> int -> buffer
end

