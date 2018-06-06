module Make (D : sig val digest_size: int end):
sig
  val to_hex: string -> string
  val of_hex: string -> string
  val pp: Format.formatter -> string -> unit
end
