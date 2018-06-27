module Make (D : sig val digest_size: int end):
sig
  val eq: string -> string -> bool
  val neq: string -> string -> bool
  val compare: string -> string -> int
  val unsafe_compare: string -> string -> int
end
