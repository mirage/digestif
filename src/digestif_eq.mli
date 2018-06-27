module Make (D : sig val digest_size: int end):
sig
  val eq: string -> string -> bool
  val neq: string -> string -> bool
end
