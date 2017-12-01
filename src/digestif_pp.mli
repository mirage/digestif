module type B = sig
  type t

  val create : int -> t
  val iter : (char -> unit) -> t -> unit
  val set : t -> int -> char -> unit
  val get : t -> int -> char
end

module type D = sig
  val digest_size : int
end

module Make (S : B) (D : D): sig
  val to_hex : S.t -> S.t
  val fold_s : ('a -> char -> 'a) -> 'a -> S.t -> 'a
  val of_hex : S.t -> S.t
  val pp : Format.formatter -> S.t -> unit
end
