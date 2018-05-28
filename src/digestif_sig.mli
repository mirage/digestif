module Bigstring = Digestif_bigstring
module Bytes = Digestif_bytes

type 'a iter = ('a -> unit) -> unit

module type S = sig

  val digest_size : int
  (** Size of the resulted hash in byte. *)

  module Bigstring:  sig
    type buffer = Bigstring.t
    (** Type of input to digest. *)

    type ctx
    (** Impure context (or abstract state) to feed it and then, return hash. *)

    type t = Bigstring.t
    (** Type of the resulted hash. *)

    val init: unit -> ctx
    (** [init ()] initializes a new context to be feeded. *)

    val feed: ctx -> buffer -> ctx
    (** [feed ctx input] feeds [ctx] with input. *)

    val feed_bytes: ctx -> Bytes.t -> ctx
    (** [feed_bytes ctx input] is same as {!feed} but it specialized with {Bytes.t}. *)

    val feed_bigstring: ctx -> Bigstring.t -> ctx
    (** [feed_bigstring ctx input] is an alias of {!feed}. *)

    val get: ctx -> t
    (** [get ctx] returns resulted hash of several feeding calls on {!ctx}. To
       be care, this function has a side-effect on [ctx], that means [with ctx
       == ctx, get ctx <> get ctx]. *)

    val digest: buffer -> t
    (** [digest input] returns directly resulted hash from [input]. *)

    val digestv: buffer list -> t
    (** [digestv inputs] returns directly resulted hash from multiple [inputs].
       It's equivalent to get a new context, feed it successively with [inputs]
       and return resulted hash. *)

    val hmac: key:buffer -> buffer -> t
    (** [hmac ~key input] returns a keyed-hash with [key] from [input]. We
       follow {{:https://tools.ietf.org/html/rfc2104}RFC2104} (MD5) and
       {{:https://tools.ietf.org/html/rfc4231}RFC4231} (SHA) and apply XOR
       operation on {i ipad} and {i opad} values with [key]. *)

    val hmacv: key:buffer -> buffer list -> t
    (** [hmacv ~key inputs] has the same behavior of {digestv} but we return a
       keyed-hash instead a hash (as {!hmac}). *)

    val feedi: ctx -> buffer iter -> ctx
    val feedi_bytes: ctx -> Bytes.t iter -> ctx
    val feedi_bigstring: ctx -> Bigstring.t iter -> ctx

    val digesti: buffer iter -> t
    val digesti_bytes: Bytes.t iter -> t
    val digesti_bigstring: Bigstring.t iter -> t

    val hmaci: key:buffer -> buffer iter -> t

    val compare: t -> t -> int
    (** [compare a b] compares [a] and [b] and return [0] if they are equal or
       the difference of the first non-equal byte founded. *)

    val eq: t -> t -> bool
    (** [eq a b = compare a b = 0] *)

    val neq: t -> t -> bool
    (** [neq a b = not (eq a b)] *)

    val pp: Format.formatter -> t -> unit
    (** Pretty-printer of {!t}. Don't use it to produce an hexadecimal
       representation of {!} (see {!to_hex}), this function is only usable to
       print hash. *)

    val of_hex: buffer -> t
    (** [of_hex s] returns hash from hexadecimal representation [s]. If
       [String.length s < digest_size], we fill it by [\x00]. [s] can have
       whitespace characters, this is does not matter.

      @raise Invalid_argument if [s] has non-authorized characters or number of
       recognized characters is not a multiple of 2. *)

    val to_hex: t -> buffer
    (** [to_hex t] procudes a hexadecimal representation of {!t}. *)
  end

  module Bytes:  sig
    type buffer = Bytes.t
    (** Type of input to digest. *)

    type ctx
    (** Impure context (or abstract state) to feed it and then, return hash. *)

    type t = Bytes.t
    (** Type of the resulted hash. *)

    val init: unit -> ctx
    (** [init ()] initializes a new context to be feeded. *)

    val feed: ctx -> buffer -> ctx
    (** [feed ctx input] feeds [ctx] with input. *)

    val feed_bytes: ctx -> Bytes.t -> ctx
    (** [feed_bytes ctx input] is an alias of {!feed}. *)

    val feed_bigstring: ctx -> Bigstring.t -> ctx
    (** [feed_bigstring ctx input] is same as {!feed} but it specialized with {!Bigstring.t}. *)

    val get: ctx -> t
    (** [get ctx] returns resulted hash of several feeding calls on {!ctx}. To
       be care, this function has a side-effect on [ctx], that means [with ctx
       == ctx, get ctx <> get ctx]. *)

    val digest: buffer -> t
    (** [digest input] returns directly resulted hash from [input]. *)

    val digestv: buffer list -> t
    (** [digestv inputs] returns directly resulted hash from multiple [inputs].
       It's equivalent to get a new context, feed it successively with [inputs]
       and return resulted hash. *)

    val hmac: key:buffer -> buffer -> t
    (** [hmac ~key input] returns a keyed-hash with [key] from [input]. We
       follow {{:https://tools.ietf.org/html/rfc2104}RFC2104} (MD5) and
       {{:https://tools.ietf.org/html/rfc4231}RFC4231} (SHA) and apply XOR
       operation on {i ipad} and {i opad} values with [key]. *)

    val hmacv: key:buffer -> buffer list -> t
    (** [hmacv ~key inputs] has the same behavior of {digestv} but we return a
       keyed-hash instead a hash (as {!hmac}). *)

    val feedi: ctx -> buffer iter -> ctx
    val feedi_bytes: ctx -> Bytes.t iter -> ctx
    val feedi_bigstring: ctx -> Bigstring.t iter -> ctx

    val digesti: buffer iter -> t
    val digesti_bytes: Bytes.t iter -> t
    val digesti_bigstring: Bigstring.t iter -> t

    val hmaci: key:buffer -> buffer iter -> t

    val compare: t -> t -> int
    (** [compare a b] compares [a] and [b] and return [0] if they are equal or
       the difference of the first non-equal byte founded. *)

    val eq: t -> t -> bool
    (** [eq a b = compare a b = 0] *)

    val neq: t -> t -> bool
    (** [neq a b = not (eq a b)] *)

    val pp: Format.formatter -> t -> unit
    (** Pretty-printer of {!t}. Don't use it to produce an hexadecimal
       representation of {!} (see {!to_hex}), this function is only usable to
       print hash. *)

    val of_hex: buffer -> t
    (** [of_hex s] returns hash from hexadecimal representation [s]. If
       [String.length s < digest_size], we fill it by [\x00]. [s] can have
       whitespace characters, this is does not matter.

      @raise Invalid_argument if [s] has non-authorized characters or number of
       recognized characters is not a multiple of 2. *)

    val to_hex: t -> buffer
    (** [to_hex t] procudes a hexadecimal representation of {!t}. *)
  end
end

type hash =
  | MD5
  | SHA1
  | RMD160
  | SHA224
  | SHA256
  | SHA384
  | SHA512
  | BLAKE2B of int
  | BLAKE2S of int
  (** Hash algorithms *)

module type C = sig
  val md5     : hash
  val sha1    : hash
  val rmd160  : hash
  val sha224  : hash
  val sha256  : hash
  val sha384  : hash
  val sha512  : hash
  val blake2b : int -> hash
  val blake2s : int -> hash
end

module type T = sig
  type t
  type buffer

  val pp      : hash -> Format.formatter -> t -> unit
  val digest  : hash -> buffer -> t
  val digestv : hash -> buffer list -> t
  val mac     : hash -> key:buffer -> buffer -> t
  val macv    : hash -> key:buffer -> buffer list -> t
  val of_hex  : hash -> buffer -> t
  val to_hex  : hash -> t -> buffer
end
