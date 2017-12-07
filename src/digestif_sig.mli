module Bigstring = Digestif_bigstring
module Bytes = Digestif_bytes

module type S = sig

  val digest_size : int

  module Bigstring:  sig
    type buffer = Bigstring.t
    type ctx
    type t = Bigstring.t

    val init           : unit -> ctx
    val feed           : ctx -> buffer -> unit
    val feed_bytes     : ctx -> Bytes.t -> unit
    val feed_bigstring : ctx -> Bigstring.t -> unit
    val get            : ctx -> t

    val digest         : buffer -> t
    val digestv        : buffer list -> t
    val hmac           : key:buffer -> buffer -> t
    val hmacv          : key:buffer -> buffer list -> t

    val compare        : t -> t -> int
    val eq             : t -> t -> bool
    val neq            : t -> t -> bool

    val pp             : Format.formatter -> t -> unit
    val of_hex         : buffer -> t
    val to_hex         : t -> buffer
  end

  module Bytes:  sig
    type buffer = Bytes.t
    type ctx
    type t = Bytes.t

    val init           : unit -> ctx
    val feed           : ctx -> buffer -> unit
    val feed_bytes     : ctx -> Bytes.t -> unit
    val feed_bigstring : ctx -> Bigstring.t -> unit
    val get            : ctx -> t

    val digest         : buffer -> t
    val digestv        : buffer list -> t
    val hmac           : key:buffer -> buffer -> t
    val hmacv          : key:buffer -> buffer list -> t

    val compare        : t -> t -> int
    val eq             : t -> t -> bool
    val neq            : t -> t -> bool

    val pp             : Format.formatter -> t -> unit
    val of_hex         : buffer -> t
    val to_hex         : t -> buffer
  end
end

type hash =
  [ `MD5
  | `SHA1
  | `SHA224
  | `SHA256
  | `SHA384
  | `SHA512
  | `BLAKE2B
  | `BLAKE2S
  | `RMD160 ]

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
