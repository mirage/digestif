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

type _ hash =
  | MD5     : nothing hash
  | SHA1    : nothing hash
  | RMD160  : nothing hash
  | SHA224  : nothing hash
  | SHA256  : nothing hash
  | SHA384  : nothing hash
  | SHA512  : nothing hash
  | BLAKE2B : int -> int hash
  | BLAKE2S : int -> int hash
and nothing = unit

module type C = sig
  val md5     : nothing hash
  val sha1    : nothing hash
  val rmd160  : nothing hash
  val sha224  : nothing hash
  val sha256  : nothing hash
  val sha384  : nothing hash
  val sha512  : nothing hash
  val blake2b : int -> int hash
  val blake2s : int -> int hash
end

module type T = sig
  type t
  type buffer

  val pp      : _ hash -> Format.formatter -> t -> unit
  val digest  : _ hash -> buffer -> t
  val digestv : _ hash -> buffer list -> t
  val mac     : _ hash -> key:buffer -> buffer -> t
  val macv    : _ hash -> key:buffer -> buffer list -> t
  val of_hex  : _ hash -> buffer -> t
  val to_hex  : _ hash -> t -> buffer
end
