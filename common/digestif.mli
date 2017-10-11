module Bi : module type of Digestif_bigstring

module type S =
sig
  val digest_size : int

  module Bigstring :
  sig
    type buffer = Bi.t
    type ctx
    type t = Bi.t

    val init           : unit -> ctx
    val feed           : ctx -> buffer -> unit
    val feed_bytes     : ctx -> Bytes.t -> unit
    val feed_bigstring : ctx -> Bi.t -> unit
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

  module Bytes :
  sig
    type buffer = Bytes.t
    type ctx
    type t = Bytes.t

    val init           : unit -> ctx
    val feed           : ctx -> buffer -> unit
    val feed_bytes     : ctx -> Bytes.t -> unit
    val feed_bigstring : ctx -> Bi.t -> unit
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

module MD5     : S
module SHA1    : S
module SHA224  : S
module SHA256  : S
module SHA384  : S
module SHA512  : S
module BLAKE2B : S
module BLAKE2S : S
module RMD160  : S

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

module Bytes :
sig
  val digest  : hash -> Bytes.t -> Bytes.t
  val digestv : hash -> Bytes.t list -> Bytes.t
  val mac     : hash -> key:Bytes.t -> Bytes.t -> Bytes.t
  val macv    : hash -> key:Bytes.t -> Bytes.t list -> Bytes.t

  val pp      : hash -> Format.formatter -> Bytes.t -> unit
  val of_hex  : hash -> Bytes.t -> Bytes.t
  val to_hex  : hash -> Bytes.t -> Bytes.t
end

module Bigstring :
sig
  val digest  : hash -> Bi.t -> Bi.t
  val digestv : hash -> Bi.t list -> Bi.t
  val mac     : hash -> key:Bi.t -> Bi.t -> Bi.t
  val macv    : hash -> key:Bi.t -> Bi.t list -> Bi.t

  val pp      : hash -> Format.formatter -> Bi.t -> unit
  val of_hex  : hash -> Bi.t -> Bi.t
  val to_hex  : hash -> Bi.t -> Bi.t
end

val digest_size : hash -> int
