module Bi : module type of Digestif_bigstring

module type S =
sig
  type ctx

  val digest_size : int

  module Bigstring :
  sig
    type buffer = Bi.t

    val init    : unit -> ctx
    val feed    : ctx -> buffer -> unit
    val get     : ctx -> buffer

    val digest  : buffer -> buffer
    val digestv : buffer list -> buffer
    val hmac    : key:buffer -> buffer -> buffer
    val hmacv   : key:buffer -> buffer list -> buffer
  end

  module Bytes :
  sig
    type buffer = Bytes.t

    val init    : unit -> ctx
    val feed    : ctx -> buffer -> unit
    val get     : ctx -> buffer

    val digest  : buffer -> buffer
    val digestv : buffer list -> buffer
    val hmac    : key:buffer -> buffer -> buffer
    val hmacv   : key:buffer -> buffer list -> buffer
  end
end

module MD5     : S
module SHA1    : S
module SHA224  : S
module SHA256  : S
module SHA384  : S
module SHA512  : S
module BLAKE2B : S

type hash =
  [ `MD5
  | `SHA1
  | `SHA224
  | `SHA256
  | `SHA384
  | `SHA512
  | `BLAKE2B ]

module Bytes :
sig
  val digest  : hash -> Bytes.t -> Bytes.t
  val digestv : hash -> Bytes.t list -> Bytes.t
  val mac     : hash -> key:Bytes.t -> Bytes.t -> Bytes.t
  val macv    : hash -> key:Bytes.t -> Bytes.t list -> Bytes.t
end

module Bigstring :
sig
  val digest  : hash -> Bi.t -> Bi.t
  val digestv : hash -> Bi.t list -> Bi.t
  val mac     : hash -> key:Bi.t -> Bi.t -> Bi.t
  val macv    : hash -> key:Bi.t -> Bi.t list -> Bi.t
end

val digest_size : hash -> int
