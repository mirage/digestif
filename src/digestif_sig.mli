type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type 'a iter = ('a -> unit) -> unit
type 'a compare = 'a -> 'a -> int
type 'a equal = 'a -> 'a -> bool
type 'a pp = Format.formatter -> 'a -> unit

module type S = sig

  val digest_size : int

  type ctx
  type kind
  type t = private string

  val init: unit -> ctx

  val feed_bytes: ctx -> ?off:int -> ?len:int -> Bytes.t -> ctx
  val feed_string: ctx -> ?off:int -> ?len:int -> String.t -> ctx
  val feed_bigstring: ctx -> ?off:int -> ?len:int -> bigstring -> ctx

  val feedi_bytes: ctx -> Bytes.t iter -> ctx
  val feedi_string: ctx -> String.t iter -> ctx
  val feedi_bigstring: ctx -> bigstring iter -> ctx

  val get: ctx -> t

  val digest_bytes: ?off:int -> ?len:int -> Bytes.t -> t
  val digest_string: ?off:int -> ?len:int -> String.t -> t
  val digest_bigstring: ?off:int -> ?len:int -> bigstring -> t

  val digesti_bytes: Bytes.t iter -> t
  val digesti_string: String.t iter -> t
  val digesti_bigstring: bigstring iter -> t

  val digestv_bytes: Bytes.t list -> t
  val digestv_string: String.t list -> t
  val digestv_bigstring: bigstring list -> t

  val hmac_bytes: key:Bytes.t -> ?off:int -> ?len:int -> Bytes.t -> t
  val hmac_string: key:String.t -> ?off:int -> ?len:int -> String.t -> t
  val hmac_bigstring: key:bigstring -> ?off:int -> ?len:int -> bigstring -> t

  val hmaci_bytes: key:Bytes.t -> Bytes.t iter -> t
  val hmaci_string: key:String.t -> String.t iter -> t
  val hmaci_bigstring: key:bigstring -> bigstring iter -> t

  val hmacv_bytes: key:Bytes.t -> Bytes.t list -> t
  val hmacv_string: key:String.t -> String.t list -> t
  val hmacv_bigstring: key:bigstring -> bigstring list -> t

  val compare: t compare
  val eq: t equal
  val neq: t equal

  val pp: t pp

  val of_hex: string -> t
  val to_hex: t -> string
end

type kind =
  [ `MD5
  | `SHA1
  | `RMD160
  | `SHA224
  | `SHA256
  | `SHA384
  | `SHA512
  | `BLAKE2B
  | `BLAKE2S ]

type 'k hash =
  | MD5 : [ `MD5 ] hash
  | SHA1 : [ `SHA1 ] hash
  | RMD160 : [ `RMD160 ] hash
  | SHA224 : [ `SHA224 ] hash
  | SHA256 : [ `SHA256 ] hash
  | SHA384 : [ `SHA384 ] hash
  | SHA512 : [ `SHA512 ] hash
  | BLAKE2B : int -> [ `BLAKE2B ] hash
  | BLAKE2S : int-> [ `BLAKE2S ] hash

module type Ctor =
sig
  val md5: [ `MD5 ] hash
  val sha1: [ `SHA1 ] hash
  val rmd160: [ `RMD160 ] hash
  val sha224: [ `SHA224 ] hash
  val sha256: [ `SHA256 ] hash
  val sha384: [ `SHA384 ] hash
  val sha512: [ `SHA512 ] hash
  val blake2b: int -> [ `BLAKE2B ] hash
  val blake2s: int -> [ `BLAKE2S ] hash
end

module type Top =
sig
  type 'kind t = private string

  val module_of: 'k hash -> (module S with type kind = 'k)

  val digesti_bytes: 'k hash -> Bytes.t iter -> 'k t
  val digesti_string: 'k hash -> String.t iter -> 'k t
  val digesti_bigstring: 'k hash -> bigstring iter -> 'k t

  val hmaci_bytes: 'k hash -> key:Bytes.t -> Bytes.t iter -> 'k t
  val hmaci_string: 'k hash -> key:String.t -> String.t iter -> 'k t
  val hmaci_bigstring: 'k hash -> key:bigstring -> bigstring iter -> 'k t

  val pp: 'k hash -> 'k t pp
  val eq: 'k hash -> 'k t equal
  val neq: 'k hash -> 'k t equal
  val compare: 'k hash -> 'k t compare

  val to_hex: 'k hash -> 'k t -> string
  val of_hex: 'k hash -> string -> 'k t
end
