type bigstring =
  ( char,
    Bigarray_compat.int8_unsigned_elt,
    Bigarray_compat.c_layout )
  Bigarray_compat.Array1.t

type 'a iter = ('a -> unit) -> unit
(** A general (inner) iterator. It applies the provided function to a collection
    of elements. For instance:

    - [let iter_k : 'a -> 'a iter = fun x f -> f x]
    - [let iter_pair : 'a * 'a -> 'a iter = fun (x, y) -> f x; f y]
    - [let iter_list : 'a list -> 'a iter = fun l f -> List.iter f l] *)

type 'a compare = 'a -> 'a -> int

type 'a equal = 'a -> 'a -> bool

type 'a pp = Format.formatter -> 'a -> unit

module type S = sig
  val digest_size : int
  (** Size of hash results, in bytes. *)

  type ctx

  type kind

  type t

  val kind : kind
  (** The kind of hash. *)

  val empty : ctx
  (** An empty hash context. *)

  val init : unit -> ctx
  (** Create a new hash state. *)

  val feed_bytes : ctx -> ?off:int -> ?len:int -> Bytes.t -> ctx
  (** [feed_bytes msg t] adds informations in [msg] to [t]. [feed] is analogous
      to appending: [feed (feed t msg1) msg2 = feed t (append msg1 msg2)] *)

  val feed_string : ctx -> ?off:int -> ?len:int -> String.t -> ctx
  (** Same as {!feed_bytes} but for {!String.t}. *)

  val feed_bigstring : ctx -> ?off:int -> ?len:int -> bigstring -> ctx
  (** Same as {!feed_bytes} but for {!bigstring}. *)

  val feedi_bytes : ctx -> Bytes.t iter -> ctx
  (** [feedi_bytes t iter = let r = ref t in iter (fun msg -> r := feed !r msg);
      !r] *)

  val feedi_string : ctx -> String.t iter -> ctx
  (** Same as {!feed_bytes} but for {!String.t}. *)

  val feedi_bigstring : ctx -> bigstring iter -> ctx
  (** Same as {!feed_bytes} but for {!bigstring}. *)

  val get : ctx -> t
  (** [get t] is the digest corresponding to [t]. *)

  val digest_bytes : ?off:int -> ?len:int -> Bytes.t -> t
  (** [digest_bytes msg] is the digest of [msg].

      [digest_bytes msg = get (feed_bytes empty msg)]. *)

  val digest_string : ?off:int -> ?len:int -> String.t -> t
  (** Same as {!digest_bytes} but for a {!String.t}. *)

  val digest_bigstring : ?off:int -> ?len:int -> bigstring -> t
  (** Same as {!digest_bytes} but for a {!bigstring}. *)

  val digesti_bytes : Bytes.t iter -> t
  (** [digesti_bytes iter = feedi_bytes empty iter |> get]. *)

  val digesti_string : String.t iter -> t
  (** Same as {!digesti_bytes} but for {!String.t}. *)

  val digesti_bigstring : bigstring iter -> t
  (** Same as {!digesti_bigstring} but for {!bigstring}. *)

  val digestv_bytes : Bytes.t list -> t
  (** Specialization of {!digesti_bytes} with a list of {!Bytes.t} (see
      {!iter}). *)

  val digestv_string : String.t list -> t
  (** Same as {!digestv_bytes} but for {!String.t}. *)

  val digestv_bigstring : bigstring list -> t
  (** Same as {!digestv_bytes} but for {!bigstring}. *)

  val hmac_bytes : key:Bytes.t -> ?off:int -> ?len:int -> Bytes.t -> t
  (** [hmac_bytes ~key bytes] is the authentication code for {!Bytes.t} under
      the secret [key], generated using the standard HMAC construction over this
      hash algorithm. *)

  val hmac_string : key:String.t -> ?off:int -> ?len:int -> String.t -> t
  (** Same as {!hmac_bytes} but for {!String.t}. *)

  val hmac_bigstring : key:bigstring -> ?off:int -> ?len:int -> bigstring -> t
  (** Same as {!hmac_bytes} but for {!bigstring}. *)

  val hmaci_bytes : key:Bytes.t -> Bytes.t iter -> t
  (** Authentication code under the secret [key] over a collection of
      {!Bytes.t}. *)

  val hmaci_string : key:String.t -> String.t iter -> t
  (** Same as {!hmaci_bytes} but for {!String.t}. *)

  val hmaci_bigstring : key:bigstring -> bigstring iter -> t
  (** Same as {!hmaci_bytes} but for {!bigstring}. *)

  val hmacv_bytes : key:Bytes.t -> Bytes.t list -> t
  (** Specialization of {!hmaci_bytes} with a list of {!Bytes.t} (see {!iter}). *)

  val hmacv_string : key:String.t -> String.t list -> t
  (** Same as {!hmacv_bytes} but for {!String.t}. *)

  val hmacv_bigstring : key:bigstring -> bigstring list -> t
  (** Same as {!hmacv_bigstring} but for {!bigstring}. *)

  val unsafe_compare : t compare
  (** [unsafe_compare] function returns [0] on equality and a negative/positive
      [int] depending on the difference (like {!String.compare}). This is
      usually OK, but this is not constant time, so in some cases it could leak
      some information. *)

  val equal : t equal
  (** The equal (constant-time) function for {!t}. *)

  val pp : t pp
  (** Pretty-printer of {!t}. *)

  val of_hex : string -> t
  (** [of_hex] tries to parse an hexadecimal representation of {!t}. [of_hex]
      raises an [invalid_argument] when input is malformed. We take only firsts
      {!digest_size} hexadecimal values and ignore rest of input. If it has not
      enough hexadecimal values, trailing values of the output hash are zero
      ([\x00]), *)

  val of_hex_opt : string -> t option
  (** [of_hex] tries to parse an hexadecimal representation of {!t}. [of_hex]
      returns [None] when input is malformed. We take only first {!digest_size}
      hexadecimal values and ignore rest of input. If it has not enough
      hexadecimal values, trailing values of the output hash are zero ([\x00]). *)

  val consistent_of_hex : string -> t
  (** [consistent_of_hex] tries to parse an hexadecimal representation of {!t}.
      [consistent_of_hex] raises an [invalid_argument] when input is malformed.
      However, instead {!of_hex}, [consistent_of_hex] expects exactly
      [{!digest_size} * 2] hexadecimal values (but continues to ignore
      whitespaces). *)

  val consistent_of_hex_opt : string -> t option
  (** [consistent_of_hex_opt] tries to parse an hexadecimal representation of
      {!t}. [consistent_of_hex] returns [None] when input is malformed. However,
      instead {!of_hex}, [consistent_of_hex] expects exactly
      [{!digest_size} * 2] hexadecimal values (but continues to ignore
      whitespaces). *)

  val to_hex : t -> string
  (** [to_hex] makes a hex-decimal representation of {!t}. *)

  val of_raw_string : string -> t
  (** [of_raw_string s] see [s] as a hash. Useful when reading serialized
      hashes. *)

  val of_raw_string_opt : string -> t option
  (** [of_raw_string_opt s] see [s] as a hash. Useful when reading serialized
      hashes. Returns [None] if [s] is not the {!digest_size} bytes long. *)

  val to_raw_string : t -> string
  (** [to_raw_string s] is [(s :> string)]. *)
end

(** Some hash algorithms expose extra MAC constructs. The interface is similar
    to the [hmac_*] functions in [S]. *)
module type MAC = sig
  type t

  val mac_bytes : key:Bytes.t -> ?off:int -> ?len:int -> Bytes.t -> t

  val mac_string : key:String.t -> ?off:int -> ?len:int -> String.t -> t

  val mac_bigstring : key:bigstring -> ?off:int -> ?len:int -> bigstring -> t

  val maci_bytes : key:Bytes.t -> Bytes.t iter -> t

  val maci_string : key:String.t -> String.t iter -> t

  val maci_bigstring : key:bigstring -> bigstring iter -> t

  val macv_bytes : key:Bytes.t -> Bytes.t list -> t

  val macv_string : key:String.t -> String.t list -> t

  val macv_bigstring : key:bigstring -> bigstring list -> t
end

type kind =
  [ `MD5
  | `SHA1
  | `RMD160
  | `SHA224
  | `SHA256
  | `SHA384
  | `SHA512
  | `SHA3_224
  | `SHA3_256
  | `SHA3_384
  | `SHA3_512
  | `WHIRLPOOL
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
  | SHA3_224 : [ `SHA3_224 ] hash
  | SHA3_256 : [ `SHA3_256 ] hash
  | SHA3_384 : [ `SHA3_384 ] hash
  | SHA3_512 : [ `SHA3_512 ] hash
  | WHIRLPOOL : [ `WHIRLPOOL ] hash
  | BLAKE2B : int -> [ `BLAKE2B ] hash
  | BLAKE2S : int -> [ `BLAKE2S ] hash

module MD5 : S with type kind = [ `MD5 ]

module SHA1 : S with type kind = [ `SHA1 ]

module SHA224 : S with type kind = [ `SHA224 ]

module SHA256 : S with type kind = [ `SHA256 ]

module SHA384 : S with type kind = [ `SHA384 ]

module SHA512 : S with type kind = [ `SHA512 ]

module SHA3_224 : S with type kind = [ `SHA3_224 ]

module SHA3_256 : S with type kind = [ `SHA3_256 ]

module SHA3_384 : S with type kind = [ `SHA3_384 ]

module SHA3_512 : S with type kind = [ `SHA3_512 ]

module WHIRLPOOL : S with type kind = [ `WHIRLPOOL ]

module BLAKE2B : sig
  include S with type kind = [ `BLAKE2B ]

  module Keyed : MAC with type t = t
end

module BLAKE2S : sig
  include S with type kind = [ `BLAKE2S ]

  module Keyed : MAC with type t = t
end

module RMD160 : S with type kind = [ `RMD160 ]

module Make_BLAKE2B (D : sig
  val digest_size : int
end) : S with type kind = [ `BLAKE2B ]

module Make_BLAKE2S (D : sig
  val digest_size : int
end) : S with type kind = [ `BLAKE2S ]

val md5 : [ `MD5 ] hash

val sha1 : [ `SHA1 ] hash

val rmd160 : [ `RMD160 ] hash

val sha224 : [ `SHA224 ] hash

val sha256 : [ `SHA256 ] hash

val sha384 : [ `SHA384 ] hash

val sha512 : [ `SHA512 ] hash

val sha3_224 : [ `SHA3_224 ] hash

val sha3_256 : [ `SHA3_256 ] hash

val sha3_384 : [ `SHA3_384 ] hash

val sha3_512 : [ `SHA3_512 ] hash

val whirlpool : [ `WHIRLPOOL ] hash

val blake2b : int -> [ `BLAKE2B ] hash

val blake2s : int -> [ `BLAKE2S ] hash

type 'kind t

val module_of : 'k hash -> (module S with type kind = 'k)

val digest_bytes : 'k hash -> Bytes.t -> 'k t

val digest_string : 'k hash -> String.t -> 'k t

val digest_bigstring : 'k hash -> bigstring -> 'k t

val digesti_bytes : 'k hash -> Bytes.t iter -> 'k t

val digesti_string : 'k hash -> String.t iter -> 'k t

val digesti_bigstring : 'k hash -> bigstring iter -> 'k t

val hmaci_bytes : 'k hash -> key:Bytes.t -> Bytes.t iter -> 'k t

val hmaci_string : 'k hash -> key:String.t -> String.t iter -> 'k t

val hmaci_bigstring : 'k hash -> key:bigstring -> bigstring iter -> 'k t

val pp : 'k hash -> 'k t pp

val equal : 'k hash -> 'k t equal

val unsafe_compare : 'k hash -> 'k t compare

val to_hex : 'k hash -> 'k t -> string

val of_hex : 'k hash -> string -> 'k t

val of_hex_opt : 'k hash -> string -> 'k t option

val consistent_of_hex : 'k hash -> string -> 'k t

val consistent_of_hex_opt : 'k hash -> string -> 'k t option

val of_raw_string : 'k hash -> string -> 'k t

val of_raw_string_opt : 'k hash -> string -> 'k t option

val to_raw_string : 'k hash -> 'k t -> string

val of_digest :
  (module S with type t = 'hash and type kind = 'k) -> 'hash -> 'k t

val of_md5 : MD5.t -> [ `MD5 ] t

val of_sha1 : SHA1.t -> [ `SHA1 ] t

val of_rmd160 : RMD160.t -> [ `RMD160 ] t

val of_sha224 : SHA224.t -> [ `SHA224 ] t

val of_sha256 : SHA256.t -> [ `SHA256 ] t

val of_sha384 : SHA384.t -> [ `SHA384 ] t

val of_sha512 : SHA512.t -> [ `SHA512 ] t

val of_sha3_224 : SHA3_224.t -> [ `SHA3_224 ] t

val of_sha3_256 : SHA3_256.t -> [ `SHA3_256 ] t

val of_sha3_384 : SHA3_384.t -> [ `SHA3_384 ] t

val of_sha3_512 : SHA3_512.t -> [ `SHA3_512 ] t

val of_whirlpool : WHIRLPOOL.t -> [ `WHIRLPOOL ] t

val of_blake2b : BLAKE2B.t -> [ `BLAKE2B ] t

val of_blake2s : BLAKE2S.t -> [ `BLAKE2S ] t
