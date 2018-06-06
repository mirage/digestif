module type S = Digestif_sig.S

type 'kind hash = 'kind Digestif_sig.hash =
  | MD5 : [ `MD5 ] hash
  | SHA1 : [ `SHA1 ] hash
  | RMD160 : [ `RMD160 ] hash
  | SHA224 : [ `SHA224 ] hash
  | SHA256 : [ `SHA256 ] hash
  | SHA384 : [ `SHA384 ] hash
  | SHA512 : [ `SHA512 ] hash
  | BLAKE2B : int -> [ `BLAKE2B ] hash
  | BLAKE2S : int-> [ `BLAKE2S ] hash
and kind = Digestif_sig.kind

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type 'a iter = 'a Digestif_sig.iter
type 'a pp = 'a Digestif_sig.pp
type 'a equal = 'a Digestif_sig.equal
type 'a compare = 'a Digestif_sig.compare

module MD5: S with type kind = [  `MD5 ]
module SHA1: S with type kind = [ `SHA1 ]
module SHA224: S with type kind = [ `SHA224 ]
module SHA256: S with type kind = [ `SHA256 ]
module SHA384: S with type kind = [ `SHA384 ]
module SHA512: S with type kind = [ `SHA512 ]
module BLAKE2B: S with type kind = [ `BLAKE2B ]
module BLAKE2S: S with type kind = [ `BLAKE2S ]
module RMD160: S with type kind = [ `RMD160 ]

module Make_BLAKE2B(D : sig val digest_size : int end): S with type kind = [ `BLAKE2B ]
module Make_BLAKE2S(D : sig val digest_size : int end): S with type kind = [ `BLAKE2S ]

include Digestif_sig.Ctor
include Digestif_sig.Top
