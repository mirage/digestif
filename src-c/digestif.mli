module type S = Digestif_sig.S
module type T = Digestif_sig.T

type hash = Digestif_sig.hash

val digest_size : hash -> int

module MD5     : S
module SHA1    : S
module SHA224  : S
module SHA256  : S
module SHA384  : S
module SHA512  : S
module BLAKE2B : S
module BLAKE2S : S
module RMD160  : S

module MakeBLAKE2B(D : sig val digest_size : int end) : S
module MakeBLAKE2S(D : sig val digest_size : int end) : S

module Bytes : T
  with type t = Bytes.t
   and type buffer = Bytes.t
module Bigstring : T
  with type t = Digestif_sig.Bigstring.t
   and type buffer = Digestif_sig.Bigstring.t
