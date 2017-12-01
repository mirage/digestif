module type S = Digestif_sig.S

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
