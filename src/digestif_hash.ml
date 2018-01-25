open Digestif_sig

type nonrec 'a hash = 'a hash
and nothing = nothing

let md5 = MD5
let sha1 = SHA1
let rmd160 = RMD160
let sha224 = SHA224
let sha256 = SHA256
let sha384 = SHA384
let sha512 = SHA512
let blake2b length = BLAKE2B length
let blake2s length = BLAKE2S length
