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

val md5: [ `MD5 ] hash
val sha1: [ `SHA1 ] hash
val rmd160: [ `RMD160 ] hash
val sha224: [ `SHA224 ] hash
val sha256: [ `SHA256 ] hash
val sha384: [ `SHA384 ] hash
val sha512: [ `SHA512 ] hash
val blake2b: int -> [ `BLAKE2B ] hash
val blake2s: int -> [ `BLAKE2S ] hash
