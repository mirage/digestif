module By = Digestif_by
module Bi = Digestif_bi

module type S = sig
  type ctx

  type kind = [ `SHA3_512 ]

  val init : unit -> ctx

  val unsafe_feed_bytes : ctx -> By.t -> int -> int -> unit

  val unsafe_feed_bigstring : ctx -> Bi.t -> int -> int -> unit

  val unsafe_get : ctx -> By.t

  val dup : ctx -> ctx
end

module Unsafe : S = struct
  type kind = [ `SHA3_512 ]

  open Baijiu_sha3.Unsafe

  type nonrec ctx = ctx

  let init () = Baijiu_sha3.Unsafe.init 64

  let unsafe_get = unsafe_get

  let dup = dup

  let unsafe_feed_bytes = unsafe_feed_bytes

  let unsafe_feed_bigstring = unsafe_feed_bigstring
end
