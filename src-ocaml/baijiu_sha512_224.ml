module By = Digestif_by
module Bi = Digestif_bi

module type S = sig
  type ctx
  type kind = [ `SHA512_224 ]

  val init : unit -> ctx
  val unsafe_feed_bytes : ctx -> By.t -> int -> int -> unit
  val unsafe_feed_bigstring : ctx -> Bi.t -> int -> int -> unit
  val unsafe_get : ctx -> By.t
  val dup : ctx -> ctx
end

(* SHA-512/224 is SHA-512 with the FIPS 180-4 5.3.6 initial hash value for
   t = 224 and the output truncated to its first 28 bytes; the compression
   function and the padding are those of SHA-512.  Same shape as
   {!Baijiu_sha384}. *)

module Unsafe : S = struct
  type kind = [ `SHA512_224 ]

  open Baijiu_sha512.Unsafe

  type nonrec ctx = ctx

  let init () =
    let b = By.make 128 '\x00' in
    {
      size = [| 0L; 0L |];
      b;
      h =
        [|
          0x8c3d37c819544da2L; 0x73e1996689dcd4d6L; 0x1dfab7ae32ff9c82L;
          0x679dd514582f9fcfL; 0x0f6d2b697bd44da8L; 0x77e36f7304c48942L;
          0x3f9d85a86a1d36c8L; 0x1112e6ad91d692a1L;
        |];
    }

  let unsafe_get ctx =
    let res = unsafe_get ctx in
    By.sub res 0 28

  let dup = dup
  let unsafe_feed_bytes = unsafe_feed_bytes
  let unsafe_feed_bigstring = unsafe_feed_bigstring
end
