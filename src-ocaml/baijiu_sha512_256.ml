module By = Digestif_by
module Bi = Digestif_bi

module type S = sig
  type ctx
  type kind = [ `SHA512_256 ]

  val init : unit -> ctx
  val unsafe_feed_bytes : ctx -> By.t -> int -> int -> unit
  val unsafe_feed_bigstring : ctx -> Bi.t -> int -> int -> unit
  val unsafe_get : ctx -> By.t
  val dup : ctx -> ctx
end

(* SHA-512/256 is SHA-512 with the FIPS 180-4 5.3.6 initial hash value for
   t = 256 and the output truncated to its first 32 bytes; the compression
   function and the padding are those of SHA-512.  Same shape as
   {!Baijiu_sha384}. *)

module Unsafe : S = struct
  type kind = [ `SHA512_256 ]

  open Baijiu_sha512.Unsafe

  type nonrec ctx = ctx

  let init () =
    let b = By.make 128 '\x00' in
    {
      size = [| 0L; 0L |];
      b;
      h =
        [|
          0x22312194fc2bf72cL; 0x9f555fa3c84c64c2L; 0x2393b86b6f53b151L;
          0x963877195940eabdL; 0x96283ee2a88effe3L; 0xbe5e1e2553863992L;
          0x2b0199fc2c85b8aaL; 0x0eb72ddc81c52ca2L;
        |];
    }

  let unsafe_get ctx =
    let res = unsafe_get ctx in
    By.sub res 0 32

  let dup = dup
  let unsafe_feed_bytes = unsafe_feed_bytes
  let unsafe_feed_bigstring = unsafe_feed_bigstring
end
