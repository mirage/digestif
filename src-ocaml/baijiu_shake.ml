module By = Digestif_by
module Bi = Digestif_bi

(* SHAKE128 and SHAKE256 are the FIPS 202 extendable-output functions: the
   Keccak sponge with the 0x1F domain separator, absorbing at rate 168 and 136
   bytes respectively.  Baijiu_sha3.Unsafe.init derives the rate from the
   digest length ([rsize = 200 - 2 * mdlen]), so 16 and 32 give exactly those
   rates; [mdlen] itself is unused here because the output length is always
   supplied by the caller.

   NOTE: this pure-OCaml backend is hand-written and has not been audited for
   constant-time behaviour.  The Keccak permutation it drives is structurally
   constant-time (no data-dependent branches, no data-indexed table lookups),
   but that property is not verified here.  digestif.c is the default
   implementation. *)

module type S = sig
  type ctx

  val init : unit -> ctx
  val unsafe_feed_bytes : ctx -> By.t -> int -> int -> unit
  val unsafe_feed_bigstring : ctx -> Bi.t -> int -> int -> unit
  val xof : ctx -> unit
  val unsafe_out_bytes : ctx -> By.t -> int -> int -> unit
  val unsafe_out_bigstring : ctx -> Bi.t -> int -> int -> unit
  val dup : ctx -> ctx
end

module U = Baijiu_sha3.Unsafe (struct
  let padding = Baijiu_sha3.shake_padding
end)

module Make (D : sig
  val rate_of : int
end) : S = struct
  open U

  type nonrec ctx = ctx

  let init () = U.init D.rate_of
  let xof = xof
  let dup = dup
  let unsafe_feed_bytes = unsafe_feed_bytes
  let unsafe_feed_bigstring = unsafe_feed_bigstring
  let unsafe_out_bytes = unsafe_out_bytes
  let unsafe_out_bigstring = unsafe_out_bigstring
end

(* [rate_of] is the value handed to Baijiu_sha3.Unsafe.init, i.e. half the
   capacity in bytes: 16 gives rate 168 (SHAKE128), 32 gives rate 136
   (SHAKE256). *)

module SHAKE128 = Make (struct
  let rate_of = 16
end)

module SHAKE256 = Make (struct
  let rate_of = 32
end)
