Digestif - Hash algorithms in C and OCaml
=========================================

[![Build Status](https://travis-ci.org/mirage/digestif.svg?branch=master)](https://travis-ci.org/mirage/digestif)

Digestif is a toolbox which implements hashes:

 * MD5
 * SHA1
 * SHA224
 * SHA256
 * SHA384
 * SHA512
 * WHIRLPOOL
 * BLAKE2B
 * BLAKE2S
 * RIPEMD160

Digestif uses a trick about linking and let the end-user to choose which
implementation he wants to use. We provide 2 implementations:

 * C implementation with `digestif.c`
 * OCaml implementation with `digestif.ocaml`
 
Both are well-tested. However, OCaml implementation is slower than the C
implementation.

**Note**: The linking trick requires `digestif.c` or `digestif.ocaml` to be the first of your dependencies.


Home page: http://din.osau.re/

Contact: Romain Calascibetta `<romain.calascibet ta@gmail.com>`

## API

For each hash, we implement the same API which is referentially transparent.
Then, on the top of these, we reflect functions (like `digesti` or `hmaci`) with
GADT - however, conversion from GADT to hash type is not possible (but you can
_destruct_ GADT with `to_raw_string`).

## Equal/Compare function

We deciced to protect users to timing-attack. In this case, `Digestif.equal` (by
[eqaf](https://github.com/mirage/eqaf.git) package) compares hashes in
constant-time.

However, we provide `unsafe_compare` function too which is __not__ a constant
time function. In some contexts, like `ocaml-git`, we don't care about timing
attack and we use `unsafe_compare` - then, we need to make a wrap where we
rename `unsafe_compare` to `compare` to be able to use it in some functors like
`Map.Make` or `Set.Make`.

It's little annoying to do that but it forces the user to get the right question
about security issues. So, please, don't ask to rename this function.

## MirageOS

Of course, this package is available to be used on MirageOS (both
implementations). User is able to compile `digestif.ocaml` with `js_of_ocaml`
and this package is platform agnostic.

## Build Requirements

 * OCaml >= 4.03.0 (may be less but need test)
 * `base-bytes` meta-package
 * `base-bigarray` meta-package
 * `dune` to build the project
 
If you want to compile the test program, you need:

 * `alcotest`

## Credits

This work is from the [nocrypto](https://github.com/mirleft/nocrypto) library
and the Vincent hanquez's work in
[ocaml-sha](https://github.com/vincenthz/ocaml-sha).

All credits appear in the begin of files and this library is motivated by two reasons:

  * delete the dependancy with `nocrypto` if you don't use the encryption (and common) part
  * aggregate all hashes functions in one library
