Digestif - Hash algorithms in C and OCaml
=========================================

Digestif is a toolbox which implements hashes:

 * MD5
 * SHA1
 * SHA2
 * SHA3
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

**Note**: The linking trick requires `digestif.c` or `digestif.ocaml` to be the
first of your dependencies.

Documentation: https://mirage.github.io/digestif/

Contact: Romain Calascibetta `<romain.calascibet ta@gmail.com>`

## Install & Usage

The library is available on [OPAM](https://opam.ocaml.org/packages/digestif/). You can install it via:
```sh
$ opam install digestif
```

This is a simple program which implements `sha1sum`:
```sh
$ cat >sha1sum.ml <<EOF
let sum ic =
  let tmp = Bytes.create 0x1000 in
  let rec go ctx = match input ic tmp 0 0x1000 with
    | 0 -> Digestif.SHA1.get ctx
    | len ->
      let ctx = Digestif.SHA1.feed_bytes ctx ~off:0 ~len tmp in
      go ctx
    | exception End_of_file -> Digestif.SHA1.get ctx in
  go Digestif.SHA1.empty

let () = match Sys.argv with
  | [| _; filename; |] when Sys.file_exists filename ->
    let ic = open_in filename in
    let hash = sum ic in
    close_in ic ; print_endline (Digestif.SHA1.to_hex hash)
  | [| _ |] ->
    let hash = sum stdin in
    print_endline (Digestif.SHA1.to_hex hash)
  | _ -> Format.eprintf "%s [<filename>]\n%!" Sys.argv.(0)
EOF
$ cat >dune <<EOF
(executable
 (name sha1sum)
 (libraries digestif))
EOF
$ dune exec ./sha1sum.exe -- sha1sum.ml
fe6e6639a817c23857b507e2d833ec776f23f327
```

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

All credits appear in the begin of files and this library is motivated by two
reasons:
  * delete the dependancy with `nocrypto` if you don't use the encryption (and
    common) part
  * aggregate all hashes functions in one library
