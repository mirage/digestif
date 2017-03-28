Digestif - Hash algorithms in OCaml
===========================================================

Digestif (and Rakia) provided some hashes functions in OCaml. Rakia provided
theses functions by a C stub and Digestif is a pure implementation in OCaml of
theses hashes. So theses hashes functions can be used in an OCaml/Mirage/JavasScript world.

Obviously, Rakia is more faster than Digestif (the hot loop was implemented in
C) but it's possible than Rakia can't compile in your architecture. In this
case, it's better to use Digestif or send a PR to fix Rakia.

## API

We provided an interface with no dependancy (only with `Bigarray`, available by
the OCaml distribution). You can choose between the `Bytes` module or the
`Bigstring` module. You can't remove the dependancy with `Bigarray` because the
context of the hash function is internally a `Bigstring.t`.

We provided the same interface:

```ocaml
val init    : unit -> ctx
val feed    : ctx -> buffer -> unit
val get     : ctx -> buffer

val digest  : buffer -> buffer
val digestv : buffer list -> buffer
val hmac    : key:buffer -> buffer -> buffer
val hmacv   : key:buffer -> buffer list -> buffer
```

## Hash function

`buffer` can be a `Bytes.t` or a `Bigstring.t`. We have an imperative and a
functionnal way to produce a hash. At this time, we implement these hash:

 * SHA1
 * SHA224
 * SHA256
 * SHA384
 * SHA512
 * BLAKE2B

If you want an other hash function, you can ask in the issue.

## Build Requirements

 * OCaml >= 4.03.0 (may be less but need test)
 * `base-bytes` meta-package
 * Bigarray module (provided by the standard library of OCaml)
 * `topkg`, `ocamlbuild` and `ocamlfind` to build the project
 
If you want to compile the test program, you need:

 * `alcotest`
