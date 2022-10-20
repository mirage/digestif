### v1.1.3 2022-10-20 Paris (France)

- Support MSVC compiler (@jonahbeckford, #137)
- Fix CI on Windows (`test_conv.ml` requires `/dev/urandom`) (@dinosaure, #138)
- Fix threads support (@dinosaure, #140)
- Delete the META trick needed for MirageOS 3 when we install `digestif` (@dinosaure, #141)
  This version of `digestif` breaks the compatibility with MirageOS 3
  and `ocaml-freestanding`. This PR should unlock the ability to
  use `dune-cache`.

### v1.1.2 2022-04-08 Paris (France)

- Minor update on the README.md (@punchagan, #133)
- Support only OCaml >= 4.08, update with `ocamlformat.0.21.0` and remove `bigarray-compat`
  dependency (@hannesm, #134)

### v1.1.1 2022-03-28 Paradou (France)

- Hide C functions (`sha3_keccakf`) (@hannesm, #125)
- Use `ocaml` to run `install.ml` instead of a shebang (@Nymphium, #127)
- Use `command -v` instead of `which` (@Numphium, #126)
- Add `@since` meta-data in documentation (@c-cube, @dinosaure, #128)
- Update the README.md (@dinosaure, @mimoo, #130)
- `ocaml-solo5` provides `__ocaml_solo5__` instead of `__ocaml_freestanding__` (@dinosaure, #131)

### v1.1.0 2021-10-11 Paris (France)

- Add Keccak256 module (ethereum padding) (@maxtori, @dinosaure, #118)
- Update README.md to include the documentation (@mimoo, @dinosaure, 65a5c12)
- Remove deprecated function from `fmt` library (@dinosaure, #121)
- **NOTE**: This version lost the support of OCaml 4.03 and OCaml 4.04.

### v1.0.1 2020-02-08 Paris (France)

- Fix `esy` support (@dinosaure, #115)
- Fix big-endian support (@dinosaure, #113)

### v1.0.0 2020-11-02 Paris (France)

- **breaking changes** Upgrade the library with MirageOS 3.9 (new layout of artifacts)
  Add tests about compilation of unikernels (execution and link)
  (#105, @dinosaure, @hannesm)
- Fix `esy` installation (#104, @dinosaure)
- **breaking changes** Better GADT (#103, @dinosaure)
  As far as I can tell, nobody really use this part of `digestif`.
  The idea is to provide a GADT which contains the type of the hash.
  From third-part libraries point-of-view, it's better to _pattern-match_ with
  such information instead to use a polymorphic variant (as before).
- **breaking changes** key used for HMAC is a constant `string` (#101, @dinosaure, @hannesm)
  The key should not follow the same type as the digest value (`string`, `bytes`, `bigstring`).
  This update restricts the user to user only constant key (as a `string`).

### v0.9.0 2020-07-10 Paris (France)

- Add sha3 implementation (#98), @lyrm, @dinosaure, @hannesm and @cfcs

### v0.8.1 2020-06-15 Paris (France)

- Move to `dune.2.6.0` (#97)
- Apply `ocamlformat.0.14.2` (#97)
- Fix tests according `alcotest.1.0.0` (#95)

### v0.8.0 2019-20-09 Saint Louis (Sénégal)

- Fake version to prioritize dune's variants instead of
  old linking trick
- Use `stdlib-shims` to keep compatibility with < ocaml.4.07.0

### v0.7.3 2019-07-09 Paris (France)

- Fix bug about specialization of BLAKE2{B,S} (#85, #86)
  reported by @samoht, fixed by @dinosaure, reviewed by @hannes and @cfcs

### v0.7.2 2019-05-16 Paris (France)

- Add conflict with `< mirage-xen-posix.3.1.0` packages (@hannesm)
- Add a note on README.md about the linking-trick and order of dependencies (@rizo)
- Use experimental feature of variants with `dune` (@dinosaure, review @rgrinberg)

  `digestif` requires at least `dune.1.9.2`

### v0.7.1 2018-11-15 Paris (France)

- Cross compilation adjustments (@hannesm) (# 76)
- Add the WHIRLPOOL hash algorithm (@clecat) (#77)
- Backport fix on opam file (@dinosaure, @kit-ty-kate)

### v0.7 2018-10-15 Paris (France)

- Fixed HMAC on BLAKE2{S,B} (@emillon) (#46, #51)
- Fixed `convenient_of_hex` (@dinosaure, @hannesm, @cfcs) (#55)
- Add `of_raw_string`/`to_raw_string` (@samoht) (#57)
- Test `digestif` on solo5 and xen backends (@samoht)
- *breaking change*, commont type `t` is an abstract type (#58, #56)
- Fixed META file (@dinosaure, @g2p) (#75)
- New dependency `eqaf` (@dinosaure, @cfcs, @hannesm) (constant-time equal function) (#33, #34, #48, #50, #52, #65)
- Remove `Obj.magic` in common implementation (@dinosaure, @samoht) (#61, #62)
- Add conveniences functions in common implementation (@hcarty) (#63)
- Add option-returning functions in common implementation (@harcty) (#63)
- Verify length of string on `of_raw_string` function (@hcarty) (#63)
- Release runtime lock (@andersfugmann, @dinosaure, @cfcs) (#69, #70)
- Bounds check (@cfcs, @dinosaure) (#71, #72)
- Fixed linking problem (@andersfugmann, @g2p, @dinosaure) (#49, #53, #73, #74)
- Update OPAM file (@dinosaure)

### v0.6.1 2018-07-24 Paris (France)

- *breaking change* API: Digestif implements a true linking trick. End-user need
  to explicitely link with `digestif.{c,ocaml}` and it needs to be the first of
  your dependencies.
- move to `jbuilder`/`dune`

### v0.6 2018-07-05 Paris (France)

- *breaking change* API:
  From a consensus between people who use `digestif`, we decide to delete `*.Bytes.*` and `*.Bigstring.*` sub-modules.
  We replace it by `feed_{bytes,string,bigstring}` (`digest_`, and `hmac_` too)
- *breaking change* semantic: streaming and referentially transparent
  Add `feedi_{bytes,string,bigstring}`, `digesti_{bytes,string,bigstring}` and `hmaci_{bytes,string,bigstring}`
  (@hannesm, @cfcs)
- Constant time for `eq`/`neq` functions
  (@cfcs)
- *breaking change* semantic on `compare` and `unsafe_compare`:
  `compare` is not a lexicographical comparison function (rename to `unsafe_compare`)
  (@cfcs)
- Add `consistent_of_hex` (@hannesm, @cfcs)

### v0.4 2017-10-30 Mysore / ಮೈಸೂರು (India)

- Add an automatised test suit
- Add the RIPEMD160 hash algorithm
- Add the BLAKE2S hash algorithm
- Update authors
- Add `feed_bytes` and `feed_bigstring` for `Bytes` and `Bigstring`

### v0.3 2017-07-21 Phnom Penh (Cambodia)

- Fixed issue #6
- Make a new test suit

### v0.2 2017-07-05 Phnom Penh (Cambodia)

- Implementation of the hash function in pure OCaml
- Link improvement (à la `mtime`) to decide to use the C stub or the OCaml implementation
- Improvement of the common interface (pretty-print, type t, etc.)

### v0.1 2017-05-12 Rạch Giá (Vietnam)

- First release
