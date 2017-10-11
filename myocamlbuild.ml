open Ocamlbuild_plugin

let headers =
  [ "src-c/native/digestif.h"
  ; "src-c/native/bitfn.h"
  ; "src-c/native/blake2b.h"
  ; "src-c/native/blake2s.h"
  ; "src-c/native/md5.h"
  ; "src-c/native/sha1.h"
  ; "src-c/native/sha256.h"
  ; "src-c/native/sha512.h"
  ; "src-c/native/ripemd160.h" ]

let static = true

let () =
  dispatch @@ function
  | After_rules ->
    dep ["record_digestif_rakia_stubs"] ["src-c/librakia_stubs.a"];
    dep ["compile"; "c"] headers;

    flag_and_dep
      ["link"; "ocaml"; "link_digestif_rakia_stubs"]
      (P "src-c/librakia_stubs.a");
    flag ["library"; "ocaml"; "byte"; "record_digestif_rakia_stubs"]
      (S [ A "-dllib"; A "-lrakia_stubs"]);
    flag ["library"; "ocaml"; "record_digestif_rakia_stubs"]
      (S [ A "-cclib"; A "-lrakia_stubs"]);
  | _ -> ()
