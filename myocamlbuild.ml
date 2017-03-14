open Ocamlbuild_plugin

let headers =
  [ "lib/native/digestif.h"
  ; "lib/native/bitfn.h"
  ; "lib/native/blake2b.h"
  ; "lib/native/md5.h"
  ; "lib/native/sha1.h"
  ; "lib/native/sha256.h"
  ; "lib/native/sha512.h" ]

let static = true

let () =
  dispatch @@ function
  | After_rules ->
    if static then flag ["link"; "ocaml"; "byte"] (A "-custom");

    flag ["link"; "ocaml"; "library"; "byte"; "use_librakia_stubs"]
      (S [A "-dllib"; A "-lrakia_stubs"; A "-cclib"; A "-lrakia_stubs"]);
    flag ["link"; "ocaml"; "library"; "native"; "use_librakia_stubs"]
      (S [A "-cclib"; A "-lrakia_stubs"]);

    dep ["compile"; "c"] headers;
    dep ["link"; "ocaml"; "use_librakia_stubs"] ["lib/librakia_stubs.a"]
  | _ -> ()
