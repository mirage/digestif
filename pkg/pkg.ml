#!/usr/bin/env ocaml

#directory "pkg";;
#use       "topfind";;
#require   "topkg";;

open Topkg

let opam = Pkg.opam_file ~lint_deps_excluding:(Some [ "ocamlbuild"; "topkg"; "ocaml"; "ocamlfind" ]) "opam"

let () =
  Pkg.describe ~opams:[opam] "digestif" @@ fun c ->

  Ok [ Pkg.lib "pkg/META"
     ; Pkg.doc "README.md"
     ; Pkg.doc "CHANGES.md"

     (* c implementation *)
     ; Pkg.clib "src-c/librakia_stubs.clib" ~lib_dst_dir:"c"
     ; Pkg.lib ~exts:Exts.module_library "src-c/digestif" ~dst:"c/digestif"

     ; Pkg.lib ~exts:Exts.module_library "src-ocaml/digestif" ~dst:"ocaml/digestif" ]
