#!/usr/bin/env ocaml

#directory "pkg";;
#use       "topfind";;
#require   "topkg";;

open Topkg

let opam = Pkg.opam_file ~lint_deps_excluding:(Some [ "ocamlbuild"; "topkg"; "ocaml"; "ocamlfind"; "base-bytes" ]) "opam"

let () =
  Pkg.describe ~opams:[opam] "digestif" @@ fun c ->

  Ok [ Pkg.lib "pkg/META"
     ; Pkg.doc "README.md"
     ; Pkg.doc "CHANGES.md"

     ; Pkg.clib "src-c/librakia_stubs.clib" ~lib_dst_dir:"c"
     ; Pkg.lib "common/digestif.cmi" ~dst:"digestif.cmi"
     ; Pkg.mllib ~api:["Digestif"] "src-c/digestif.mllib" ~dst_dir:"c"
     ; Pkg.mllib ~api:["Digestif"] "src-ocaml/digestif.mllib" ~dst_dir:"ocaml" ]
