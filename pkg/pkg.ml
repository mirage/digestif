#!/usr/bin/env ocaml

#directory "pkg";;
#use       "topfind";;
#require   "topkg";;

open Topkg

let opam = Pkg.opam_file ~lint_deps_excluding:None "opam"

let () =
  Pkg.describe ~opams:[opam] "digestif" @@ fun c ->


  Ok [ Pkg.lib "pkg/META"
     ; Pkg.doc "README.md"
     ; Pkg.doc "CHANGES.md"
     ; Pkg.bin "lib/digestif" ~dst:"digestif" ]
