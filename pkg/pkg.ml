#!/usr/bin/env ocaml

#directory "pkg";;
#use       "topfind";;
#require   "topkg";;

open Topkg

let opam = Pkg.opam_file ~lint_deps_excluding:None "opam"
let alcotest = Conf.with_pkg ~default:false "alcotest"

let () =
  Pkg.describe ~opams:[opam] "digestif" @@ fun c ->

  let alcotest = Conf.value c alcotest in

  Ok [ Pkg.lib "pkg/META"
     ; Pkg.doc "README.md"
     ; Pkg.doc "CHANGES.md"
     ; Pkg.clib "lib/librakia_stubs.clib"
     ; Pkg.lib ~exts:Exts.module_library "lib/digestif"
     ; Pkg.lib ~exts:Exts.module_library "lib/rakia"
     ; Pkg.test ~cond:alcotest "test/test" ]
