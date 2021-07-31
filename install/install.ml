#!/usr/bin/env ocaml

#load "unix.cma"

let freestanding =
  "freestanding_linkopts = \"-l:libdigestif_freestanding_stubs.a\""

let meta =
  match Sys.getenv "DUNE_BUILD_DIR" with
  | _build -> _build ^ "/default/META.digestif"
  | exception Not_found -> "_build/default/META.digestif"

let new_line = '\n'

let output_line oc line =
  output_string oc line ;
  output_char oc new_line

let () =
  try
    Unix.chmod meta 0o644 ;
    let oc = open_out_gen [ Open_append ] 0o644 meta in
    output_line oc freestanding ;
    close_out oc
  with Unix.Unix_error (Unix.ENOENT, _, _) ->
    Format.eprintf
      "The MirageOS 3.* support is not available in your build environment.\n%!"
