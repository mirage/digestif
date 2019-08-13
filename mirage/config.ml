open Mirage

let hasheur =
  foreign "Unikernel.Make"
    (console @-> job)

let packages =
  [ package ~sublibs:["c"] "digestif"
  ; package "fmt" ]

let () =
  register "hasheur"
    ~packages [ hasheur $ default_console ]
