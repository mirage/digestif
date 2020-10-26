open Mirage

let main = foreign "Unikernel.Make" (console @-> job)

let packages = [ package "digestif" ]

let () = register ~packages "digestif-test" [ main $ default_console ]
