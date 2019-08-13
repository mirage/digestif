module Make (Console : Mirage_types_lwt.CONSOLE)
= struct
  let log console fmt = Fmt.kstrf (Console.log console) fmt

  let start console =
    let hash = Digestif.SHA1.digest_string "" in
    log console "%a%!" Digestif.SHA1.pp hash
end
