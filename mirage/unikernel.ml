module Make (Console : Mirage_console.S) = struct
  let log console fmt = Format.kasprintf (Console.log console) fmt

  let start console =
    let hash = Digestif.SHA1.digest_string "Hello World!" in
    log console "%a" Digestif.SHA1.pp hash
end
