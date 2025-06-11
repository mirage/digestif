let go () =
  let s = "Hello, World!\n" in
  (* MD5 of a string *)
  let md5 = Digestif.MD5.digest_string s in
  Printf.printf "md5: %s\n" (Digestif.MD5.to_hex md5);
  (* You can check this with echo 'Hello, World!' > foo then use md5sum foo
     (Linux) or md5 foo (MacOS) on the command line. *)
  (* We can send multiple strings too. Here, we send our string a byte at a time. *)
  let context = ref (Digestif.MD5.init ()) in
    for x = 0 to String.length s - 1 do
      context := Digestif.MD5.feed_string !context ~off:x ~len:1 s
    done;
  (* Get the digest from the context *)
  let md5' = Digestif.MD5.get !context in
  Printf.printf "md5: %s\n" (Digestif.MD5.to_hex md5')

(* SHA256 of a file *)
let from_file filename =
  (* A file's data is naturally read in chunks. *)
  let fh = open_in_bin filename in
  let buf = Bytes.create 1024 in
  let context = ref (Digestif.SHA256.init ()) in
  let fin = ref false in
    while not !fin do
      let n = input fh buf 0 1024 in
        context := Digestif.SHA256.feed_bytes !context ~off:0 ~len:n buf;
        if n = 0 then fin := true
    done;
  let sha256 = Digestif.SHA256.get !context in
  Printf.printf "sha256: %s\n" (Digestif.SHA256.to_hex sha256)
  (* You can check this with shasum -a 256 <file> on MacOS or sha256sum <file> on Linux. *)

let () =
  match Sys.argv with
  | [|_|] -> go ()
  | [|_; filename|] -> from_file filename
  | _ -> Printf.eprintf "digestif example: unknown command line\n"
