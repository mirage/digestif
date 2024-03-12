open Crowbar

type pack = Pack : 'a Digestif.hash -> pack

let hash =
  choose
    [
      const (Pack Digestif.sha1); const (Pack Digestif.sha256);
      const (Pack Digestif.sha512);
    ]

let with_get_into_bytes off len (type ctx)
    (module Hash : Digestif.S with type ctx = ctx) (ctx : ctx) =
  let buf = Bytes.create len in
  let () =
    try Hash.get_into_bytes ctx ~off buf
    with Invalid_argument e -> (
      (* Skip if the invalid argument is valid; otherwise fail *)
      match Bytes.sub buf off Hash.digest_size with
      | _ -> failf "Hash.get_into_bytes: Invalid_argument %S" e
      | exception Invalid_argument _ -> bad_test ()) in
  Bytes.sub_string buf off Hash.digest_size

let () =
  add_test ~name:"get_into_bytes" [ hash; int8; range 1024; bytes ]
  @@ fun (Pack hash) off len bytes ->
  let (module Hash) = Digestif.module_of hash in
  let ctx = Hash.empty in
  let ctx = Hash.feed_string ctx bytes in
  let a = with_get_into_bytes off len (module Hash) ctx in
  let b = Hash.(to_raw_string (get ctx)) in
  check_eq ~eq:String.equal a b
