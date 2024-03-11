open Crowbar

type pack = Pack : 'a Digestif.hash -> pack

let hash =
  choose
    [
      const (Pack Digestif.sha1); const (Pack Digestif.sha256);
      const (Pack Digestif.sha512);
    ]

let with_get_into_bytes off (type ctx)
    (module Hash : Digestif.S with type ctx = ctx) (ctx : ctx) =
  let buf = Bytes.create (off + Hash.digest_size) in
  Hash.get_into_bytes ctx ~off buf ;
  Bytes.sub_string buf off Hash.digest_size

let () =
  add_test ~name:"get_into_bytes" [ hash; int8; bytes ]
  @@ fun (Pack hash) off bytes ->
  let (module Hash) = Digestif.module_of hash in
  let ctx = Hash.empty in
  let ctx = Hash.feed_string ctx bytes in
  let a = with_get_into_bytes (abs off) (module Hash) ctx in
  let b = Hash.(to_raw_string (get ctx)) in
  check_eq ~eq:String.equal a b
