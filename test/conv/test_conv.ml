external random_seed : unit -> int array = "caml_sys_random_seed"

let seed = random_seed ()

let () = Random.full_init seed

let () = Fmt.epr "seed: %a.\n%!" Fmt.(Dump.array int) seed

let strf = Fmt.str

let invalid_arg = Fmt.invalid_arg

let list_init f l =
  let rec go acc = function
    | 0 -> List.rev acc
    | n -> go (f n :: acc) (pred n) in
  go [] l

let random_string length _ =
  let ic = open_in_bin "/dev/urandom" in
  let rs = really_input_string ic length in
  close_in ic ;
  rs

let hashes = list_init (random_string Digestif.SHA1.digest_size) 32

let hashes = List.map Digestif.SHA1.of_raw_string hashes

let consistent_hex =
  List.map Digestif.SHA1.to_hex (* XXX(dinosaure): an oracle [to_hex]? *) hashes

let random_wsp length =
  let go _ =
    match Random.int 4 with
    | 0 -> ' '
    | 1 -> '\t'
    | 2 -> '\n'
    | 3 -> '\r'
    | _ -> assert false in
  String.init length go

let spaces_expand hex =
  let rt = ref [] in
  String.iter
    (fun chr -> rt := !rt @ [ random_wsp (Random.int 10); String.make 1 chr ])
    hex ;
  String.concat "" !rt

let spaces_hex = List.map spaces_expand consistent_hex

let random_hex length =
  let go _ =
    match Random.int (10 + 26 + 26) with
    | n when n < 10 -> Char.chr (Char.code '0' + n)
    | n when n < 10 + 26 -> Char.chr (Char.code 'a' + n - 10)
    | n -> Char.chr (Char.code 'A' + n - (10 + 26)) in
  String.init length go

let inconsistent_hex =
  let expand hex =
    String.concat ""
      [ spaces_expand hex; spaces_expand (random_hex (5 + Random.int 20)) ]
  in
  List.map expand consistent_hex

let test_consistent_hex_success i hex =
  Alcotest.test_case (strf "consistent hex:%d" i) `Quick @@ fun () ->
  ignore @@ Digestif.SHA1.consistent_of_hex hex

let test_hex_success i hex =
  Alcotest.test_case (strf "hex:%d" i) `Quick @@ fun () ->
  ignore @@ Digestif.SHA1.of_hex hex

let test_consistent_hex_fail i hex =
  Alcotest.test_case (strf "consistent hex fail:%d" i) `Quick @@ fun () ->
  try
    let _ = Digestif.SHA1.consistent_of_hex hex in
    assert false
  with Invalid_argument _ -> ()

let sha1 = Alcotest.testable Digestif.SHA1.pp Digestif.SHA1.equal

let test_hex_iso i random_input =
  Alcotest.test_case (strf "iso:%d" i) `Quick @@ fun () ->
  let hash : Digestif.SHA1.t = Digestif.SHA1.of_raw_string random_input in
  let hex = Digestif.SHA1.to_hex hash in
  Alcotest.(check sha1) "iso hex" (Digestif.SHA1.of_hex hex) hash

let test_consistent_hex_iso i random_input =
  Alcotest.test_case (strf "iso:%d" i) `Quick @@ fun () ->
  let hash : Digestif.SHA1.t = Digestif.SHA1.of_raw_string random_input in
  let hex = Digestif.SHA1.to_hex hash in
  Alcotest.(check sha1)
    "iso consistent hex"
    (Digestif.SHA1.consistent_of_hex hex)
    hash

let tests () =
  Alcotest.run "digestif"
    [
      ("of_hex 0", List.mapi test_hex_success consistent_hex);
      ( "consistent_of_hex 0",
        List.mapi test_consistent_hex_success consistent_hex );
      ("of_hex 1", List.mapi test_hex_success spaces_hex);
      ("consistent_of_hex 1", List.mapi test_consistent_hex_success spaces_hex);
      ("of_hex 2", List.mapi test_hex_success inconsistent_hex);
      ( "consistent_of_hex 2",
        List.mapi test_consistent_hex_fail inconsistent_hex );
      ( "iso of_hex",
        List.mapi test_hex_iso
          (list_init (random_string Digestif.SHA1.digest_size) 64) );
      ( "iso consistent_of_hex",
        List.mapi test_consistent_hex_iso
          (list_init (random_string Digestif.SHA1.digest_size) 64) );
    ]

let () = tests ()
