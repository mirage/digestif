external unsafe_set_uint8 :
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  int ->
  int ->
  unit = "%caml_ba_set_1"

external unsafe_set_uint32 :
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  int ->
  int32 ->
  unit = "%caml_bigstring_set32"

let fill ba chr =
  let len = Bigarray.Array1.dim ba in
  let len0 = len land 3 in
  let len1 = len asr 2 in
  let v0 = Char.code chr in
  let v1 = Int32.of_int v0 in

  for i = 0 to len1 - 1 do
    let i = i * 4 in
    unsafe_set_uint32 ba i v1
  done ;

  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    unsafe_set_uint8 ba i v0
  done

let sha3_cve_2022_37454_0 =
  Alcotest.test_case "buffer overflow" `Slow @@ fun () ->
  Gc.full_major () ;
  let a = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 1 in
  let b = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 4294967295 in
  fill a '\x00' ;
  fill b '\x00' ;
  let ctx = Digestif.SHA3_224.empty in
  let ctx = Digestif.SHA3_224.feed_bigstring ctx a in
  let ctx = Digestif.SHA3_224.feed_bigstring ctx b in
  let hash = Digestif.SHA3_224.get ctx in
  Alcotest.(check (testable Digestif.SHA3_224.pp Digestif.SHA3_224.equal))
    "result" hash
    (Digestif.SHA3_224.of_hex
       "c5bcc3bc73b5ef45e91d2d7c70b64f196fac08eee4e4acf6e6571ebe")

let sha3_cve_2022_37454_1 =
  Alcotest.test_case "infinite loop" `Slow @@ fun () ->
  Gc.full_major () ;
  let a = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 1 in
  let b = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 4294967296 in
  fill a '\x00' ;
  fill b '\x00' ;
  let ctx = Digestif.SHA3_224.empty in
  let ctx = Digestif.SHA3_224.feed_bigstring ctx a in
  let ctx = Digestif.SHA3_224.feed_bigstring ctx b in
  let hash = Digestif.SHA3_224.get ctx in
  Alcotest.(check (testable Digestif.SHA3_224.pp Digestif.SHA3_224.equal))
    "result" hash
    (Digestif.SHA3_224.of_hex
       "ec66be1ebccf055f839fccf2d12e641dcbbda4f5c71a3bdee6509495")

let bytes_larger_than_4gib =
  Alcotest.test_case "bytes larger than 4 GiB" `Slow @@ fun () ->
  Gc.full_major () ;
  let b = Bytes.make 4294967297 '\x00' in
  let hash = Digestif.SHA256.digest_bytes b in
  Alcotest.(check (testable Digestif.SHA256.pp Digestif.SHA256.equal))
    "result" hash
    (Digestif.SHA256.of_hex
       "fbb82f7b353676bb562eb82157fcf0ea42c36492ca13ee56dbf82c08b6802c5c")

let blake3_bytes_of_2gib =
  Alcotest.test_case "bytes of 2 GiB" `Slow @@ fun () ->
  Gc.full_major () ;
  let half = 1073741824 in
  let b = Bytes.make (2 * half) '\x00' in
  let ctx = Digestif.BLAKE3.feed_bytes Digestif.BLAKE3.empty ~len:half b in
  let ctx = Digestif.BLAKE3.feed_bytes ctx ~off:half b in
  Alcotest.(check (testable Digestif.BLAKE3.pp Digestif.BLAKE3.equal))
    "result"
    (Digestif.BLAKE3.digest_bytes b)
    (Digestif.BLAKE3.get ctx)

let blake3_xof_of_2gib =
  Alcotest.test_case "XOF output of 2 GiB" `Slow @@ fun () ->
  Gc.full_major () ;
  let len = 2147483648 in
  let ctx = Digestif.BLAKE3.empty in
  let out = Digestif.BLAKE3.get_xof ctx len in
  Alcotest.(check string)
    "prefix"
    (Digestif.BLAKE3.to_raw_string (Digestif.BLAKE3.get ctx))
    (String.sub out 0 32) ;
  Alcotest.(check string)
    "suffix"
    (Digestif.BLAKE3.get_xof ctx ~seek:(Int64.of_int (len - 32)) 32)
    (String.sub out (len - 32) 32)

let () =
  Alcotest.run "digestif (CVE)"
    [
      ("sha3 (CVE-2022-37454)", [ sha3_cve_2022_37454_0; sha3_cve_2022_37454_1 ]);
      ("blake3 lengths", [ blake3_bytes_of_2gib; blake3_xof_of_2gib ]);
      ("large inputs", [ bytes_larger_than_4gib ]);
    ]
