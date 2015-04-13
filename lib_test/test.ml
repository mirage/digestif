open Printf
open OUnit

let test_abcd _ =
  let result = Sha1.sha1_to_hexstring (Sha1.encrypt "abcd") in
  let expected = "81fe8bfe87576c3ecb22426f8e57847382917acf" in
  assert_equal result expected

let test_alphabet _ =
  let result = Sha1.sha1_to_hexstring 
		 (Sha1.encrypt "abcdefghijklmnopqrstuvwxyz") in
  let expected = "32d10c7b8cf96570ca04ce37f2a19d84240d3a89" in
  assert_equal result expected

let test_case_sensitive1 _ =
  let result = 
    Sha1.sha1_to_hexstring 
      (Sha1.encrypt "The Quick Brown Fox Jumps Over The Lazy Dog") in
  let expected = "645218467886dd414ea66a09b6cceea806127fb5" in
  assert_equal result expected

let test_case_sensitive2 _ =
  let result = 
    Sha1.sha1_to_hexstring 
      (Sha1.encrypt "The quick brown fox jumps over the lazy dog") in
  let expected = "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12" in
  assert_equal result expected

let test_upper_than_512bits _ =
  let result =
    Sha1.sha1_to_hexstring
      (Sha1.encrypt "012345678901234567890123456789012345678901234567890123456789") in
  let expected = "f52e3c2732de7bea28f216d877d78dae1aa1ac6a" in
  assert_equal result expected

let _ = 
  run_test_tt_main
  ("Sha1 test" >:::
    [
      "test_abcd" >:: test_abcd;
      "test_alphabet" >:: test_alphabet;
      "test_case_sensitive1" >:: test_case_sensitive1;
      "test_case_sensitive2" >:: test_case_sensitive2;
      "test_upper_than_512bits" >:: test_upper_than_512bits;
    ])
