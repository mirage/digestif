let () = Printexc.record_backtrace true

module type S =
sig
  type t

  val eq : t -> t -> bool
  val pp : t Fmt.t
end

let title = function
  | `MD5     -> "hmac:md5"
  | `SHA1    -> "hmac:sha1"
  | `SHA224  -> "hmac:sha224"
  | `SHA256  -> "hmac:sha256"
  | `SHA384  -> "hmac:sha384"
  | `SHA512  -> "hmac:sha512"
  | `BLAKE2B -> "hmac:blake2b"
  | `BLAKE2S -> "hmac:blake2s"
  | `RMD160  -> "hmac:rmd160"

type _ buffer =
  | Bytes : Bytes.t buffer
  | Bigstring : bigstring buffer
and bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let bytes = Bytes
let bigstring = Bigstring

let atest (type buffer) (module Buffer : S with type t = buffer) hash hmac key (input : buffer) (expect : buffer) =
  let result = hmac hash ~key input in
  let title  = title hash in

  Alcotest.(check (Alcotest.testable Buffer.pp Buffer.eq)) title expect result

module type HASH = sig type t = Digestif.hash val v : t end

module By (Hash : HASH) =
struct
  type t = Bytes.t

  let eq = Bytes.equal
  let pp = Digestif.Bytes.pp Hash.v
end

module Bi (Hash : HASH) =
struct
  type t = bigstring

  let eq = Digestif.Bi.eq
  let pp = Digestif.Bigstring.pp Hash.v
end

let test
  : type a. a buffer -> Digestif.hash -> a -> a -> a -> unit
  = fun buffer hash key input expect ->
    match buffer with
    | Bytes ->
      let module By = By(struct type t = Digestif.hash let v = hash end) in
      atest (module By) hash Digestif.Bytes.mac key input expect
    | Bigstring ->
      let module Bi = Bi(struct type t = Digestif.hash let v = hash end) in
      atest (module Bi) hash Digestif.Bigstring.mac key input expect

let make
  : type a. name:string -> a buffer -> Digestif.hash -> a -> a -> a -> unit Alcotest.test_case
  = fun ~name buffer hash key input expect ->
    name, `Slow, (fun () -> test buffer hash key input expect)

let combine a b c =
  let rec aux r a b c = match a, b, c with
    | xa :: ra, xb :: rb, xc :: rc ->
      aux ((xa, xb, xc) :: r) ra rb rc
    | [], [], [] -> List.rev r
    | _ -> raise (Invalid_argument "combine")
  in
  aux [] a b c

let makes ~name buffer hash keys inputs expects =
  List.map (fun (key, input, expect) -> make ~name buffer hash key input expect)
    (combine keys inputs expects)

let to_bigstring s =
  let ln = Bytes.length s in
  let bi = Digestif.Bi.create ln in

  Digestif.Bi.blit_from_bytes s 0 bi 0 ln;
  bi

let keys_by, keys_bi =
  [ "Salut"
  ; "Jefe"
  ; "Lorenzo"
  ; "Le son qui fait plaiz'"
  ; "La c'est un peu chaud en vrai"]
  |> List.map (fun s -> Bytes.unsafe_of_string s, to_bigstring (Bytes.unsafe_of_string s))
  |> List.split

let inputs_by, inputs_bi =
  [ "Hi There"
  ; "what do ya want for nothing?"
  ; "C'est Lolo je bois de l'Ice Tea quand j'suis fonsde"
  ; "Mes pecs dansent le flamenco, Lolo l'empereur du sale, dans le deal on \
    m'surnomme Joe La Crapule"
  ; "Y'a un pack de douze a cote du cadavre dans le coffre. Pourquoi t'etais \
    Charlie mais t'etais pas Jean-Pierre Coffe. Ca sniffe tellement la coke, \
    mes crottes de nez c'est d'la MD. J'deteste juste les keufs, j'aime bien \
    les obeses et les pedes. Mamene finira dans le dico'. J'ai qu'un reuf: le \
    poto Rico. Ca rotte-ca l'argent des clodos. C'est moi qu'ecrit tous les \
    pornos. Cite-moi en controle de philo'. Toutes les miss grimpent aux \
    rideaux." ]
  |> List.map (fun s -> Bytes.unsafe_of_string s, to_bigstring (Bytes.unsafe_of_string s))
  |> List.split

let results_md5_by, results_md5_bi =
  [ "689e721d493b6eeea482947be736c808"
  ; "750c783e6ab0b503eaa86e310a5db738"
  ; "1cdd24eef6163afee7adc7c53dd6c9df"
  ; "0316ebcad933675e84a81850e24d55b2"
  ; "9ee938a2659d546ccc2e5993601964eb" ]
  |> List.map (fun x -> Bytes.unsafe_of_string x |> Digestif.Bytes.of_hex `MD5)
  |> List.map (fun s -> s, to_bigstring s)
  |> List.split

let results_sha1_by, results_sha1_bi =
  [ "b0a6490a6fcb9479a7aa2306ecb56730d6225dba"
  ; "effcdf6ae5eb2fa2d27416d5f184df9c259a7c79"
  ; "d80589525b1cc9f5e5ffd48ffd73d710ac89a3f1"
  ; "0a5212b295e11a1de5c71873e70ce54f45119516"
  ; "deaf6465e5945a0d04cba439c628ee9f47b95aef" ]
  |> List.map (fun x -> Bytes.unsafe_of_string x |> Digestif.Bytes.of_hex `SHA1)
  |> List.map (fun s -> s, to_bigstring s)
  |> List.split

let results_sha224_by, results_sha224_bi =
  [ "9a26f1380aae8c580441676891765c8a647ddf16a7d12fa427090901"
  ; "a30e01098bc6dbbf45690f3a7e9e6d0f8bbea2a39e6148008fd05e44"
  ; "b94a09654fc749ae6cb21c7765bf4938ff9af03e13d83fbf23342ce7"
  ; "7c66e4c7297a22ca80e2e1db9774afea64b1e086be366d2da3e6bc83"
  ; "438dc3311243cd54cc7ee24c9aac8528a1750abc595f06e68a331d2a" ]
  |> List.map (fun x -> Bytes.unsafe_of_string x |> Digestif.Bytes.of_hex `SHA224)
  |> List.map (fun s -> s, to_bigstring s)
  |> List.split

let results_sha256_by, results_sha256_bi =
  [ "2178f5f21b4311607bf9347bcde5f6552edb9ec5aa13b954d53de2fbfd8b75de"
  ; "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
  ; "aa36cd61caddefe26b07ba1d3d07ea978ed575c9d1f921837dff9f73e019713e"
  ; "a7c8b53d68678a8e6e4d403c6b97cf0f82c4ef7b835c41039c0a73aa4d627d05"
  ; "b2a83b628f7e0da71c3879b81075775072d0d35935c62cc6c5a79b337ccccca1" ]
  |> List.map (fun x -> Bytes.unsafe_of_string x |> Digestif.Bytes.of_hex `SHA256)
  |> List.map (fun s -> s, to_bigstring s)
  |> List.split

let results_sha384_by, results_sha384_bi =
  [ "43e75797c1d875c5e5e7e90d0525061703d6b95b6137461566c2d067304458e62c144bbe12c0b741dcfaa38f7d41575e"
  ; "af45d2e376484031617f78d2b58a6b1b9c7ef464f5a01b47e42ec3736322445e8e2240ca5e69e2c78b3239ecfab21649"
  ; "bd3b5c82edcd0f206aadff7aa89dbbc3a7655844ffc9f8f9fa17c90eb36b13ec7828fba7252c3f5d90cff666ea44d557"
  ; "16461c2a44877c69fb38e4dce2edc822d68517917fc84d252de64132bd43c7cbe3310b7e8661741b7728000e8abf51e0"
  ; "2c3751d1dc792344514928fad94672a256cf2f66344e4df96b0cc4cc3f6800aa5a628e9becf5f65672e1acf013284893" ]
  |> List.map (fun x -> Bytes.unsafe_of_string x |> Digestif.Bytes.of_hex `SHA384)
  |> List.map (fun s -> s, to_bigstring s)
  |> List.split

let results_sha512_by, results_sha512_bi =
  [ "5f26752be4a1282646ed8c6a611d4c621e22e3fa96e9e6bc9e19a86deaacf0315151c46f779c3184632ab5793e2ddcb2ff87ca11cc886130f033364b08aef4e2"
  ; "164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737"
  ; "c2f2077f538171d7c6cbee0c94948f82987117a50229fb0b48a534e3c63553a9a9704cdb460c597c8b46b631e49c22a9d2d46bded40f8a77652f754ec725e351"
  ; "89d7284e89642ec195f7a8ef098ef4e411fa3df17a07724cf13033bc6b7863968aad449cee973df9b92800d803ba3e14244231a86253cfacd1de882a542e945f"
  ; "f6ecfca37d2abcff4b362f1919629e784c4b618af77e1061bb992c11d7f518716f5df5978b0a1455d68ceeb10ced9251306d2f26181407be76a219d48c36b592" ]
  |> List.map (fun x -> Bytes.unsafe_of_string x |> Digestif.Bytes.of_hex `SHA512)
  |> List.map (fun s -> s, to_bigstring s)
  |> List.split

let results_blake2b_by, results_blake2b_bi =
  [ "47e8cd6c879a6c1f9b8f89d202c8000f11ad42636e4b967fbad8ea31489eeec8d80583af1418816acac808c6787b8d72226bcbb9116ffc8fa0e7519dc1e67501"
  ; "380246f80263db862b00d41ebb70e6d26fa97c4b42ae7985991deb963b4317aa33735ff9dc76bd294455731365ab3a9eb67d33f83f98360f2bae5f7a4356e6b1"
  ; "948194b8cffdafa5ceb8d56f02be0dee66014d5b8f7ca3536863334a498d073e2ef64c66e6933a2a3ae952aac4838a679fa49846133349f58cbd0db029ba0b3a"
  ; "c3a0eec1f5a3c60064e40de1b2ce0657edfde39ac23036350f4467ce1adf2756e5f7fd536b6d68646b48b26708649db1b25c3c98522b3ce532e2fd159b0d5f0e"
  ; "0b6cb224edfd69df745a102660c4629cc75c6ba25c342702815744d41434e75a451560d692dd64cec0fe5cace12385c807b4a6244cf1849c3566c3cc48d71e74" ]
  |> List.map (fun x -> Bytes.unsafe_of_string x |> Digestif.Bytes.of_hex `BLAKE2B)
  |> List.map (fun s -> s, to_bigstring s)
  |> List.split

let results_rmd160_by, results_rmd160_bi =
  [ "65b3cb3360881842a0d454bd6e7bc1bfe838b384"
  ; "dda6c0213a485a9e24f4742064a7f033b43c4069"
  ; "f071dcd2514fd89de78a5a2db1128dfa3e54d503"
  ; "bda5511e63389385218a8d902a70f2d8dc4dc074"
  ; "6c2486f169432281b6d71ae5b6765239c3cc1ea6" ]
  |> List.map (fun x -> Bytes.unsafe_of_string x |> Digestif.Bytes.of_hex `RMD160)
  |> List.map (fun s -> s, to_bigstring s)
  |> List.split

let results_blake2s_by, results_blake2s_bi =
  [ "73842d110e4c2b77a9231c9dbc76b791a2ea9b1e660a8237c0c8c99caa653628"
  ; "d7fe099d889ba98178a934de6bd36da084600d7831ff16b8deaeefa8f6c00af4"
  ; "88f53c94bf50819acd1d5db805c61fed44de72d58962802780b9972cf974274b"
  ; "af80be61a4103fc5daac2fe4b70125f146999850627d63a38aa416e59f237644"
  ; "238e5dd72ac0ab3fb43899714123c0e194e6c3429a1727f093d80175010ba67b" ]
  |> List.map (fun x -> Bytes.unsafe_of_string x |> Digestif.Bytes.of_hex `BLAKE2S)
  |> List.map (fun s -> s, to_bigstring s)
  |> List.split

let tests () =
  Alcotest.run "digestif"
    [ "md5",                 makes ~name:"md5"     bytes     `MD5     keys_by inputs_by results_md5_by
    ; "md5 (bigstring)",     makes ~name:"md5"     bigstring `MD5     keys_bi inputs_bi results_md5_bi
    ; "sha1",                makes ~name:"sha1"    bytes     `SHA1    keys_by inputs_by results_sha1_by
    ; "sha1 (bigstring)",    makes ~name:"sha1"    bigstring `SHA1    keys_bi inputs_bi results_sha1_bi
    ; "sha224",              makes ~name:"sha224"  bytes     `SHA224  keys_by inputs_by results_sha224_by
    ; "sha224 (bigstring)",  makes ~name:"sha224"  bigstring `SHA224  keys_bi inputs_bi results_sha224_bi
    ; "sha256",              makes ~name:"sha256"  bytes     `SHA256  keys_by inputs_by results_sha256_by
    ; "sha256 (bigstring)",  makes ~name:"sha256"  bigstring `SHA256  keys_bi inputs_bi results_sha256_bi
    ; "sha384",              makes ~name:"sha384"  bytes     `SHA384  keys_by inputs_by results_sha384_by
    ; "sha384 (bigstring)",  makes ~name:"sha384"  bigstring `SHA384  keys_bi inputs_bi results_sha384_bi
    ; "sha512",              makes ~name:"sha512"  bytes     `SHA512  keys_by inputs_by results_sha512_by
    ; "sha512 (bigstring)",  makes ~name:"sha512"  bigstring `SHA512  keys_bi inputs_bi results_sha512_bi
    ; "blake2b",             makes ~name:"blake2b" bytes     `BLAKE2B keys_by inputs_by results_blake2b_by
    ; "blake2b (bigstring)", makes ~name:"blake2b" bigstring `BLAKE2B keys_bi inputs_bi results_blake2b_bi
    ; "rmd160",              makes ~name:"rmd160"  bytes     `RMD160  keys_by inputs_by results_rmd160_by
    ; "rmd160 (bigstring)",  makes ~name:"rmd160"  bigstring `RMD160  keys_bi inputs_bi results_rmd160_bi
    ; "blake2s",             makes ~name:"blake2s" bytes     `BLAKE2S keys_by inputs_by results_blake2s_by
    ; "blake2s (bigstring)", makes ~name:"blake2s" bigstring `BLAKE2S keys_bi inputs_bi results_blake2s_bi ]

let () = tests ()
