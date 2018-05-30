module Bi         = Digestif_bigstring
module By         = Digestif_bytes
module Pp         = Digestif_pp
module Xor        = Baijiu_xor

module type S = Digestif_sig.S
module type T = Digestif_sig.T

module type Desc =
sig
  val digest_size : int
  val block_size  : int
end

module type BUFFER = Baijiu_buffer.S

module type Convenience =
sig
  type t

  val compare : t -> t -> int
  val eq      : t -> t -> bool
  val neq     : t -> t -> bool
end

module type Hash =
  functor (Buffer : BUFFER) -> sig
    type ctx
    type buffer

    val init : unit -> ctx
    val feed : ctx -> buffer -> int -> int -> unit
    val feed_bytes : ctx -> By.t -> int -> int -> unit
    val feed_bigstring : ctx -> Bi.t -> int -> int -> unit
    val get  : ctx -> buffer
    val dup  : ctx -> ctx
  end with type buffer = Buffer.buffer

module Core (Hash : Hash) (D : Desc) =
struct
  let digest_size = D.digest_size
  let block_size  = D.block_size

  module Bigstring =
  struct
    include (Bi : Convenience with type t = Bi.t)
    include Hash (struct include Bi type buffer = t end)
    include Pp.Make (Bi) (D)

    let empty = init ()

    let unsafe_feed_bigstring ctx buf =
      feed_bigstring ctx buf 0 (Bi.length buf)

    let unsafe_feed_bytes ctx buf =
      feed_bytes ctx buf 0 (By.length buf)

    let feed_bigstring t buf =
      let t = dup t in
      ( unsafe_feed_bigstring t buf; t )

    let feed_bytes t buf =
      let t = dup t in
      ( unsafe_feed_bytes t buf; t )

    let feed = feed_bigstring

    let feedi_bigstring t iter =
      let t = dup t in
      ( iter (unsafe_feed_bigstring t); t )

    let feedi_bytes t iter =
      let t = dup t in
      ( iter (unsafe_feed_bytes t); t )

    let feedi = feedi_bigstring

    let unsafe_get = get
    let get t = let t = dup t in unsafe_get t

    let digest_bigstring buf = feed_bigstring empty buf |> get
    let digest_bytes buf = feed_bytes empty buf |> get
    let digest = digest_bigstring

    let digesti_bigstring iter = feedi_bigstring empty iter |> get
    let digesti_bytes iter = feedi_bytes empty iter |> get
    let digesti = digesti_bigstring

    let digestv bufs = digesti (fun f -> List.iter f bufs)
  end

  module Bytes =
  struct
    include (By : Convenience with type t = By.t)
    include Hash (struct include By type buffer = t end)
    include Pp.Make (By) (D)

    let empty : ctx = init ()

    let unsafe_feed_bigstring ctx buf =
      feed_bigstring ctx buf 0 (Bi.length buf)

    let unsafe_feed_bytes ctx buf =
      feed_bytes ctx buf 0 (By.length buf)

    let feed_bytes t buf =
      let t = dup t in
      ( unsafe_feed_bytes t buf; t )

    let feed_bigstring t buf =
      let t = dup t in
      ( unsafe_feed_bigstring t buf; t )

    let feed = feed_bytes

    let feedi_bytes t iter =
      let t = dup t in
      ( iter (unsafe_feed_bytes t); t )

    let feedi_bigstring t iter =
      let t = dup t in
      ( iter (unsafe_feed_bigstring t); t )

    let feedi = feedi_bytes

    let unsafe_get = get
    let get t = let t = dup t in unsafe_get t

    let digest_bytes buf = feed_bytes empty buf |> get
    let digest_bigstring buf = feed_bigstring empty buf |> get
    let digest = digest_bytes

    let digesti_bytes iter = feedi_bytes empty iter |> get
    let digesti_bigstring iter = feedi_bigstring empty iter |> get
    let digesti = digesti_bytes

    let digestv bufs = digesti (fun f -> List.iter f bufs)
  end
end

module Make (H : Hash) (D : Desc) =
struct
  module C = Core (H) (D)

  let block_size  = C.block_size
  and digest_size = C.digest_size
  and ctx_size = 0

  module Bytes =
  struct
    module Xor = Xor.Make (struct include By type buffer = t end)

    include C.Bytes

    let opad = By.init C.block_size (fun _ -> '\x5c')
    let ipad = By.init C.block_size (fun _ -> '\x36')

    let rec norm key =
      match Pervasives.compare (By.length key) C.block_size with
      | 1  -> norm (C.Bytes.digest key)
      | -1 -> By.rpad key C.block_size '\000'
      | _  -> key

    let hmaci ~key iter =
      let key = norm key in
      let outer = Xor.xor key opad in
      let inner = Xor.xor key ipad in
      let res = C.Bytes.digesti (fun f -> f inner; iter f) in
      C.Bytes.digesti (fun f -> f outer; f res)

    let hmac ~key msg = hmaci ~key (fun f -> f msg)
    let hmacv ~key bufs = hmaci ~key (fun f -> List.iter f bufs)
  end

  module Bigstring =
  struct
    module Xor = Xor.Make (struct include Bi type buffer = t end)
    include C.Bigstring

    let opad = Bi.init C.block_size (fun _ -> '\x5c')
    let ipad = Bi.init C.block_size (fun _ -> '\x36')

    let rec norm key =
      match Pervasives.compare (Bi.length key) C.block_size with
      | 1  -> norm (C.Bigstring.digest key)
      | -1 -> Bi.rpad key C.block_size '\000'
      | _  -> key

    let hmaci ~key iter =
      let key = norm key in
      let outer = Xor.xor key opad in
      let inner = Xor.xor key ipad in
      let res = C.Bigstring.digesti (fun f -> f inner; iter f) in
      C.Bigstring.digesti (fun f -> f outer; f res)

    let hmac ~key msg = hmaci ~key (fun f -> f msg)
    let hmacv ~key bufs = hmaci ~key (fun f -> List.iter f bufs)
  end
end

module type HashBLAKE2 =
  functor (Buffer : BUFFER) -> sig
    type ctx
    type buffer

    val with_outlen_and_key : int -> buffer -> int -> int -> ctx
    val feed : ctx -> buffer -> int -> int -> unit
    val feed_bytes : ctx -> By.t -> int -> int -> unit
    val feed_bigstring : ctx -> Bi.t -> int -> int -> unit
    val get  : ctx -> buffer
    val dup  : ctx -> ctx
  end with type buffer = Buffer.buffer

let empty_bytes = By.create 0
let empty_bigstring = Bi.create 0

module Make_BLAKE2 (Hash : HashBLAKE2) (D : Desc) : S =
struct
  let digest_size = D.digest_size

  module Bytes =
  struct
    include (By : Convenience with type t = By.t)
    include Hash (struct include By type buffer = t end)
    include Pp.Make (By) (D)

    let init () =
      with_outlen_and_key digest_size empty_bytes 0 0
    let unsafe_feed_bytes ctx buf =
      feed_bytes ctx buf 0 (By.length buf)
    let unsafe_feed_bigstring ctx buf =
      feed_bigstring ctx buf 0 (Bi.length buf)

    let empty = init ()

    let feed_bytes t buf =
      let t = dup t in
      ( unsafe_feed_bytes t buf; t )

    let feed_bigstring t buf =
      let t = dup t in
      ( unsafe_feed_bigstring t buf; t )

    let feed = feed_bytes

    let feedi_bytes t iter =
      let t = dup t in
      ( iter (unsafe_feed_bytes t); t )

    let feedi_bigstring t iter =
      let t = dup t in
      ( iter (unsafe_feed_bigstring t); t )

    let feedi = feedi_bytes

    let unsafe_get = get
    let get t = let t = dup t in unsafe_get t

    let digest_bytes buf = feed_bytes empty buf |> get
    let digest_bigstring buf = feed_bigstring empty buf |> get
    let digest = digest_bytes

    let digesti_bytes iter = feedi_bytes empty iter |> get
    let digesti_bigstring iter = feedi_bigstring empty iter |> get
    let digesti = digesti_bytes

    let digestv bufs = digesti (fun f -> List.iter f bufs)

    let hmaci ~key iter =
      let ctx = with_outlen_and_key digest_size key 0 (By.length key) in
      feedi_bytes ctx iter |> get

    let hmac ~key msg = hmaci ~key (fun f -> f msg)
    let hmacv ~key bufs = hmaci ~key (fun f -> List.iter f bufs)
  end

  module Bigstring =
  struct
    include (Bi : Convenience with type t = Bi.t)
    include Hash (struct include Bi type buffer = t end)
    include Pp.Make (Bi) (D)

    let init () =
      with_outlen_and_key digest_size empty_bigstring 0 0
    let unsafe_feed_bytes ctx buf =
      feed_bytes ctx buf 0 (By.length buf)
    let unsafe_feed_bigstring ctx buf =
      feed_bigstring ctx buf 0 (Bi.length buf)

    let empty = init ()

    let feed_bytes t buf =
      let t = dup t in
      ( unsafe_feed_bytes t buf; t )

    let feed_bigstring t buf =
      let t = dup t in
      ( unsafe_feed_bigstring t buf; t )

    let feed = feed_bigstring

    let feedi_bytes t iter =
      let t = dup t in
      ( iter (unsafe_feed_bytes t); t )

    let feedi_bigstring t iter =
      let t = dup t in
      ( iter (unsafe_feed_bigstring t); t )

    let feedi = feedi_bigstring

    let unsafe_get = get
    let get t = let t = dup t in unsafe_get t

    let digest_bytes buf = feed_bytes empty buf |> get
    let digest_bigstring buf = feed_bigstring empty buf |> get
    let digest = digest_bigstring

    let digesti_bytes iter = feedi_bytes empty iter |> get
    let digesti_bigstring iter = feedi_bigstring empty iter |> get
    let digesti = digesti_bigstring

    let digestv bufs = digesti (fun f -> List.iter f bufs)

    let hmaci ~key iter =
      let ctx = with_outlen_and_key digest_size key 0 (Bi.length key) in
      feedi_bigstring ctx iter |> get

    let hmac ~key msg = hmaci ~key (fun f -> f msg)
    let hmacv ~key bufs = hmaci ~key (fun f -> List.iter f bufs)
  end
end

module MD5     : S = Make (Baijiu_md5.Make) (struct let (digest_size, block_size) = (16, 64) end)
module SHA1    : S = Make (Baijiu_sha1.Make)   (struct let (digest_size, block_size) = (20, 64) end)
module SHA224  : S = Make (Baijiu_sha224.Make) (struct let (digest_size, block_size) = (28, 64) end)
module SHA256  : S = Make (Baijiu_sha256.Make) (struct let (digest_size, block_size) = (32, 64) end)
module SHA384  : S = Make (Baijiu_sha384.Make) (struct let (digest_size, block_size) = (48, 128) end)
module SHA512  : S = Make (Baijiu_sha512.Make) (struct let (digest_size, block_size) = (64, 128) end)
module BLAKE2B = Make_BLAKE2(Baijiu_blake2b.Make) (struct let (digest_size, block_size) = (64, 128) end)
module BLAKE2S = Make_BLAKE2(Baijiu_blake2s.Make) (struct let (digest_size, block_size) = (32, 64) end)
module RMD160  : S = Make (Baijiu_rmd160.Make) (struct let (digest_size, block_size) = (20, 64) end)

module MakeBLAKE2B (D : sig val digest_size : int end) : S =
struct
  include Make_BLAKE2(Baijiu_blake2b.Make)(struct let (digest_size, block_size) = (D.digest_size, 128) end)
end

module MakeBLAKE2S (D : sig val digest_size : int end) : S =
struct
  include Make_BLAKE2(Baijiu_blake2s.Make)(struct let (digest_size, block_size) = (D.digest_size, 64) end)
end

include Digestif_hash

let module_of hash =
  let b2b = Hashtbl.create 13 in
  let b2s = Hashtbl.create 13 in
  match hash with
  | Digestif_sig.MD5     -> (module MD5     : S)
  | Digestif_sig.SHA1    -> (module SHA1    : S)
  | Digestif_sig.RMD160  -> (module RMD160  : S)
  | Digestif_sig.SHA224  -> (module SHA224  : S)
  | Digestif_sig.SHA256  -> (module SHA256  : S)
  | Digestif_sig.SHA384  -> (module SHA384  : S)
  | Digestif_sig.SHA512  -> (module SHA512  : S)
  | Digestif_sig.BLAKE2B digest_size -> begin
      match Hashtbl.find b2b digest_size with
      | exception Not_found ->
        let m = (module MakeBLAKE2B(struct let digest_size = digest_size end) : S) in
        Hashtbl.replace b2b digest_size m ;
        m
      | m -> m
    end
  | Digestif_sig.BLAKE2S digest_size -> begin
      match Hashtbl.find b2s digest_size with
      | exception Not_found ->
        let m = (module MakeBLAKE2S(struct let digest_size = digest_size end) : S) in
        Hashtbl.replace b2s digest_size m ;
        m
      | m -> m
    end

module Bytes =
struct
  type t = Bytes.t
  type buffer = Bytes.t

  let digest hash =
    let module H = (val (module_of hash)) in
    H.Bytes.digest

  let digestv hash =
    let module H = (val (module_of hash)) in
    H.Bytes.digestv

  let mac hash =
    let module H = (val (module_of hash)) in
    H.Bytes.hmac

  let macv hash =
    let module H = (val (module_of hash)) in
    H.Bytes.hmacv

  let of_hex hash =
    let module H = (val (module_of hash)) in
    H.Bytes.of_hex

  let to_hex hash =
    let module H = (val (module_of hash)) in
    H.Bytes.to_hex

  let pp hash =
    let module H = (val (module_of hash)) in
    H.Bytes.pp
end

module Bigstring =
struct
  type t = Bi.t
  type buffer = Bi.t

  let digest hash =
    let module H = (val (module_of hash)) in
    H.Bigstring.digest

  let digestv hash =
    let module H = (val (module_of hash)) in
    H.Bigstring.digestv

  let mac hash =
    let module H = (val (module_of hash)) in
    H.Bigstring.hmac

  let macv hash =
    let module H = (val (module_of hash)) in
    H.Bigstring.hmacv

  let of_hex hash =
    let module H = (val (module_of hash)) in
    H.Bigstring.of_hex

  let to_hex hash =
    let module H = (val (module_of hash)) in
    H.Bigstring.to_hex

  let pp hash =
    let module H = (val (module_of hash)) in
    H.Bigstring.pp
end

let digest_size hash =
  let module H = (val (module_of hash)) in
  H.digest_size
