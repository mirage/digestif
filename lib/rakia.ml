module Bi         = Digestif_bigstring
module By         = Digestif_bytes
module Native     = Rakia_native

module type S =
sig
  type ctx

  val digest_size : int

  module Bigstring :
  sig
    type buffer = Native.ba

    val init        : unit -> ctx
    val feed        : ctx -> buffer -> unit
    val get         : ctx -> buffer

    val digest      : buffer -> buffer
    val digestv     : buffer list -> buffer
    val hmac        : key:buffer -> buffer -> buffer
    val hmacv       : key:buffer -> buffer list -> buffer
  end

  module Bytes :
  sig
    type buffer = Native.st

    val init        : unit -> ctx
    val feed        : ctx -> buffer -> unit
    val get         : ctx -> buffer

    val digest      : buffer -> buffer
    val digestv     : buffer list -> buffer
    val hmac        : key:buffer -> buffer -> buffer
    val hmacv       : key:buffer -> buffer list -> buffer
  end
end

module type Foreign =
sig
  open Native

  module Bigstring :
  sig
    val init     : ctx -> unit
    val update   : ctx -> ba -> int -> int -> unit
    val finalize : ctx -> ba -> int -> unit
  end

  module Bytes :
  sig
    val init     : ctx -> unit
    val update   : ctx -> st -> int -> int -> unit
    val finalize : ctx -> st -> int -> unit
  end

  val ctx_size   : unit -> int
end

module type Desc =
sig
  val block_size  : int
  val digest_size : int
end

module Core (F : Foreign) (D : Desc) =
struct
  let block_size  = D.block_size
  and digest_size = D.digest_size
  and ctx_size    = F.ctx_size ()

  module Bytes =
  struct
    type buffer = Native.st

    let init () =
      let t = Bi.create ctx_size in
      ( F.Bytes.init t; t )

    let feed t buf =
      F.Bytes.update t buf 0 (By.length buf)

    let get t =
      let res = By.create digest_size in
      F.Bytes.finalize t res 0;
      res

    let digest buf =
      let t = init () in ( feed t buf; get t )

    let digestv bufs =
      let t = init () in ( List.iter (feed t) bufs; get t )
  end

  module Bigstring =
  struct
    type buffer = Native.ba

    let init () =
      let t = Bi.create ctx_size in
      ( F.Bigstring.init t; t )

    let feed t buf =
      F.Bigstring.update t buf 0 (Bi.length buf)

    let get t =
      let res = Bi.create digest_size in
      F.Bigstring.finalize t res 0;
      res

    let digest buf =
      let t = init () in ( feed t buf; get t )

    let digestv bufs =
      let t = init () in ( List.iter (feed t) bufs; get t )
  end
end

module Make (F : Foreign) (D : Desc) =
struct
  type ctx = Native.ctx

  module C = Core (F) (D)

  let block_size  = C.block_size
  and digest_size = C.digest_size
  and ctx_size    = C.ctx_size

  module Bytes =
  struct
    include C.Bytes

    let opad = By.init C.block_size (fun _ -> '\x5c')
    let ipad = By.init C.block_size (fun _ -> '\x35')

    let rec norm key =
      match compare (By.length key) C.block_size with
      | 1  -> norm (C.Bytes.digest key)
      | -1 -> By.rpad key C.block_size '\000'
      | _  -> key

    let hmacv ~key msg =
      let key = norm key in
      let outer = Native.XOR.Bytes.xor key opad in
      let inner = Native.XOR.Bytes.xor key ipad in
      C.Bytes.digestv [ outer; C.Bytes.digestv (inner :: msg) ]

    let hmac ~key msg = hmacv ~key [ msg ]
  end

  module Bigstring =
  struct
    include C.Bigstring

    let opad = Bi.init C.block_size (fun _ -> '\x5c')
    let ipad = Bi.init C.block_size (fun _ -> '\x36')

    let rec norm key =
      match compare (Bi.length key) C.block_size with
      | 1  -> norm (C.Bigstring.digest key)
      | -1 -> Bi.rpad key C.block_size '\000'
      | _  -> key

    let hmacv ~key msg =
      let key = norm key in
      let outer = Native.XOR.Bigstring.xor key opad in
      let inner = Native.XOR.Bigstring.xor key ipad in
      C.Bigstring.digestv [ outer; C.Bigstring.digestv (inner :: msg) ]

    let hmac ~key msg = hmacv ~key [ msg ]
  end
end

module MD5     : S = Make (Native.MD5)    (struct let (digest_size, block_size) = (16, 64) end)
module SHA1    : S = Make (Native.SHA1)   (struct let (digest_size, block_size) = (20, 64) end)
module SHA224  : S = Make (Native.SHA224) (struct let (digest_size, block_size) = (28, 64) end)
module SHA256  : S = Make (Native.SHA256) (struct let (digest_size, block_size) = (32, 64) end)
module SHA384  : S = Make (Native.SHA384) (struct let (digest_size, block_size) = (48, 128) end)
module SHA512  : S = Make (Native.SHA512) (struct let (digest_size, block_size) = (64, 128) end)

module Make_BLAKE2B (F : Foreign) (D : Desc) : S =
struct
  type ctx = Native.ctx

  let block_size  = D.block_size
  and digest_size = D.digest_size
  and ctx_size    = F.ctx_size ()
  and key_size    = Native.BLAKE2B.key_size ()

  module Bytes =
  struct
    type buffer = Native.st

    let init () =
      let t = Bi.create ctx_size in
      ( Native.BLAKE2B.Bytes.init' t digest_size Bytes.empty 0 0; t )

    let feed t buf =
      F.Bytes.update t buf 0 (Bytes.length buf)

    let get t =
      let res = Bytes.create digest_size in
      F.Bytes.finalize t res 0;
      res

    let digest buf =
      let t = init () in ( feed t buf; get t )

    let digestv bufs =
      let t = init () in ( List.iter (feed t) bufs; get t )

    let hmacv ~key msg =
      let ctx = Bi.create ctx_size in
      let res = By.create digest_size in
      Native.BLAKE2B.Bytes.init' ctx digest_size key 0 (By.length key);
      List.iter (fun x -> F.Bytes.update ctx x 0 (By.length x)) msg;
      F.Bytes.finalize ctx res 0;
      res

    let hmac ~key msg =
      hmacv ~key [ msg ]
  end

  module Bigstring =
  struct
    type buffer = Native.ba

    let init () =
      let t = Bi.create ctx_size in
      ( Native.BLAKE2B.Bigstring.init' t digest_size Bi.empty 0 0; t )

    let feed t buf =
      F.Bigstring.update t buf 0 (Bi.length buf)

    let get t =
      let res = Bi.create digest_size in
      F.Bigstring.finalize t res 0;
      res

    let digest buf =
      let t = init () in ( feed t buf; get t )

    let digestv bufs =
      let t = init () in ( List.iter (feed t) bufs; get t )

    let hmacv ~key msg =
      if Bi.length key > key_size
      then raise (Invalid_argument "BLAKE2B.hmac{v}: invalid key");

      let ctx = Bi.create ctx_size in
      let res = Bi.create digest_size in
      Native.BLAKE2B.Bigstring.init' ctx digest_size key 0 (Bi.length key);
      List.iter (fun x -> F.Bigstring.update ctx x 0 (Bi.length x)) msg;
      F.Bigstring.finalize ctx res 0;
      res

    let hmac ~key msg =
      hmacv ~key [ msg ]
  end
end

module BLAKE2B = Make_BLAKE2B(Native.BLAKE2B) (struct let (digest_size, block_size) = (64, 128) end)

type hash =
  [ `MD5
  | `SHA1
  | `SHA224
  | `SHA256
  | `SHA384
  | `SHA512
  | `BLAKE2B ]

let module_of = function
  | `MD5     -> (module MD5     : S)
  | `SHA1    -> (module SHA1    : S)
  | `SHA224  -> (module SHA224  : S)
  | `SHA256  -> (module SHA256  : S)
  | `SHA384  -> (module SHA384  : S)
  | `SHA512  -> (module SHA512  : S)
  | `BLAKE2B -> (module BLAKE2B : S)

module Bytes =
struct
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
end

module Bigstring =
struct
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
end

let digest_size hash =
  let module H = (val (module_of hash)) in
  H.digest_size
