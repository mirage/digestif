module Bi = Digestif_bigstring

module type S =
sig
  val digest_size : int

  module Bigstring :
  sig
    type buffer = Bi.t
    type ctx
    type t = Bi.t

    val init           : unit -> ctx
    val feed           : ctx -> buffer -> unit
    val feed_bytes     : ctx -> Bytes.t -> unit
    val feed_bigstring : ctx -> Bi.t -> unit
    val get            : ctx -> t

    val digest         : buffer -> t
    val digestv        : buffer list -> t
    val hmac           : key:buffer -> buffer -> t
    val hmacv          : key:buffer -> buffer list -> t

    val compare        : t -> t -> int
    val eq             : t -> t -> bool
    val neq            : t -> t -> bool

    val pp             : Format.formatter -> t -> unit
    val of_hex         : buffer -> t
    val to_hex         : t -> buffer
  end

  module Bytes :
  sig
    type buffer = Bytes.t
    type ctx
    type t = Bytes.t

    val init           : unit -> ctx
    val feed           : ctx -> buffer -> unit
    val feed_bytes     : ctx -> Bytes.t -> unit
    val feed_bigstring : ctx -> Bi.t -> unit
    val get            : ctx -> t

    val digest         : buffer -> t
    val digestv        : buffer list -> t
    val hmac           : key:buffer -> buffer -> t
    val hmacv          : key:buffer -> buffer list -> t

    val compare        : t -> t -> int
    val eq             : t -> t -> bool
    val neq            : t -> t -> bool

    val pp             : Format.formatter -> t -> unit
    val of_hex         : buffer -> t
    val to_hex         : t -> buffer
  end
end

let not_implemented () = raise (Failure "You need to link your library/program with digestif.c or digestif.ocaml")

module Not_implemented =
struct
  let digest_size = 0

  module I =
  struct
    let init () = not_implemented ()
    let feed           _ _ = not_implemented ()
    let feed_bytes     _ _ = not_implemented ()
    let feed_bigstring _ _ = not_implemented ()
    let get            _   = not_implemented ()

    let digest         _ = not_implemented ()
    let digestv        _ = not_implemented ()
    let hmac           ~key:_ _ = not_implemented ()
    let hmacv          ~key:_ _ = not_implemented ()

    let compare        _ _ = not_implemented ()
    let eq             _ _ = not_implemented ()
    let neq            _ _ = not_implemented ()

    let pp             _ _ = not_implemented ()
    let of_hex         _   = not_implemented ()
    let to_hex         _   = not_implemented ()
  end

  module Bigstring =
  struct
    type buffer = Bi.t
    type ctx
    type t = Bi.t

    include I
  end

  module Bytes =
  struct
    type buffer = Bytes.t
    type ctx
    type t = Bytes.t

    include I
  end
end

module MD5     : S = Not_implemented
module SHA1    : S = Not_implemented
module SHA224  : S = Not_implemented
module SHA256  : S = Not_implemented
module SHA384  : S = Not_implemented
module SHA512  : S = Not_implemented
module BLAKE2B : S = Not_implemented
module RMD160  : S = Not_implemented

type hash =
  [ `MD5
  | `SHA1
  | `SHA224
  | `SHA256
  | `SHA384
  | `SHA512
  | `BLAKE2B
  | `RMD160 ]

module Bytes =
struct
  let digest  _ _ = not_implemented ()
  let digest  _ _ = not_implemented ()
  let digestv _ _ = not_implemented ()
  let mac     _ ~key:_ _ = not_implemented ()
  let macv    _ ~key:_ _ = not_implemented ()

  let pp      _ _ _ = not_implemented ()
  let of_hex  _ _ = not_implemented ()
  let to_hex  _ _ = not_implemented ()
end

module Bigstring =
struct
  let digest  _ _ = not_implemented ()
  let digest  _ _ = not_implemented ()
  let digestv _ _ = not_implemented ()
  let mac     _ ~key:_ _ = not_implemented ()
  let macv    _ ~key:_ _ = not_implemented ()

  let pp      _ _ _ = not_implemented ()
  let of_hex  _ _ = not_implemented ()
  let to_hex  _ _ = not_implemented ()
end

let digest_size _ = not_implemented ()
