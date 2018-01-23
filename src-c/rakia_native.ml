(* Copyright (c) 2014-2016 David Kaloper Meršinjak

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)

module Bigstring = Digestif_bigstring

type off    = int
type size   = int
type ba     = Bigstring.t
type st     = Bytes.t
type ctx    = Bigstring.t

module MD5 =
struct
  module Bigstring =
  struct
    external init     : ctx -> unit
                      = "caml_digestif_md5_ba_init"
                      [@@noalloc]
    external update   : ctx -> ba -> off -> size -> unit
                      = "caml_digestif_md5_ba_update"
                      [@@noalloc]
    external finalize : ctx -> ba -> off -> unit
                      = "caml_digestif_md5_ba_finalize"
                      [@@noalloc]
  end

  module Bytes =
  struct
    external init     : ctx -> unit
                      = "caml_digestif_md5_st_init"
                      [@@noalloc]
    external update   : ctx -> st -> off -> size -> unit
                      = "caml_digestif_md5_st_update"
                      [@@noalloc]
    external finalize : ctx -> st -> off -> unit
                      = "caml_digestif_md5_st_finalize"
                      [@@noalloc]
  end

  external ctx_size   : unit -> int
                      = "caml_digestif_md5_ctx_size"
                      [@@noalloc]
end

module SHA1 =
struct
  module Bigstring =
  struct
    external init     : ctx -> unit
                      = "caml_digestif_sha1_ba_init"
                      [@@noalloc]
    external update   : ctx -> ba -> off -> size -> unit
                      = "caml_digestif_sha1_ba_update"
                      [@@noalloc]
    external finalize : ctx -> ba -> off -> unit
                      = "caml_digestif_sha1_ba_finalize"
                      [@@noalloc]
  end

  module Bytes =
  struct
    external init     : ctx -> unit
                      = "caml_digestif_sha1_st_init"
                      [@@noalloc]
    external update   : ctx -> st -> off -> size -> unit
                      = "caml_digestif_sha1_st_update"
                      [@@noalloc]
    external finalize : ctx -> st -> off -> unit
                      = "caml_digestif_sha1_st_finalize"
                      [@@noalloc]
  end

  external ctx_size   : unit -> int
                      = "caml_digestif_sha1_ctx_size"
                      [@@noalloc]
end

module SHA224 =
struct
  module Bigstring =
  struct
    external init     : ctx -> unit
                      = "caml_digestif_sha224_ba_init"
                      [@@noalloc]
    external update   : ctx -> ba -> off -> size -> unit
                      = "caml_digestif_sha224_ba_update"
                      [@@noalloc]
    external finalize : ctx -> ba -> off -> unit
                      = "caml_digestif_sha224_ba_finalize"
                      [@@noalloc]
  end

  module Bytes =
  struct
    external init     : ctx -> unit
                      = "caml_digestif_sha224_st_init"
                      [@@noalloc]
    external update   : ctx -> st -> off -> size -> unit
                      = "caml_digestif_sha224_st_update"
                      [@@noalloc]
    external finalize : ctx -> st -> off -> unit
                      = "caml_digestif_sha224_st_finalize"
                      [@@noalloc]
  end

  external ctx_size   : unit -> int
                      = "caml_digestif_sha224_ctx_size"
                      [@@noalloc]
end

module SHA256 =
struct
  module Bigstring =
  struct
    external init     : ctx -> unit
                      = "caml_digestif_sha256_ba_init"
                      [@@noalloc]
    external update   : ctx -> ba -> off -> size -> unit
                      = "caml_digestif_sha256_ba_update"
                      [@@noalloc]
    external finalize : ctx -> ba -> off -> unit
                      = "caml_digestif_sha256_ba_finalize"
                      [@@noalloc]
  end

  module Bytes =
  struct
    external init     : ctx -> unit
                      = "caml_digestif_sha256_st_init"
                      [@@noalloc]
    external update   : ctx -> st -> off -> size -> unit
                      = "caml_digestif_sha256_st_update"
                      [@@noalloc]
    external finalize : ctx -> st -> off -> unit
                      = "caml_digestif_sha256_st_finalize"
                      [@@noalloc]
  end

  external ctx_size   : unit -> int
                      = "caml_digestif_sha256_ctx_size"
                      [@@noalloc]
end

module SHA384 =
struct
  module Bigstring =
  struct
    external init     : ctx -> unit
                      = "caml_digestif_sha384_ba_init"
                      [@@noalloc]
    external update   : ctx -> ba -> off -> size -> unit
                      = "caml_digestif_sha384_ba_update"
                      [@@noalloc]
    external finalize : ctx -> ba -> off -> unit
                      = "caml_digestif_sha384_ba_finalize"
                      [@@noalloc]
  end

  module Bytes =
  struct
    external init     : ctx -> unit
                      = "caml_digestif_sha384_st_init"
                      [@@noalloc]
    external update   : ctx -> st -> off -> size -> unit
                      = "caml_digestif_sha384_st_update"
                      [@@noalloc]
    external finalize : ctx -> st -> off -> unit
                      = "caml_digestif_sha384_st_finalize"
                      [@@noalloc]
  end

  external ctx_size   : unit -> int
                      = "caml_digestif_sha384_ctx_size"
                      [@@noalloc]
end

module SHA512 =
struct
  module Bigstring =
  struct
    external init     : ctx -> unit
                      = "caml_digestif_sha512_ba_init"
                      [@@noalloc]
    external update   : ctx -> ba -> off -> size -> unit
                      = "caml_digestif_sha512_ba_update"
                      [@@noalloc]
    external finalize : ctx -> ba -> off -> unit
                      = "caml_digestif_sha512_ba_finalize"
                      [@@noalloc]
  end

  module Bytes =
  struct
    external init     : ctx -> unit
                      = "caml_digestif_sha512_st_init"
                      [@@noalloc]
    external update   : ctx -> st -> off -> size -> unit
                      = "caml_digestif_sha512_st_update"
                      [@@noalloc]
    external finalize : ctx -> st -> off -> unit
                      = "caml_digestif_sha512_st_finalize"
                      [@@noalloc]
  end

  external ctx_size   : unit -> int
                      = "caml_digestif_sha512_ctx_size"
                      [@@noalloc]
end

module BLAKE2B =
struct
  module Bigstring =
  struct
    external init     : ctx -> unit
                      = "caml_digestif_blake2b_ba_init"
                      [@@noalloc]
    external update   : ctx -> ba -> off -> size -> unit
                      = "caml_digestif_blake2b_ba_update"
                      [@@noalloc]
    external finalize : ctx -> ba -> off -> unit
                      = "caml_digestif_blake2b_ba_finalize"
                      [@@noalloc]
    external with_outlen_and_key : ctx -> size -> ba -> off -> size -> unit
                      = "caml_digestif_blake2b_ba_init_with_outlen_and_key"
                      [@@noalloc]
  end

  module Bytes =
  struct
    external init     : ctx -> unit
                      = "caml_digestif_blake2b_st_init"
                      [@@noalloc]
    external update   : ctx -> st -> off -> size -> unit
                      = "caml_digestif_blake2b_st_update"
                      [@@noalloc]
    external finalize : ctx -> st -> off -> unit
                      = "caml_digestif_blake2b_st_finalize"
                      [@@noalloc]
    external with_outlen_and_key : ctx -> size -> st -> off -> size -> unit
                      = "caml_digestif_blake2b_st_init_with_outlen_and_key"
                      [@@noalloc]
  end

  external ctx_size   : unit -> int
                      = "caml_digestif_blake2b_ctx_size"
                      [@@noalloc]

  external key_size   : unit -> int
                      = "caml_digestif_blake2b_key_size"
                      [@@noalloc]

  external digest_size : ctx -> int
                       = "caml_digestif_blake2b_digest_size"
                       [@@noalloc]
end

module BLAKE2S =
struct
  module Bigstring =
  struct
    external init     : ctx -> unit
                      = "caml_digestif_blake2s_ba_init"
                      [@@noalloc]
    external update   : ctx -> ba -> off -> size -> unit
                      = "caml_digestif_blake2s_ba_update"
                      [@@noalloc]
    external finalize : ctx -> ba -> off -> unit
                      = "caml_digestif_blake2s_ba_finalize"
                      [@@noalloc]
    external with_outlen_and_key : ctx -> size -> ba -> off -> size -> unit
                      = "caml_digestif_blake2s_ba_init_with_outlen_and_key"
                      [@@noalloc]
  end

  module Bytes =
  struct
    external init     : ctx -> unit
                      = "caml_digestif_blake2s_st_init"
                      [@@noalloc]
    external update   : ctx -> st -> off -> size -> unit
                      = "caml_digestif_blake2s_st_update"
                      [@@noalloc]
    external finalize : ctx -> st -> off -> unit
                      = "caml_digestif_blake2s_st_finalize"
                      [@@noalloc]
    external with_outlen_and_key : ctx -> size -> st -> off -> size -> unit
                      = "caml_digestif_blake2s_st_init_with_outlen_and_key"
                      [@@noalloc]
  end

  external ctx_size   : unit -> int
                      = "caml_digestif_blake2s_ctx_size"
                      [@@noalloc]

  external key_size   : unit -> int
                      = "caml_digestif_blake2s_key_size"
                      [@@noalloc]

  external digest_size : ctx -> int
                       = "caml_digestif_blake2s_digest_size"
                       [@@noalloc]
end

module RMD160 =
struct
  module Bigstring =
  struct
    external init     : ctx -> unit
                      = "caml_digestif_rmd160_ba_init"
                      [@@noalloc]
    external update   : ctx -> ba -> off -> size -> unit
                      = "caml_digestif_rmd160_ba_update"
                      [@@noalloc]
    external finalize : ctx -> ba -> off -> unit
                      = "caml_digestif_rmd160_ba_finalize"
                      [@@noalloc]
  end

  module Bytes =
  struct
    external init     : ctx -> unit
                      = "caml_digestif_rmd160_st_init"
                      [@@noalloc]
    external update   : ctx -> st -> off -> size -> unit
                      = "caml_digestif_rmd160_st_update"
                      [@@noalloc]
    external finalize : ctx -> st -> off -> unit
                      = "caml_digestif_rmd160_st_finalize"
                      [@@noalloc]
  end

  external ctx_size   : unit -> int
                      = "caml_digestif_rmd160_ctx_size"
                      [@@noalloc]
end

let imin (a : int) (b : int) = if a < b then a else b

module XOR =
struct
  module Bigstring =
  struct
    external xor_into   : ba -> off -> ba -> off -> size -> unit
                        = "caml_digestif_ba_xor_into"
                        [@@noalloc]

    let xor_into a b n =
      if n > imin (Bigstring.length a) (Bigstring.length b)
      then raise (Invalid_argument "Native.Bigstring.xor_into: buffers to small")
      else xor_into a 0 b 0 n

    let xor a b =
      let l = imin (Bigstring.length a) (Bigstring.length b) in
      let r = Bigstring.copy (Bigstring.sub b 0 l) in
      ( xor_into a r l; r )
  end

  module Bytes =
  struct
    external xor_into   : st -> off -> st -> off -> size -> unit
                        = "caml_digestif_st_xor_into"
                        [@@noalloc]

    let xor_into a b n =
      if n > imin (Bytes.length a) (Bytes.length b)
      then raise (Invalid_argument "Native.Bigstring.xor_into: buffers to small")
      else xor_into a 0 b 0 n

    let xor a b =
      let l = imin (Bytes.length a) (Bytes.length b) in
      let r = Bytes.copy (Bytes.sub b 0 l) in
      ( xor_into a r l; r )

  end
end
