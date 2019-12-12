(* Copyright (c) 2014-2016 David Kaloper Meršinjak

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
   SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
   OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
   CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

open Bigarray_compat

type off = int
type size = int
type ba = (char, int8_unsigned_elt, c_layout) Array1.t
type st = Bytes.t
type ctx = Bytes.t

val dup : ctx -> ctx

module MD5 : sig
  type kind = [`MD5]

  module Bigstring : sig
    external init : ctx -> unit = "caml_digestif_md5_ba_init" [@@noalloc]

    external update :
      ctx -> ba -> off -> size -> unit
      = "caml_digestif_md5_ba_update"

    external finalize :
      ctx -> ba -> off -> unit
      = "caml_digestif_md5_ba_finalize"
      [@@noalloc]
  end

  module Bytes : sig
    external init : ctx -> unit = "caml_digestif_md5_st_init" [@@noalloc]

    external update :
      ctx -> st -> off -> size -> unit
      = "caml_digestif_md5_st_update"
      [@@noalloc]

    external finalize :
      ctx -> st -> off -> unit
      = "caml_digestif_md5_st_finalize"
      [@@noalloc]
  end

  external ctx_size : unit -> int = "caml_digestif_md5_ctx_size" [@@noalloc]
end

module SHA1 : sig
  type kind = [`SHA1]

  module Bigstring : sig
    external init : ctx -> unit = "caml_digestif_sha1_ba_init" [@@noalloc]

    external update :
      ctx -> ba -> off -> size -> unit
      = "caml_digestif_sha1_ba_update"

    external finalize :
      ctx -> ba -> off -> unit
      = "caml_digestif_sha1_ba_finalize"
      [@@noalloc]
  end

  module Bytes : sig
    external init : ctx -> unit = "caml_digestif_sha1_st_init" [@@noalloc]

    external update :
      ctx -> st -> off -> size -> unit
      = "caml_digestif_sha1_st_update"
      [@@noalloc]

    external finalize :
      ctx -> st -> off -> unit
      = "caml_digestif_sha1_st_finalize"
      [@@noalloc]
  end

  external ctx_size : unit -> int = "caml_digestif_sha1_ctx_size" [@@noalloc]
end

module SHA224 : sig
  type kind = [`SHA224]

  module Bigstring : sig
    external init : ctx -> unit = "caml_digestif_sha224_ba_init" [@@noalloc]

    external update :
      ctx -> ba -> off -> size -> unit
      = "caml_digestif_sha224_ba_update"

    external finalize :
      ctx -> ba -> off -> unit
      = "caml_digestif_sha224_ba_finalize"
      [@@noalloc]
  end

  module Bytes : sig
    external init : ctx -> unit = "caml_digestif_sha224_st_init" [@@noalloc]

    external update :
      ctx -> st -> off -> size -> unit
      = "caml_digestif_sha224_st_update"
      [@@noalloc]

    external finalize :
      ctx -> st -> off -> unit
      = "caml_digestif_sha224_st_finalize"
      [@@noalloc]
  end

  external ctx_size : unit -> int = "caml_digestif_sha224_ctx_size" [@@noalloc]
end

module SHA256 : sig
  type kind = [`SHA256]

  module Bigstring : sig
    external init : ctx -> unit = "caml_digestif_sha256_ba_init" [@@noalloc]

    external update :
      ctx -> ba -> off -> size -> unit
      = "caml_digestif_sha256_ba_update"

    external finalize :
      ctx -> ba -> off -> unit
      = "caml_digestif_sha256_ba_finalize"
      [@@noalloc]
  end

  module Bytes : sig
    external init : ctx -> unit = "caml_digestif_sha256_st_init" [@@noalloc]

    external update :
      ctx -> st -> off -> size -> unit
      = "caml_digestif_sha256_st_update"
      [@@noalloc]

    external finalize :
      ctx -> st -> off -> unit
      = "caml_digestif_sha256_st_finalize"
      [@@noalloc]
  end

  external ctx_size : unit -> int = "caml_digestif_sha256_ctx_size" [@@noalloc]
end

module SHA384 : sig
  type kind = [`SHA384]

  module Bigstring : sig
    external init : ctx -> unit = "caml_digestif_sha384_ba_init" [@@noalloc]

    external update :
      ctx -> ba -> off -> size -> unit
      = "caml_digestif_sha384_ba_update"

    external finalize :
      ctx -> ba -> off -> unit
      = "caml_digestif_sha384_ba_finalize"
      [@@noalloc]
  end

  module Bytes : sig
    external init : ctx -> unit = "caml_digestif_sha384_st_init" [@@noalloc]

    external update :
      ctx -> st -> off -> size -> unit
      = "caml_digestif_sha384_st_update"
      [@@noalloc]

    external finalize :
      ctx -> st -> off -> unit
      = "caml_digestif_sha384_st_finalize"
      [@@noalloc]
  end

  external ctx_size : unit -> int = "caml_digestif_sha384_ctx_size" [@@noalloc]
end

module SHA512 : sig
  type kind = [`SHA512]

  module Bigstring : sig
    external init : ctx -> unit = "caml_digestif_sha512_ba_init" [@@noalloc]

    external update :
      ctx -> ba -> off -> size -> unit
      = "caml_digestif_sha512_ba_update"

    external finalize :
      ctx -> ba -> off -> unit
      = "caml_digestif_sha512_ba_finalize"
      [@@noalloc]
  end

  module Bytes : sig
    external init : ctx -> unit = "caml_digestif_sha512_st_init" [@@noalloc]

    external update :
      ctx -> st -> off -> size -> unit
      = "caml_digestif_sha512_st_update"
      [@@noalloc]

    external finalize :
      ctx -> st -> off -> unit
      = "caml_digestif_sha512_st_finalize"
      [@@noalloc]
  end

  external ctx_size : unit -> int = "caml_digestif_sha512_ctx_size" [@@noalloc]
end

module WHIRLPOOL : sig
  type kind = [`WHIRLPOOL]

  module Bigstring : sig
    external init : ctx -> unit = "caml_digestif_whirlpool_ba_init" [@@noalloc]

    external update :
      ctx -> ba -> off -> size -> unit
      = "caml_digestif_whirlpool_ba_update"

    external finalize :
      ctx -> ba -> off -> unit
      = "caml_digestif_whirlpool_ba_finalize"
      [@@noalloc]
  end

  module Bytes : sig
    external init : ctx -> unit = "caml_digestif_whirlpool_st_init" [@@noalloc]

    external update :
      ctx -> st -> off -> size -> unit
      = "caml_digestif_whirlpool_st_update"
      [@@noalloc]

    external finalize :
      ctx -> st -> off -> unit
      = "caml_digestif_whirlpool_st_finalize"
      [@@noalloc]
  end

  external ctx_size : unit -> int = "caml_digestif_whirlpool_ctx_size" [@@noalloc]
end

module BLAKE2B : sig
  type kind = [`BLAKE2B]

  module Bigstring : sig
    external init : ctx -> unit = "caml_digestif_blake2b_ba_init" [@@noalloc]

    external update :
      ctx -> ba -> off -> size -> unit
      = "caml_digestif_blake2b_ba_update"

    external finalize :
      ctx -> ba -> off -> unit
      = "caml_digestif_blake2b_ba_finalize"
      [@@noalloc]

    external with_outlen_and_key :
      ctx -> size -> ba -> off -> size -> unit
      = "caml_digestif_blake2b_ba_init_with_outlen_and_key"
      [@@noalloc]
  end

  module Bytes : sig
    external init : ctx -> unit = "caml_digestif_blake2b_st_init" [@@noalloc]

    external update :
      ctx -> st -> off -> size -> unit
      = "caml_digestif_blake2b_st_update"
      [@@noalloc]

    external finalize :
      ctx -> st -> off -> unit
      = "caml_digestif_blake2b_st_finalize"
      [@@noalloc]

    external with_outlen_and_key :
      ctx -> size -> st -> off -> size -> unit
      = "caml_digestif_blake2b_st_init_with_outlen_and_key"
      [@@noalloc]
  end

  external ctx_size : unit -> int = "caml_digestif_blake2b_ctx_size"
    [@@noalloc]

  external key_size : unit -> int = "caml_digestif_blake2b_key_size"
    [@@noalloc]

  external max_outlen : unit -> int = "caml_digestif_blake2b_max_outlen"
    [@@noalloc]

  external digest_size : ctx -> int = "caml_digestif_blake2b_digest_size"
    [@@noalloc]
end

module BLAKE2S : sig
  type kind = [`BLAKE2S]

  module Bigstring : sig
    external init : ctx -> unit = "caml_digestif_blake2s_ba_init" [@@noalloc]

    external update :
      ctx -> ba -> off -> size -> unit
      = "caml_digestif_blake2s_ba_update"

    external finalize :
      ctx -> ba -> off -> unit
      = "caml_digestif_blake2s_ba_finalize"
      [@@noalloc]

    external with_outlen_and_key :
      ctx -> size -> ba -> off -> size -> unit
      = "caml_digestif_blake2s_ba_init_with_outlen_and_key"
      [@@noalloc]
  end

  module Bytes : sig
    external init : ctx -> unit = "caml_digestif_blake2s_st_init" [@@noalloc]

    external update :
      ctx -> st -> off -> size -> unit
      = "caml_digestif_blake2s_st_update"
      [@@noalloc]

    external finalize :
      ctx -> st -> off -> unit
      = "caml_digestif_blake2s_st_finalize"
      [@@noalloc]

    external with_outlen_and_key :
      ctx -> size -> st -> off -> size -> unit
      = "caml_digestif_blake2s_st_init_with_outlen_and_key"
      [@@noalloc]
  end

  external ctx_size : unit -> int = "caml_digestif_blake2s_ctx_size"
    [@@noalloc]

  external key_size : unit -> int = "caml_digestif_blake2s_key_size"
    [@@noalloc]

  external max_outlen : unit -> int = "caml_digestif_blake2s_max_outlen"
    [@@noalloc]

  external digest_size : ctx -> int = "caml_digestif_blake2s_digest_size"
    [@@noalloc]
end

module RMD160 : sig
  type kind = [`RMD160]

  module Bigstring : sig
    external init : ctx -> unit = "caml_digestif_rmd160_ba_init" [@@noalloc]

    external update :
      ctx -> ba -> off -> size -> unit
      = "caml_digestif_rmd160_ba_update"

    external finalize :
      ctx -> ba -> off -> unit
      = "caml_digestif_rmd160_ba_finalize"
      [@@noalloc]
  end

  module Bytes : sig
    external init : ctx -> unit = "caml_digestif_rmd160_st_init" [@@noalloc]

    external update :
      ctx -> st -> off -> size -> unit
      = "caml_digestif_rmd160_st_update"
      [@@noalloc]

    external finalize :
      ctx -> st -> off -> unit
      = "caml_digestif_rmd160_st_finalize"
      [@@noalloc]
  end

  external ctx_size : unit -> int = "caml_digestif_rmd160_ctx_size" [@@noalloc]
end

module XOR : sig
  module Bigstring : sig
    val xor_into : ba -> ba -> int -> unit
    val xor : ba -> ba -> ba
  end

  module Bytes : sig
    val xor_into : st -> st -> int -> unit
    val xor : st -> st -> st
  end
end
