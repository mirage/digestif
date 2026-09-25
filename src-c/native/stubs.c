#include "digestif.h"

#include "md5.h"
#include "sha1.h"
#include "sha256.h"
#include "sha512.h"
#include "sha3.h"
#include "whirlpool.h"
#include "blake2b.h"
#include "blake2s.h"
#include "blake3.h"
#include "ripemd160.h"
#include <caml/memory.h>
#include <string.h>

#ifndef Bytes_val
#define Bytes_val(x) String_val(x)
#endif

#if defined(__ocaml_freestanding__) || defined(__ocaml_solo5__)
#define __define_ba_update(name)                                             \
  CAMLprim value                                                             \
  caml_digestif_ ## name ## _ba_update                                       \
      (value ctx, value src, value off, value len) {                         \
    digestif_ ## name ## _update (                                           \
      (name ## _ctx *) String_val (ctx),                                     \
      _ba_uint8_off (src, off), Int_val (len));                              \
    return Val_unit;                                                         \
  }

#define __define_sha3_ba_update(mdlen)                                       \
  CAMLprim value                                                             \
  caml_digestif_sha3_ ## mdlen ## _ba_update                                 \
      (value ctx, value src, value off, value len) {                         \
    digestif_sha3_update (                                                   \
      (sha3_ctx *) String_val (ctx),                                         \
      _ba_uint8_off (src, off), Int_val (len));                              \
    return Val_unit;                                                         \
  }
#else
/* XXX(dinosaure): even if they are not defined (only defined by
 * [caml/threads.h]), they exists without [threads.cmxa]. For compatibility
 * reason, we keep a protection when we compile for Solo5/ocaml-freestanding
 * but for the rest, these functions should be available in any cases.
 *
 * In some cases (Solo5 or Esperanto), [caml/threads.h] is not available but
 * these functions still exist!
 */

CAMLextern void caml_enter_blocking_section (void);
CAMLextern void caml_leave_blocking_section (void);

#define __define_ba_update(name)                                             \
  CAMLprim value                                                             \
  caml_digestif_ ## name ## _ba_update                                       \
      (value ctx, value src, value off, value len) {                         \
    CAMLparam4 (ctx, src, off, len);                                         \
    uint8_t *off_ = ((uint8_t*) Caml_ba_data_val(src)) + Long_val (off);     \
    uint32_t len_ = Long_val (len);                                          \
    name ## _ctx ctx_;                                                       \
    memcpy(&ctx_, Bytes_val(ctx), sizeof(name ## _ctx));                     \
    caml_enter_blocking_section();                                           \
    digestif_ ## name ## _update (&ctx_, off_, len_);                        \
    caml_leave_blocking_section();                                           \
    memcpy(Bytes_val(ctx), &ctx_, sizeof(name ## _ctx));                     \
    CAMLreturn (Val_unit);                                                   \
  }

#define __define_sha3_ba_update(mdlen)                                       \
  CAMLprim value                                                             \
  caml_digestif_sha3_ ## mdlen ## _ba_update                                 \
      (value ctx, value src, value off, value len) {                         \
    CAMLparam4 (ctx, src, off, len);                                         \
    uint8_t *off_ = ((uint8_t*) Caml_ba_data_val(src)) + Long_val (off);     \
    uint32_t len_ = Long_val (len);                                          \
    sha3_ctx ctx_;                                                           \
    memcpy(&ctx_, Bytes_val(ctx), sizeof(sha3_ctx));                         \
    caml_enter_blocking_section();                                           \
    digestif_sha3_update (&ctx_, off_, len_);                                \
    caml_leave_blocking_section();                                           \
    memcpy(Bytes_val(ctx), &ctx_, sizeof(sha3_ctx));                         \
    CAMLreturn (Val_unit);                                                   \
  }
#endif

#define __define_hash(name, upper)                                           \
                                                                             \
  CAMLprim value                                                             \
  caml_digestif_ ## name ## _ba_init (value ctx) {                           \
    digestif_ ## name ## _init ((name ## _ctx *) String_val (ctx));          \
    return Val_unit;                                                         \
  }                                                                          \
                                                                             \
  CAMLprim value                                                             \
  caml_digestif_ ## name ## _st_init (value ctx) {                           \
    digestif_ ## name ## _init ((name ## _ctx *) String_val (ctx));          \
    return Val_unit;                                                         \
  }                                                                          \
                                                                             \
  __define_ba_update(name)                                                   \
                                                                             \
  CAMLprim value                                                             \
  caml_digestif_ ## name ## _st_update                                       \
      (value ctx, value src, value off, value len) {                         \
    digestif_ ## name ## _update (                                           \
      (name ## _ctx *) String_val (ctx),                                     \
      _st_uint8_off (src, off), Int_val (len));                              \
    return Val_unit;                                                         \
  }                                                                          \
                                                                             \
  CAMLprim value                                                             \
  caml_digestif_ ## name ## _ba_finalize (value ctx, value dst, value off) { \
    digestif_ ## name ## _finalize (                                         \
      (name ## _ctx *) String_val (ctx),                                     \
      _ba_uint8_off (dst, off));                                             \
    return Val_unit;                                                         \
  }                                                                          \
                                                                             \
  CAMLprim value                                                             \
  caml_digestif_ ## name ## _st_finalize (value ctx, value dst, value off) { \
    digestif_ ## name ## _finalize(                                          \
      (name ## _ctx *) String_val (ctx),                                     \
      _st_uint8_off (dst, off));                                             \
    return Val_unit;                                                         \
  }                                                                          \
                                                                             \
  CAMLprim value                                                             \
  caml_digestif_ ## name ## _ctx_size (__unit ()) {                          \
    return Val_int (upper ## _CTX_SIZE);                                     \
  }

/* BLAKE3 */

#define BLAKE3_CTX_SIZE (sizeof(blake3_ctx))
typedef blake3_hasher blake3_ctx;

void
digestif_blake3_finalize(const blake3_ctx *ctx, uint8_t *out)
{
  digestif_blake3_finalize_with_out_len(ctx, out, BLAKE3_OUT_LEN);
}

__define_hash (md5, MD5)
__define_hash (sha1, SHA1)
__define_hash (sha224, SHA224)
__define_hash (sha256, SHA256)
__define_hash (sha384, SHA384)
__define_hash (sha512, SHA512)
__define_hash (whirlpool, WHIRLPOOL)
__define_hash (blake2b, BLAKE2B)
__define_hash (blake2s, BLAKE2S)
__define_hash (blake3, BLAKE3)
__define_hash (rmd160, RMD160)

/* BLAKE3 */

CAMLprim value
caml_digestif_blake3_st_init_keyed(value ctx, value key, value off)
{
  digestif_blake3_init_keyed(
    (blake3_ctx *) String_val(ctx), _st_uint8_off(key, off));
  return Val_unit;
}

CAMLprim value
caml_digestif_blake3_st_init_derive_key(value ctx, value context,
                                        value off, value len)
{
  digestif_blake3_init_derive_key_raw(
    (blake3_ctx *) String_val(ctx), _st_uint8_off(context, off), Int_val(len));
  return Val_unit;
}

CAMLprim value
caml_digestif_blake3_st_finalize_seek(value ctx, value seek, value dst,
                                      value off, value len)
{
  digestif_blake3_finalize_seek(
    (blake3_ctx *) String_val(ctx), Int64_val(seek),
    _st_uint8_off(dst, off), Int_val(len));
  return Val_unit;
}

/* BLAKE2 */

CAMLprim value
caml_digestif_blake2b_ba_init_with_outlen_and_key(value ctx, value outlen, value key, value off, value len)
{
  digestif_blake2b_init_with_outlen_and_key(
    (blake2b_ctx *) String_val (ctx), Int_val (outlen),
    _ba_uint8_off(key, off), Int_val (len));

  return Val_unit;
}

CAMLprim value
caml_digestif_blake2b_st_init_with_outlen_and_key(value ctx, value outlen, value key, value off, value len)
{
  digestif_blake2b_init_with_outlen_and_key(
    (blake2b_ctx *) String_val (ctx), Int_val (outlen),
    _st_uint8_off(key, off), Int_val (len));

  return Val_unit;
}

CAMLprim value
caml_digestif_blake2b_key_size(__unit ()) {
  return Val_int (BLAKE2B_KEYBYTES);
}

CAMLprim value
caml_digestif_blake2b_max_outlen(__unit ()) {
  return Val_int (BLAKE2B_OUTBYTES);
}

CAMLprim value
caml_digestif_blake2b_digest_size(value ctx) {
  return Val_int(((blake2b_ctx *) String_val (ctx))->outlen);
}

CAMLprim value
caml_digestif_blake2s_ba_init_with_outlen_and_key(value ctx, value outlen, value key, value off, value len)
{
  digestif_blake2s_init_with_outlen_and_key(
    (blake2s_ctx *) String_val (ctx), Int_val (outlen),
    _ba_uint8_off(key, off), Int_val (len));

  return Val_unit;
}

CAMLprim value
caml_digestif_blake2s_st_init_with_outlen_and_key(value ctx, value outlen, value key, value off, value len)
{
  digestif_blake2s_init_with_outlen_and_key(
    (blake2s_ctx *) String_val (ctx), Int_val (outlen),
    _st_uint8_off(key, off), Int_val (len));

  return Val_unit;
}

CAMLprim value
caml_digestif_blake2s_key_size(__unit ()) {
  return Val_int (BLAKE2S_KEYBYTES);
}

CAMLprim value
caml_digestif_blake2s_max_outlen(__unit ()) {
  return Val_int (BLAKE2S_OUTBYTES);
}

CAMLprim value
caml_digestif_blake2s_digest_size(value ctx) {
  return Val_int(((blake2s_ctx *) String_val (ctx))->outlen);
}

/* SHA3 */

CAMLprim value
caml_digestif_keccak_256_ba_finalize
(value ctx, value dst, value off) {
  digestif_sha3_finalize (
    (sha3_ctx *) String_val (ctx),
    _ba_uint8_off (dst, off), 0x01);
  return Val_unit;
}

CAMLprim value
caml_digestif_keccak_256_st_finalize
(value ctx, value dst, value off) {
  digestif_sha3_finalize(
    (sha3_ctx *) String_val (ctx),
    _st_uint8_off (dst, off), 0x01);
  return Val_unit;
}

#define __define_hash_sha3(mdlen)                                            \
                                                                             \
  CAMLprim value                                                             \
  caml_digestif_sha3_ ## mdlen ## _ba_init (value ctx) {                     \
    digestif_sha3_init ((sha3_ctx *) String_val (ctx), mdlen);               \
    return Val_unit;                                                         \
  }                                                                          \
                                                                             \
  CAMLprim value                                                             \
  caml_digestif_sha3_ ## mdlen ## _st_init (value ctx) {                     \
    digestif_sha3_init ((sha3_ctx *) String_val (ctx), mdlen);               \
    return Val_unit;                                                         \
  }                                                                          \
                                                                             \
  __define_sha3_ba_update(mdlen)                                             \
                                                                             \
  CAMLprim value                                                             \
  caml_digestif_sha3_ ## mdlen ## _st_update                                 \
      (value ctx, value src, value off, value len) {                         \
    digestif_sha3_update (                                                   \
      (sha3_ctx *) String_val (ctx),                                         \
      _st_uint8_off (src, off), Int_val (len));                              \
    return Val_unit;                                                         \
  }                                                                          \
                                                                             \
  CAMLprim value                                                             \
  caml_digestif_sha3_ ## mdlen ## _ba_finalize                               \
  (value ctx, value dst, value off) {                                        \
    digestif_sha3_finalize (                                                 \
      (sha3_ctx *) String_val (ctx),                                         \
      _ba_uint8_off (dst, off), 0x06);                                       \
    return Val_unit;                                                         \
  }                                                                          \
                                                                             \
  CAMLprim value                                                             \
  caml_digestif_sha3_ ## mdlen ## _st_finalize                               \
  (value ctx, value dst, value off) {                                        \
    digestif_sha3_finalize(                                                  \
      (sha3_ctx *) String_val (ctx),                                         \
      _st_uint8_off (dst, off), 0x06);                                       \
    return Val_unit;                                                         \
  }                                                                          \
                                                                             \
  CAMLprim value                                                             \
  caml_digestif_sha3_ ## mdlen ## _ctx_size (__unit ()) {                    \
    return Val_int (SHA3_CTX_SIZE);                                          \
  }

__define_hash_sha3 (224)
__define_hash_sha3 (256)
__define_hash_sha3 (384)
__define_hash_sha3 (512)
