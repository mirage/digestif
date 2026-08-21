/*  Copyright (c) 2015 Markku-Juhani O. Saarinen */

#ifndef CRYPTOHASH_SHA3_H
#define CRYPTOHASH_SHA3_H

#include <stddef.h>
#include <stdint.h>


struct sha3_ctx
{
  union {                                 // state:
    uint8_t  b[200];                      // 8-bit bytes
    uint64_t q[25];                       // 64-bit words
  } st;
  int pt, rsiz, mdlen;                    // these don't overflow
};

#define SHA3_CTX_SIZE		sizeof(struct sha3_ctx)

void digestif_sha3_init(struct sha3_ctx *ctx, int mdlen);
void digestif_sha3_update(struct sha3_ctx *ctx, uint8_t *data, uint32_t len);
void digestif_sha3_finalize(struct sha3_ctx *ctx, uint8_t *out, uint8_t padding);

/* Extendable-output (SHAKE) mode.  [digestif_sha3_xof] closes the absorbing
   phase; [digestif_sha3_out] then squeezes an arbitrary number of bytes and
   may be called repeatedly to continue the same output stream. */
void digestif_sha3_xof(struct sha3_ctx *ctx, uint8_t padding);
void digestif_sha3_out(struct sha3_ctx *ctx, uint8_t *out, size_t len);

#endif
