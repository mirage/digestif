/*  Copyright (c) 2015 Markku-Juhani O. Saarinen */

#ifndef CRYPTOHASH_SHA3_H
#define CRYPTOHASH_SHA3_H

#include <stdint.h>


typedef struct
{
  union {                                 // state:
    uint8_t  b[200];                      // 8-bit bytes
    uint64_t q[25];                       // 64-bit words
  } st;
  int pt, rsiz, mdlen;                    // these don't overflow
} sha3_ctx;

#define SHA3_CTX_SIZE		sizeof(sha3_ctx)

void digestif_sha3_init(sha3_ctx *ctx, int mdlen);
void digestif_sha3_update(sha3_ctx *ctx, uint8_t *data, uint32_t len);
void digestif_sha3_finalize(sha3_ctx *ctx, uint8_t *out, uint8_t padding);

#endif
