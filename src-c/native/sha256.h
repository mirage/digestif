/*
 * Copyright (C) 2006-2009 Vincent Hanquez <vincent@snarc.org>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef CRYPTOHASH_SHA256_H
#define CRYPTOHASH_SHA256_H

#include <stdint.h>

typedef struct
{
	uint64_t sz;
	uint8_t  buf[128];
	uint32_t h[8];
} sha256_ctx;

#define sha224_ctx 		sha256_ctx

#define SHA224_DIGEST_SIZE	28
#define SHA224_CTX_SIZE		sizeof(sha224_ctx)

#define SHA256_DIGEST_SIZE	32
#define SHA256_CTX_SIZE		sizeof(sha256_ctx)

void digestif_sha224_init(sha224_ctx *ctx);
void digestif_sha224_update(sha224_ctx *ctx, uint8_t *data, uint32_t len);
void digestif_sha224_finalize(sha224_ctx *ctx, uint8_t *out);

void digestif_sha256_init(sha256_ctx *ctx);
void digestif_sha256_update(sha256_ctx *ctx, uint8_t *data, uint32_t len);
void digestif_sha256_finalize(sha256_ctx *ctx, uint8_t *out);

#endif
