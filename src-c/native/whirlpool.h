#ifndef CRYPTOHASH_WHIRLPOOL_H
#define CRYPTOHASH_WHIRLPOOL_H

#include <stdint.h>

struct whirlpool_ctx
{
	uint64_t sz;
	uint8_t buf[64];
	uint64_t h[8];
};

#define WHIRLPOOL_DIGEST_SIZE	64
#define WHIRLPOOL_CTX_SIZE		sizeof(struct whirlpool_ctx)

void digestif_whirlpool_init(struct whirlpool_ctx* ctx);
void digestif_whirlpool_update(struct whirlpool_ctx* ctx, uint8_t *data, uint32_t len);
void digestif_whirlpool_finalize(struct whirlpool_ctx* ctx, uint8_t *out);

#endif
