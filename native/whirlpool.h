/* whirlpool.c - an implementation of the Whirlpool Hash Function.
 *
 * Copyright: 2009-2012 Aleksey Kravchenko <rhash.admin@gmail.com>
 *
 * Permission is hereby granted,  free of charge,  to any person  obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction,  including without limitation
 * the rights to  use, copy, modify,  merge, publish, distribute, sublicense,
 * and/or sell copies  of  the Software,  and to permit  persons  to whom the
 * Software is furnished to do so.
 *
 * This program  is  distributed  in  the  hope  that it will be useful,  but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  Use this program  at  your own risk!
 *
 * Documentation:
 * P. S. L. M. Barreto, V. Rijmen, ``The Whirlpool hashing function,''
 * NESSIE submission, 2000 (tweaked version, 2001)
 *
 * The algorithm is named after the Whirlpool Galaxy in Canes Venatici.
 */
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
