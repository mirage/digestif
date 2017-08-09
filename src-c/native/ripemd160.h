/***********************************************************************/
/*                                                                     */
/*                      The Cryptokit library                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2005 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file LICENSE.        */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* RIPEMD160 hashing */

typedef unsigned int u32;

struct ripemd160_ctx {
  u32 state[5];
  u32 length[2];
  int numbytes;
  unsigned char buffer[64];
};

#define RIPEMD160_DIGEST_SIZE 20
#define RIPEMD160_CTX_SIZE (sizeof(struct ripemd160_ctx))

extern void digestif_ripemd160_init(struct ripemd160_ctx * ctx);
extern void digestif_ripemd160_update(struct ripemd160_ctx * ctx, 
                               unsigned char * data,
                               unsigned long len);
extern void digestif_ripemd160_finalize(struct ripemd160_ctx * ctx, 
                             unsigned char output[20]);
