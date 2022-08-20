#include <stdint.h>
#include <string.h>

#define CBC 0
#define CTR 0
#include "tiny-AES-c/aes.h"

void aes128EncryptScheduleImpl(const uint8_t *key, uint32_t *aesEncInitialKey,
uint32_t *aesEncRoundKeys, uint32_t *aesEncFinalKey) {
  struct AES_ctx ctx;
  AES_init_ctx(&ctx, key);
  for (unsigned i = 0; i < 4; ++i) {
    aesEncInitialKey[i] = ctx.RoundKey[i * 4    ] << 24
                        | ctx.RoundKey[i * 4 + 1] << 16
                        | ctx.RoundKey[i * 4 + 2] << 8
                        | ctx.RoundKey[i * 4 + 3];
  }
  for (unsigned i = 0; i < 36; ++i) {
    aesEncRoundKeys[i] = ctx.RoundKey[(i + 4) * 4    ] << 24
                       | ctx.RoundKey[(i + 4) * 4 + 1] << 16
                       | ctx.RoundKey[(i + 4) * 4 + 2] << 8
                       | ctx.RoundKey[(i + 4) * 4 + 3];
  }
  for (unsigned i = 0; i < 4; ++i) {
    aesEncFinalKey[i] = ctx.RoundKey[(i + 40) * 4    ] << 24
                      | ctx.RoundKey[(i + 40) * 4 + 1] << 16
                      | ctx.RoundKey[(i + 40) * 4 + 2] << 8
                      | ctx.RoundKey[(i + 40) * 4 + 3];
  }
}

void aesEncryptBlockImpl(const uint32_t *aesEncInitialKey,
const uint32_t *aesEncRoundKeys, const uint32_t *aesEncFinalKey,
const uint8_t *plaintext, uint8_t *ciphertext) {
  struct AES_ctx ctx;
  for (unsigned i = 0; i < 4; ++i) {
    ctx.RoundKey[i * 4    ] = aesEncInitialKey[i] >> 24;
    ctx.RoundKey[i * 4 + 1] = aesEncInitialKey[i] >> 16;
    ctx.RoundKey[i * 4 + 2] = aesEncInitialKey[i] >> 8;
    ctx.RoundKey[i * 4 + 3] = aesEncInitialKey[i];
  }
  for (unsigned i = 0; i < 36; ++i) {
    ctx.RoundKey[(i + 4) * 4    ] = aesEncRoundKeys[i] >> 24;
    ctx.RoundKey[(i + 4) * 4 + 1] = aesEncRoundKeys[i] >> 16;
    ctx.RoundKey[(i + 4) * 4 + 2] = aesEncRoundKeys[i] >> 8;
    ctx.RoundKey[(i + 4) * 4 + 3] = aesEncRoundKeys[i];
  }
  for (unsigned i = 0; i < 4; ++i) {
    ctx.RoundKey[(i + 40) * 4    ] = aesEncFinalKey[i] >> 24;
    ctx.RoundKey[(i + 40) * 4 + 1] = aesEncFinalKey[i] >> 16;
    ctx.RoundKey[(i + 40) * 4 + 2] = aesEncFinalKey[i] >> 8;
    ctx.RoundKey[(i + 40) * 4 + 3] = aesEncFinalKey[i];
  }
  memcpy(ciphertext, plaintext, AES_BLOCKLEN);
  AES_ECB_encrypt(&ctx, ciphertext);
}
