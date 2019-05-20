#ifndef FIXED9_H
#define FIXED9_H

#include <stdint.h>
#include <stdio.h>

static const uint64_t FIXED9_E9 = 1000000000L;
const uint64_t FIXED9_SIGN_MASK64 = 0x1000000000000000;

uint64_t fixed9_fixed9Multiply(int64_t a, int64_t b) {
  __int128_t a1 = a;
  __int128_t c = a1 * b;
  __int128_t res = c / FIXED9_E9;
  uint64_t result = (uint64_t)res;
  return (int64_t)(((uint64_t)(res >> 64) & FIXED9_SIGN_MASK64) | result);
}

uint64_t fixed9_fixed9Divide(int64_t a, int64_t b) {
  __int128_t a1 = ((__int128_t)a) * FIXED9_E9;
  __int128_t res = a1 / b;
  uint64_t result = (uint64_t)res;
  return (int64_t)(((uint64_t)(res >> 64) & FIXED9_SIGN_MASK64) | result);
}

#endif // FIXED9_H
