
#ifndef __DIFFTEST_COMMON_H__
#define __DIFFTEST_COMMON_H__
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>


#define RESET_VECTOR 0x80000000
#define concat_temp(x, y) x ## y
#define concat(x, y) concat_temp(x, y)


#define BITMASK(bits) ((1ull << (bits)) - 1)
#define BITS(x, hi, lo) (((x) >> (lo)) & BITMASK((hi) - (lo) + 1)) // similar to x[hi:lo] in verilog
#define SEXT(x, len) ({ struct { int64_t n : len; } __x = { .n = x }; (uint64_t)__x.n; })

#endif