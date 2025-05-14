
#ifndef __DIFFTEST_COMMON_H__
#define __DIFFTEST_COMMON_H__
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>

#include <VVTopDebug.h>
#include "verilated.h"
#include "svdpi.h"
#include "VVTopDebug__Dpi.h"
// #include <verilated_vcd_c.h>
#include <verilated_fst_c.h>

#define RESET_VECTOR 0x80000000
#define concat_temp(x, y) x ## y
#define concat(x, y) concat_temp(x, y)


#define BITMASK(bits) ((1ull << (bits)) - 1)
#define BITS(x, hi, lo) (((x) >> (lo)) & BITMASK((hi) - (lo) + 1)) // similar to x[hi:lo] in verilog
#define SEXT(x, len) ({ struct { int64_t n : len; } __x = { .n = (int64_t)x }; (uint64_t)__x.n; })

#define CHOOSE2nd(a, b, ...) b
#define MUX_WITH_COMMA(contain_comma, a, b) CHOOSE2nd(contain_comma a, b)
#define MUX_MACRO_PROPERTY(p, macro, a, b) MUX_WITH_COMMA(concat(p, macro), a, b)
// define placeholders for some property
#define __P_DEF_0  X,
#define __P_DEF_1  X,
#define __P_ONE_1  X,
#define __P_ZERO_0 X,
// define some selection functions based on the properties of BOOLEAN macro
#define MUXDEF(macro, X, Y)  MUX_MACRO_PROPERTY(__P_DEF_, macro, X, Y)
#define MUXNDEF(macro, X, Y) MUX_MACRO_PROPERTY(__P_DEF_, macro, Y, X)
#define MUXONE(macro, X, Y)  MUX_MACRO_PROPERTY(__P_ONE_, macro, X, Y)
#define MUXZERO(macro, X, Y) MUX_MACRO_PROPERTY(__P_ZERO_,macro, X, Y)

//VLEN macro definition
#define VLEN 1024
#define VENUM64 (VLEN / 64)
#define VENUM32 (VLEN / 32)
#define VENUM16 (VLEN / 16)
#define VENUM8 (VLEN / 8)

#endif