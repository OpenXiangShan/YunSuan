#ifndef __REG_H__
#define __REG_H__
#include <stdint.h>

#define VLEN 1024


#define LMUL 1


#define TA 1
#define MA 1
#define TU 0
#define MU 0
typedef union {
    float as_float;
    uint32_t as_uint32;
    int32_t as_int32;
}FloatUintUnion;

typedef struct {
    uint32_t low;   // 低 32 位（fp32 存储在这里）
    uint32_t high;   // 高 32 位（fp32 模式下需置 1）
} Uint32x2;       // 以两个 uint32_t 访问

typedef union {
    Uint32x2 as_uint32x2;
    uint64_t as_uint64;  // 以 uint64_t 访问（fp64 模式）
    double   as_fp64;    // 以 double 访问（fp64 模式）
    float    as_fp32;    // 以 float 访问（fp32 模式）
} FloatReg;

typedef struct {
    uint64_t gpr[32];
    FloatReg fpr[32];
    union {
        uint32_t u;
        float f;
        int32_t i;
    } vreg[32][VLEN/32];
}CPU_STATE;

typedef struct {
    int lmul;
    int sew;
    int vlenb;
    int vta;
    int vma;
    int vstart;
    int vl;
}VCSR;



extern const char *regs[];
extern CPU_STATE cpu;
extern VCSR vcsr;
#endif