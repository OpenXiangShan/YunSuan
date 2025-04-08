#ifndef __REG_H__
#define __REG_H__
#include <stdint.h>

#define VLEN 2048
#define TA 1
#define MA 1
#define TU 0
#define MU 0
typedef struct {
    uint64_t gpr[32];
    float fpr[32];
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

typedef union {
    float as_float;
}FloatUintUnion;

extern const char *regs[];
extern CPU_STATE cpu;
extern VCSR vcsr;
#endif