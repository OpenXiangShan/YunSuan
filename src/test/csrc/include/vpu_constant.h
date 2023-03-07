#ifndef __VPU_CONSTANT_H
#define __VPU_CONSTANT_H
#include <string>
#include <bitset>

#ifdef __cplusplus

extern "C"{

// NOTE: should keep same with chisel.
// TODO: may dynamic generate this file when compiling chisel

#define FU_NUM 4 // for random
#define VFloatAdder (0)
#define VFloatFMA   (1)
#define VFloatDivider (2)
#define VIntegerALU (3)

// NOTE: The string should has the length of 8!

// TODO: add other type
#define VIALU_NUM 10 // todo
#define VADD (binstoi("00000000"))

#define VFA_NUM 10 // for random
#define VFADD (binstoi("10000000"))

// rounding mode
#define RM_RNE (0)
#define RM_RTZ (1)
#define RM_RDN (2)
#define RM_RUP (3)
#define RM_RMM (4)

// float-flags
#define FFLAGS_NV (0x10)
#define FFLAGS_DZ (0x08)
#define FFLAGS_OF (0x04)
#define FFLAGS_UF (0x02)
#define FFLAGS_NX (0x01)

// pre-compile stoi
constexpr uint8_t binstoi(const char str[]) {
  uint8_t num = 0;
  for (int i = 0; str[i] != '\0' && i < 8; i++) {
    if (str[i] != '1' || str[i] != '0') {
      // Error
    }
    num = (num << 1) + (str[i] - '0');
  }
  return num;
}

};
#endif

#endif