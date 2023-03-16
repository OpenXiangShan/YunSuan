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
#define ALL_FUTYPES {VFloatAdder,VFloatFMA,VFloatDivider,VIntegerALU}

#define INT_ROUNDING(result, xrm, gb) \
  do { \
    const uint64_t lsb = 1UL << (gb); \
    const uint64_t lsb_half = lsb >> 1; \
    switch (xrm) { \
      case RM_S_RNU: \
        result += lsb_half; \
        break; \
      case RM_S_RNE: \
        if ((result & lsb_half) && ((result & (lsb_half - 1)) || (result & lsb))) \
          result += lsb; \
        break; \
      case RM_S_RDN: \
        break; \
      case RM_S_ROD: \
        if (result & (lsb - 1)) \
          result |= lsb; \
    } \
  } while (0)

// NOTE: The string should has the length of 8!

// TODO: add other type
#define VIALU_NUM 42 // todo
#define VADD    (binstoi("00000000"))
#define VADC    (binstoi("00000001"))
#define VSUB    (binstoi("00000010"))
#define VSBC    (binstoi("00000011"))
#define VWADDU  (binstoi("00000100"))
#define VWSUBU  (binstoi("00000101"))
#define VWADD   (binstoi("00000110"))
#define VWSUB   (binstoi("00000111"))
#define VWWADDU (binstoi("00001000"))
#define VWWSUBU (binstoi("00001001"))
#define VWWADD  (binstoi("00001010"))
#define VWWSUB  (binstoi("00001011"))
#define VMAXU   (binstoi("00001100"))
#define VMINU   (binstoi("00001101"))
#define VMAX    (binstoi("00001110"))
#define VMIN    (binstoi("00001111"))
#define VMSEQ   (binstoi("00010000"))
#define VMSNE   (binstoi("00010001"))
#define VMSLTU  (binstoi("00010010"))
#define VMSLT   (binstoi("00010011"))
#define VMSLEU  (binstoi("00010100"))
#define VMSLE   (binstoi("00010101"))
#define VMSGTU  (binstoi("00010110"))
#define VMSGT   (binstoi("00010111"))
#define VMSGEU  (binstoi("00011000"))
#define VMSGE   (binstoi("00011001"))
#define VAND    (binstoi("00011010"))
#define VNAND   (binstoi("00011011"))
#define VANDN   (binstoi("00011100"))
#define VOR     (binstoi("00011101"))
#define VNOR    (binstoi("00011110"))
#define VORN    (binstoi("00011111"))
#define VXOR    (binstoi("00100000"))
#define VXNOR   (binstoi("00100001"))
#define VSLL    (binstoi("00100010"))
#define VSRL    (binstoi("00100011"))
#define VSRA    (binstoi("00100100"))
#define VSSRL   (binstoi("00100101"))
#define VSSRA   (binstoi("00100110"))
#define VRSUB   (binstoi("00100111"))
#define VMADC   (binstoi("00101000"))
#define VMADC0  (binstoi("00101001"))

// rounding mode for fix point
#define RM_S_RNU (0)
#define RM_S_RNE (1)
#define RM_S_RDN (2)
#define RM_S_ROD (3)

#define VFA_NUM 10 // for random
#define VFADD (binstoi("00000"))
#define VFSUB (binstoi("00001"))
#define VFMIN (binstoi("00010"))
#define VFMAX (binstoi("00011"))

#define VFEQ  (binstoi("01001"))
#define VFNE  (binstoi("01010"))
#define VFLT  (binstoi("01011"))
#define VFLE  (binstoi("01100"))
#define VFGT  (binstoi("01101"))
#define VFGE  (binstoi("01110"))

#define VFA_ALL_OPTYPES {VFADD,VFSUB,VFMIN,VFMAX,VFEQ,VFNE,VFLT,VFLE,VFGT,VFGE}


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