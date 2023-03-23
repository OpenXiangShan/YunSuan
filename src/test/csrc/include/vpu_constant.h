#ifndef __VPU_CONSTANT_H
#define __VPU_CONSTANT_H
#include <string>
#include <bitset>

#ifdef __cplusplus

extern "C"{

// NOTE: should keep same with chisel.
// TODO: may dynamic generate this file when compiling chisel

#define VLEN 128
#define XLEN 64

#define FU_NUM 5 // for random
#define VFloatAdder (0)
#define VFloatFMA   (1)
#define VFloatDivider (2)
#define VIntegerALU (3)
#define VPermutation (4)
#define VIntegerALUV2 (5)
#define ALL_FUTYPES {VFloatAdder,VFloatFMA,VFloatDivider,VIntegerALU,VPermutation,VIntegerALUV2}

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

// vialuF funcop type
#define VIAF_NUM 65 
#define VADD_VV      (binstoi("00000000"))
#define VSUB_VV      (binstoi("00000001"))
#define VRSUB_VV     (binstoi("11000001"))
#define VWADDU_VV    (binstoi("01000000"))
#define VWSUBU_VV    (binstoi("01000001"))
#define VWADD_VV     (binstoi("01100000"))
#define VWSUB_VV     (binstoi("01100001"))
#define VWADDU_WV    (binstoi("10000000"))
#define VWSUBU_WV    (binstoi("10000001"))
#define VWADD_WV     (binstoi("10100000"))
#define VWSUB_WV     (binstoi("10100001"))
#define VZEXT_VF2    (binstoi("00000010"))
#define VSEXT_VF2    (binstoi("00100010"))
#define VZEXT_VF4    (binstoi("01000010"))
#define VSEXT_VF4    (binstoi("01100010"))
#define VZEXT_VF8    (binstoi("10000010"))
#define VSEXT_VF8    (binstoi("10100010"))
#define VADC_VVM     (binstoi("00000011"))
#define VMADC_VVM    (binstoi("01100100"))
#define VMADC_VV     (binstoi("01000100"))
#define VSBC_VVM     (binstoi("00000101"))
#define VMSBC_VVM    (binstoi("01100110"))
#define VMSBC_VV     (binstoi("01000110"))
#define VAND_VV      (binstoi("01000111"))
#define VOR_VV       (binstoi("00001011"))
#define VXOR_VV      (binstoi("00001010"))
#define VSLL_VV      (binstoi("00001111"))
#define VSRL_VV      (binstoi("00010000"))
#define VSRA_VV      (binstoi("00010001"))
#define VNSRL_WV     (binstoi("11010000"))
#define VNSRA_WV     (binstoi("11010001"))
#define VMSEQ_VV     (binstoi("01010010"))
#define VMSNE_VV     (binstoi("01010011"))
#define VMSLTU_VV    (binstoi("01010100"))
#define VMSLT_VV     (binstoi("01110100"))
#define VMSLEU_VV    (binstoi("01010101"))
#define VMSLE_VV     (binstoi("01110101"))
#define VMSGTU_VV    (binstoi("01010110"))
#define VMSGT_VV     (binstoi("01110110"))
#define VMINU_VV     (binstoi("00010111"))
#define VMIN_VV      (binstoi("00110111"))
#define VMAXU_VV     (binstoi("00011000"))
#define VMAX_VV      (binstoi("00111000"))
#define VMERGE_VVM   (binstoi("00011001"))
#define VMV_V_V      (binstoi("00011010"))
#define VSADDU_VV    (binstoi("00011011"))
#define VSADD_VV     (binstoi("00111011"))
#define VSSUBU_VV    (binstoi("00011100"))
#define VSSUB_VV     (binstoi("00111100"))
#define VAADDU_VV    (binstoi("00011101"))
#define VAADD_VV     (binstoi("00111101"))
#define VASUBU_VV    (binstoi("00011110"))
#define VASUB_VV     (binstoi("00111110"))
#define VSSRL_VV     (binstoi("00011111"))
#define VSSRA_VV     (binstoi("00111111"))
#define VNCLIPU_WV   (binstoi("11011111"))
#define VNCLIP_WV    (binstoi("11111111"))
#define VMAND_MM     (binstoi("10000111"))
#define VMNAND_MM    (binstoi("10001000"))
#define VMANDN_MM    (binstoi("10001001"))
#define VMXOR_MM     (binstoi("10001010"))
#define VMOR_MM      (binstoi("10001011"))
#define VMNOR_MM     (binstoi("10001100"))
#define VMORN_MM     (binstoi("10001101"))
#define VMXNOR_MM    (binstoi("10001110"))

#define VIAF_ALL_OPTYPES { \
  VADD_VV   ,VSUB_VV   ,VRSUB_VV  ,VWADDU_VV ,VWSUBU_VV ,VWADD_VV  ,VWSUB_VV  ,VWADDU_WV ,VWSUBU_WV ,VWADD_WV  ,VWSUB_WV  ,VZEXT_VF2 , \
  VSEXT_VF2 ,VZEXT_VF4 ,VSEXT_VF4 ,VZEXT_VF8 ,VSEXT_VF8 ,VADC_VVM  ,VMADC_VVM ,VMADC_VV  ,VSBC_VVM  ,VMSBC_VVM ,VMSBC_VV  ,VAND_VV   , \
  VOR_VV    ,VXOR_VV   ,VSLL_VV   ,VSRL_VV   ,VSRA_VV   ,VNSRL_WV  ,VNSRA_WV  ,VMSEQ_VV  ,VMSNE_VV  ,VMSLTU_VV ,VMSLT_VV  ,VMSLEU_VV , \
  VMSLE_VV  ,VMSGTU_VV ,VMSGT_VV  ,VMINU_VV  ,VMIN_VV   ,VMAXU_VV  ,VMAX_VV   ,VMERGE_VVM,VMV_V_V   ,VSADDU_VV ,VSADD_VV  ,VSSUBU_VV , \
  VSSUB_VV  ,VAADDU_VV ,VAADD_VV  ,VASUBU_VV ,VASUB_VV  ,VSSRL_VV  ,VSSRA_VV  ,VNCLIPU_WV,VNCLIP_WV ,VMAND_MM  ,VMNAND_MM ,VMANDN_MM , \
  VMXOR_MM  ,VMOR_MM   ,VMNOR_MM  ,VMORN_MM  ,VMXNOR_MM } \

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

// vperm funcop type
#define VPERM_NUM 8
#define VSLIDEUP    (binstoi("0000"))
#define VSLIDEDOWN  (binstoi("0001"))
#define VSLIDE1UP   (binstoi("0010"))
#define VSLIDE1DOWN (binstoi("0011"))
#define VRGATHER    (binstoi("0100"))
#define VRGATHERRS1 (binstoi("0101"))
#define VCOMPRESS   (binstoi("0110"))
#define VWREGMOV    (binstoi("0111"))

// rounding mode for fix point
#define RM_S_RNU (0)
#define RM_S_RNE (1)
#define RM_S_RDN (2)
#define RM_S_ROD (3)

#define VFA_NUM 16 // for random

#define VFADD   (binstoi("00000"))
#define VFSUB   (binstoi("00001"))
#define VFMIN   (binstoi("00010"))
#define VFMAX   (binstoi("00011"))
#define VFMERGE (binstoi("00100"))
#define VFMOVE  (binstoi("00101"))
#define VFSGNJ  (binstoi("00110"))
#define VFSGNJN (binstoi("00111"))
#define VFSGNJX (binstoi("01000"))
#define VFEQ    (binstoi("01001"))
#define VFNE    (binstoi("01010"))
#define VFLT    (binstoi("01011"))
#define VFLE    (binstoi("01100"))
#define VFGT    (binstoi("01101"))
#define VFGE    (binstoi("01110"))
#define VFGE    (binstoi("01110"))
#define VFCLASS (binstoi("01111"))

#define VFA_ALL_OPTYPES {VFADD,VFSUB,VFMIN,VFMAX,VFMERGE,VFMOVE,VFSGNJ,VFSGNJN,VFSGNJX,VFEQ,VFNE,VFLT,VFLE,VFGT,VFGE,VFCLASS}

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