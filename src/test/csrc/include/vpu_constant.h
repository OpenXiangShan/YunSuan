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

// #define FU_NUM 8 // for random
#define VFloatAdder (0)
#define VFloatFMA   (1)
#define VFloatDivider (2)
#define VIntegerALU (3)
#define VPermutation (4)
#define VIntegerALUV2 (5)
#define VIntegerDivider (6)
#define VFloatCvt (7)
#define FloatCvtF2X (8) //f->i/ui/f
#define FloatCvtI2F (9) //i/ui->f
#define VIntegerMAC (10)
#define IntegerMul (11)
#define FloatCompare (12)
#define FloatALU (13)
#define FloatMul (14)
#define FloatFMA (15)
// #define ALL_FUTYPES {VFloatAdder,VFloatFMA,VFloatDivider,VIntegerALU,VPermutation,VIntegerALUV2,VIntegerDivider,VFloatCvt}

//will be delated
#define FU_NUM 15
#define ALL_FUTYPES {VFloatFMA,VFloatDivider,VIntegerALU,VPermutation,VIntegerALUV2,VIntegerDivider,VFloatCvt,FloatCvtF2X,FloatCvtI2F,VIntegerMAC,IntegerMul,FloatCompare,FloatALU,FloatMul,FloatFMA}

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
        break; \
      default: \
        break; \
    } \
  } while (0)

// NOTE: The string should has the length of 8!

// vialuF funcop type
#define VIAF_NUM 72
#define VADD_VV      (binstoi("001000000"))
#define VSUB_VV      (binstoi("001010000"))
// #define VRSUB_VV     // same as VSUB in fu
#define VWADDU_VV    (binstoi("010000000"))
#define VWSUBU_VV    (binstoi("010010000"))
#define VWADD_VV     (binstoi("011000000"))
#define VWSUB_VV     (binstoi("011010000"))
#define VWADDU_WV    (binstoi("100000000"))
#define VWSUBU_WV    (binstoi("100010000"))
#define VWADD_WV     (binstoi("101000000"))
#define VWSUB_WV     (binstoi("101010000"))
#define VZEXT_VF2    (binstoi("000111111"))
#define VSEXT_VF2    (binstoi("001111111"))
#define VZEXT_VF4    (binstoi("010111111"))
#define VSEXT_VF4    (binstoi("011111111"))
#define VZEXT_VF8    (binstoi("100111111"))
#define VSEXT_VF8    (binstoi("101111111"))
#define VADC_VVM     (binstoi("000000001"))
#define VMADC_VVM    (binstoi("100000100"))
#define VMADC_VV     (binstoi("010000100"))
#define VSBC_VVM     (binstoi("000010001"))
#define VMSBC_VVM    (binstoi("100010100"))
#define VMSBC_VV     (binstoi("010010100"))
#define VAND_VV      (binstoi("000100001"))
#define VOR_VV       (binstoi("000100101"))
#define VXOR_VV      (binstoi("000100100"))
#define VSLL_VV      (binstoi("000101000"))
#define VSRL_VV      (binstoi("000101010"))
#define VSRA_VV      (binstoi("001101100"))
#define VNSRL_WV     (binstoi("110101010"))
#define VNSRA_WV     (binstoi("111101100"))
#define VMSEQ_VV     (binstoi("010000101"))
#define VMSNE_VV     (binstoi("010000110"))
#define VMSLTU_VV    (binstoi("010010101"))
#define VMSLT_VV     (binstoi("011010101"))
#define VMSLEU_VV    (binstoi("010010110"))
#define VMSLE_VV     (binstoi("011010110"))
#define VMSGTU_VV    (binstoi("010010111"))
#define VMSGT_VV     (binstoi("011010111"))
#define VMINU_VV     (binstoi("000011000"))
#define VMIN_VV      (binstoi("001011000"))
#define VMAXU_VV     (binstoi("000011001"))
#define VMAX_VV      (binstoi("001011001"))
#define VSADDU_VV    (binstoi("000000010"))
#define VSADD_VV     (binstoi("001000010"))
#define VSSUBU_VV    (binstoi("000010010"))
#define VSSUB_VV     (binstoi("001010010"))
#define VAADDU_VV    (binstoi("000000011"))
#define VAADD_VV     (binstoi("001000011"))
#define VASUBU_VV    (binstoi("000010011"))
#define VASUB_VV     (binstoi("001010011"))
#define VSSRL_VV     (binstoi("000101011"))
#define VSSRA_VV     (binstoi("001101101"))
#define VNCLIPU_WV   (binstoi("110101011"))
#define VNCLIP_WV    (binstoi("111101101"))
#define VMAND_MM     (binstoi("110100001"))
#define VMNAND_MM    (binstoi("110100010"))
#define VMANDN_MM    (binstoi("110100011"))
#define VMXOR_MM     (binstoi("110100100"))
#define VMOR_MM      (binstoi("110100101"))
#define VMNOR_MM     (binstoi("110100110"))
#define VMORN_MM     (binstoi("110100111"))
#define VMXNOR_MM    (binstoi("110100000"))
#define VANDN_VV     (binstoi("000100011"))
#define VBREV_V      (binstoi("000110001"))
#define VBREV8_V     (binstoi("000110010"))
#define VREV8_V      (binstoi("000110011"))
#define VCLZ_V       (binstoi("000110100"))
#define VCTZ_V       (binstoi("000110101"))
#define VCPOP_V      (binstoi("000110000"))
#define VROL_VV      (binstoi("000101001"))
#define VROR_VV      (binstoi("000101111"))
#define VWSLL_VV     (binstoi("010101000"))

#define VIAF_ALL_OPTYPES { \
  VADD_VV   ,VSUB_VV   ,VWADDU_VV ,VWSUBU_VV ,VWADD_VV  ,VWSUB_VV  ,VWADDU_WV ,VWSUBU_WV ,VWADD_WV  ,VWSUB_WV  ,VZEXT_VF2 , \
  VSEXT_VF2 ,VZEXT_VF4 ,VSEXT_VF4 ,VZEXT_VF8 ,VSEXT_VF8 ,VADC_VVM  ,VMADC_VVM ,VMADC_VV  ,VSBC_VVM  ,VMSBC_VVM ,VMSBC_VV  ,VAND_VV   , \
  VOR_VV    ,VXOR_VV   ,VSLL_VV   ,VSRL_VV   ,VSRA_VV   ,VNSRL_WV  ,VNSRA_WV  ,VMSEQ_VV  ,VMSNE_VV  ,VMSLTU_VV ,VMSLT_VV  ,VMSLEU_VV , \
  VMSLE_VV  ,VMSGTU_VV ,VMSGT_VV  ,VMINU_VV  ,VMIN_VV   ,VMAXU_VV  ,VMAX_VV   ,VSADDU_VV ,VSADD_VV  ,VSSUBU_VV ,VSSUB_VV  ,VAADDU_VV , \
  VAADD_VV  ,VASUBU_VV ,VASUB_VV  ,VSSRL_VV  ,VSSRA_VV  ,VNCLIPU_WV,VNCLIP_WV ,VMAND_MM  ,VMNAND_MM ,VMANDN_MM ,VMXOR_MM  ,VMOR_MM   , \
  VMNOR_MM  ,VMORN_MM  ,VMXNOR_MM ,VANDN_VV  ,VBREV_V   ,VBREV8_V  ,VREV8_V   ,VCLZ_V    ,VCTZ_V    ,VCPOP_V   ,VROL_VV   ,VROR_VV   , \
  VWSLL_VV} \

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

#define VPERM_ALL_OPTYPES {VSLIDEUP,VSLIDEDOWN,VSLIDE1UP,VSLIDE1DOWN,VRGATHER,VRGATHERRS1,VCOMPRESS}

// rounding mode for fix point
#define RM_S_RNU (0)
#define RM_S_RNE (1)
#define RM_S_RDN (2)
#define RM_S_ROD (3)

#define F16_SIGN ((uint64_t)1ul << 15)
#define F32_SIGN ((uint64_t)1ul << 31)
#define F64_SIGN ((uint64_t)1ul << 63)

#define VFA_NUM 14 // for random
// vfa funcop type
#define VFADD   (binstoi("00000"))
#define VFSUB   (binstoi("00001"))
#define VFMIN   (binstoi("00010"))
#define VFMAX   (binstoi("00011"))
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
#define VFA_ALL_OPTYPES {VFADD,VFSUB,VFMIN,VFMAX,VFSGNJ,VFSGNJN,VFSGNJX,VFEQ,VFNE,VFLT,VFLE,VFGT,VFGE,VFCLASS}
#define VFA_MUST_FRS1_OPTYPES {VFGT,VFGE}
#define VFA_NEED_FRS1_OPTYPES {VFADD,VFSUB,VFMIN,VFMAX,VFSGNJ,VFSGNJN,VFSGNJX,VFEQ,VFNE,VFLT,VFLE}

#define FCMP_NUM 6
#define FCMP_FEQ    (binstoi("0000"))
#define FCMP_FLT    (binstoi("0001"))
#define FCMP_FLE    (binstoi("0010"))
#define FCMP_FLTQ   (binstoi("0101"))
#define FCMP_FLEQ   (binstoi("0110"))
#define FCMP_FCLASS (binstoi("1000"))
#define FCMP_ALL_OPTYPES {FCMP_FEQ,FCMP_FLT,FCMP_FLE,FCMP_FLTQ,FCMP_FLEQ,FCMP_FCLASS}

#define FALU_NUM 9
#define FALU_FADD   (binstoi("00000"))
#define FALU_FSUB   (binstoi("00001"))
#define FALU_FMIN   (binstoi("00010"))
#define FALU_FMAX   (binstoi("00011"))
#define FALU_FSGNJ  (binstoi("00110"))
#define FALU_FSGNJN (binstoi("00111"))
#define FALU_FSGNJX (binstoi("01000"))
#define FALU_FMAXM  (binstoi("10011"))
#define FALU_FMINM  (binstoi("11110"))
#define FALU_ALL_OPTYPES {FALU_FADD,FALU_FSUB,FALU_FMIN,FALU_FMAX,FALU_FSGNJ,FALU_FSGNJN,FALU_FSGNJX,FALU_FMAXM,FALU_FMINM}

#define FMUL_NUM 1
#define FMUL_FMUL (binstoi("0000"))
#define FMUL_ALL_OPTYPES {FMUL_FMUL}

#define FMA_NUM 4
#define FMA_FMACC  (binstoi("0001"))
#define FMA_FNMACC (binstoi("0010"))
#define FMA_FMSAC  (binstoi("0011"))
#define FMA_FNMSAC (binstoi("0100"))
#define FMA_ALL_OPTYPES {FMA_FMACC,FMA_FNMACC,FMA_FMSAC,FMA_FNMSAC}
// vff funcop type
#define VFF_NUM 9
#define VFMUL   (binstoi("0000"))
#define VFMACC  (binstoi("0001"))
#define VFNMACC (binstoi("0010"))
#define VFMSAC  (binstoi("0011"))
#define VFNMSAC (binstoi("0100"))
#define VFMADD  (binstoi("0101"))
#define VFNMADD (binstoi("0110"))
#define VFMSUB  (binstoi("0111"))
#define VFNMSUB (binstoi("1000"))
#define VFF_ALL_OPTYPES {VFMUL,VFMACC,VFNMACC,VFMSAC,VFNMSAC,VFMADD,VFNMADD,VFMSUB,VFNMSUB}
#define VFF_NEED_FRS1_OPTYPES {VFMUL,VFMACC,VFNMACC,VFMSAC,VFNMSAC,VFMADD,VFNMADD,VFMSUB,VFNMSUB}
// vff funcop type
#define VFD_NUM 2
#define VFDIV   (binstoi("0"))
#define VFSQRT  (binstoi("1"))
#define VFD_ALL_OPTYPES {VFDIV,VFSQRT}

// vid funcop type
#define VID_NUM 4
#define VIDIVU (binstoi("00"))
#define VIDIV  (binstoi("01"))
#define VIREMU (binstoi("10"))
#define VIREM  (binstoi("11"))
#define VID_ALL_OPTYPES {VIDIVU, VIDIV, VIREMU, VIREM}

// rounding mode
#define RM_RNE (0)
#define RM_RTZ (1)
#define RM_RDN (2)
#define RM_RUP (3)
#define RM_RMM (4)
#define RM_RTO (6)

// float-flags
#define FFLAGS_NV (0x10)
#define FFLAGS_DZ (0x08)
#define FFLAGS_OF (0x04)
#define FFLAGS_UF (0x02)
#define FFLAGS_NX (0x01)


// vconvert type 
  #define VFCVT_NUM 23  // for random
  #define VFCVT_XUFV       (binstoi("10000000")) 
  #define VFCVT_XFV        (binstoi("10000001")) 
  #define VFCVT_FXUV       (binstoi("01000010")) 
  #define VFCVT_FXV        (binstoi("01000011")) 
  #define VFCVT_RTZ_XUFV   (binstoi("10000110")) 
  #define VFCVT_RTZ_XFV    (binstoi("10000111")) 

  #define VFWCVT_XUFV      (binstoi("10001000")) 
  #define VFWCVT_XFV       (binstoi("10001001")) 
  #define VFWCVT_FXUV      (binstoi("01001010")) 
  #define VFWCVT_FXV       (binstoi("01001011")) 
  #define VFWCVT_FFV       (binstoi("11001100")) 
  #define VFWCVT_RTZ_XUFV  (binstoi("10001110")) 
  #define VFWCVT_RTZ_XFV   (binstoi("10001111")) 

  #define VFNCVT_XUFW      (binstoi("10010000")) 
  #define VFNCVT_XFW       (binstoi("10010001")) 
  #define VFNCVT_FXUW      (binstoi("01010010")) 
  #define VFNCVT_FXW       (binstoi("01010011")) 
  #define VFNCVT_FFW       (binstoi("11010100")) 
  #define VFNCVT_ROD_FFW   (binstoi("11010101")) 
  #define VFNCVT_RTZ_XUFW  (binstoi("10010110")) 
  #define VFNCVT_RTZ_XFW   (binstoi("10010111")) 

  #define VFRSQRT7         (binstoi("11100000")) 
  #define VFREC7           (binstoi("11100001")) 

  //FloatCvtF2X
  //sew == 1
  #define FCVT_S_H         (binstoi("11001000"))
  #define FCVT_D_H         (binstoi("11011000"))
  #define FCVT_W_H         (binstoi("10001001"))
  #define FCVT_WU_H        (binstoi("10001000"))
  #define FCVT_L_H         (binstoi("10011001"))
  #define FCVT_LU_H        (binstoi("10011000"))
  #define FCVT_H_S         (binstoi("11010000"))
  //sew == 2
  #define FCVT_W_S         (binstoi("10000001"))     
  #define FCVT_WU_S        (binstoi("10000000"))     
  #define FCVT_L_S         (binstoi("10001001"))     
  #define FCVT_LU_S        (binstoi("10001000"))     
  #define FCVT_W_D         (binstoi("10010001"))     
  #define FCVT_WU_D        (binstoi("10010000"))     
  #define FCVT_S_D         (binstoi("11010100"))     
  #define FCVT_D_S         (binstoi("11001100"))     
  //sew == 3
  #define FCVT_H_D         (binstoi("11011000"))
  #define FCVT_L_D         (binstoi("10000001"))     
  #define FCVT_LU_D        (binstoi("10000000"))     

  //FloatCvtI2F
  #define FCVT_H_WU        (binstoi("00000000"))
  #define FCVT_H_W         (binstoi("00000001"))
  #define FCVT_H_LU        (binstoi("00001000"))
  #define FCVT_H_L         (binstoi("00001001"))

  #define FCVT_S_WU        (binstoi("00000010"))
  #define FCVT_S_W         (binstoi("00000011"))
  #define FCVT_S_LU        (binstoi("00001010"))
  #define FCVT_S_L         (binstoi("00001011"))

  #define FCVT_D_WU        (binstoi("00000100"))
  #define FCVT_D_W         (binstoi("00000101"))
  #define FCVT_D_LU        (binstoi("00001100"))
  #define FCVT_D_L         (binstoi("00001101"))

  // scalar integer mul
  #define IMUL_MUL    (binstoi("00000"))
  #define IMUL_MULH   (binstoi("00001"))
  #define IMUL_MULHSU (binstoi("00010"))
  #define IMUL_MULHU  (binstoi("00011"))
  #define IMUL_MULW   (binstoi("00100"))
  #define IMUL_MULW7  (binstoi("01100"))

  #define VFCVT_ALL_OPTYPES {VFCVT_XUFV, VFCVT_XFV, VFCVT_FXUV, VFCVT_FXV, VFCVT_RTZ_XUFV, VFCVT_RTZ_XFV, \
  VFWCVT_XUFV, VFWCVT_XFV, VFWCVT_FXUV, VFWCVT_FXV, VFWCVT_FFV, VFWCVT_RTZ_XUFV, VFWCVT_RTZ_XFV, \
  VFNCVT_XUFW, VFNCVT_XFW, VFNCVT_FXUW, VFNCVT_FXW, VFNCVT_FFW, VFNCVT_ROD_FFW, VFNCVT_RTZ_XUFW ,VFNCVT_RTZ_XFW, VFRSQRT7, VFREC7}

  #define VFCVT_8_NUM 6
  #define VFCVT_8_OPTYPES {VFWCVT_FXUV, VFWCVT_FXV, VFNCVT_XUFW, VFNCVT_XFW, VFNCVT_RTZ_XUFW, VFNCVT_RTZ_XFW}

  #define VFCVT_16_NUM 19
  #define VFCVT_16_OPTYPES {VFCVT_XUFV,VFCVT_XFV,VFCVT_FXUV,VFCVT_FXV,VFCVT_RTZ_XUFV,VFCVT_RTZ_XFV,VFWCVT_XUFV,VFWCVT_XFV,VFWCVT_FXUV,VFWCVT_FXV,VFWCVT_FFV,VFWCVT_RTZ_XUFV,VFWCVT_RTZ_XFV,VFNCVT_XUFW,VFNCVT_XFW,VFNCVT_RTZ_XUFW,VFNCVT_RTZ_XFW,VFRSQRT7,VFREC7}

  #define VFCVT_32_NUM  23
  #define VFCVT_32_OPTYPES {VFCVT_XUFV, VFCVT_XFV, VFCVT_FXUV, VFCVT_FXV, VFCVT_RTZ_XUFV, VFCVT_RTZ_XFV,VFWCVT_XUFV, VFWCVT_XFV, VFWCVT_FXUV, VFWCVT_FXV, VFWCVT_FFV, VFWCVT_RTZ_XUFV, VFWCVT_RTZ_XFV,VFNCVT_XUFW, VFNCVT_XFW, VFNCVT_FXUW, VFNCVT_FXW, VFNCVT_FFW, VFNCVT_ROD_FFW, VFNCVT_RTZ_XUFW ,VFNCVT_RTZ_XFW, VFRSQRT7, VFREC7}

  #define VFCVT_64_NUM  8
  #define VFCVT_64_OPTYPES {VFCVT_XUFV,VFCVT_XFV,VFCVT_FXUV,VFCVT_FXV,VFCVT_RTZ_XUFV,VFCVT_RTZ_XFV,VFRSQRT7,VFREC7}

  //F2X
  //sew == 1 
  #define FCVT_16_NUM  7
  #define FCVT_16_OPTYPES {FCVT_H_S,FCVT_S_H,FCVT_D_H,FCVT_W_H,FCVT_WU_H,FCVT_L_H,FCVT_LU_H}
  //sew == 2
  #define FCVT_32_NUM  8
  #define FCVT_32_OPTYPES {FCVT_W_S,FCVT_WU_S,FCVT_D_S,FCVT_L_S,FCVT_LU_S,FCVT_S_D,FCVT_W_D,FCVT_WU_D}
  //sew == 3
  #define FCVT_64_NUM  3
  #define FCVT_64_OPTYPES {FCVT_H_D,FCVT_L_D,FCVT_LU_D}

  //I2F
  #define I2FCVT_64_NUM  12
  #define I2FCVT_64_OPTYPES {FCVT_H_WU,FCVT_H_W,FCVT_H_LU,FCVT_H_L,FCVT_S_WU,FCVT_S_W,FCVT_S_LU,FCVT_S_L,FCVT_D_WU,FCVT_D_W,FCVT_D_LU,FCVT_D_L}

  //VIMAC
  #define VIMAC_NUM 16
  #define VMUL             (binstoi("01100000"))
  #define VWMUL            (binstoi("01101000"))
  #define VWMULU           (binstoi("00001000"))
  #define VWMULSU          (binstoi("01001000"))
  #define VMULH            (binstoi("01100001"))
  #define VMULHU           (binstoi("00000001"))
  #define VMULHSU          (binstoi("01000001"))
  #define VMACC            (binstoi("01110010"))
  #define VWMACCU          (binstoi("00001010"))
  #define VWMACC           (binstoi("01111010"))
  #define VWMACCSU         (binstoi("00111010"))
  #define VWMACCUS         (binstoi("01011010"))
  #define VNMSAC           (binstoi("01110011"))
  #define VMADD            (binstoi("01110100"))
  #define VNMSUB           (binstoi("01110101"))
  #define VSMUL            (binstoi("01100110"))

  #define VIMAC_ALL_OPTYPES {VMUL,VWMUL,VWMULU,VWMULSU,VMULH,VMULHU,VMULHSU,VMACC,VWMACCU,VWMACC,VWMACCSU,VWMACCUS,VNMSAC,VMADD,VNMSUB,VSMUL}

  //scalar Mul
  #define IMUL_NUM 6
  #define IMUL_OPTYPES {IMUL_MUL,IMUL_MULH,IMUL_MULHSU,IMUL_MULHU,IMUL_MULW,IMUL_MULW7}

// pre-compile stoi
constexpr uint16_t binstoi(const char str[]) {
  uint16_t num = 0;
  for (int i = 0; str[i] != '\0' && i < 16; i++) {
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
