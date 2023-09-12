#include "../include/gm_common.h"

ElementOutput VGMFloatBase::calculation_e8(ElementInput input) {
  printf("Float does not support e8");
  exit(1);
  ElementOutput output;
  output.result = 0;
  return output;
}

void VGMFloatBase::fp_set_rm(int rm) {
  switch (rm) {
    case RM_RNE: softfloat_roundingMode = softfloat_round_near_even; break;
    case RM_RTZ: softfloat_roundingMode = softfloat_round_minMag; break;
    case RM_RDN: softfloat_roundingMode = softfloat_round_min; break;
    case RM_RUP: softfloat_roundingMode = softfloat_round_max; break;
    case RM_RMM: softfloat_roundingMode = softfloat_round_near_maxMag; break;
    case RM_RTO: softfloat_roundingMode = softfloat_round_odd; break;
    default:
      printf("Error RM:%d\n", rm);
      exit(1);
  }
}

uint32_t VGMFloatBase::fp_get_exception() {
  uint32_t ex = 0;
  uint32_t softfp_ex = softfloat_exceptionFlags;
  if (softfp_ex & softfloat_flag_inexact  ) ex |= FFLAGS_NX;
  if (softfp_ex & softfloat_flag_underflow) ex |= FFLAGS_UF;
  if (softfp_ex & softfloat_flag_overflow ) ex |= FFLAGS_OF;
  if (softfp_ex & softfloat_flag_infinite ) ex |= FFLAGS_DZ;
  if (softfp_ex & softfloat_flag_invalid  ) ex |= FFLAGS_NV;
  return ex;
}

void VGMFloatBase::fp_clear_exception() {
  softfloat_exceptionFlags = 0;
}

float16_t VGMFloatBase::i2f16(uint16_t i) {
  float16_t *fp = (float16_t *)&i;
  return *fp;
}

float32_t VGMFloatBase::i2f32(uint32_t i) {
  float32_t *fp = (float32_t *)&i;
  return *fp;
}

float64_t VGMFloatBase::i2f64(uint64_t i) {
  float64_t *fp = (float64_t *)&i;
  return *fp;
}