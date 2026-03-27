#include "../include/gm_common.h"
#include "../include/vfpu_functions.h"
#include <typeinfo>
#include <stdint.h>

#define BOX_MASK_FP16 0xFFFFFFFFFFFF0000
#define BOX_MASK_FP32 0xFFFFFFFF00000000

static inline uint64_t boxf16(uint16_t r) {
  return BOX_MASK_FP16 | r;
}

static inline uint64_t boxf32(uint32_t r) {
  return BOX_MASK_FP32 | r;
}

static inline uint16_t unboxf16(uint64_t r) {
  return (r & BOX_MASK_FP16) == BOX_MASK_FP16
    ? (uint16_t)(r & ~BOX_MASK_FP16) : defaultNaN_ui16;
}

static inline uint32_t unboxf32(uint64_t r) {
  return (r & BOX_MASK_FP32) == BOX_MASK_FP32
    ? (uint32_t)(r & ~BOX_MASK_FP32) : defaultNaN_ui32;
}

static inline float16_t rtlToF16(uint64_t r) {
  float16_t f = { .v = unboxf16(r) };
  return f;
}

static inline float32_t rtlToF32(uint64_t r) {
  float32_t f = { .v = unboxf32(r) };
  return f;
}

VecOutput SGMFloatCompare::get_expected_output(VecInput input) {
  return VPUGoldenModel::get_expected_output(input);
}

ElementOutput SGMFloatCompare::calculation_e16(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  float16_t src1 = rtlToF16(input.src1);
  float16_t src2 = rtlToF16(input.src2);
  bool hasNan = isNaNF16UI(src1.v) || isNaNF16UI(src2.v);
  bool hasSigNan = softfloat_isSigNaNF16UI(src1.v) || softfloat_isSigNaNF16UI(src2.v);
  ElementOutput output = {0, 0, false};

  switch(input.fuOpType) {
    case FCMP_FEQ:
      output.result = f16_eq(src1, src2) ? (uint64_t)1 : (uint64_t)0;
      break;
    case FCMP_FLT:
      output.result = f16_lt(src1, src2) ? (uint64_t)1 : (uint64_t)0;
      break;
    case FCMP_FLE:
      output.result = f16_le(src1, src2) ? (uint64_t)1 : (uint64_t)0;
      break;
    case FCMP_FMIN:
      output.result = boxf16(f16_min(src1, src2).v);
      break;
    case FCMP_FMAX:
      output.result = boxf16(f16_max(src1, src2).v);
      break;
    case FCMP_FSGNJ:
      output.result = boxf16(f16_sgnj(src1, src2, false, false).v);
      break;
    case FCMP_FSGNJX:
      output.result = boxf16(f16_sgnj(src1, src2, false, true).v);
      break;
    case FCMP_FSGNJN:
      output.result = boxf16(f16_sgnj(src1, src2, true, false).v);
      break;
    case FCMP_FMINM:
      if (hasNan) {
        if (hasSigNan) { softfloat_exceptionFlags |= softfloat_flag_invalid; }
        output.result = boxf16(defaultNaN_ui16);
      } else {
        output.result = boxf16(f16_min(src1, src2).v);
      }
      break;
    case FCMP_FMAXM:
      if (hasNan) {
        if (hasSigNan) { softfloat_exceptionFlags |= softfloat_flag_invalid; }
        output.result = boxf16(defaultNaN_ui16);
      } else {
        output.result = boxf16(f16_max(src1, src2).v);
      }
      break;
    case FCMP_FLTQ:
      output.result = f16_lt_quiet(src1, src2) ? (uint64_t)1 : (uint64_t)0;
      break;
    case FCMP_FLEQ:
      output.result = f16_le_quiet(src1, src2) ? (uint64_t)1 : (uint64_t)0;
      break;
    case FCMP_FCLASS:
      output.result = f16_classify(src1);
      break;
    default:
      printf("FloatCompare unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }

  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput SGMFloatCompare::calculation_e32(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  float32_t src1 = rtlToF32(input.src1);
  float32_t src2 = rtlToF32(input.src2);
  bool hasNan = isNaNF32UI(src1.v) || isNaNF32UI(src2.v);
  bool hasSigNan = softfloat_isSigNaNF32UI(src1.v) || softfloat_isSigNaNF32UI(src2.v);
  ElementOutput output = {0, 0, false};

  switch(input.fuOpType) {
    case FCMP_FEQ:
      output.result = f32_eq(src1, src2) ? (uint64_t)1 : (uint64_t)0;
      break;
    case FCMP_FLT:
      output.result = f32_lt(src1, src2) ? (uint64_t)1 : (uint64_t)0;
      break;
    case FCMP_FLE:
      output.result = f32_le(src1, src2) ? (uint64_t)1 : (uint64_t)0;
      break;
    case FCMP_FMIN:
      output.result = boxf32(f32_min(src1, src2).v);
      break;
    case FCMP_FMAX:
      output.result = boxf32(f32_max(src1, src2).v);
      break;
    case FCMP_FSGNJ:
      output.result = boxf32(f32_sgnj(src1, src2, false, false).v);
      break;
    case FCMP_FSGNJX:
      output.result = boxf32(f32_sgnj(src1, src2, false, true).v);
      break;
    case FCMP_FSGNJN:
      output.result = boxf32(f32_sgnj(src1, src2, true, false).v);
      break;
    case FCMP_FMINM:
      if (hasNan) {
        if (hasSigNan) { softfloat_exceptionFlags |= softfloat_flag_invalid; }
        output.result = boxf32(defaultNaN_ui32);
      } else {
        output.result = boxf32(f32_min(src1, src2).v);
      }
      break;
    case FCMP_FMAXM:
      if (hasNan) {
        if (hasSigNan) { softfloat_exceptionFlags |= softfloat_flag_invalid; }
        output.result = boxf32(defaultNaN_ui32);
      } else {
        output.result = boxf32(f32_max(src1, src2).v);
      }
      break;
    case FCMP_FLTQ:
      output.result = f32_lt_quiet(src1, src2) ? (uint64_t)1 : (uint64_t)0;
      break;
    case FCMP_FLEQ:
      output.result = f32_le_quiet(src1, src2) ? (uint64_t)1 : (uint64_t)0;
      break;
    case FCMP_FCLASS:
      output.result = f32_classify(src1);
      break;
    default:
      printf("FloatCompare unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }

  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput SGMFloatCompare::calculation_e64(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  float64_t src1 = i2f64((uint64_t)input.src1);
  float64_t src2 = i2f64((uint64_t)input.src2);
  bool hasNan = isNaNF64UI(src1.v) || isNaNF64UI(src2.v);
  bool hasSigNan = softfloat_isSigNaNF64UI(src1.v) || softfloat_isSigNaNF64UI(src2.v);
  ElementOutput output = {0, 0, false};

  switch(input.fuOpType) {
    case FCMP_FEQ:
      output.result = f64_eq(src1, src2) ? (uint64_t)1 : (uint64_t)0;
      break;
    case FCMP_FLT:
      output.result = f64_lt(src1, src2) ? (uint64_t)1 : (uint64_t)0;
      break;
    case FCMP_FLE:
      output.result = f64_le(src1, src2) ? (uint64_t)1 : (uint64_t)0;
      break;
    case FCMP_FMIN:
      output.result = f64_min(src1, src2).v;
      break;
    case FCMP_FMAX:
      output.result = f64_max(src1, src2).v;
      break;
    case FCMP_FSGNJ:
      output.result = f64_sgnj(src1, src2, false, false).v;
      break;
    case FCMP_FSGNJX:
      output.result = f64_sgnj(src1, src2, false, true).v;
      break;
    case FCMP_FSGNJN:
      output.result = f64_sgnj(src1, src2, true, false).v;
      break;
    case FCMP_FMINM:
      if (hasNan) {
        if (hasSigNan) { softfloat_exceptionFlags |= softfloat_flag_invalid; }
        output.result = defaultNaN_ui64;
      } else {
        output.result = f64_min(src1, src2).v;
      }
      break;
    case FCMP_FMAXM:
      if (hasNan) {
        if (hasSigNan) { softfloat_exceptionFlags |= softfloat_flag_invalid; }
        output.result = defaultNaN_ui64;
      } else {
        output.result = f64_max(src1, src2).v;
      }
      break;
    case FCMP_FLTQ:
      output.result = f64_lt_quiet(src1, src2) ? (uint64_t)1 : (uint64_t)0;
      break;
    case FCMP_FLEQ:
      output.result = f64_le_quiet(src1, src2) ? (uint64_t)1 : (uint64_t)0;
      break;
    case FCMP_FCLASS:
      output.result = f64_classify(src1);
      break;
    default:
      printf("FloatCompare unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }

  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}
