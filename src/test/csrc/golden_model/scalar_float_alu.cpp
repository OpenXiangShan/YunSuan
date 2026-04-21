#include "../include/gm_common.h"
#include "../include/vfpu_functions.h"
#include <typeinfo>
#include <stdint.h>

#define BOX_MASK_FP16 0xFFFFFFFFFFFF0000
#define BOX_MASK_FP32 0xFFFFFFFF00000000

static inline uint64_t box_f16(uint16_t value) {
  return BOX_MASK_FP16 | value;
}

static inline uint64_t box_f32(uint32_t value) {
  return BOX_MASK_FP32 | value;
}

static inline uint16_t unbox_f16(uint64_t value) {
  return (value & BOX_MASK_FP16) == BOX_MASK_FP16 ? (uint16_t)(value & ~BOX_MASK_FP16) : defaultNaN_ui16;
}

static inline uint32_t unbox_f32(uint64_t value) {
  return (value & BOX_MASK_FP32) == BOX_MASK_FP32 ? (uint32_t)(value & ~BOX_MASK_FP32) : defaultNaN_ui32;
}

static inline float16_t rtl_to_f16(uint64_t value) {
  return f16(unbox_f16(value));
}

static inline float32_t rtl_to_f32(uint64_t value) {
  return f32(unbox_f32(value));
}

static inline float16_t f16_minm(float16_t a, float16_t b) {
  bool a_is_nan = isNaNF16UI(a.v);
  bool b_is_nan = isNaNF16UI(b.v);

  if (softfloat_isSigNaNF16UI(a.v) || softfloat_isSigNaNF16UI(b.v)) {
    softfloat_exceptionFlags |= softfloat_flag_invalid;
  }
  if (a_is_nan || b_is_nan) {
    return *defaultNaN_f16;
  }
  return f16_min(a, b);
}

static inline float16_t f16_maxm(float16_t a, float16_t b) {
  bool a_is_nan = isNaNF16UI(a.v);
  bool b_is_nan = isNaNF16UI(b.v);

  if (softfloat_isSigNaNF16UI(a.v) || softfloat_isSigNaNF16UI(b.v)) {
    softfloat_exceptionFlags |= softfloat_flag_invalid;
  }
  if (a_is_nan || b_is_nan) {
    return *defaultNaN_f16;
  }
  return f16_max(a, b);
}

static inline float32_t f32_minm(float32_t a, float32_t b) {
  bool a_is_nan = isNaNF32UI(a.v);
  bool b_is_nan = isNaNF32UI(b.v);

  if (softfloat_isSigNaNF32UI(a.v) || softfloat_isSigNaNF32UI(b.v)) {
    softfloat_exceptionFlags |= softfloat_flag_invalid;
  }
  if (a_is_nan || b_is_nan) {
    return *defaultNaN_f32;
  }
  return f32_min(a, b);
}

static inline float32_t f32_maxm(float32_t a, float32_t b) {
  bool a_is_nan = isNaNF32UI(a.v);
  bool b_is_nan = isNaNF32UI(b.v);

  if (softfloat_isSigNaNF32UI(a.v) || softfloat_isSigNaNF32UI(b.v)) {
    softfloat_exceptionFlags |= softfloat_flag_invalid;
  }
  if (a_is_nan || b_is_nan) {
    return *defaultNaN_f32;
  }
  return f32_max(a, b);
}

static inline float64_t f64_minm(float64_t a, float64_t b) {
  bool a_is_nan = isNaNF64UI(a.v);
  bool b_is_nan = isNaNF64UI(b.v);

  if (softfloat_isSigNaNF64UI(a.v) || softfloat_isSigNaNF64UI(b.v)) {
    softfloat_exceptionFlags |= softfloat_flag_invalid;
  }
  if (a_is_nan || b_is_nan) {
    return *defaultNaN_f64;
  }
  return f64_min(a, b);
}

static inline float64_t f64_maxm(float64_t a, float64_t b) {
  bool a_is_nan = isNaNF64UI(a.v);
  bool b_is_nan = isNaNF64UI(b.v);

  if (softfloat_isSigNaNF64UI(a.v) || softfloat_isSigNaNF64UI(b.v)) {
    softfloat_exceptionFlags |= softfloat_flag_invalid;
  }
  if (a_is_nan || b_is_nan) {
    return *defaultNaN_f64;
  }
  return f64_max(a, b);
}

ElementOutput SGMFloatALU::calculation_e16(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  float16_t src1 = rtl_to_f16(input.src1);
  float16_t src2 = rtl_to_f16(input.src2);
  ElementOutput output = {0, 0, false};

  switch(input.fuOpType) {
    case FALU_FADD:
      output.result = box_f16(f16_add(src1, src2).v); break;
    case FALU_FSUB:
      output.result = box_f16(f16_sub(src1, src2).v); break;
    case FALU_FMIN:
      output.result = box_f16(f16_min(src1, src2).v); break;
    case FALU_FMAX:
      output.result = box_f16(f16_max(src1, src2).v); break;
    case FALU_FSGNJ:
      output.result = box_f16(f16_sgnj(src1, src2, false, false).v); break;
    case FALU_FSGNJN:
      output.result = box_f16(f16_sgnj(src1, src2, true, false).v); break;
    case FALU_FSGNJX:
      output.result = box_f16(f16_sgnj(src1, src2, false, true).v); break;
    case FALU_FMINM:
      output.result = box_f16(f16_minm(src1, src2).v); break;
    case FALU_FMAXM:
      output.result = box_f16(f16_maxm(src1, src2).v); break;
    default:
      printf("Scalar Float ALU Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }

  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) {
    ElementInput display_input = input;
    ElementOutput display_output = output;
    display_input.src1 = (uint64_t)(uint16_t)input.src1;
    display_input.src2 = (uint64_t)(uint16_t)input.src2;
    display_output.result = (uint64_t)(uint16_t)output.result;
    display_calculation(typeid(this).name(), __func__, display_input, display_output);
  }
  return output;
}

ElementOutput SGMFloatALU::calculation_e32(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  float32_t src1 = rtl_to_f32(input.src1);
  float32_t src2 = rtl_to_f32(input.src2);
  ElementOutput output = {0, 0, false};

  switch(input.fuOpType) {
    case FALU_FADD:
      output.result = box_f32(f32_add(src1, src2).v); break;
    case FALU_FSUB:
      output.result = box_f32(f32_sub(src1, src2).v); break;
    case FALU_FMIN:
      output.result = box_f32(f32_min(src1, src2).v); break;
    case FALU_FMAX:
      output.result = box_f32(f32_max(src1, src2).v); break;
    case FALU_FSGNJ:
      output.result = box_f32(f32_sgnj(src1, src2, false, false).v); break;
    case FALU_FSGNJN:
      output.result = box_f32(f32_sgnj(src1, src2, true, false).v); break;
    case FALU_FSGNJX:
      output.result = box_f32(f32_sgnj(src1, src2, false, true).v); break;
    case FALU_FMINM:
      output.result = box_f32(f32_minm(src1, src2).v); break;
    case FALU_FMAXM:
      output.result = box_f32(f32_maxm(src1, src2).v); break;
    default:
      printf("Scalar Float ALU Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }

  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) {
    ElementInput display_input = input;
    ElementOutput display_output = output;
    display_input.src1 = (uint64_t)(uint32_t)input.src1;
    display_input.src2 = (uint64_t)(uint32_t)input.src2;
    display_output.result = (uint64_t)(uint32_t)output.result;
    display_calculation(typeid(this).name(), __func__, display_input, display_output);
  }
  return output;
}

ElementOutput SGMFloatALU::calculation_e64(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  float64_t src1 = i2f64((uint64_t)input.src1);
  float64_t src2 = i2f64((uint64_t)input.src2);
  ElementOutput output = {0, 0, false};

  switch(input.fuOpType) {
    case FALU_FADD:
      output.result = f64_add(src1, src2).v; break;
    case FALU_FSUB:
      output.result = f64_sub(src1, src2).v; break;
    case FALU_FMIN:
      output.result = f64_min(src1, src2).v; break;
    case FALU_FMAX:
      output.result = f64_max(src1, src2).v; break;
    case FALU_FSGNJ:
      output.result = f64_sgnj(src1, src2, false, false).v; break;
    case FALU_FSGNJN:
      output.result = f64_sgnj(src1, src2, true, false).v; break;
    case FALU_FSGNJX:
      output.result = f64_sgnj(src1, src2, false, true).v; break;
    case FALU_FMINM:
      output.result = f64_minm(src1, src2).v; break;
    case FALU_FMAXM:
      output.result = f64_maxm(src1, src2).v; break;
    default:
      printf("Scalar Float ALU Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }

  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}
