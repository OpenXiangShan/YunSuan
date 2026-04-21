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

ElementOutput SGMFloatFMA::calculation_e16(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  float16_t src1 = rtl_to_f16(input.src1);
  float16_t src2 = rtl_to_f16(input.src2);
  float16_t src3 = rtl_to_f16(input.src3);
  ElementOutput output = {0, 0, false};

  switch(input.fuOpType) {
    case FMA_FMACC:
      output.result = box_f16(f16_mulAdd(src1, src2, src3).v); break;
    case FMA_FNMACC:
      output.result = box_f16(f16_mulAdd(f16(src1.v ^ F16_SIGN), src2, f16(src3.v ^ F16_SIGN)).v); break;
    case FMA_FMSAC:
      output.result = box_f16(f16_mulAdd(src1, src2, f16(src3.v ^ F16_SIGN)).v); break;
    case FMA_FNMSAC:
      output.result = box_f16(f16_mulAdd(f16(src1.v ^ F16_SIGN), src2, src3).v); break;
    default:
      printf("Scalar Float FMA Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }

  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) {
    ElementInput display_input = input;
    ElementOutput display_output = output;
    display_input.src1 = (uint64_t)(uint16_t)input.src1;
    display_input.src2 = (uint64_t)(uint16_t)input.src2;
    display_input.src3 = (uint64_t)(uint16_t)input.src3;
    display_output.result = (uint64_t)(uint16_t)output.result;
    display_calculation(typeid(this).name(), __func__, display_input, display_output);
  }
  return output;
}

ElementOutput SGMFloatFMA::calculation_e32(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  float32_t src1 = rtl_to_f32(input.src1);
  float32_t src2 = rtl_to_f32(input.src2);
  float32_t src3 = rtl_to_f32(input.src3);
  ElementOutput output = {0, 0, false};

  switch(input.fuOpType) {
    case FMA_FMACC:
      output.result = box_f32(f32_mulAdd(src1, src2, src3).v); break;
    case FMA_FNMACC:
      output.result = box_f32(f32_mulAdd(f32(src1.v ^ F32_SIGN), src2, f32(src3.v ^ F32_SIGN)).v); break;
    case FMA_FMSAC:
      output.result = box_f32(f32_mulAdd(src1, src2, f32(src3.v ^ F32_SIGN)).v); break;
    case FMA_FNMSAC:
      output.result = box_f32(f32_mulAdd(f32(src1.v ^ F32_SIGN), src2, src3).v); break;
    default:
      printf("Scalar Float FMA Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }

  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) {
    ElementInput display_input = input;
    ElementOutput display_output = output;
    display_input.src1 = (uint64_t)(uint32_t)input.src1;
    display_input.src2 = (uint64_t)(uint32_t)input.src2;
    display_input.src3 = (uint64_t)(uint32_t)input.src3;
    display_output.result = (uint64_t)(uint32_t)output.result;
    display_calculation(typeid(this).name(), __func__, display_input, display_output);
  }
  return output;
}

ElementOutput SGMFloatFMA::calculation_e64(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  float64_t src1 = i2f64((uint64_t)input.src1);
  float64_t src2 = i2f64((uint64_t)input.src2);
  float64_t src3 = i2f64((uint64_t)input.src3);
  ElementOutput output = {0, 0, false};

  switch(input.fuOpType) {
    case FMA_FMACC:
      output.result = f64_mulAdd(src1, src2, src3).v; break;
    case FMA_FNMACC:
      output.result = f64_mulAdd(f64(src1.v ^ F64_SIGN), src2, f64(src3.v ^ F64_SIGN)).v; break;
    case FMA_FMSAC:
      output.result = f64_mulAdd(src1, src2, f64(src3.v ^ F64_SIGN)).v; break;
    case FMA_FNMSAC:
      output.result = f64_mulAdd(f64(src1.v ^ F64_SIGN), src2, src3).v; break;
    default:
      printf("Scalar Float FMA Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }

  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}
