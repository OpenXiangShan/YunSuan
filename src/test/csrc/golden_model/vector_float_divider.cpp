#include "../include/gm_common.h"
#include <typeinfo>

// bool VGMFloatDivider::bad_fuType(VecInput input) {
//   return false;
// }

// bool VGMFloatDivider::bad_fuOpType(VecInput input) {
//   return false;
// }

ElementOutput VGMFloatDivider::calculation_e16(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  ElementOutput output;

  switch (input.fuOpType) {
    case VFDIVOPCODE_VFDIV_FP16:
      output.result = f16_div(i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src2)).v;
      break;
    case VFDIVOPCODE_VFSQRT_FP16:
      output.result = f16_sqrt(i2f16((uint16_t)input.src1)).v;
      break;
    default:
      printf("VFD Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }
  
  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMFloatDivider::calculation_e32(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  ElementOutput output;

  switch (input.fuOpType) {
    case VFDIVOPCODE_VFDIV_FP32:
      output.result = f32_div(i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src2)).v;
      break;
    case VFDIVOPCODE_VFSQRT_FP32:
      output.result = f32_sqrt(i2f32((uint32_t)input.src1)).v;
      break;
    default:
      printf("VFD Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }

  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMFloatDivider::calculation_e64(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  ElementOutput output;

  switch (input.fuOpType) {
    case VFDIVOPCODE_VFDIV_FP64:
      output.result = f64_div(i2f64(input.src1), i2f64(input.src2)).v;
      break;
    case VFDIVOPCODE_VFSQRT_FP64:
      output.result = f64_sqrt(i2f64((uint64_t)input.src1)).v;
      break;
    default:
      printf("VFD Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }

  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}