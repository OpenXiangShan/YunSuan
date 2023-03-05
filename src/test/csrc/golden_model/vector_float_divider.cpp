#include "../include/gm_common.h"

// bool VGMFloatDivider::bad_fuType(VecInput input) {
//   return false;
// }

// bool VGMFloatDivider::bad_fuOpType(VecInput input) {
//   return false;
// }

ElementOutput VGMFloatDivider::calculation_e16(ElementInput input) {
  // printf("VFD e16\n");
  fp_set_rm(input.rm);
  fp_clear_exception();
  ElementOutput output;
  output.result = f16_div(i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src2)).v;
  output.fflags = softfloat_exceptionFlags & 0x1f;
  // printf("VFD: src1:%lx src2:%lx result:%lx fflags:%x\n", input.src1, input.src2, output.result, output.fflags);
  return output;
}

ElementOutput VGMFloatDivider::calculation_e32(ElementInput input) {
  // printf("VFD e32\n");
  fp_set_rm(input.rm);
  fp_clear_exception();
  ElementOutput output;
  output.result = f32_div(i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src2)).v;
  output.fflags = softfloat_exceptionFlags & 0x1f;
  // printf("ALU: src1:%lx src2:%lx result:%lx fflags:%x\n", input.src1, input.src2, output.result, output.fflags);
  return output;
}

ElementOutput VGMFloatDivider::calculation_e64(ElementInput input) {
  // printf("VFD e64\n");
  fp_set_rm(input.rm);
  fp_clear_exception();
  ElementOutput output;
  output.result = f64_div(i2f64(input.src1), i2f64(input.src2)).v;
  output.fflags = softfloat_exceptionFlags & 0x1f;
  // printf("ALU: src1:%lx src2:%lx result:%lx fflags:%x\n", input.src1, input.src2, output.result, output.fflags);
  return output;
}