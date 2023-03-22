#include "../include/gm_common.h"
#include <typeinfo>

ElementOutput VGMFloatFMA::calculation_e16(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  ElementOutput output;

  switch(input.fuOpType) {
    case VFMACC:
      output.result = f16_mulAdd(i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src2), i2f16((uint16_t)input.src3)).v;  break;
    default:
      printf("VFFMA Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }
  
  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMFloatFMA::calculation_e32(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  ElementOutput output;

  switch(input.fuOpType) {
    case VFMACC:
      output.result = f32_mulAdd(i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src2), i2f32((uint32_t)input.src3)).v;  break;
    default:
      printf("VFFMA Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }

  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMFloatFMA::calculation_e64(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  ElementOutput output;

  switch(input.fuOpType) {
    case VFMACC:
      output.result = f64_mulAdd(i2f64((uint64_t)input.src1), i2f64((uint64_t)input.src2), i2f64((uint64_t)input.src3)).v;  break;
    default:
      printf("VFFMA Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }
  
  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

