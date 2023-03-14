#include "../include/gm_common.h"
#include "../include/vfpu_functions.h"
#include <typeinfo>

ElementOutput VGMFloatAdder::calculation_e16(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  ElementOutput output;
  switch(input.fuOpType) {
    case VFADD:
      output.result = f16_add(i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src2)).v;  break;
    case VFMIN:
      output.result = f16_min(i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src2)).v;  break;
    case VFMAX:
      output.result = f16_max(i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src2)).v;  break;
    default:
      printf("VFADD Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }
  
  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMFloatAdder::calculation_e32(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  ElementOutput output;

  switch(input.fuOpType) {
    case VFADD:
      if (input.widen) {
        if (input.src_widen) output.result = f32_add(f16_to_f32(i2f16((uint16_t)input.src1)), i2f32((uint32_t)input.src2)).v;
        else output.result = f32_add(f16_to_f32(i2f16((uint16_t)input.src1)), f16_to_f32(i2f16((uint16_t)input.src2))).v;
      }
      else output.result = f32_add(i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src2)).v;
      break;
    case VFMIN:
      output.result = f32_min(i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src2)).v;  break;
    case VFMAX:
      output.result = f32_max(i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src2)).v;  break;
    default:
      printf("VFADD Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }

  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMFloatAdder::calculation_e64(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  ElementOutput output;

  switch(input.fuOpType) {
    case VFADD:
      if (input.widen) {
        if (input.src_widen) output.result = f64_add(f32_to_f64(i2f32((uint32_t)input.src1)), i2f64((uint64_t)input.src2)).v;
        else output.result = f64_add(f32_to_f64(i2f32((uint32_t)input.src1)), f32_to_f64(i2f32((uint32_t)input.src2))).v;
      }
      else output.result = f64_add(i2f64((uint64_t)input.src1), i2f64((uint64_t)input.src2)).v;
      break;
    case VFMIN:
      output.result = f64_min(i2f64((uint64_t)input.src1), i2f64((uint64_t)input.src2)).v;  break;
    case VFMAX:
      output.result = f64_max(i2f64((uint64_t)input.src1), i2f64((uint64_t)input.src2)).v;  break;
    default:
      printf("VFADD Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }
  
  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

