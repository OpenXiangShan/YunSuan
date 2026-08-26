#include "../include/gm_common.h"
#include <typeinfo>

ElementOutput VGMFloatFMA::calculation_e16(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  ElementOutput output;

  switch(input.fuOpType) {
    case VFMACOPCODE_VFMUL_FP16:
      output.result = f16_mul(i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src2)).v;  break;
    case VFMACOPCODE_VFMACC_FP16:
      output.result = f16_mulAdd(i2f16((uint16_t)input.src2), i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src3)).v;  break;
    case VFMACOPCODE_VFNMACC_FP16:
      output.result = f16_mulAdd(i2f16((uint16_t)input.src2 ^ F16_SIGN), i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src3 ^ F16_SIGN)).v;  break;
    case VFMACOPCODE_VFMSAC_FP16:
      output.result = f16_mulAdd(i2f16((uint16_t)input.src2), i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src3 ^ F16_SIGN)).v;  break;
    case VFMACOPCODE_VFNMSAC_FP16:
      output.result = f16_mulAdd(i2f16((uint16_t)input.src2 ^ F16_SIGN), i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src3)).v;  break;
    case VFMACOPCODE_VFMADD_FP16:
      output.result = f16_mulAdd(i2f16((uint16_t)input.src2), i2f16((uint16_t)input.src3), i2f16((uint16_t)input.src1)).v;  break;
    case VFMACOPCODE_VFNMADD_FP16:
      output.result = f16_mulAdd(i2f16((uint16_t)input.src2 ^ F16_SIGN), i2f16((uint16_t)input.src3), i2f16((uint16_t)input.src1 ^ F16_SIGN)).v;  break;
    case VFMACOPCODE_VFMSUB_FP16:
      output.result = f16_mulAdd(i2f16((uint16_t)input.src2), i2f16((uint16_t)input.src3), i2f16((uint16_t)input.src1 ^ F16_SIGN)).v;  break;
    case VFMACOPCODE_VFNMSUB_FP16:
      output.result = f16_mulAdd(i2f16((uint16_t)input.src2 ^ F16_SIGN), i2f16((uint16_t)input.src3), i2f16((uint16_t)input.src1)).v;  break;
    default:
      printf("VFMA Unsupported fuOpType %d\n", input.fuOpType);
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
    case VFMACOPCODE_VFMUL_FP32:
      if(input.widen) output.result = f32_mul(f16_to_f32(i2f16((uint16_t)input.src1)), f16_to_f32(i2f16((uint16_t)input.src2))).v;
      else  output.result = f32_mul(i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src2)).v;
      break;
    case VFMACOPCODE_VFMACC_FP32:
      if(input.widen) output.result = f32_mulAdd(f16_to_f32(i2f16((uint16_t)input.src2)), f16_to_f32(i2f16((uint16_t)input.src1)), i2f32((uint32_t)input.src3)).v;
      else  output.result = f32_mulAdd(i2f32((uint32_t)input.src2), i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src3)).v;
      break;
    case VFMACOPCODE_VFNMACC_FP32:
      if(input.widen) output.result = f32_mulAdd(f16_to_f32(i2f16((uint16_t)input.src2 ^ F16_SIGN)), f16_to_f32(i2f16((uint16_t)input.src1)), i2f32((uint32_t)input.src3 ^ F32_SIGN)).v;
      else  output.result = f32_mulAdd(i2f32((uint32_t)input.src2 ^ F32_SIGN), i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src3 ^ F32_SIGN)).v;
      break;
    case VFMACOPCODE_VFMSAC_FP32:
      if(input.widen) output.result = f32_mulAdd(f16_to_f32(i2f16((uint16_t)input.src2)), f16_to_f32(i2f16((uint16_t)input.src1)), i2f32((uint32_t)input.src3 ^ F32_SIGN)).v;
      else  output.result = f32_mulAdd(i2f32((uint32_t)input.src2), i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src3 ^ F32_SIGN)).v;
      break;
    case VFMACOPCODE_VFNMSAC_FP32:
      if(input.widen) output.result = f32_mulAdd(f16_to_f32(i2f16((uint16_t)input.src2 ^ F16_SIGN)), f16_to_f32(i2f16((uint16_t)input.src1)), i2f32((uint32_t)input.src3)).v;
      else  output.result = f32_mulAdd(i2f32((uint32_t)input.src2 ^ F32_SIGN), i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src3)).v;
      break;
    case VFMACOPCODE_VFMADD_FP32:
      output.result = f32_mulAdd(i2f32((uint32_t)input.src2), i2f32((uint32_t)input.src3), i2f32((uint32_t)input.src1)).v;  break;
    case VFMACOPCODE_VFNMADD_FP32:
      output.result = f32_mulAdd(i2f32((uint32_t)input.src2 ^ F32_SIGN), i2f32((uint32_t)input.src3), i2f32((uint32_t)input.src1 ^ F32_SIGN)).v;  break;
    case VFMACOPCODE_VFMSUB_FP32:
      output.result = f32_mulAdd(i2f32((uint32_t)input.src2), i2f32((uint32_t)input.src3), i2f32((uint32_t)input.src1 ^ F32_SIGN)).v;  break;
    case VFMACOPCODE_VFNMSUB_FP32:
      output.result = f32_mulAdd(i2f32((uint32_t)input.src2 ^ F32_SIGN), i2f32((uint32_t)input.src3), i2f32((uint32_t)input.src1)).v;  break;
    default:
      printf("VFMA Unsupported fuOpType %d\n", input.fuOpType);
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
    case VFMACOPCODE_VFMUL_FP64:
      if(input.widen) output.result = f64_mul(f32_to_f64(i2f32((uint32_t)input.src1)), f32_to_f64(i2f32((uint32_t)input.src2))).v;
      else  output.result = f64_mul(i2f64((uint64_t)input.src1), i2f64((uint64_t)input.src2)).v;
      break;
    case VFMACOPCODE_VFMACC_FP64:
      if(input.widen) output.result = f64_mulAdd(f32_to_f64(i2f32((uint32_t)input.src2)), f32_to_f64(i2f32((uint32_t)input.src1)), i2f64((uint64_t)input.src3)).v;
      else  output.result = f64_mulAdd(i2f64((uint64_t)input.src2), i2f64((uint64_t)input.src1), i2f64((uint64_t)input.src3)).v;
      break;
    case VFMACOPCODE_VFNMACC_FP64:
      if(input.widen) output.result = f64_mulAdd(f32_to_f64(i2f32((uint32_t)input.src2 ^ F32_SIGN)), f32_to_f64(i2f32((uint32_t)input.src1)), i2f64((uint64_t)input.src3 ^ F64_SIGN)).v;
      else  output.result = f64_mulAdd(i2f64((uint64_t)input.src2 ^ F64_SIGN), i2f64((uint64_t)input.src1), i2f64((uint64_t)input.src3 ^ F64_SIGN)).v;
      break;
    case VFMACOPCODE_VFMSAC_FP64:
      if(input.widen) output.result = f64_mulAdd(f32_to_f64(i2f32((uint32_t)input.src2)), f32_to_f64(i2f32((uint32_t)input.src1)), i2f64((uint64_t)input.src3 ^ F64_SIGN)).v;
      else  output.result = f64_mulAdd(i2f64((uint64_t)input.src2), i2f64((uint64_t)input.src1), i2f64((uint64_t)input.src3 ^ F64_SIGN)).v;
      break;
    case VFMACOPCODE_VFNMSAC_FP64:
      if(input.widen) output.result = f64_mulAdd(f32_to_f64(i2f32((uint32_t)input.src2 ^ F32_SIGN)), f32_to_f64(i2f32((uint32_t)input.src1)), i2f64((uint64_t)input.src3)).v;
      else  output.result = f64_mulAdd(i2f64((uint64_t)input.src2 ^ F64_SIGN), i2f64((uint64_t)input.src1), i2f64((uint64_t)input.src3)).v;
      break;
    case VFMACOPCODE_VFMADD_FP64:
      output.result = f64_mulAdd(i2f64((uint64_t)input.src2), i2f64((uint64_t)input.src3), i2f64((uint64_t)input.src1)).v;  break;
    case VFMACOPCODE_VFNMADD_FP64:
      output.result = f64_mulAdd(i2f64((uint64_t)input.src2 ^ F64_SIGN), i2f64((uint64_t)input.src3), i2f64((uint64_t)input.src1 ^ F64_SIGN)).v;  break;
    case VFMACOPCODE_VFMSUB_FP64:
      output.result = f64_mulAdd(i2f64((uint64_t)input.src2), i2f64((uint64_t)input.src3), i2f64((uint64_t)input.src1 ^ F64_SIGN)).v;  break;
    case VFMACOPCODE_VFNMSUB_FP64:
      output.result = f64_mulAdd(i2f64((uint64_t)input.src2 ^ F64_SIGN), i2f64((uint64_t)input.src3), i2f64((uint64_t)input.src1)).v;  break;
    default:
      printf("VFMA Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }
  
  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

