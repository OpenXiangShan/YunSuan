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
    case VFSUB:
      output.result = f16_sub(i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src2)).v;  break;
    case VFMIN:
      output.result = f16_min(i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src2)).v;  break;
    case VFMAX:
      output.result = f16_max(i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src2)).v;  break;
    case VFMERGE:
      output.result = ((uint8_t)input.src3 & 0x01) == 1? i2f16((uint16_t)input.src2).v: i2f16((uint16_t)input.src1).v;  break;
    case VFMOVE:
      output.result = i2f16((uint16_t)input.src2).v;  break;
    case VFSGNJ:
      output.result = f16_sgnj(i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src2), false, false).v;  break;
    case VFSGNJN:
      output.result = f16_sgnj(i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src2), true, false).v;  break;
    case VFSGNJX:
      output.result = f16_sgnj(i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src2), false, true).v;  break;
    case VFEQ:
      output.result = f16_eq(i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src2)) ? (uint64_t)1 : (uint64_t)0;  break;
    case VFNE:
      output.result = f16_eq(i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src2)) ? (uint64_t)0 : (uint64_t)1;  break;
    case VFLT:
      output.result = f16_lt(i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src2)) ? (uint64_t)1 : (uint64_t)0;  break;
    case VFLE:
      output.result = f16_le(i2f16((uint16_t)input.src1), i2f16((uint16_t)input.src2)) ? (uint64_t)1 : (uint64_t)0;  break;
    case VFGT:
      output.result = f16_lt(i2f16((uint16_t)input.src2), i2f16((uint16_t)input.src1)) ? (uint64_t)1 : (uint64_t)0;  break;
    case VFGE:
      output.result = f16_le(i2f16((uint16_t)input.src2), i2f16((uint16_t)input.src1)) ? (uint64_t)1 : (uint64_t)0;  break;
    case VFCLASS:
      output.result = f16_classify(i2f16((uint16_t)input.src1));  break;
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
        if (input.src_widen)  output.result = f32_add(f16_to_f32(i2f16((uint16_t)input.src1)), i2f32((uint32_t)input.src2)).v;
        else  output.result = f32_add(f16_to_f32(i2f16((uint16_t)input.src1)), f16_to_f32(i2f16((uint16_t)input.src2))).v;
      }
      else output.result = f32_add(i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src2)).v;
      break;
    case VFSUB:
      if (input.widen) {
        if (input.src_widen)  output.result = f32_sub(f16_to_f32(i2f16((uint16_t)input.src1)), i2f32((uint32_t)input.src2)).v;
        else  output.result = f32_sub(f16_to_f32(i2f16((uint16_t)input.src1)), f16_to_f32(i2f16((uint16_t)input.src2))).v;
      }
      else output.result = f32_sub(i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src2)).v;
      break;
    case VFMIN:
      output.result = f32_min(i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src2)).v;  break;
    case VFMAX:
      output.result = f32_max(i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src2)).v;  break;
    case VFMERGE:
      output.result = ((uint8_t)input.src3 & 0x01) == 1? i2f32((uint32_t)input.src2).v: i2f32((uint32_t)input.src1).v;  break;
    case VFMOVE:
      output.result = i2f32((uint32_t)input.src2).v;  break;
    case VFSGNJ:
      output.result = f32_sgnj(i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src2), false, false).v;  break;
    case VFSGNJN:
      output.result = f32_sgnj(i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src2), true, false).v;  break;
    case VFSGNJX:
      output.result = f32_sgnj(i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src2), false, true).v;  break;
    case VFEQ:
      output.result = f32_eq(i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src2)) ? (uint64_t)1 : (uint64_t)0;  break;
    case VFNE:
      output.result = f32_eq(i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src2)) ? (uint64_t)0 : (uint64_t)1;  break;
    case VFLT:
      output.result = f32_lt(i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src2)) ? (uint64_t)1 : (uint64_t)0;  break;
    case VFLE:
      output.result = f32_le(i2f32((uint32_t)input.src1), i2f32((uint32_t)input.src2)) ? (uint64_t)1 : (uint64_t)0;  break;
    case VFGT:
      output.result = f32_lt(i2f32((uint32_t)input.src2), i2f32((uint32_t)input.src1)) ? (uint64_t)1 : (uint64_t)0;  break;
    case VFGE:
      output.result = f32_le(i2f32((uint32_t)input.src2), i2f32((uint32_t)input.src1)) ? (uint64_t)1 : (uint64_t)0;  break;
    case VFCLASS:
      output.result = f32_classify(i2f32((uint32_t)input.src1));  break;
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
    case VFSUB:
      if (input.widen) {
        if (input.src_widen)  output.result = f64_sub(f32_to_f64(i2f32((uint32_t)input.src1)), i2f64((uint64_t)input.src2)).v;
        else  output.result = f64_sub(f32_to_f64(i2f32((uint32_t)input.src1)), f32_to_f64(i2f32((uint32_t)input.src2))).v;
      }
      else output.result = f64_sub(i2f64((uint64_t)input.src1), i2f64((uint64_t)input.src2)).v;
      break;
    case VFMIN:
      output.result = f64_min(i2f64((uint64_t)input.src1), i2f64((uint64_t)input.src2)).v;  break;
    case VFMAX:
      output.result = f64_max(i2f64((uint64_t)input.src1), i2f64((uint64_t)input.src2)).v;  break;
    case VFMERGE:
      output.result = ((uint8_t)input.src3 & 0x01) == 1? i2f64((uint64_t)input.src2).v: i2f64((uint64_t)input.src1).v;  break;
    case VFMOVE:
      output.result = i2f64((uint64_t)input.src2).v;  break;
    case VFSGNJ:
      output.result = f64_sgnj(i2f64((uint64_t)input.src1), i2f64((uint64_t)input.src2), false, false).v;  break;
    case VFSGNJN:
      output.result = f64_sgnj(i2f64((uint64_t)input.src1), i2f64((uint64_t)input.src2), true, false).v;  break;
    case VFSGNJX:
      output.result = f64_sgnj(i2f64((uint64_t)input.src1), i2f64((uint64_t)input.src2), false, true).v;  break;
    case VFEQ:
      output.result = f64_eq(i2f64((uint64_t)input.src1), i2f64((uint64_t)input.src2)) ? (uint64_t)1 : (uint64_t)0;  break;
    case VFNE:
      output.result = f64_eq(i2f64((uint64_t)input.src1), i2f64((uint64_t)input.src2)) ? (uint64_t)0 : (uint64_t)1;  break;
    case VFLT:
      output.result = f64_lt(i2f64((uint64_t)input.src1), i2f64((uint64_t)input.src2)) ? (uint64_t)1 : (uint64_t)0;  break;
    case VFLE:
      output.result = f64_le(i2f64((uint64_t)input.src1), i2f64((uint64_t)input.src2)) ? (uint64_t)1 : (uint64_t)0;  break;
    case VFGT:
      output.result = f64_lt(i2f64((uint64_t)input.src2), i2f64((uint64_t)input.src1)) ? (uint64_t)1 : (uint64_t)0;  break;
    case VFGE:
      output.result = f64_le(i2f64((uint64_t)input.src2), i2f64((uint64_t)input.src1)) ? (uint64_t)1 : (uint64_t)0;  break;
    case VFCLASS:
      output.result = f64_classify(i2f64((uint64_t)input.src1));  break;
    default:
      printf("VFADD Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }
  
  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

