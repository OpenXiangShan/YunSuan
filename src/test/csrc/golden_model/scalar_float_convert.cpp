#include "../include/gm_common.h"
#include "../include/vfpu_functions.h"
#include <typeinfo>
#include <stdint.h>

#define BOX_MASK_FP16 0xFFFFFFFFFFFF0000
#define BOX_MASK_FP32 0xFFFFFFFF00000000
#define defaultNaNF16UI 0x7E00
#define defaultNaNF32UI 0x7FC00000

static inline uint64_t unboxf16(uint64_t r) {
  return (r & BOX_MASK_FP16) == BOX_MASK_FP16
    ? (r & ~BOX_MASK_FP16) : defaultNaNF16UI;
}
static inline uint64_t unboxf32(uint64_t r) {
  return (r & BOX_MASK_FP32) == BOX_MASK_FP32
    ? (r & ~BOX_MASK_FP32) : defaultNaNF32UI;
}

static inline float32_t  my_f16_to_f32 (float16_t a) {
  return f16_to_f32(a);
}
static inline float64_t  my_f16_to_f64 (float16_t a) {
  return f16_to_f64(a);
}
static inline float16_t  my_f32_to_f16 (float32_t a) {
  return f32_to_f16(a);
}
static inline float64_t  my_f32_to_f64 (float32_t a) {
  return f32_to_f64(a);
}
static inline float32_t rtlToF32(uint64_t r) {
  float32_t f = { .v = (uint32_t)unboxf32(r) };
  return f;
}
static inline float16_t rtlToF16(uint64_t r) {
  float16_t f = { .v = (uint16_t)unboxf16(r) };
  return f;
}

ElementOutput SGMFloatCvt::calculation_e8(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  ElementOutput output;
//   switch(input.fuOpType) {
//     // widen 8->16
//     case VFWCVT_FXUV: //ui8 -> f16
//      output.result = ui32_to_f16((uint32_t)input.src1).v;  break;
//     case VFWCVT_FXV:  //i8 -> f16 
//       output.result = i32_to_f16((int32_t)(int8_t)input.src1).v;  break; //todo
//     default:
//       printf("VFConvert Unsupported fuOpType %d\n", input.fuOpType);
//       exit(1);
//   }
  
//   output.fflags = softfloat_exceptionFlags & 0x1f;
//   if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}


ElementOutput SGMFloatCvt::calculation_e16(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  float16_t fsrc1 = rtlToF16(input.src1);
  ElementOutput output;
  switch(input.fuOpType) {
    //scalar
    case FCVT_D_H:  //f16->f64
      output.result = my_f16_to_f64(rtlToF16(input.src1)).v;  break;
    case FCVT_S_H:  //f16 -> f32 
      output.result = my_f16_to_f32(rtlToF16(input.src1)).v; break;
    case FCVT_L_H:  //f16->i64
      output.result = f16_to_i64(rtlToF16(input.src1), softfloat_roundingMode, true);  break; 
    case FCVT_LU_H:  //f16->ui64
      output.result = f16_to_ui64(rtlToF16(input.src1), softfloat_roundingMode, true);  break;
    case FCVT_W_H:  //f16->i32
      output.result = f16_to_i32(rtlToF16(input.src1), softfloat_roundingMode, true);  break; 
    case FCVT_WU_H:  //f16->ui32
      output.result = f16_to_ui32(rtlToF16(input.src1), softfloat_roundingMode, true);  break;
    default:
      printf("SFConvert Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }
  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput SGMFloatCvt::calculation_e32(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  ElementOutput output;
  switch(input.fuOpType) {
    //saclar
    case FCVT_H_W:  // i32 ->f16
      output.result = i32_to_f16((uint32_t)input.src1).v;  break;
    case FCVT_H_WU:  // ui32 ->f16
      output.result = ui32_to_f16((uint32_t)input.src1).v;  break;
    case FCVT_S_W:  // i32 ->f32
      output.result = i32_to_f32((uint32_t)input.src1).v;  break;
    case FCVT_S_WU:  // ui32 ->f32
      output.result = ui32_to_f32((uint32_t)input.src1).v;  break;
    case FCVT_D_W:  // i32 ->f64
      output.result = i32_to_f64((uint32_t)input.src1).v;  break;
    case FCVT_D_WU:  // ui32 ->f64
      output.result = ui32_to_f64((uint32_t)input.src1).v;  break;
    case FCVT_H_S:  // f32 ->f16
      output.result = my_f32_to_f16(rtlToF32(input.src1)).v;  break;
    case FCVT_D_S:  //f32 -> f64 
        output.result = my_f32_to_f64(rtlToF32(input.src1)).v; break; 
    case FCVT_W_S: //f32 -> i32
        output.result = f32_to_i32(rtlToF32(input.src1), softfloat_roundingMode, true);  break; 
    case FCVT_WU_S: //f32 -> ui32
        output.result = f32_to_ui32(rtlToF32(input.src1), softfloat_roundingMode, true);  break; 
    case FCVT_L_S: //f32 -> i64
        output.result = f32_to_i64(rtlToF32(input.src1), softfloat_roundingMode, true);  break; 
    case FCVT_LU_S: //f32 -> ui64
        output.result = f32_to_ui64(rtlToF32(input.src1), softfloat_roundingMode, true);  break; 
    default:
      printf("SFConvert Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }
  
  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput SGMFloatCvt::calculation_e64(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  ElementOutput output;
  switch(input.fuOpType) {
    case FCVT_H_D:  //f64 -> f16  
      output.result = f64_to_f16(i2f64((uint64_t)input.src1)).v;  break;
    case FCVT_S_D:  //f64 -> f32  
      output.result = f64_to_f32(i2f64((uint64_t)input.src1)).v;  break;
    case FCVT_H_L:  // i64 ->f16
      output.result = i64_to_f16((uint64_t)input.src1).v;  break;
    case FCVT_H_LU:  // ui64 ->f16
      output.result = ui64_to_f16((uint64_t)input.src1).v;  break;
    case FCVT_S_L:  // i64 ->f32
      output.result = i64_to_f32((uint64_t)input.src1).v;  break;
    case FCVT_S_LU:  // ui64 ->f32
      output.result = ui64_to_f32((uint64_t)input.src1).v;  break;
    case FCVT_D_L:  // i64 ->f64
      output.result = i64_to_f64((uint64_t)input.src1).v;  break;
    case FCVT_D_LU:  // ui64 ->f64
      output.result = ui64_to_f64((uint64_t)input.src1).v;  break;
    case FCVT_L_D: //f64 -> i64
      output.result = f64_to_i64(i2f64((uint64_t)input.src1), softfloat_roundingMode, true);  break;
    case FCVT_LU_D: //f64 -> ui64
      output.result = f64_to_ui64(i2f64((uint64_t)input.src1), softfloat_roundingMode, true);  break;
    case FCVT_W_D: //f64 -> i32
      output.result = f64_to_i32(i2f64((uint64_t)input.src1), softfloat_roundingMode, true);  break;
    case FCVT_WU_D: //f64 -> ui32
      output.result = f64_to_ui32(i2f64((uint64_t)input.src1), softfloat_roundingMode, true);  break;
    default:
      printf("SFConvert Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }
  
  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}