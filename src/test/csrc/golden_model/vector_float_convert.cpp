#include "../include/gm_common.h"
#include "../include/vfpu_functions.h"
#include <typeinfo>
#include <stdint.h>


//                               width of output
ElementOutput VGMFloatCvt::calculation_e8(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  ElementOutput output;
  switch(input.fuOpType) {
    // widen 8->16
    case VFWCVT_FXUV: //ui8 -> f16
     output.result = ui32_to_f16((uint32_t)input.src1).v;  break;
    case VFWCVT_FXV:  //i8 -> f16 
      output.result = i32_to_f16((int32_t)(int8_t)input.src1).v;  break; //todo
    default:
      printf("VFConvert Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }
  
  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMFloatCvt::calculation_e16(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  ElementOutput output;
  switch(input.fuOpType) {
    // single 16->16
    case VFCVT_XUFV: // f16->ui16
      output.result = f16_to_ui16(i2f16((uint16_t)input.src1), softfloat_roundingMode, true);  break; 
    case VFCVT_XFV:  // f16->si16      
      output.result = f16_to_i16(i2f16((uint16_t)input.src1), softfloat_roundingMode, true);  break; 
    case VFCVT_FXUV: //ui16-> f16
      output.result = ui32_to_f16((uint32_t)input.src1).v;  break;
    case VFCVT_FXV: //i16 -> f16
      output.result = i32_to_f16((int32_t)(int16_t)input.src1).v;  break;
    case VFCVT_RTZ_XUFV: // f16->Ui16 trun
      output.result = f16_to_ui16(i2f16((uint16_t)input.src1), softfloat_round_minMag, true);  break; 
    case VFCVT_RTZ_XFV:  // f16->Ui16 trun
      output.result = f16_to_i16(i2f16((uint16_t)input.src1), softfloat_round_minMag, true);  break;

    case VFRSQRT7: // f16->f16
      output.result = f16_rsqrte7(i2f16((uint16_t)input.src1)).v; break;
    case VFREC7:  // f16->f16
      output.result = f16_recip7(i2f16((uint16_t)input.src1)).v; break;

    // widen 16->32
    case VFWCVT_XUFV: //f16->ui32 
      output.result = f16_to_ui32(i2f16((uint16_t)input.src1), softfloat_roundingMode, true);  break; 
    case VFWCVT_XFV:  //f16->i32  
      output.result = f16_to_i32(i2f16((uint16_t)input.src1), softfloat_roundingMode, true);  break; 
    case VFWCVT_FXUV: //ui16 -> f32
       output.result = ui32_to_f32((uint32_t)input.src1).v;  break;
    case VFWCVT_FXV:  //i16 -> f32 
      output.result = i32_to_f32((int32_t)(int16_t)input.src1).v;  break;
    case VFWCVT_FFV:  //f16 -> f32 
      output.result = f16_to_f32(i2f16((uint16_t)input.src1)).v;
      break;
    case VFWCVT_RTZ_XUFV: //f16 -> ui32 trun 
      output.result = f16_to_ui32(i2f16((uint16_t)input.src1), softfloat_round_minMag, true);  break;
    case VFWCVT_RTZ_XFV: //f16 -> i32 trun  
      output.result = f16_to_i32(i2f16((uint16_t)input.src1), softfloat_round_minMag, true);  break;
    // norrow 16->8
    case VFNCVT_XUFW: // f16 ->ui8
      output.result = f16_to_ui8(i2f16((uint16_t)input.src1), softfloat_roundingMode, true);  break; 
    case VFNCVT_XFW: // f16 ->i8
      output.result = f16_to_i8(i2f16((uint16_t)input.src1), softfloat_roundingMode, true);  break; 
    case VFNCVT_RTZ_XUFW: //f16 -> ui8 trun
      output.result = f16_to_ui8(i2f16((uint16_t)input.src1), softfloat_round_minMag, true);  break; 
    case VFNCVT_RTZ_XFW:  //f16 -> i8 trun
      output.result = f16_to_i8(i2f16((uint16_t)input.src1), softfloat_round_minMag, true);  break; 
    default:
      printf("VFConvert Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }
  
  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMFloatCvt::calculation_e32(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  ElementOutput output;
  switch(input.fuOpType) {
    //single 32->32
    case VFCVT_XUFV: // f32->ui32
      output.result = f32_to_ui32(i2f32((uint32_t)input.src1), softfloat_roundingMode, true);  break; 
    case VFCVT_XFV:  // f32->i32      
      output.result = f32_to_i32(i2f32((uint32_t)input.src1), softfloat_roundingMode, true);  break; 
    case VFCVT_FXUV: //ui32-> f32
      output.result = ui32_to_f32((uint32_t)input.src1).v;  break;
    case VFCVT_FXV: //i32 -> f32
      output.result = i32_to_f32((uint32_t)input.src1).v;  break;
    case VFCVT_RTZ_XUFV: // f32->Ui32 trun
      output.result = f32_to_ui32(i2f32((uint32_t)input.src1), softfloat_round_minMag, true);  break; 
    case VFCVT_RTZ_XFV:  // f32->Ui32 trun
      output.result = f32_to_i32(i2f32((uint32_t)input.src1), softfloat_round_minMag, true);  break; 

    case VFRSQRT7: // f32->f32
      output.result = f32_rsqrte7(i2f32((uint32_t)input.src1)).v; break;
    case VFREC7:  // f32->f32
      output.result = f32_recip7(i2f32((uint32_t)input.src1)).v; break;

    // widen 32 -> 64
    case VFWCVT_XUFV: //f32->ui64 
      output.result = f32_to_ui64(i2f32((uint32_t)input.src1), softfloat_roundingMode, true);  break; 
    case VFWCVT_XFV:  //f32->i64  
      output.result = f32_to_i64(i2f32((uint32_t)input.src1), softfloat_roundingMode, true);  break; 
    case VFWCVT_FXUV: //ui32 -> f64
      output.result = ui32_to_f64((uint32_t)input.src1).v;  break; 
    case VFWCVT_FXV:  //i32 -> f64 
      output.result = i32_to_f64((uint32_t)input.src1).v;  break; 
    case VFWCVT_FFV:  //f32 -> f64 
      output.result = f32_to_f64(i2f32((uint32_t)input.src1)).v;   
      break; 

    case VFWCVT_RTZ_XUFV: //f32 -> ui64 trun 
      output.result = f32_to_ui64(i2f32((uint32_t)input.src1), softfloat_round_minMag, true);  break;
    case VFWCVT_RTZ_XFV: //f32 -> i64 trun  
      output.result = f32_to_i64(i2f32((uint32_t)input.src1), softfloat_round_minMag, true);  break;
    // norrow 32 -> 16
    case VFNCVT_XUFW: // f32 ->ui16
      output.result = f32_to_ui16(i2f32((uint32_t)input.src1), softfloat_roundingMode, true);  break; 
    case VFNCVT_XFW: // f32 ->i16
      output.result = f32_to_i16(i2f32((uint32_t)input.src1), softfloat_roundingMode, true);  break; 
    case VFNCVT_FXUW: // ui32 ->f16
     output.result = ui32_to_f16((uint32_t)input.src1).v;  break;
    case VFNCVT_FXW:  // i32 ->f16
      output.result = i32_to_f16((uint32_t)input.src1).v;  break;
    case VFNCVT_FFW:  // f32 ->f16
      output.result = f32_to_f16(i2f32((uint32_t)input.src1)).v;  break;
    case VFNCVT_ROD_FFW:// f32 ->f16 rounding towards odd ？？？
      softfloat_roundingMode = softfloat_round_odd;
      output.result = f32_to_f16(i2f32((uint32_t)input.src1)).v;  
      break; 
    case VFNCVT_RTZ_XUFW: //f32 -> ui16 trun
      output.result = f32_to_ui16(i2f32((uint32_t)input.src1), softfloat_round_minMag, true);  break; 
    case VFNCVT_RTZ_XFW:  //f32 -> i16 trun
      output.result = f32_to_i16(i2f32((uint32_t)input.src1), softfloat_round_minMag, true);  break; 
    default:
      printf("VFConvert Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }
  
  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMFloatCvt::calculation_e64(ElementInput input) {
  fp_set_rm(input.rm);
  fp_clear_exception();
  ElementOutput output;
  switch(input.fuOpType) {
    // single 64 -> 64
    case VFCVT_XUFV: // f64->ui64
      output.result = f64_to_ui64(i2f64((uint64_t)input.src1), softfloat_roundingMode, true);  break; 
    case VFCVT_XFV:  // f64->si64      
      output.result = f64_to_i64(i2f64((uint64_t)input.src1), softfloat_roundingMode, true);  break; 
    case VFCVT_FXUV: //ui64-> f64
      output.result = ui64_to_f64((uint64_t)input.src1).v;  break;
    case VFCVT_FXV: //i64 -> f64
      output.result = i64_to_f64((uint64_t)input.src1).v;  
      break;
    case VFCVT_RTZ_XUFV: // f64->Ui64 trun
      output.result = f64_to_ui64(i2f64((uint64_t)input.src1), softfloat_round_minMag, true);  break; 
    case VFCVT_RTZ_XFV:  // f64->Ui64 trun
      output.result = f64_to_i64(i2f64((uint64_t)input.src1), softfloat_round_minMag, true);
      break;


    case VFRSQRT7: // f64->f64
      output.result = f64_rsqrte7(i2f64((uint64_t)input.src1)).v;
      break;
    case VFREC7:  // f64->f64
      output.result = f64_recip7(i2f64((uint64_t)input.src1)).v; 
      break;
    
    // norrow 64->32
    case VFNCVT_XUFW: // f64 ->ui32
      output.result = f64_to_ui32(i2f64((uint64_t)input.src1), softfloat_roundingMode, true);  break;
    case VFNCVT_XFW: // f64 ->i32
      output.result = f64_to_i32(i2f64((uint64_t)input.src1), softfloat_roundingMode, true);  break;
    case VFNCVT_FXUW: // ui64 ->f32
      output.result = ui64_to_f32((uint64_t)input.src1).v;  break;
    case VFNCVT_FXW:  // i64 ->f32
      output.result = i64_to_f32((uint64_t)input.src1).v;  break;
    case VFNCVT_FFW:  // f64 ->f32
      output.result = f64_to_f32(i2f64((uint64_t)input.src1)).v;  break;
    case VFNCVT_ROD_FFW:// f64 ->f32 rounding towards odd ???
      softfloat_roundingMode = softfloat_round_odd;
      output.result = f64_to_f32(i2f64((uint64_t)input.src1)).v;  
      break; 
    case VFNCVT_RTZ_XUFW: //f64 -> ui32 trun
      output.result = f64_to_ui32(i2f64((uint64_t)input.src1), softfloat_round_minMag, true);  break;
    case VFNCVT_RTZ_XFW:  //f64 -> i32 trun
      output.result = f64_to_i32(i2f64((uint64_t)input.src1), softfloat_round_minMag, true);  break;
    default:
      printf("VFConvert Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }
  
  output.fflags = softfloat_exceptionFlags & 0x1f;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

