#include "../include/gm_common.h"
#include <typeinfo>

#define	GET_BIT(x, bit)	((x & (1 << bit)) >> bit)

VPUGoldenModel::VPUGoldenModel():
  verbose(false)
{}

VecOutput VPUGoldenModel::get_expected_output(VecInput input) {
  int sew = input.sew;
  int number = (128 / 8) >> sew;
  int half_number = number >> 1;
  int result_shift_len = 8 << sew;
  int widenNorrow = (input.fuOpType >> 3) & 0X3;
  int i2f_inputType = (input.fuOpType >> 3) & 0X1;
  int i2f_number = (128 / 8) >> (i2f_inputType+2);
  int i2f_half_number = i2f_number >> 1;
  int i2f_outputType = (input.fuOpType >> 1) & 0X3;
  softfloat_detectTininess = softfloat_tininess_afterRounding;
  uint64_t mask = 0;
  VecOutput output;
  ElementOutput output_part[number];
  if (input.fuType == VFloatCvt){

    if(widenNorrow == 1){ //widen
      half_number = half_number >> 1;
      result_shift_len = result_shift_len << 1;
      for(int i = 0; i < number; i++) {
        ElementInput element = select_element(input, i);
        switch (sew) {
          case 0: output_part[i] = calculation_e8(element); mask = 0xFFFF; break;
          case 1: output_part[i] = calculation_e16(element); mask = 0xFFFFFFFF; break;
          case 2: output_part[i] = calculation_e32(element); mask = 0xFFFFFFFFFFFFFFFF; break;
          default:
            printf("VPU Golden Modle, bad sew %d\n", input.sew);
            exit(1);
        }
        if (output_part[i].fflags > 0x1f) {
          printf("Bad fflags of %x, check golden model e8 %d\n", output_part[i].fflags, i);
          exit(1);
        }
      }
    }else if(widenNorrow == 2){ // norrow
      half_number = half_number >> 1;
      for(int i = 0; i < number/2; i++) {
        ElementInput element = select_element(input, i);
        switch (sew) {
          case 0: output_part[i] = calculation_e16(element); mask = 0xFF; break;
          case 1: output_part[i] = calculation_e32(element); mask = 0xFFFF; break;
          case 2: output_part[i] = calculation_e64(element); mask = 0xFFFFFFFF; break;
          default:
            printf("VPU Golden Modle, bad sew %d\n", input.sew);
            exit(1);
        }
        if (output_part[i].fflags > 0x1f) {
          printf("Bad fflags of %x, check golden model e8 %d\n", output_part[i].fflags, i);
          exit(1);
        }
      }
    }else if(widenNorrow == 0){ // single
      for(int i = 0; i < number; i++) {
        ElementInput element = select_element(input, i);
        switch (sew) {
          case 1: output_part[i] = calculation_e16(element); mask = 0xFFFF; break;
          case 2: output_part[i] = calculation_e32(element); mask = 0xFFFFFFFF; break;
          case 3: output_part[i] = calculation_e64(element); mask = 0xFFFFFFFFFFFFFFFF; break;
          default:
            printf("VPU Golden Modle, bad sew %d\n", input.sew);
            exit(1);
        }
        if (output_part[i].fflags > 0x1f) {
          printf("Bad fflags of %x, check golden model e8 %d\n", output_part[i].fflags, i);
          exit(1);
        }
      }
    }
  }else if(input.fuType == FloatCvtF2X){
    half_number = 1;
    if(widenNorrow == 1){ //widen   
      // half_number = half_number >> 1;
      result_shift_len = result_shift_len << 1;
      for(int i = 0; i < number; i++) {
        ElementInput element = select_element(input, i);
        switch (sew) {
          case 1: output_part[i] = calculation_e16(element); mask = 0xFFFFFFFF; break;        //fp16->fp32/int32/uint32
          case 2: output_part[i] = calculation_e32(element); mask = 0xFFFFFFFFFFFFFFFF; break;//fp32->fp64/int64/uint64
          default:
            printf("VPU Golden Modle, bad sew %d\n", input.sew);
            exit(1);
        }
        if (output_part[i].fflags > 0x1f) {
          printf("Bad fflags of %x, check golden model e8 %d\n", output_part[i].fflags, i);
          exit(1);
        }
      }
    }else if(widenNorrow == 2){//narrow 
      // half_number = half_number >> 1;
      for(int i = 0; i < number/2; i++) {
        ElementInput element = select_element(input, i);
        switch (sew) {
          case 0: output_part[i] = calculation_e16(element); mask = 0xFF; break;
          case 1: output_part[i] = calculation_e32(element); mask = 0xFFFF; break;     //fp32->fp16
          case 2: output_part[i] = calculation_e64(element); mask = 0xFFFFFFFF; break; //fp64->fp32/i32/ui32
          default:
            printf("VPU Golden Modle, bad sew %d\n", input.sew);
            exit(1);
        }
        if (output_part[i].fflags > 0x1f) {
          printf("Bad fflags of %x, check golden model e8 %d\n", output_part[i].fflags, i);
          exit(1);
        }
      }
    }else if(widenNorrow == 3){
      if(sew == 1){//cross high fp16->fp64/int64/uint64
        for(int i = 0; i < number; i++) {
          ElementInput element = select_element(input, i);
          output_part[i] = calculation_e16(element); 
          mask = 0xFFFFFFFFFFFFFFFF;
          if (output_part[i].fflags > 0x1f) {
            printf("Bad fflags of %x, check golden model e8 %d\n", output_part[i].fflags, i);
            exit(1);
          }
        }
      }else if(sew == 3){//corss low  fp64->fp16
        for(int i = 0; i < number; i++) {
          ElementInput element = select_element(input, i);
          output_part[i] = calculation_e64(element); 
          mask = 0xFFFF;
          if (output_part[i].fflags > 0x1f) {
            printf("Bad fflags of %x, check golden model e8 %d\n", output_part[i].fflags, i);
            exit(1);
          }
        }
      }
    }else if(widenNorrow == 0){ // single
      for(int i = 0; i < number; i++) {
        ElementInput element = select_element(input, i);
        switch (sew) {
          case 2: output_part[i] = calculation_e32(element); mask = 0xFFFFFFFF; break;        //fp32->i32/u32
          case 3: output_part[i] = calculation_e64(element); mask = 0xFFFFFFFFFFFFFFFF; break;//fp64->i64/u64
          default:
            printf("VPU Golden Modle, bad sew %d\n", input.sew);
            exit(1);
        }
        if (output_part[i].fflags > 0x1f) {
          printf("Bad fflags of %x, check golden model e8 %d\n", output_part[i].fflags, i);
          exit(1);
        }
      }
    }
  }else if(input.fuType == FloatCvtI2F){
    if(i2f_inputType == 1){// input:i64/ui64
      for(int i = 0; i < i2f_number; i++){
        ElementInput element = select_element(input, i);
        output_part[i] = calculation_e64(element);
        switch (i2f_outputType) {
        case 0: mask = 0xFFFFFFFFFFFF0000; half_number = 1; break; //64->16
        case 1: mask = 0xFFFFFFFF00000000; half_number = 1; break; // 64->32       
        case 2: mask = 0x0000000000000000; half_number = 1; break; // 64->64
        default:
          printf("VPU Golden Modle, bad i2f_outputType %d\n", i2f_outputType);
          exit(1);
        }
        if (output_part[i].fflags > 0x1f) {
          printf("Bad fflags of %x, check golden model e8 %d\n", output_part[i].fflags, i);
          exit(1);
        }
      }
    }else{// input:i32/ui32
      for(int i = 0; i < i2f_number; i++){
        ElementInput element = select_element(input, i);
        output_part[i] = calculation_e32(element);
        switch (i2f_outputType) {
        case 0: mask = 0xFFFFFFFFFFFF0000; half_number = 2; break; // 32->16
        case 1: mask = 0xFFFFFFFF00000000; half_number = 2; break; // 32->32
        case 2: mask = 0x0000000000000000; half_number = 1; break; // 32->64
        default:
          printf("VPU Golden Modle, bad i2f_outputType %d\n", i2f_outputType);
          exit(1);
        }
        if (output_part[i].fflags > 0x1f) {
          printf("Bad fflags of %x, check golden model e8 %d\n", output_part[i].fflags, i);
          exit(1);
        }
      }
    }
  }
  else{
    for(int i = 0; i < number; i++) {
      ElementInput element = select_element(input, i);
      switch (sew) {
        case 0: output_part[i] = calculation_e8(element);  mask = 0xFF; break;
        case 1: output_part[i] = calculation_e16(element); mask = 0xFFFF; break;
        case 2: output_part[i] = calculation_e32(element); mask = 0xFFFFFFFF; break;
        case 3: output_part[i] = calculation_e64(element); mask = 0xFFFFFFFFFFFFFFFF; break;
        default:
          printf("VPU Golden Modle, bad sew %d\n", input.sew);
          exit(1);
      }
      if (output_part[i].fflags > 0x1f) {
        printf("Bad fflags of %x, check golden model e8 %d\n", output_part[i].fflags, i);
        exit(1);
      }
    }
  }
  
  for (int i = 0; i < 2; i++) {
    output.result[i] = 0;
    output.fflags[i] = 0;
    for (int j = 0; j < half_number; j++) {
      if(input.fuType == VIntegerDivider) {
        output.result[i] += (uint64_t)(output_part[i*half_number+j].result&mask) << (j*result_shift_len);
        output.fflags[i] += (uint32_t)output_part[i*half_number+j].fflags << j;
      }else if(input.fuType == VFloatCvt){
        if(widenNorrow == 1){//widen
          output.result[i] += ((uint64_t)output_part[(i<<1)*half_number+j].result&mask) << (j*result_shift_len);
          output.fflags[i] += (uint32_t)output_part[(i<<1)*half_number+j].fflags << (j*5);
        }else {//single or norrow
          output.result[i] += ((uint64_t)output_part[i*half_number+j].result&mask) << (j*result_shift_len);
          output.fflags[i] += (uint32_t)output_part[i*half_number+j].fflags << (j*5);
        }
      }else if(input.fuType == FloatCvtF2X){
        if(widenNorrow == 1){//widen
          if(sew == 1){//fp16->fp32/int32/uint32
            output.result[i] = ((uint64_t)output_part[(i<<2)*half_number+j].result) ;
            output.fflags[i] = (uint32_t)output_part[(i<<2)*half_number+j].fflags ;
            if(output.result[i] == 0x00000000FFFFFFFF)
              output.result[i] = 0xFFFFFFFFFFFFFFFF;
          }else if(sew == 2){//fp32->fp64/int64/uint64
            output.result[i] = ((uint64_t)output_part[(i<<1)*half_number+j].result) ;
            output.fflags[i] = (uint32_t)output_part[(i<<1)*half_number+j].fflags ;
          }
        }else if(widenNorrow == 2){//norrow
          if(sew == 1){//fp32->fp16
            output.result[i] = ((uint64_t)output_part[(i<<1)*half_number+j].result) ;
            output.fflags[i] = (uint32_t)output_part[(i<<1)*half_number+j].fflags ;
          }else if(sew == 2){//fp64->fp32/i32/ui32
            output.result[i] = ((uint64_t)output_part[i*half_number+j].result) ;
            output.fflags[i] = (uint32_t)output_part[i*half_number+j].fflags ;
            if(output.result[i] == 0x00000000FFFFFFFF)
              output.result[i] = 0xFFFFFFFFFFFFFFFF;
          }
        }else if(widenNorrow == 0) {//single 
          if(sew == 2){//fp32->i32/u32
            output.result[i] = ((uint64_t)output_part[(i<<1)*half_number+j].result) ;
            output.fflags[i] = (uint32_t)output_part[(i<<1)*half_number+j].fflags ;
            if(output.result[i] == 0x00000000FFFFFFFF)
              output.result[i] = 0xFFFFFFFFFFFFFFFF;
          }else if(sew == 3){//fp64->i64/u64
            output.result[i] = ((uint64_t)output_part[i*half_number+j].result) ;
            output.fflags[i] = (uint32_t)output_part[i*half_number+j].fflags ;
          }
        }else if(widenNorrow == 3){//cross
          if(sew == 1){//cross high 16->64
            output.result[i] = ((uint64_t)output_part[(i<<2)*half_number+j].result) ;
            output.fflags[i] = (uint32_t)output_part[(i<<2)*half_number+j].fflags ;
          }else if(sew == 3){//cross low fp64->fp16
            output.result[i] = ((uint64_t)output_part[i*half_number+j].result) ;
            output.fflags[i] = (uint32_t)output_part[i*half_number+j].fflags ;
          }
        }
      }else if(input.fuType == FloatCvtI2F){
        if(i2f_inputType == 0 && i2f_outputType == 2){//widen
          output.result[i] = ((uint64_t)output_part[(i<<1)*half_number].result|mask);
          output.fflags[i] = (uint32_t)output_part[(i<<1)*half_number].fflags;
        }else if(i2f_inputType == 1 && i2f_outputType == 0){//cross low 
          output.result[i] = ((uint64_t)output_part[i*half_number].result|mask);
          output.fflags[i] = (uint32_t)output_part[i*half_number].fflags;
       }else {//single or norrow
          output.result[i] = ((uint64_t)output_part[i*half_number].result|mask);
          output.fflags[i] = (uint32_t)output_part[i*half_number].fflags;
        }
      }else {
        output.result[i] += ((uint64_t)output_part[i*half_number+j].result) << (j*result_shift_len);
        output.fflags[i] += (uint32_t)output_part[i*half_number+j].fflags << (j*5);
      }
      if (verbose) {
        printf("%s::%s ResultJoint i:%d j:%d result:%lx fflags:%x\n", typeid(this).name(), __func__,i,j,output.result[i], output.fflags[i]);
      }
    }
    if (output.fflags[i] > 0xfffff) {
      printf("Bad fflags %d: result %lx fflags %x\n", i, output.result[i], output.fflags[i]);
      exit(1);
    }

  }
  return output;
}

ElementInput VPUGoldenModel::select_element(VecInput input, int idx) {
  int sew = input.sew;
  int number = (128 / 8) >> sew;
  if (idx > number) { printf("Bad idx %d > %d at sew %d\n", idx, number, sew); exit(1); }
  int widen_idx = input.uop_idx % 2 == 0 ? idx : number + idx;

  ElementInput element;
  VecInputE8 *input8 = (VecInputE8 *) &input;
  VecInputE16 *input16 = (VecInputE16 *) &input;
  VecInputE32 *input32 = (VecInputE32 *) &input;
  VecInput *input64 = (VecInput *) &input;
  if (input.widen) {
    if(input.fuType == VFloatAdder) {
      switch (sew) {
        case 2:
          element.src1 = (uint64_t)input16->src1[widen_idx];
          element.src2 = input.is_frs1 ? (uint64_t)input64->src2[0] : (input.src_widen ? (uint64_t)input32->src2[idx] : (uint64_t)input16->src2[widen_idx]);
          element.src3 = (uint64_t)input32->src3[idx];
          break;
        case 3:
          element.src1 = (uint64_t)input32->src1[widen_idx];
          element.src2 = input.is_frs1 ? (uint64_t)input64->src2[0] : (input.src_widen ? (uint64_t)input64->src2[idx] : (uint64_t)input32->src2[widen_idx]);
          element.src3 = (uint64_t)input64->src3[idx];
          break;
        default:
          printf("VPU Golden Modle, bad widen sew %d\n", input.sew);
          exit(1);
      }
    }
    else if(input.fuType == VFloatFMA) {
      switch (sew) {
        case 2:
          element.src1 = (uint64_t)input16->src1[widen_idx];
          element.src2 = input.is_frs1 ? (uint64_t)input64->src2[0] : (uint64_t)input16->src2[widen_idx];
          element.src3 = (uint64_t)input32->src3[idx];
          break;
        case 3:
          element.src1 = (uint64_t)input32->src1[widen_idx];
          element.src2 = input.is_frs1 ? (uint64_t)input64->src2[0] : (uint64_t)input32->src2[widen_idx];
          element.src3 = (uint64_t)input64->src3[idx];
          break;
        default:
          printf("VPU Golden Modle, bad widen sew %d\n", input.sew);
          exit(1);
      }
    }
    else{
      printf("VPU Golden Modle, not support widen fuType %d\n", input.fuType);
      exit(1);
    }
  }else if((input.fuType == VFloatCvt) && (((input.fuOpType >>3) & 0X3) == 2) ){  //cvt norrow select 2sew
    switch (sew) {
      case 0:
        element.src1 = input.is_frs2 ? (uint64_t)input64->src1[0] : (uint64_t)input16->src1[idx];
        element.src2 = input.is_frs1 ? (uint64_t)input64->src2[0] : (uint64_t)input16->src2[idx];
        element.src3 = (uint64_t)input16->src3[idx];
        break;
      case 1:
        element.src1 = input.is_frs2 ? (uint64_t)input64->src1[0] : (uint64_t)input32->src1[idx];
        element.src2 = input.is_frs1 ? (uint64_t)input64->src2[0] : (uint64_t)input32->src2[idx];
        element.src3 = (uint64_t)input32->src3[idx];
        break;
      case 2:
        element.src1 = input.is_frs2 ? (uint64_t)input64->src1[0] : (uint64_t)input64->src1[idx];
        element.src2 = input.is_frs1 ? (uint64_t)input64->src2[0] : (uint64_t)input64->src2[idx];
        element.src3 = (uint64_t)input64->src3[idx];
        break;        
      default:
        printf("VPU Golden Modle, bad sew %d\n", input.sew);
        exit(1);
    }
  }else if((input.fuType == FloatCvtF2X) && (((input.fuOpType >>3) & 0X3) == 2) ){  //cvt norrow select 2sew
    switch (sew) {
      case 0:
        element.src1 = input.is_frs2 ? (uint64_t)input64->src1[0] : (uint64_t)input16->src1[idx];
        element.src2 = input.is_frs1 ? (uint64_t)input64->src2[0] : (uint64_t)input16->src2[idx];
        element.src3 = (uint64_t)input16->src3[idx];
        break;
      case 1:
        element.src1 = input.is_frs2 ? (uint64_t)input64->src1[0] : (uint64_t)input32->src1[idx];
        element.src2 = input.is_frs1 ? (uint64_t)input64->src2[0] : (uint64_t)input32->src2[idx];
        element.src3 = (uint64_t)input32->src3[idx];
        break;
      case 2:
        element.src1 = input.is_frs2 ? (uint64_t)input64->src1[0] : (uint64_t)input64->src1[idx];
        element.src2 = input.is_frs1 ? (uint64_t)input64->src2[0] : (uint64_t)input64->src2[idx];
        element.src3 = (uint64_t)input64->src3[idx];
        break;        
      default:
        printf("VPU Golden Modle, bad sew %d\n", input.sew);
        exit(1);
    }
  }else if((input.fuType == FloatCvtF2X) && (((input.fuOpType>>3) & 0x3) == 3) && (sew == 3)){//f64->f16
      element.src1 = input.is_frs2 ? (uint64_t)input64->src1[0] : (uint64_t)input64->src1[idx];
      element.src2 = input.is_frs1 ? (uint64_t)input64->src2[0] : (uint64_t)input64->src2[idx];
      element.src3 = (uint64_t)input64->src3[idx];
  }else if(input.fuType == FloatCvtI2F){
    if(((input.fuOpType>>3) & 0x1) == 1){
      element.src1 = input.is_frs2 ? (uint64_t)input64->src1[0] : (uint64_t)input64->src1[idx];
      element.src2 = input.is_frs1 ? (uint64_t)input64->src2[0] : (uint64_t)input64->src2[idx];
      element.src3 = (uint64_t)input64->src3[idx];
    }else {
      element.src1 = input.is_frs2 ? (uint64_t)input64->src1[0] : (uint64_t)input32->src1[idx];
      element.src2 = input.is_frs1 ? (uint64_t)input64->src2[0] : (uint64_t)input32->src2[idx];
      element.src3 = (uint64_t)input32->src3[idx];
    }
  }
  else {
    switch (sew) {
      case 0:
        element.src1 = (uint64_t)input8->src1[idx];
        element.src2 = (uint64_t)input8->src2[idx];
        element.src3 = (uint64_t)input8->src3[idx];
        break;
      case 1:
        element.src1 = input.is_frs2 ? (uint64_t)input64->src1[0] : (uint64_t)input16->src1[idx];
        element.src2 = input.is_frs1 ? (uint64_t)input64->src2[0] : (uint64_t)input16->src2[idx];
        element.src3 = (uint64_t)input16->src3[idx];
        break;
      case 2:
        element.src1 = input.is_frs2 ? (uint64_t)input64->src1[0] : (uint64_t)input32->src1[idx];
        element.src2 = input.is_frs1 ? (uint64_t)input64->src2[0] : (uint64_t)input32->src2[idx];
        element.src3 = (uint64_t)input32->src3[idx];
        break;
      case 3:
        element.src1 = input.is_frs2 ? (uint64_t)input64->src1[0] : (uint64_t)input64->src1[idx];
        element.src2 = input.is_frs1 ? (uint64_t)input64->src2[0] : (uint64_t)input64->src2[idx];
        element.src3 = (uint64_t)input64->src3[idx];
        break;
      default:
        printf("VPU Golden Modle, bad sew %d\n", input.sew);
        exit(1);
    }
  }
  element.src4 = (uint64_t)GET_BIT(input16->src4[0], idx);
  element.fuOpType = input.fuOpType;
  element.src_widen = input.src_widen;
  element.widen = input.widen;
  element.rm = input.rm;
  element.uop_idx = input.uop_idx;
  return element;
}

void VPUGoldenModel::display_calculation(const char *objType, const char *func, ElementInput input, ElementOutput output) {
  printf("%s::%s src1:%lx src2:%lx result:%lx fflags:%x\n", objType, func, input.src1, input.src2, output.result, output.fflags);
}