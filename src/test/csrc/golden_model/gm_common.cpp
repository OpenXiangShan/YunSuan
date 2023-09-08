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
  softfloat_detectTininess = softfloat_tininess_afterRounding;
  uint64_t mask = 0;
  VecOutput output;
  ElementOutput output_part[number];

  if (input.fuType == VFloatCvt){

    if(widenNorrow == 1){ //widen
      half_number = half_number >> 1;
      result_shift_len = result_shift_len << 1;
      // printf("\n==========half_number %d\n", half_number);
      // printf("\n==========result_shift_len %d\n", result_shift_len);
      for(int i = 0; i < number; i++) {
        ElementInput element = select_element(input, i);
        printf("\n==========widen element %lx\n", element.src1);
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
        printf("\n==========norrow element %lx\n", element.src1);
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
        printf("\n==========single element %lx\n", element.src1);
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
  }
  else{
    ElementOutput output_part[number];
    for(int i = 0; i < number; i++) {
      ElementInput element = select_element(input, i);
      switch (sew) {
        case 0: output_part[i] = calculation_e8(element); mask = 0xFF; break;
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
    // printf("\n==========half_number %d\n", half_number);
    // printf("\n==========result_shift_len %d\n", result_shift_len);
    // printf("\n==========mask %lx\n", mask);
    for (int j = 0; j < half_number; j++) {
      if(input.fuType == VIntegerDivider) {
        output.result[i] += (uint64_t)(output_part[i*half_number+j].result&mask) << (j*result_shift_len);
        output.fflags[i] += (uint32_t)output_part[i*half_number+j].fflags << j;
      } else if(input.fuType == VFloatCvt){
        if(widenNorrow == 1){//widen
          output.result[i] += ((uint64_t)output_part[(i<<1)*half_number+j].result&mask) << (j*result_shift_len);
          output.fflags[i] += (uint32_t)output_part[(i<<1)*half_number+j].fflags << (j*5);
        }else {//single or norrow
          output.result[i] += ((uint64_t)output_part[i*half_number+j].result&mask) << (j*result_shift_len);
          output.fflags[i] += (uint32_t)output_part[i*half_number+j].fflags << (j*5);
        }
        // printf("\noutput_part[i*half_number+j].result %lx\n", output_part[i*half_number+j].result);
        // printf("\n==========output.result %lx\n", output.result[i]);
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
  }else if((input.fuType == VFloatCvt) && (((input.fuOpType >>3) & 0X3) == 2)){  //cvt norrow select 2sew
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
  }else {
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
        // printf("\n==========select element.src1 %lx\n", element.src1);
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