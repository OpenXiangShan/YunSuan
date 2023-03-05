#include "../include/gm_common.h"
#include <typeinfo>

VPUGoldenModel::VPUGoldenModel():
  verbose(false)
{}

VecOutput VPUGoldenModel::get_expected_output(VecInput input) {
  int sew = input.sew;
  int number = (128 / 8) >> sew;
  int half_number = number >> 1;
  int result_shift_len = 8 << sew;
  int fflags_shift_len;
  if (sew == 0) { fflags_shift_len = 5; }
  else { fflags_shift_len = 5 << (sew - 1); }

  VecOutput output;
  ElementOutput output_part[number];
  for(int i = 0; i < number; i++) {
    ElementInput element = select_element(input, i);
    switch (sew) {
      case 0: output_part[i] = calculation_e8(element); break;
      case 1: output_part[i] = calculation_e16(element); break;
      case 2: output_part[i] = calculation_e32(element); break;
      case 3: output_part[i] = calculation_e64(element); break;
      default:
        printf("VPU Golden Modle, bad sew %d\n", input.sew);
        exit(1);
    }
    if (output_part[i].fflags != 0) {
      printf("Bad fflags of %x, check golden model e8 %d\n", output_part[i].fflags, i);
      exit(1);
    }
  }
  for (int i = 0; i < 2; i++) {
    output.result[i] = 0;
    output.fflags[i] = 0;
    for (int j = 0; j < half_number; j++) {
      output.result[i] += (uint64_t)output_part[i*half_number+j].result << (j*result_shift_len);
      output.fflags[i] += (uint32_t)output_part[i*half_number+j].fflags << (j*fflags_shift_len);
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

  ElementInput element;
  VecInputE8 *input8 = (VecInputE8 *) &input;
  VecInputE16 *input16 = (VecInputE16 *) &input;
  VecInputE32 *input32 = (VecInputE32 *) &input;
  VecInput *input64 = (VecInput *) &input;
  switch (sew) {
    case 0:
      element.src1 = (uint64_t)input8->src1[idx];
      element.src2 = (uint64_t)input8->src2[idx];
      element.src3 = (uint64_t)input8->src3[idx];
      break;
    case 1:
      element.src1 = (uint64_t)input16->src1[idx];
      element.src2 = (uint64_t)input16->src2[idx];
      element.src3 = (uint64_t)input16->src3[idx];
      break;
    case 2:
      element.src1 = (uint64_t)input32->src1[idx];
      element.src2 = (uint64_t)input32->src2[idx];
      element.src3 = (uint64_t)input32->src3[idx];
      break;
    case 3:
      element.src1 = (uint64_t)input64->src1[idx];
      element.src2 = (uint64_t)input64->src2[idx];
      element.src3 = (uint64_t)input64->src3[idx];
      break;
    default:
      printf("VPU Golden Modle, bad sew %d\n", input.sew);
      exit(1);
  }
  element.fuOpType = input.fuOpType;
  element.src_widen = input.src_widen;
  element.widen = input.widen;
  element.rm = input.rm;
  return element;
}

void VPUGoldenModel::display_calculation(const char *objType, const char *func, ElementInput input, ElementOutput output) {
  printf("%s::%s src1:%lx src2:%lx result:%lx fflags:%x\n", objType, func, input.src1, input.src2, output.result, output.fflags);
}