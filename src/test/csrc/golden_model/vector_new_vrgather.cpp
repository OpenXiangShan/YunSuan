#include "../include/gm_common.h"
#include <typeinfo>
#include <stdio.h>
#include <cstring>

VecOutput VGMNewVrgather::get_expected_output(VecInput input)
{
    VecOutputE16 output16;
    VecOutputE8 output8;
    VecInputE8 *input8 = (VecInputE8 *) &input;
    VecInputE16 *input16 = (VecInputE16 *) &input;

    switch(input.sew){
      case 1:
        output16 = get_output_vrg_e16(input16); break;
      case 0:
        output8 = get_output_vrg_e8(input8); break;
      default:
        printf("VGM New Vrgather, bad sew %d\n", input.sew);
        exit(1);
    }
    VecOutput output;
    int width = 8 << input.sew;

    for(int i = 0; i < VLEN/XLEN; i++){
        for(int j = 0; j < XLEN/width; j++){
          if(input.sew == 0){
            output.result[i] += ((uint64_t)(output8.result[8*i+j]) << (8*j));
          }
          else if(input.sew == 1){
            output.result[i] += ((uint64_t)(output16.result[4*i+j]) << (16*j));
            // printf("output16.result[%d] = %d\n", i, (uint16_t)output.result[i]);
          }
        }
        output.fflags[i] = 0;
    }
    return output;
}

VecOutputE16 VGMNewVrgather::get_output_vrg_e16(VecInputE16 *input) {
  VecOutputE16 output;
  int elements = VLEN / 16;
  if(input->fuOpType == VRG_VRG) {
    for(int i = 0; i < elements; i++){
        // printf("src1[%d] = %d\t", i, (uint16_t)input->src1[i]);
        // printf("src2[%d] = %d\t", i, (uint16_t)input->src2[i]);
        uint16_t index = (uint16_t)input->src1[i];        
        if(index >= elements) {
        output.result[i] = 0;
        }
        else {
        output.result[i] = (uint16_t)input->src2[index];
        }
        output.fflags[i] = 0;
        // printf("output.result[%d] = %d\n", i, (uint16_t)output.result[i]);
    }
  }
  else {
    printf("VGM New Vrgather, bad fuOpType %d\n", input->fuOpType);
    exit(1);
  }
  return output;
}


VecOutputE8 VGMNewVrgather::get_output_vrg_e8(VecInputE8 *input) {
  VecOutputE8 output;
  int elements = VLEN / 8;
  if(input->fuOpType == VRG_VRG) {
    for(int i = 0; i < elements; i++){
        // printf("src1[%d] = %d\t", i, (int8_t)input->src1[i]);
        // printf("src2[%d] = %d\t", i, (int8_t)input->src2[i]);
        uint8_t index = (uint8_t)input->src1[i];        
        if(index >= elements) {
        output.result[i] = 0;
        }
        else {
        output.result[i] = (int8_t)input->src2[index];
        }
        output.fflags[i] = 0;
        // printf("output.result[%d] = %d\n", i, (int8_t)output.result[i]);
    }
  }
  else {
    printf("VGM New Vrgather, bad fuOpType %d\n", input->fuOpType);
    exit(1);
  }
  return output;
}

ElementOutput VGMNewVrgather::calculation_e8(ElementInput  input) {ElementOutput rs; return rs;}
ElementOutput VGMNewVrgather::calculation_e16(ElementInput input) {ElementOutput rs; return rs;}
ElementOutput VGMNewVrgather::calculation_e32(ElementInput input) {ElementOutput rs; return rs;}
ElementOutput VGMNewVrgather::calculation_e64(ElementInput input) {ElementOutput rs; return rs;}