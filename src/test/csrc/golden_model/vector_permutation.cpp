#include "../include/gm_common.h"

VecOutput VGMPermutation::get_expected_output(VecInput input) {
  VecOutput output;

  switch(input.fuOpType) {
    case VSLIDEUP: {
      output = get_output_vslideup(input);
      break;
    }
    case VSLIDEDOWN: {
      output = get_output_vslidedown(input);
      break;
    }
    case VSLIDE1UP: {
      output = get_output_vslide1up(input);
      break;
    }
    case VSLIDE1DOWN: {
      output = get_output_vslide1down(input);
      break;
    }
    case VRGATHER: {
      output = get_output_vrgather(input);
      break;
    }
    case VRGATHERRS1: {
      output = get_output_vrgather(input);
      break;
    }
    case VCOMPRESS: {
      output = get_output_vcompress(input);
      break;
    }
    default: {
      printf("VGM Permutation, bad fuOpType %d\n", input.fuOpType);
      exit(1);
    }
  }
  return output;
}

VecOutput VGMPermutation::get_output_vslideup(VecInput input) {
  VecOutput output;

  int sew = input.sew;
  int vlmul = input.vinfo.vlmul;
  int elements_per_reg = (VLEN / 8) >> sew;
  int elements = (vlmul & 0x4) ? (elements_per_reg >> ((~vlmul & 0x7) + 1)) : elements_per_reg;

  if (verbose) {
    printf("%s::%s  vlmul:%d elements_per_reg:%d elements:%d cond:%d\n", typeid(this).name(), __func__, vlmul, elements_per_reg, elements, (vlmul & 0x4));
  }

  int uop_idx = input.uop_idx;
  int mask_start_idx;
  if (uop_idx == 0) mask_start_idx = 0;
  else if ((1 <= uop_idx) && (uop_idx <= 2)) mask_start_idx = 1;
  else if ((3 <= uop_idx) && (uop_idx <= 5)) mask_start_idx = 2;
  else if ((6 <= uop_idx) && (uop_idx <= 9)) mask_start_idx = 3;
  else if ((10 <= uop_idx) && (uop_idx <= 14)) mask_start_idx = 4;
  else if ((15 <= uop_idx) && (uop_idx <= 20)) mask_start_idx = 5;
  else if ((21 <= uop_idx) && (uop_idx <= 27)) mask_start_idx = 6;
  else if ((28 <= uop_idx) && (uop_idx <= 35)) mask_start_idx = 7;
  else { printf("VGM Permutation vslideup, bad uop_idx %d\n", uop_idx); exit(1); }
  mask_start_idx = mask_start_idx * elements_per_reg;

  uint16_t mask_selected = (mask_start_idx >= 64) ? input.src4[1] >> (mask_start_idx - 64) : input.src4[0] >> mask_start_idx;
  
  int slide_base;
  if ( uop_idx == 0 || uop_idx == 2 || uop_idx == 5 || uop_idx == 9 || uop_idx == 14 || uop_idx == 20 || uop_idx == 27 || uop_idx == 35 ) slide_base = 0;
  else if ( uop_idx == 1 || uop_idx == 4 || uop_idx == 8 || uop_idx == 13 || uop_idx == 19 || uop_idx == 26 || uop_idx == 34 ) slide_base = 1;
  else if ( uop_idx == 3 || uop_idx == 7 || uop_idx == 12 || uop_idx == 18 || uop_idx == 25 || uop_idx == 33 ) slide_base = 2;
  else if ( uop_idx == 6 || uop_idx == 11 || uop_idx == 17 || uop_idx == 24 || uop_idx == 32 ) slide_base = 3;
  else if ( uop_idx == 10 || uop_idx == 16 || uop_idx == 23 || uop_idx == 31 ) slide_base = 4;
  else if ( uop_idx == 15 || uop_idx == 22 || uop_idx == 30 ) slide_base = 5;
  else if ( uop_idx == 21 || uop_idx == 29 ) slide_base = 6;
  else if ( uop_idx == 28 ) slide_base = 7;
  else { printf("VGM Permutation vslideup, bad uop_idx %d\n", uop_idx); exit(1); }
  slide_base = slide_base * elements_per_reg;

  VSlideInput vslideup_input;
  vslideup_input.src_data = (uint64_t *)(input.src2);
  vslideup_input.prev_data = (uint64_t *)(input.src3);
  vslideup_input.mask = mask_selected;
  vslideup_input.slide = input.src1[0];
  vslideup_input.mask_start_idx = mask_start_idx;
  vslideup_input.slide_base = slide_base;
  vslideup_input.elements = elements;
  vslideup_input.vinfo = &(input.vinfo);

  switch(input.sew) {
    case 0: {
      VecOutputE8 output_e8 = vslideup_calculation_e8(&vslideup_input);
      output.result[0] = *(uint64_t *)(&output_e8.result[0]);
      output.result[1] = *(uint64_t *)(&output_e8.result[8]);
      break;
    }
    case 1: {
      VecOutputE16 output_e16 = vslideup_calculation_e16(&vslideup_input);
      output.result[0] = *(uint64_t *)(&output_e16.result[0]);
      output.result[1] = *(uint64_t *)(&output_e16.result[4]);
      break;
    }
    case 2: {
      VecOutputE32 output_e32 = vslideup_calculation_e32(&vslideup_input);
      output.result[0] = *(uint64_t *)(&output_e32.result[0]);
      output.result[1] = *(uint64_t *)(&output_e32.result[2]);
      break;
    }
    case 3: {
      VecOutput output_e64 = vslideup_calculation_e64(&vslideup_input);
      output.result[0] = output_e64.result[0];
      output.result[1] = output_e64.result[1];
      break;
    }
    default: {
      printf("VGM Permutation vslideup, bad sew %d\n", input.sew);
      exit(1);
    }
  }

  if (input.vinfo.vstart >= input.vinfo.vl) {
    output.result[0] = input.src3[0];
    output.result[1] = input.src3[1];
  }
  output.fflags[0] = output.fflags[1] = 0;
  output.vxsat = 0;

  if (verbose) {
    printf("%s::%s  src: %016lx_%016lx prev: %016lx_%016lx mask: %x\n", typeid(this).name(), __func__, vslideup_input.src_data[1], vslideup_input.src_data[0], vslideup_input.prev_data[1], vslideup_input.prev_data[0], vslideup_input.mask);
    printf("%s::%s  slide:%ld mask_start_idx:%d slide_base:%d elements:%d\n", typeid(this).name(), __func__, vslideup_input.slide, vslideup_input.mask_start_idx, vslideup_input.slide_base, vslideup_input.elements);
  }

  return output;
}
VecOutputE8 VGMPermutation::vslideup_calculation_e8(VSlideInput *input) {
  VecOutputE8 output;
  uint8_t *src_data = (uint8_t *)(input->src_data);
  uint8_t *prev_data = (uint8_t *)(input->prev_data);
  uint16_t mask = input->mask;
  uint64_t slide = input->slide;
  int mask_start_idx = input->mask_start_idx;
  int slide_base = input->slide_base;
  int elements = input->elements;
  int vstart = input->vinfo->vstart;
  int vl = input->vinfo->vl;
  bool vm = input->vinfo->vm;
  bool ta = input->vinfo->ta;
  bool ma = input->vinfo->ma;

  for (int i = 0; i < elements; i++) {
    int elements_idx = mask_start_idx + i;
    bool mask_i = (mask >> i) & 0x1;
    bool res_keep_old_vd = (!vm && !mask_i && !ma) || (elements_idx < vstart) || ((elements_idx >= vl) && !ta);
    bool res_agnostic = ((elements_idx >= vl) && ta) || (!vm && !mask_i && ma);

    if(res_keep_old_vd)
      output.result[i] = prev_data[i];
    else if(res_agnostic)
      output.result[i] = 0xff;
    else if((0 <= (slide_base - slide + i)) && ((slide_base - slide + i) < elements))
      output.result[i] = src_data[slide_base - slide + i];
    else
      output.result[i] = prev_data[i];
  }

  for (int i = elements; i < 16; i++) {
    output.result[i] = ta ? 0xff : prev_data[i];
  }

  return output;
}
VecOutputE16 VGMPermutation::vslideup_calculation_e16(VSlideInput *input) {
  VecOutputE16 output;
  uint16_t *src_data = (uint16_t *)(input->src_data);
  uint16_t *prev_data = (uint16_t *)(input->prev_data);
  uint16_t mask = input->mask;
  uint64_t slide = input->slide;
  int mask_start_idx = input->mask_start_idx;
  int slide_base = input->slide_base;
  int elements = input->elements;
  int vstart = input->vinfo->vstart;
  int vl = input->vinfo->vl;
  bool vm = input->vinfo->vm;
  bool ta = input->vinfo->ta;
  bool ma = input->vinfo->ma;

  for (int i = 0; i < elements; i++) {
    int elements_idx = mask_start_idx + i;
    bool mask_i = (mask >> i) & 0x1;
    bool res_keep_old_vd = (!vm && !mask_i && !ma) || (elements_idx < vstart) || ((elements_idx >= vl) && !ta);
    bool res_agnostic = ((elements_idx >= vl) && ta) || (!vm && !mask_i && ma);

    if (verbose) {
      printf("%s::%s  src: %016lx_%016lx prev: %016lx_%016lx mask: %x\n", typeid(this).name(), __func__, input->src_data[1], input->src_data[0], input->prev_data[1], input->prev_data[0], mask);
      printf("%s::%s  slide:%ld mask_start_idx:%d slide_base:%d elements:%d\n", typeid(this).name(), __func__, slide, mask_start_idx, slide_base, elements);
    }

    if(res_keep_old_vd)
      output.result[i] = prev_data[i];
    else if(res_agnostic)
      output.result[i] = 0xffff;
    else if((0 <= (slide_base - slide + i)) && ((slide_base - slide + i) < elements))
      output.result[i] = src_data[slide_base - slide + i];
    else
      output.result[i] = prev_data[i];

    if (verbose) {
      printf("%s::%s  elements_idx:%d mask_i:%d elements:%d res_keep_old_vd:%d res_agnostic:%d output.result[i]:%d\n", typeid(this).name(), __func__, elements_idx, mask_i, elements, res_keep_old_vd, res_agnostic, output.result[i]);
    }
  }

  for (int i = elements; i < 8; i++) {
    output.result[i] = ta ? 0xffff : prev_data[i];
  }

  return output;
}
VecOutputE32 VGMPermutation::vslideup_calculation_e32(VSlideInput *input) {
  VecOutputE32 output;
  uint32_t *src_data = (uint32_t *)(input->src_data);
  uint32_t *prev_data = (uint32_t *)(input->prev_data);
  uint16_t mask = input->mask;
  uint64_t slide = input->slide;
  int mask_start_idx = input->mask_start_idx;
  int slide_base = input->slide_base;
  int elements = input->elements;
  int vstart = input->vinfo->vstart;
  int vl = input->vinfo->vl;
  bool vm = input->vinfo->vm;
  bool ta = input->vinfo->ta;
  bool ma = input->vinfo->ma;

  for (int i = 0; i < elements; i++) {
    int elements_idx = mask_start_idx + i;
    bool mask_i = (mask >> i) & 0x1;
    bool res_keep_old_vd = (!vm && !mask_i && !ma) || (elements_idx < vstart) || ((elements_idx >= vl) && !ta);
    bool res_agnostic = ((elements_idx >= vl) && ta) || (!vm && !mask_i && ma);

    if(res_keep_old_vd)
      output.result[i] = prev_data[i];
    else if(res_agnostic)
      output.result[i] = 0xffffffff;
    else if((0 <= (slide_base - slide + i)) && ((slide_base - slide + i) < elements))
      output.result[i] = src_data[slide_base - slide + i];
    else
      output.result[i] = prev_data[i];
  }

  for (int i = elements; i < 4; i++) {
    output.result[i] = ta ? 0xffffffff : prev_data[i];
  }

  return output;
}
VecOutput VGMPermutation::vslideup_calculation_e64(VSlideInput *input) {
  VecOutput output;
  uint64_t *src_data = (uint64_t *)(input->src_data);
  uint64_t *prev_data = (uint64_t *)(input->prev_data);
  uint16_t mask = input->mask;
  uint64_t slide = input->slide;
  int mask_start_idx = input->mask_start_idx;
  int slide_base = input->slide_base;
  int elements = input->elements;
  int vstart = input->vinfo->vstart;
  int vl = input->vinfo->vl;
  bool vm = input->vinfo->vm;
  bool ta = input->vinfo->ta;
  bool ma = input->vinfo->ma;

  for (int i = 0; i < elements; i++) {
    int elements_idx = mask_start_idx + i;
    bool mask_i = (mask >> i) & 0x1;
    bool res_keep_old_vd = (!vm && !mask_i && !ma) || (elements_idx < vstart) || ((elements_idx >= vl) && !ta);
    bool res_agnostic = ((elements_idx >= vl) && ta) || (!vm && !mask_i && ma);

    if(res_keep_old_vd)
      output.result[i] = prev_data[i];
    else if(res_agnostic)
      output.result[i] = 0xffffffffffffffff;
    else if((0 <= (slide_base - slide + i)) && ((slide_base - slide + i) < elements))
      output.result[i] = src_data[slide_base - slide + i];
    else
      output.result[i] = prev_data[i];
  }

  for (int i = elements; i < 2; i++) {
    output.result[i] = ta ? 0xffffffffffffffff : prev_data[i];
  }

  return output;
}


VecOutput VGMPermutation::get_output_vslidedown(VecInput input) {
  VecOutput output;

  int sew = input.sew;
  int vlmul = input.vinfo.vlmul;
  int elements_per_reg = (VLEN / 8) >> sew;
  int elements = (vlmul & 0x4) ? (elements_per_reg >> ((~vlmul & 0x7) + 1)) : elements_per_reg;

  if (verbose) {
    printf("%s::%s  vlmul:%d elements_per_reg:%d elements:%d cond:%d\n", typeid(this).name(), __func__, vlmul, elements_per_reg, elements, (vlmul & 0x4));
  }

  int uop_idx = input.uop_idx;
  int mask_start_idx;
  if (uop_idx == 0 || (vlmul == 1 && uop_idx == 1) || (vlmul == 2 && 1 <= uop_idx && uop_idx <= 3) || (vlmul == 3 && 1 <= uop_idx && uop_idx <= 7))
    mask_start_idx = 0;
  else if ((vlmul == 1 && uop_idx == 2) || (vlmul == 2 && 4 <= uop_idx && uop_idx <= 6) || (vlmul == 3 && 8 <= uop_idx && uop_idx <= 14))
    mask_start_idx = 1;
  else if ((vlmul == 2 && 7 <= uop_idx && uop_idx <= 8) || (vlmul == 3 && 15 <= uop_idx && uop_idx <= 20))
    mask_start_idx = 2;
  else if ((vlmul == 2 && uop_idx == 9) || (vlmul == 3 && 21 <= uop_idx && uop_idx <= 25))
    mask_start_idx = 3;
  else if (26 <= uop_idx && uop_idx <= 29)
    mask_start_idx = 4;
  else if (30 <= uop_idx && uop_idx <= 32)
    mask_start_idx = 5;
  else if (33 <= uop_idx && uop_idx <= 34)
    mask_start_idx = 6;
  else if (uop_idx == 35)
    mask_start_idx = 7;
  else { printf("VGM Permutation vslidedown, bad uop_idx %d\n", uop_idx); exit(1); }
  mask_start_idx = mask_start_idx * elements_per_reg;

  uint16_t mask_selected = (mask_start_idx >= 64) ? input.src4[1] >> (mask_start_idx - 64) : input.src4[0] >> mask_start_idx;
  
  int slide_base;
  switch(vlmul) {
    case 3: {
      if (uop_idx == 0) slide_base = 7;
      else if (uop_idx == 1 || uop_idx == 8) slide_base = 6;
      else if (uop_idx == 2 || uop_idx == 9 || uop_idx == 15) slide_base = 5;
      else if (uop_idx == 3 || uop_idx == 10 || uop_idx == 16 || uop_idx == 21) slide_base = 4;
      else if (uop_idx == 4 || uop_idx == 11 || uop_idx == 17 || uop_idx == 22 || uop_idx == 26) slide_base = 3;
      else if (uop_idx == 5 || uop_idx == 12 || uop_idx == 18 || uop_idx == 23 || uop_idx == 27 || uop_idx == 30) slide_base = 2;
      else if (uop_idx == 6 || uop_idx == 13 || uop_idx == 19 || uop_idx == 24 || uop_idx == 28 || uop_idx == 31 || uop_idx == 33) slide_base = 1;
      else slide_base = 0;
      break;
    }
    case 2: {
      if (uop_idx == 0) slide_base = 3;
      else if (uop_idx == 1 || uop_idx == 4) slide_base = 2;
      else if (uop_idx == 2 || uop_idx == 5 || uop_idx == 7) slide_base = 1;
      else slide_base = 0;
      break;
    }
    case 1: {
      if (uop_idx == 0) slide_base = 1;
      else slide_base = 0;
      break;
    }
    default: slide_base = 0;
  }
  slide_base = slide_base * elements_per_reg;

  bool first_slide = (uop_idx == 0) || \
                     (vlmul == 1 && uop_idx == 2) || \
                     (vlmul == 2 && (uop_idx == 4 || uop_idx == 7 || uop_idx == 9)) || \
                     (vlmul == 3 && (uop_idx == 8 || uop_idx == 15 || uop_idx == 21 || uop_idx == 26 || uop_idx == 30 || uop_idx == 33 || uop_idx == 35));

  VSlideInput vslidedown_input;
  vslidedown_input.src_data = (uint64_t *)(input.src2);
  vslidedown_input.prev_data = (uint64_t *)(input.src3);
  vslidedown_input.mask = mask_selected;
  vslidedown_input.slide = input.src1[0];
  vslidedown_input.mask_start_idx = mask_start_idx;
  vslidedown_input.slide_base = slide_base;
  vslidedown_input.elements = elements;
  vslidedown_input.first_slide = first_slide;
  vslidedown_input.vinfo = &(input.vinfo);

  switch(input.sew) {
    case 0: {
      VecOutputE8 output_e8 = vslidedown_calculation_e8(&vslidedown_input);
      output.result[0] = *(uint64_t *)(&output_e8.result[0]);
      output.result[1] = *(uint64_t *)(&output_e8.result[8]);
      break;
    }
    case 1: {
      VecOutputE16 output_e16 = vslidedown_calculation_e16(&vslidedown_input);
      output.result[0] = *(uint64_t *)(&output_e16.result[0]);
      output.result[1] = *(uint64_t *)(&output_e16.result[4]);
      break;
    }
    case 2: {
      VecOutputE32 output_e32 = vslidedown_calculation_e32(&vslidedown_input);
      output.result[0] = *(uint64_t *)(&output_e32.result[0]);
      output.result[1] = *(uint64_t *)(&output_e32.result[2]);
      break;
    }
    case 3: {
      VecOutput output_e64 = vslidedown_calculation_e64(&vslidedown_input);
      output.result[0] = output_e64.result[0];
      output.result[1] = output_e64.result[1];
      break;
    }
    default: {
      printf("VGM Permutation vslidedown, bad sew %d\n", input.sew);
      exit(1);
    }
  }

  if (input.vinfo.vstart >= input.vinfo.vl) {
    output.result[0] = input.src3[0];
    output.result[1] = input.src3[1];
  }
  output.fflags[0] = output.fflags[1] = 0;
  output.vxsat = 0;

  if (verbose) {
    printf("%s::%s  src: %016lx_%016lx prev: %016lx_%016lx mask: %x\n", typeid(this).name(), __func__, vslidedown_input.src_data[1], vslidedown_input.src_data[0], vslidedown_input.prev_data[1], vslidedown_input.prev_data[0], vslidedown_input.mask);
    printf("%s::%s  slide:%ld mask_start_idx:%d slide_base:%d elements:%d first_slide:%d\n", typeid(this).name(), __func__, vslidedown_input.slide, vslidedown_input.mask_start_idx, vslidedown_input.slide_base, vslidedown_input.elements, vslidedown_input.first_slide);
  }

  return output;
}
VecOutputE8 VGMPermutation::vslidedown_calculation_e8(VSlideInput *input) {
  VecOutputE8 output;
  uint8_t *src_data = (uint8_t *)(input->src_data);
  uint8_t *prev_data = (uint8_t *)(input->prev_data);
  uint16_t mask = input->mask;
  uint64_t slide = input->slide;
  int mask_start_idx = input->mask_start_idx;
  int slide_base = input->slide_base;
  int elements = input->elements;
  bool first_slide = input->first_slide;
  int vstart = input->vinfo->vstart;
  int vl = input->vinfo->vl;
  bool vm = input->vinfo->vm;
  bool ta = input->vinfo->ta;
  bool ma = input->vinfo->ma;

  for (int i = 0; i < elements; i++) {
    int elements_idx = mask_start_idx + i;
    bool mask_i = (mask >> i) & 0x1;
    bool res_keep_old_vd = (!vm && !mask_i && !ma) || (elements_idx < vstart) || ((elements_idx >= vl) && !ta);
    bool res_agnostic = ((elements_idx >= vl) && ta) || (!vm && !mask_i && ma);

    if(res_keep_old_vd)
      output.result[i] = prev_data[i];
    else if(res_agnostic)
      output.result[i] = 0xff;
    else if((0 <= (i + slide - slide_base)) && ((i + slide - slide_base) < elements))
      output.result[i] = src_data[i + slide - slide_base];
    else if(first_slide)
      output.result[i] = 0;
    else
      output.result[i] = prev_data[i];
  }

  for (int i = elements; i < 16; i++) {
    output.result[i] = ta ? 0xff : prev_data[i];
  }

  return output;
}
VecOutputE16 VGMPermutation::vslidedown_calculation_e16(VSlideInput *input) {
  VecOutputE16 output;
  uint16_t *src_data = (uint16_t *)(input->src_data);
  uint16_t *prev_data = (uint16_t *)(input->prev_data);
  uint16_t mask = input->mask;
  uint64_t slide = input->slide;
  int mask_start_idx = input->mask_start_idx;
  int slide_base = input->slide_base;
  int elements = input->elements;
  bool first_slide = input->first_slide;
  int vstart = input->vinfo->vstart;
  int vl = input->vinfo->vl;
  bool vm = input->vinfo->vm;
  bool ta = input->vinfo->ta;
  bool ma = input->vinfo->ma;

  for (int i = 0; i < elements; i++) {
    int elements_idx = mask_start_idx + i;
    bool mask_i = (mask >> i) & 0x1;
    bool res_keep_old_vd = (!vm && !mask_i && !ma) || (elements_idx < vstart) || ((elements_idx >= vl) && !ta);
    bool res_agnostic = ((elements_idx >= vl) && ta) || (!vm && !mask_i && ma);

    if(res_keep_old_vd)
      output.result[i] = prev_data[i];
    else if(res_agnostic)
      output.result[i] = 0xffff;
    else if((0 <= (i + slide - slide_base)) && ((i + slide - slide_base) < elements))
      output.result[i] = src_data[i + slide - slide_base];
    else if(first_slide)
      output.result[i] = 0;
    else
      output.result[i] = prev_data[i];
  }

  for (int i = elements; i < 8; i++) {
    output.result[i] = ta ? 0xffff : prev_data[i];
  }

  return output;
}
VecOutputE32 VGMPermutation::vslidedown_calculation_e32(VSlideInput *input) {
  VecOutputE32 output;
  uint32_t *src_data = (uint32_t *)(input->src_data);
  uint32_t *prev_data = (uint32_t *)(input->prev_data);
  uint16_t mask = input->mask;
  uint64_t slide = input->slide;
  int mask_start_idx = input->mask_start_idx;
  int slide_base = input->slide_base;
  int elements = input->elements;
  bool first_slide = input->first_slide;
  int vstart = input->vinfo->vstart;
  int vl = input->vinfo->vl;
  bool vm = input->vinfo->vm;
  bool ta = input->vinfo->ta;
  bool ma = input->vinfo->ma;

  for (int i = 0; i < elements; i++) {
    int elements_idx = mask_start_idx + i;
    bool mask_i = (mask >> i) & 0x1;
    bool res_keep_old_vd = (!vm && !mask_i && !ma) || (elements_idx < vstart) || ((elements_idx >= vl) && !ta);
    bool res_agnostic = ((elements_idx >= vl) && ta) || (!vm && !mask_i && ma);

    if(res_keep_old_vd)
      output.result[i] = prev_data[i];
    else if(res_agnostic)
      output.result[i] = 0xffffffff;
    else if((0 <= (i + slide - slide_base)) && ((i + slide - slide_base) < elements))
      output.result[i] = src_data[i + slide - slide_base];
    else if(first_slide)
      output.result[i] = 0;
    else
      output.result[i] = prev_data[i];
  }

  for (int i = elements; i < 4; i++) {
    output.result[i] = ta ? 0xffffffff : prev_data[i];
  }

  return output;
}
VecOutput VGMPermutation::vslidedown_calculation_e64(VSlideInput *input) {
  VecOutput output;
  uint64_t *src_data = (uint64_t *)(input->src_data);
  uint64_t *prev_data = (uint64_t *)(input->prev_data);
  uint16_t mask = input->mask;
  uint64_t slide = input->slide;
  int mask_start_idx = input->mask_start_idx;
  int slide_base = input->slide_base;
  int elements = input->elements;
  bool first_slide = input->first_slide;
  int vstart = input->vinfo->vstart;
  int vl = input->vinfo->vl;
  bool vm = input->vinfo->vm;
  bool ta = input->vinfo->ta;
  bool ma = input->vinfo->ma;

  for (int i = 0; i < elements; i++) {
    int elements_idx = mask_start_idx + i;
    bool mask_i = (mask >> i) & 0x1;
    bool res_keep_old_vd = (!vm && !mask_i && !ma) || (elements_idx < vstart) || ((elements_idx >= vl) && !ta);
    bool res_agnostic = ((elements_idx >= vl) && ta) || (!vm && !mask_i && ma);

    if(res_keep_old_vd)
      output.result[i] = prev_data[i];
    else if(res_agnostic)
      output.result[i] = 0xffffffffffffffff;
    else if((0 <= (i + slide - slide_base)) && ((i + slide - slide_base) < elements))
      output.result[i] = src_data[i + slide - slide_base];
    else if(first_slide)
      output.result[i] = 0;
    else
      output.result[i] = prev_data[i];
  }

  for (int i = elements; i < 2; i++) {
    output.result[i] = ta ? 0xffffffffffffffff : prev_data[i];
  }

  return output;
}

VecOutput VGMPermutation::get_output_vslide1up(VecInput input) {
  VecOutput output;

  int sew = input.sew;
  int vlmul = input.vinfo.vlmul;
  int elements_per_reg = (VLEN / 8) >> sew;
  int elements = (vlmul & 0x4) ? (elements_per_reg >> ((~vlmul & 0x7) + 1)) : elements_per_reg;

  if (verbose) {
    printf("%s::%s  vlmul:%d elements_per_reg:%d elements:%d cond:%d\n", typeid(this).name(), __func__, vlmul, elements_per_reg, elements, (vlmul & 0x4));
  }

  int uop_idx = input.uop_idx;
  int mask_start_idx = uop_idx * elements_per_reg;

  uint16_t mask_selected = (mask_start_idx >= 64) ? input.src4[1] >> (mask_start_idx - 64) : input.src4[0] >> mask_start_idx;

  VSlideOneInput vslide1up_input;
  vslide1up_input.src_data_lo = (uint64_t *)(input.src1);
  vslide1up_input.src_data_hi = (uint64_t *)(input.src2);
  vslide1up_input.prev_data = (uint64_t *)(input.src3);
  vslide1up_input.mask = mask_selected;
  vslide1up_input.slide = 1;
  vslide1up_input.mask_start_idx = mask_start_idx;
  vslide1up_input.elements = elements;
  vslide1up_input.vinfo = &(input.vinfo);

  switch(input.sew) {
    case 0: {
      VecOutputE8 output_e8 = vslide1up_calculation_e8(&vslide1up_input);
      output.result[0] = *(uint64_t *)(&output_e8.result[0]);
      output.result[1] = *(uint64_t *)(&output_e8.result[8]);
      break;
    }
    case 1: {
      VecOutputE16 output_e16 = vslide1up_calculation_e16(&vslide1up_input);
      output.result[0] = *(uint64_t *)(&output_e16.result[0]);
      output.result[1] = *(uint64_t *)(&output_e16.result[4]);
      break;
    }
    case 2: {
      VecOutputE32 output_e32 = vslide1up_calculation_e32(&vslide1up_input);
      output.result[0] = *(uint64_t *)(&output_e32.result[0]);
      output.result[1] = *(uint64_t *)(&output_e32.result[2]);
      break;
    }
    case 3: {
      VecOutput output_e64 = vslide1up_calculation_e64(&vslide1up_input);
      output.result[0] = output_e64.result[0];
      output.result[1] = output_e64.result[1];
      break;
    }
    default: {
      printf("VGM Permutation vslide1up, bad sew %d\n", input.sew);
      exit(1);
    }
  }

  if (input.vinfo.vstart >= input.vinfo.vl) {
    output.result[0] = input.src3[0];
    output.result[1] = input.src3[1];
  }
  output.fflags[0] = output.fflags[1] = 0;
  output.vxsat = 0;

  if (verbose) {
    printf("%s::%s  src_lo: %016lx_%016lx src_hi: %016lx_%016lx prev: %016lx_%016lx mask: %x\n", typeid(this).name(), __func__, vslide1up_input.src_data_lo[1], vslide1up_input.src_data_lo[0], vslide1up_input.src_data_hi[1], vslide1up_input.src_data_hi[0], vslide1up_input.prev_data[1], vslide1up_input.prev_data[0], vslide1up_input.mask);
    printf("%s::%s  slide:%d mask_start_idx:%d elements:%d\n", typeid(this).name(), __func__, vslide1up_input.slide, vslide1up_input.mask_start_idx, vslide1up_input.elements);
  }

  return output;
}
VecOutputE8 VGMPermutation::vslide1up_calculation_e8(VSlideOneInput *input) {
  VecOutputE8 output;
  uint8_t *src_data_lo = (uint8_t *)(input->src_data_lo);
  uint8_t *src_data_hi = (uint8_t *)(input->src_data_hi);
  uint8_t *prev_data = (uint8_t *)(input->prev_data);
  uint16_t mask = input->mask;
  int slide = input->slide;
  int mask_start_idx = input->mask_start_idx;
  int elements = input->elements;
  int vstart = input->vinfo->vstart;
  int vl = input->vinfo->vl;
  bool vm = input->vinfo->vm;
  bool ta = input->vinfo->ta;
  bool ma = input->vinfo->ma;

  for (int i = 0; i < elements; i++) {
    int elements_idx = mask_start_idx + i;
    bool mask_i = (mask >> i) & 0x1;
    bool res_keep_old_vd = (!vm && !mask_i && !ma) || (elements_idx < vstart) || ((elements_idx >= vl) && !ta);
    bool res_agnostic = ((elements_idx >= vl) && ta) || (!vm && !mask_i && ma);

    if(res_keep_old_vd)
      output.result[i] = prev_data[i];
    else if(res_agnostic)
      output.result[i] = 0xff;
    else if(0 <= (i - slide))
      output.result[i] = src_data_hi[i - slide];
    else
      output.result[i] = src_data_lo[i + elements - slide];
  }

  for (int i = elements; i < 16; i++) {
    output.result[i] = ta ? 0xff : prev_data[i];
  }

  return output;
}
VecOutputE16 VGMPermutation::vslide1up_calculation_e16(VSlideOneInput *input) {
  VecOutputE16 output;
  uint16_t *src_data_lo = (uint16_t *)(input->src_data_lo);
  uint16_t *src_data_hi = (uint16_t *)(input->src_data_hi);
  uint16_t *prev_data = (uint16_t *)(input->prev_data);
  uint16_t mask = input->mask;
  int slide = input->slide;
  int mask_start_idx = input->mask_start_idx;
  int elements = input->elements;
  int vstart = input->vinfo->vstart;
  int vl = input->vinfo->vl;
  bool vm = input->vinfo->vm;
  bool ta = input->vinfo->ta;
  bool ma = input->vinfo->ma;

  for (int i = 0; i < elements; i++) {
    int elements_idx = mask_start_idx + i;
    bool mask_i = (mask >> i) & 0x1;
    bool res_keep_old_vd = (!vm && !mask_i && !ma) || (elements_idx < vstart) || ((elements_idx >= vl) && !ta);
    bool res_agnostic = ((elements_idx >= vl) && ta) || (!vm && !mask_i && ma);

    if(res_keep_old_vd)
      output.result[i] = prev_data[i];
    else if(res_agnostic)
      output.result[i] = 0xffff;
    else if(0 <= (i - slide))
      output.result[i] = src_data_hi[i - slide];
    else
      output.result[i] = src_data_lo[i + elements - slide];
  }

  for (int i = elements; i < 8; i++) {
    output.result[i] = ta ? 0xffff : prev_data[i];
  }

  return output;
}
VecOutputE32 VGMPermutation::vslide1up_calculation_e32(VSlideOneInput *input) {
  VecOutputE32 output;
  uint32_t *src_data_lo = (uint32_t *)(input->src_data_lo);
  uint32_t *src_data_hi = (uint32_t *)(input->src_data_hi);
  uint32_t *prev_data = (uint32_t *)(input->prev_data);
  uint16_t mask = input->mask;
  int slide = input->slide;
  int mask_start_idx = input->mask_start_idx;
  int elements = input->elements;
  int vstart = input->vinfo->vstart;
  int vl = input->vinfo->vl;
  bool vm = input->vinfo->vm;
  bool ta = input->vinfo->ta;
  bool ma = input->vinfo->ma;

  for (int i = 0; i < elements; i++) {
    int elements_idx = mask_start_idx + i;
    bool mask_i = (mask >> i) & 0x1;
    bool res_keep_old_vd = (!vm && !mask_i && !ma) || (elements_idx < vstart) || ((elements_idx >= vl) && !ta);
    bool res_agnostic = ((elements_idx >= vl) && ta) || (!vm && !mask_i && ma);

    if(res_keep_old_vd)
      output.result[i] = prev_data[i];
    else if(res_agnostic)
      output.result[i] = 0xffffffff;
    else if(0 <= (i - slide))
      output.result[i] = src_data_hi[i - slide];
    else
      output.result[i] = src_data_lo[i + elements - slide];
  }

  for (int i = elements; i < 4; i++) {
    output.result[i] = ta ? 0xffffffff : prev_data[i];
  }

  return output;
}
VecOutput VGMPermutation::vslide1up_calculation_e64(VSlideOneInput *input) {
  VecOutput output;
  uint64_t *src_data_lo = (uint64_t *)(input->src_data_lo);
  uint64_t *src_data_hi = (uint64_t *)(input->src_data_hi);
  uint64_t *prev_data = (uint64_t *)(input->prev_data);
  uint16_t mask = input->mask;
  int slide = input->slide;
  int mask_start_idx = input->mask_start_idx;
  int elements = input->elements;
  int vstart = input->vinfo->vstart;
  int vl = input->vinfo->vl;
  bool vm = input->vinfo->vm;
  bool ta = input->vinfo->ta;
  bool ma = input->vinfo->ma;

  for (int i = 0; i < elements; i++) {
    int elements_idx = mask_start_idx + i;
    bool mask_i = (mask >> i) & 0x1;
    bool res_keep_old_vd = (!vm && !mask_i && !ma) || (elements_idx < vstart) || ((elements_idx >= vl) && !ta);
    bool res_agnostic = ((elements_idx >= vl) && ta) || (!vm && !mask_i && ma);

    if(res_keep_old_vd)
      output.result[i] = prev_data[i];
    else if(res_agnostic)
      output.result[i] = 0xffffffffffffffff;
    else if(0 <= (i - slide))
      output.result[i] = src_data_hi[i - slide];
    else
      output.result[i] = src_data_lo[i + elements - slide];
  }

  for (int i = elements; i < 2; i++) {
    output.result[i] = ta ? 0xffffffffffffffff : prev_data[i];
  }

  return output;
}

VecOutput VGMPermutation::get_output_vslide1down(VecInput input) {
  VecOutput output;

  int sew = input.sew;
  int vlmul = input.vinfo.vlmul;
  int elements_per_reg = (VLEN / 8) >> sew;
  int elements = (vlmul & 0x4) ? (elements_per_reg >> ((~vlmul & 0x7) + 1)) : elements_per_reg;

  if (verbose) {
    printf("%s::%s  vlmul:%d elements_per_reg:%d elements:%d cond:%d\n", typeid(this).name(), __func__, vlmul, elements_per_reg, elements, (vlmul & 0x4));
  }

  int uop_idx = input.uop_idx;
  int mask_start_idx = uop_idx / 2 * elements_per_reg;

  uint16_t mask_selected = (mask_start_idx >= 64) ? input.src4[1] >> (mask_start_idx - 64) : input.src4[0] >> mask_start_idx;
  
  bool ld_without_prev = ((vlmul >= 5 || vlmul == 0) && (uop_idx == 0)) || \
                         ((vlmul == 1) && (uop_idx == 2)) || \
                         ((vlmul == 2) && (uop_idx == 6)) || \
                         (uop_idx == 14);

  bool ld_with_prev = (uop_idx % 2 == 1);

  bool from_vs1 = ((1 <= vlmul && vlmul <= 3) && (uop_idx == 0)) || \
                  ((2 <= vlmul && vlmul <= 3) && (uop_idx == 2 || uop_idx == 4)) || \
                  ((vlmul == 3) && (uop_idx == 6)) || \
                  (uop_idx == 8 || uop_idx == 10 || uop_idx == 12);

  VSlideOneInput vslide1down_input;
  vslide1down_input.src_data_lo = (uint64_t *)(input.src2);
  vslide1down_input.src_data_hi = (uint64_t *)(input.src1);
  vslide1down_input.prev_data = (uint64_t *)(input.src3);
  vslide1down_input.mask = mask_selected;
  vslide1down_input.slide = 1;
  vslide1down_input.mask_start_idx = mask_start_idx;
  vslide1down_input.elements = elements;
  vslide1down_input.ld_without_prev = ld_without_prev;
  vslide1down_input.ld_with_prev = ld_with_prev;
  vslide1down_input.from_vs1 = from_vs1;
  vslide1down_input.vinfo = &(input.vinfo);

  switch(input.sew) {
    case 0: {
      VecOutputE8 output_e8 = vslide1down_calculation_e8(&vslide1down_input);
      output.result[0] = *(uint64_t *)(&output_e8.result[0]);
      output.result[1] = *(uint64_t *)(&output_e8.result[8]);
      break;
    }
    case 1: {
      VecOutputE16 output_e16 = vslide1down_calculation_e16(&vslide1down_input);
      output.result[0] = *(uint64_t *)(&output_e16.result[0]);
      output.result[1] = *(uint64_t *)(&output_e16.result[4]);
      break;
    }
    case 2: {
      VecOutputE32 output_e32 = vslide1down_calculation_e32(&vslide1down_input);
      output.result[0] = *(uint64_t *)(&output_e32.result[0]);
      output.result[1] = *(uint64_t *)(&output_e32.result[2]);
      break;
    }
    case 3: {
      VecOutput output_e64 = vslide1down_calculation_e64(&vslide1down_input);
      output.result[0] = output_e64.result[0];
      output.result[1] = output_e64.result[1];
      break;
    }
    default: {
      printf("VGM Permutation vslide1down, bad sew %d\n", input.sew);
      exit(1);
    }
  }

  if (input.vinfo.vstart >= input.vinfo.vl) {
    output.result[0] = input.src3[0];
    output.result[1] = input.src3[1];
  }
  output.fflags[0] = output.fflags[1] = 0;
  output.vxsat = 0;

  if (verbose) {
    printf("%s::%s  src_lo: %016lx_%016lx src_hi: %016lx_%016lx prev: %016lx_%016lx mask: %x\n", typeid(this).name(), __func__, vslide1down_input.src_data_lo[1], vslide1down_input.src_data_lo[0], vslide1down_input.src_data_hi[1], vslide1down_input.src_data_hi[0], vslide1down_input.prev_data[1], vslide1down_input.prev_data[0], vslide1down_input.mask);
    printf("%s::%s  slide:%d mask_start_idx:%d elements:%d ld_without_prev:%d, ld_with_prev:%d, from_vs1:%d\n", typeid(this).name(), __func__, vslide1down_input.slide, vslide1down_input.mask_start_idx, vslide1down_input.elements, vslide1down_input.ld_without_prev, vslide1down_input.ld_with_prev, vslide1down_input.from_vs1);
  }

  return output;
}
VecOutputE8 VGMPermutation::vslide1down_calculation_e8(VSlideOneInput *input) {
  VecOutputE8 output;
  uint8_t *src_data_lo = (uint8_t *)(input->src_data_lo);
  uint8_t *src_data_hi = (uint8_t *)(input->src_data_hi);
  uint8_t *prev_data = (uint8_t *)(input->prev_data);
  uint16_t mask = input->mask;
  int slide = input->slide;
  int mask_start_idx = input->mask_start_idx;
  int elements = input->elements;
  bool ld_without_prev = input->ld_without_prev;
  bool ld_with_prev = input->ld_with_prev;
  bool from_vs1 = input->from_vs1;
  int vstart = input->vinfo->vstart;
  int vl = input->vinfo->vl;
  bool vm = input->vinfo->vm;
  bool ta = input->vinfo->ta;
  bool ma = input->vinfo->ma;

  for (int i = 0; i < elements; i++) {
    int elements_idx = mask_start_idx + i;
    bool mask_i = (mask >> i) & 0x1;
    bool res_keep_old_vd = (!vm && !mask_i && !ma) || (elements_idx < vstart) || ((elements_idx >= vl) && !ta) || (ld_with_prev && (elements_idx != (vl - 1)));
    bool res_agnostic = ((elements_idx >= vl) && ta) || (!vm && !mask_i && ma);

    if(res_keep_old_vd)
      output.result[i] = prev_data[i];
    else if(res_agnostic)
      output.result[i] = 0xff;
    else if((ld_with_prev || ld_without_prev) && (elements_idx == (vl - 1)))
      output.result[i] = src_data_hi[0];
    else if((i + slide) < elements)
      output.result[i] = src_data_lo[i + slide];
    else if(from_vs1)
      output.result[i] = src_data_hi[0];
    else
      output.result[i] = 0;
  }

  for (int i = elements; i < 16; i++) {
    output.result[i] = ta ? 0xff : prev_data[i];
  }

  return output;
}
VecOutputE16 VGMPermutation::vslide1down_calculation_e16(VSlideOneInput *input) {
  VecOutputE16 output;
  uint16_t *src_data_lo = (uint16_t *)(input->src_data_lo);
  uint16_t *src_data_hi = (uint16_t *)(input->src_data_hi);
  uint16_t *prev_data = (uint16_t *)(input->prev_data);
  uint16_t mask = input->mask;
  int slide = input->slide;
  int mask_start_idx = input->mask_start_idx;
  int elements = input->elements;
  bool ld_without_prev = input->ld_without_prev;
  bool ld_with_prev = input->ld_with_prev;
  bool from_vs1 = input->from_vs1;
  int vstart = input->vinfo->vstart;
  int vl = input->vinfo->vl;
  bool vm = input->vinfo->vm;
  bool ta = input->vinfo->ta;
  bool ma = input->vinfo->ma;

  for (int i = 0; i < elements; i++) {
    int elements_idx = mask_start_idx + i;
    bool mask_i = (mask >> i) & 0x1;
    bool res_keep_old_vd = (!vm && !mask_i && !ma) || (elements_idx < vstart) || ((elements_idx >= vl) && !ta) || (ld_with_prev && (elements_idx != (vl - 1)));
    bool res_agnostic = ((elements_idx >= vl) && ta) || (!vm && !mask_i && ma);

    if(res_keep_old_vd)
      output.result[i] = prev_data[i];
    else if(res_agnostic)
      output.result[i] = 0xffff;
    else if((ld_with_prev || ld_without_prev) && (elements_idx == (vl - 1)))
      output.result[i] = src_data_hi[0];
    else if((i + slide) < elements)
      output.result[i] = src_data_lo[i + slide];
    else if(from_vs1)
      output.result[i] = src_data_hi[0];
    else
      output.result[i] = 0;
  }

  for (int i = elements; i < 8; i++) {
    output.result[i] = ta ? 0xffff : prev_data[i];
  }

  return output;
}
VecOutputE32 VGMPermutation::vslide1down_calculation_e32(VSlideOneInput *input) {
  VecOutputE32 output;
  uint32_t *src_data_lo = (uint32_t *)(input->src_data_lo);
  uint32_t *src_data_hi = (uint32_t *)(input->src_data_hi);
  uint32_t *prev_data = (uint32_t *)(input->prev_data);
  uint16_t mask = input->mask;
  int slide = input->slide;
  int mask_start_idx = input->mask_start_idx;
  int elements = input->elements;
  bool ld_without_prev = input->ld_without_prev;
  bool ld_with_prev = input->ld_with_prev;
  bool from_vs1 = input->from_vs1;
  int vstart = input->vinfo->vstart;
  int vl = input->vinfo->vl;
  bool vm = input->vinfo->vm;
  bool ta = input->vinfo->ta;
  bool ma = input->vinfo->ma;

  for (int i = 0; i < elements; i++) {
    int elements_idx = mask_start_idx + i;
    bool mask_i = (mask >> i) & 0x1;
    bool res_keep_old_vd = (!vm && !mask_i && !ma) || (elements_idx < vstart) || ((elements_idx >= vl) && !ta) || (ld_with_prev && (elements_idx != (vl - 1)));
    bool res_agnostic = ((elements_idx >= vl) && ta) || (!vm && !mask_i && ma);

    if(res_keep_old_vd)
      output.result[i] = prev_data[i];
    else if(res_agnostic)
      output.result[i] = 0xffffffff;
    else if((ld_with_prev || ld_without_prev) && (elements_idx == (vl - 1)))
      output.result[i] = src_data_hi[0];
    else if((i + slide) < elements)
      output.result[i] = src_data_lo[i + slide];
    else if(from_vs1)
      output.result[i] = src_data_hi[0];
    else
      output.result[i] = 0;
  }

  for (int i = elements; i < 4; i++) {
    output.result[i] = ta ? 0xffffffff : prev_data[i];
  }

  return output;
}
VecOutput VGMPermutation::vslide1down_calculation_e64(VSlideOneInput *input) {
  VecOutput output;
  uint64_t *src_data_lo = (uint64_t *)(input->src_data_lo);
  uint64_t *src_data_hi = (uint64_t *)(input->src_data_hi);
  uint64_t *prev_data = (uint64_t *)(input->prev_data);
  uint16_t mask = input->mask;
  int slide = input->slide;
  int mask_start_idx = input->mask_start_idx;
  int elements = input->elements;
  bool ld_without_prev = input->ld_without_prev;
  bool ld_with_prev = input->ld_with_prev;
  bool from_vs1 = input->from_vs1;
  int vstart = input->vinfo->vstart;
  int vl = input->vinfo->vl;
  bool vm = input->vinfo->vm;
  bool ta = input->vinfo->ta;
  bool ma = input->vinfo->ma;

  for (int i = 0; i < elements; i++) {
    int elements_idx = mask_start_idx + i;
    bool mask_i = (mask >> i) & 0x1;
    bool res_keep_old_vd = (!vm && !mask_i && !ma) || (elements_idx < vstart) || ((elements_idx >= vl) && !ta) || (ld_with_prev && (elements_idx != (vl - 1)));
    bool res_agnostic = ((elements_idx >= vl) && ta) || (!vm && !mask_i && ma);

    if(res_keep_old_vd)
      output.result[i] = prev_data[i];
    else if(res_agnostic)
      output.result[i] = 0xffffffffffffffff;
    else if((ld_with_prev || ld_without_prev) && (elements_idx == (vl - 1)))
      output.result[i] = src_data_hi[0];
    else if((i + slide) < elements)
      output.result[i] = src_data_lo[i + slide];
    else if(from_vs1)
      output.result[i] = src_data_hi[0];
    else
      output.result[i] = 0;
  }

  for (int i = elements; i < 2; i++) {
    output.result[i] = ta ? 0xffffffffffffffff : prev_data[i];
  }

  return output;
}

VecOutput VGMPermutation::get_output_vrgather(VecInput input) {
  VecOutput output;

  int sew = input.sew;
  int vlmul = input.vinfo.vlmul;
  int elements_per_reg = (VLEN / 8) >> sew;
  int elements = (vlmul & 0x4) ? (elements_per_reg >> ((~vlmul & 0x7) + 1)) : elements_per_reg;

  if (verbose) {
    printf("%s::%s  vlmul:%d elements_per_reg:%d elements:%d cond:%d\n", typeid(this).name(), __func__, vlmul, elements_per_reg, elements, (vlmul & 0x4));
  }

  int uop_idx = input.uop_idx;
  int mask_start_idx;
  if (uop_idx == 0 || (vlmul == 1 && uop_idx == 1) || (vlmul == 2 && 1 <= uop_idx && uop_idx <= 3) || (vlmul == 3 && 1 <= uop_idx && uop_idx <= 7))
    mask_start_idx = 0;
  else if ((vlmul == 1 && 2 <= uop_idx && uop_idx <= 3) || (vlmul == 2 && 4 <= uop_idx && uop_idx <= 7) || (vlmul == 3 && 8 <= uop_idx && uop_idx <= 15))
    mask_start_idx = 1;
  else if ((vlmul == 2 && 8 <= uop_idx && uop_idx <= 11) || (vlmul == 3 && 16 <= uop_idx && uop_idx <= 23))
    mask_start_idx = 2;
  else if ((vlmul == 2 && 12 <= uop_idx && uop_idx <= 15) || (vlmul == 3 && 24 <= uop_idx && uop_idx <= 31))
    mask_start_idx = 3;
  else if (32 <= uop_idx && uop_idx <= 39)
    mask_start_idx = 4;
  else if (40 <= uop_idx && uop_idx <= 47)
    mask_start_idx = 5;
  else if (48 <= uop_idx && uop_idx <= 55)
    mask_start_idx = 6;
  else if (56 <= uop_idx && uop_idx <= 63)
    mask_start_idx = 7;
  else { printf("VGM Permutation vrgather, bad uop_idx %d\n", uop_idx); exit(1); }
  mask_start_idx = mask_start_idx * elements_per_reg;

  uint16_t mask_selected = (mask_start_idx >= 64) ? input.src4[1] >> (mask_start_idx - 64) : input.src4[0] >> mask_start_idx;

  int table_idx;
  if (uop_idx == 0 || (vlmul == 1 && (uop_idx & 0x1) == 0) || (vlmul == 2 && (uop_idx & 0x3) == 0) || (vlmul == 3 && (uop_idx & 0x7) == 0))
    table_idx = 0;
  else if ((vlmul == 1 && (uop_idx & 0x1) == 1) || (vlmul == 2 && (uop_idx & 0x3) == 1) || (vlmul == 3 && (uop_idx & 0x7) == 1))
    table_idx = 1;
  else if ((vlmul == 2 && (uop_idx & 0x3) == 2) || (vlmul == 3 && (uop_idx & 0x7) == 2))
    table_idx = 2;
  else if ((vlmul == 2 && (uop_idx & 0x3) == 3) || (vlmul == 3 && (uop_idx & 0x7) == 3))
    table_idx = 3;
  else if (vlmul == 3 && (uop_idx & 0x7) == 4)
    table_idx = 4;
  else if (vlmul == 3 && (uop_idx & 0x7) == 5)
    table_idx = 5;
  else if (vlmul == 3 && (uop_idx & 0x7) == 6)
    table_idx = 6;
  else if (vlmul == 3 && (uop_idx & 0x7) == 7)
    table_idx = 7;
  else { printf("VGM Permutation vrgather, bad uop_idx %d\n", uop_idx); exit(1); }

  int table_range_min = table_idx * elements_per_reg;
  int table_range_max = (table_idx + 1) * elements_per_reg;

  bool first_gather = (uop_idx == 0) || \
                      (vlmul == 1 && (uop_idx & 0x1) == 0) || \
                      (vlmul == 2 && (uop_idx & 0x3) == 0) || \
                      (vlmul == 3 && (uop_idx & 0x7) == 0);

  VRGatherInput vrgather_input;
  vrgather_input.index_data = (uint64_t *)(input.src1);
  vrgather_input.table_data = (uint64_t *)(input.src2);
  vrgather_input.prev_data = (uint64_t *)(input.src3);
  vrgather_input.mask = mask_selected;
  vrgather_input.index = input.src1[0];
  vrgather_input.mask_start_idx = mask_start_idx;
  vrgather_input.table_range_min = table_range_min;
  vrgather_input.table_range_max = table_range_max;
  vrgather_input.elements = elements;
  vrgather_input.first_gather = first_gather;
  vrgather_input.is_gather_vx = (input.fuOpType == VRGATHERRS1);
  vrgather_input.vinfo = &(input.vinfo);

  switch(input.sew) {
    case 0: {
      VecOutputE8 output_e8 = vrgather_calculation_e8(&vrgather_input);
      output.result[0] = *(uint64_t *)(&output_e8.result[0]);
      output.result[1] = *(uint64_t *)(&output_e8.result[8]);
      break;
    }
    case 1: {
      VecOutputE16 output_e16 = vrgather_calculation_e16(&vrgather_input);
      output.result[0] = *(uint64_t *)(&output_e16.result[0]);
      output.result[1] = *(uint64_t *)(&output_e16.result[4]);
      break;
    }
    case 2: {
      VecOutputE32 output_e32 = vrgather_calculation_e32(&vrgather_input);
      output.result[0] = *(uint64_t *)(&output_e32.result[0]);
      output.result[1] = *(uint64_t *)(&output_e32.result[2]);
      break;
    }
    case 3: {
      VecOutput output_e64 = vrgather_calculation_e64(&vrgather_input);
      output.result[0] = output_e64.result[0];
      output.result[1] = output_e64.result[1];
      break;
    }
    default: {
      printf("VGM Permutation vrgather, bad sew %d\n", input.sew);
      exit(1);
    }
  }

  if (input.vinfo.vstart >= input.vinfo.vl) {
    output.result[0] = input.src3[0];
    output.result[1] = input.src3[1];
  }
  output.fflags[0] = output.fflags[1] = 0;
  output.vxsat = 0;

  if (verbose) {
    printf("%s::%s  index_data: %016lx_%016lx table_data: %016lx_%016lx prev: %016lx_%016lx mask: %x\n", typeid(this).name(), __func__, vrgather_input.index_data[1], vrgather_input.index_data[0], vrgather_input.table_data[1], vrgather_input.table_data[0], vrgather_input.prev_data[1], vrgather_input.prev_data[0], vrgather_input.mask);
    printf("%s::%s  index:%lu mask_start_idx:%d elements:%d table_range_min:%d table_range_max:%d first_gather:%d\n", typeid(this).name(), __func__, vrgather_input.index, vrgather_input.mask_start_idx, vrgather_input.elements, vrgather_input.table_range_min, vrgather_input.table_range_max, vrgather_input.first_gather);
  }

  return output;
}
VecOutputE8 VGMPermutation::vrgather_calculation_e8(VRGatherInput *input) {
  VecOutputE8 output;
  uint8_t *index_data = (uint8_t *)(input->index_data);
  uint8_t *table_data = (uint8_t *)(input->table_data);
  uint8_t *prev_data = (uint8_t *)(input->prev_data);
  uint16_t mask = input->mask;
  int index = input->index;
  int mask_start_idx = input->mask_start_idx;
  int elements = input->elements;
  int table_range_min = input->table_range_min;
  int table_range_max = input->table_range_max;
  bool first_gather = input->first_gather;
  bool is_gather_vx = input->is_gather_vx;
  int vstart = input->vinfo->vstart;
  int vl = input->vinfo->vl;
  bool vm = input->vinfo->vm;
  bool ta = input->vinfo->ta;
  bool ma = input->vinfo->ma;

  for (int i = 0; i < elements; i++) {
    int elements_idx = mask_start_idx + i;
    int index_curr = is_gather_vx ? index : index_data[i];
    bool mask_i = (mask >> i) & 0x1;
    bool res_keep_old_vd = (!vm && !mask_i && !ma) || (elements_idx < vstart) || ((elements_idx >= vl) && !ta);
    bool res_agnostic = ((elements_idx >= vl) && ta) || (!vm && !mask_i && ma);

    if(res_keep_old_vd)
      output.result[i] = prev_data[i];
    else if(res_agnostic)
      output.result[i] = 0xff;
    else if(table_range_min <= index_curr && index_curr < table_range_max)
      output.result[i] = table_data[index_curr - table_range_min];
    else if(first_gather)
      output.result[i] = 0;
    else
      output.result[i] = prev_data[i];
  }

  for (int i = elements; i < 16; i++) {
    output.result[i] = ta ? 0xff : prev_data[i];
  }

  return output;
}
VecOutputE16 VGMPermutation::vrgather_calculation_e16(VRGatherInput *input) {
  VecOutputE16 output;
  uint16_t *index_data = (uint16_t *)(input->index_data);
  uint16_t *table_data = (uint16_t *)(input->table_data);
  uint16_t *prev_data = (uint16_t *)(input->prev_data);
  uint16_t mask = input->mask;
  int index = input->index;
  int mask_start_idx = input->mask_start_idx;
  int elements = input->elements;
  int table_range_min = input->table_range_min;
  int table_range_max = input->table_range_max;
  bool first_gather = input->first_gather;
  bool is_gather_vx = input->is_gather_vx;
  int vstart = input->vinfo->vstart;
  int vl = input->vinfo->vl;
  bool vm = input->vinfo->vm;
  bool ta = input->vinfo->ta;
  bool ma = input->vinfo->ma;

  for (int i = 0; i < elements; i++) {
    int elements_idx = mask_start_idx + i;
    int index_curr = is_gather_vx ? index : index_data[i];
    bool mask_i = (mask >> i) & 0x1;
    bool res_keep_old_vd = (!vm && !mask_i && !ma) || (elements_idx < vstart) || ((elements_idx >= vl) && !ta);
    bool res_agnostic = ((elements_idx >= vl) && ta) || (!vm && !mask_i && ma);

    if(res_keep_old_vd)
      output.result[i] = prev_data[i];
    else if(res_agnostic)
      output.result[i] = 0xffff;
    else if(table_range_min <= index_curr && index_curr < table_range_max)
      output.result[i] = table_data[index_curr - table_range_min];
    else if(first_gather)
      output.result[i] = 0;
    else
      output.result[i] = prev_data[i];
  }

  for (int i = elements; i < 8; i++) {
    output.result[i] = ta ? 0xffff : prev_data[i];
  }

  return output;
}
VecOutputE32 VGMPermutation::vrgather_calculation_e32(VRGatherInput *input) {
  VecOutputE32 output;
  uint32_t *index_data = (uint32_t *)(input->index_data);
  uint32_t *table_data = (uint32_t *)(input->table_data);
  uint32_t *prev_data = (uint32_t *)(input->prev_data);
  uint16_t mask = input->mask;
  int index = input->index;
  int mask_start_idx = input->mask_start_idx;
  int elements = input->elements;
  int table_range_min = input->table_range_min;
  int table_range_max = input->table_range_max;
  bool first_gather = input->first_gather;
  bool is_gather_vx = input->is_gather_vx;
  int vstart = input->vinfo->vstart;
  int vl = input->vinfo->vl;
  bool vm = input->vinfo->vm;
  bool ta = input->vinfo->ta;
  bool ma = input->vinfo->ma;

  for (int i = 0; i < elements; i++) {
    int elements_idx = mask_start_idx + i;
    int index_curr = is_gather_vx ? index : index_data[i];
    bool mask_i = (mask >> i) & 0x1;
    bool res_keep_old_vd = (!vm && !mask_i && !ma) || (elements_idx < vstart) || ((elements_idx >= vl) && !ta);
    bool res_agnostic = ((elements_idx >= vl) && ta) || (!vm && !mask_i && ma);

    if(res_keep_old_vd)
      output.result[i] = prev_data[i];
    else if(res_agnostic)
      output.result[i] = 0xffffffff;
    else if(table_range_min <= index_curr && index_curr < table_range_max)
      output.result[i] = table_data[index_curr - table_range_min];
    else if(first_gather)
      output.result[i] = 0;
    else
      output.result[i] = prev_data[i];
  }

  for (int i = elements; i < 4; i++) {
    output.result[i] = ta ? 0xffffffff : prev_data[i];
  }

  return output;
}
VecOutput VGMPermutation::vrgather_calculation_e64(VRGatherInput *input) {
  VecOutput output;
  uint64_t *index_data = (uint64_t *)(input->index_data);
  uint64_t *table_data = (uint64_t *)(input->table_data);
  uint64_t *prev_data = (uint64_t *)(input->prev_data);
  uint16_t mask = input->mask;
  int index = input->index;
  int mask_start_idx = input->mask_start_idx;
  int elements = input->elements;
  int table_range_min = input->table_range_min;
  int table_range_max = input->table_range_max;
  bool first_gather = input->first_gather;
  bool is_gather_vx = input->is_gather_vx;
  int vstart = input->vinfo->vstart;
  int vl = input->vinfo->vl;
  bool vm = input->vinfo->vm;
  bool ta = input->vinfo->ta;
  bool ma = input->vinfo->ma;

  for (int i = 0; i < elements; i++) {
    int elements_idx = mask_start_idx + i;
    int index_curr = is_gather_vx ? index : index_data[i];
    bool mask_i = (mask >> i) & 0x1;
    bool res_keep_old_vd = (!vm && !mask_i && !ma) || (elements_idx < vstart) || ((elements_idx >= vl) && !ta);
    bool res_agnostic = ((elements_idx >= vl) && ta) || (!vm && !mask_i && ma);

    if(res_keep_old_vd)
      output.result[i] = prev_data[i];
    else if(res_agnostic)
      output.result[i] = 0xffffffffffffffff;
    else if(table_range_min <= index_curr && index_curr < table_range_max)
      output.result[i] = table_data[index_curr - table_range_min];
    else if(first_gather)
      output.result[i] = 0;
    else
      output.result[i] = prev_data[i];
  }

  for (int i = elements; i < 2; i++) {
    output.result[i] = ta ? 0xffffffffffffffff : prev_data[i];
  }

  return output;
}

int VGMPermutation::get_ones_sum_base(int uop_idx, int sew) {
  int elements_per_reg = (VLEN / 8) >> sew;
  int vd_idx;
  if ((uop_idx == 0) || (uop_idx == 2) || (uop_idx == 5) || (uop_idx == 9) || (uop_idx == 14) || (uop_idx == 20) || (uop_idx == 27) || (uop_idx == 35))
    vd_idx = 0;
  else if ((uop_idx == 3) || (uop_idx == 6) || (uop_idx == 10) || (uop_idx == 15) || (uop_idx == 21) || (uop_idx == 28) || (uop_idx == 36))
    vd_idx = 1;
  else if ((uop_idx == 7) || (uop_idx == 11) || (uop_idx == 16) || (uop_idx == 22) || (uop_idx == 29) || (uop_idx == 37))
    vd_idx = 2;
  else if ((uop_idx == 12) || (uop_idx == 17) || (uop_idx == 23) || (uop_idx == 30) || (uop_idx == 38))
    vd_idx = 3;
  else if ((uop_idx == 18) || (uop_idx == 24) || (uop_idx == 31) || (uop_idx == 39))
    vd_idx = 4;
  else if ((uop_idx == 25) || (uop_idx == 32) || (uop_idx == 40))
    vd_idx = 5;
  else if ((uop_idx == 33) || (uop_idx == 41))
    vd_idx = 6;
  else if (uop_idx == 42)
    vd_idx = 7;
  else if (uop_idx <= 42)
    vd_idx = -1;
  else { printf("VGM Permutation get_ones_sum_base, bad uop_idx %d\n", uop_idx); exit(1); }

  int ones_sum_base;
  if (vd_idx == -1) ones_sum_base = -1;
  else ones_sum_base = vd_idx * elements_per_reg;
  return ones_sum_base;
}
VecOutput VGMPermutation::get_output_vcompress(VecInput input) {
  VecOutput output;

  int sew = input.sew;
  int vlmul = input.vinfo.vlmul;
  int elements_per_reg = (VLEN / 8) >> sew;
  int elements = (vlmul & 0x4) ? (elements_per_reg >> ((~vlmul & 0x7) + 1)) : elements_per_reg;

  if (verbose) {
    printf("%s::%s  vlmul:%d elements_per_reg:%d elements:%d cond:%d\n", typeid(this).name(), __func__, vlmul, elements_per_reg, elements, (vlmul & 0x4));
  }

  int uop_idx = input.uop_idx;
  int vs1_idx;
  if (uop_idx == 0 || uop_idx == 1)
    vs1_idx = 0;
  else if ((2 <= uop_idx) && (uop_idx <= 4))
    vs1_idx = 1;
  else if ((5 <= uop_idx) && (uop_idx <= 8))
    vs1_idx = 2;
  else if ((9 <= uop_idx) && (uop_idx <= 13))
    vs1_idx = 3;
  else if ((14 <= uop_idx) && (uop_idx <= 19))
    vs1_idx = 4;
  else if ((20 <= uop_idx) && (uop_idx <= 26))
    vs1_idx = 5;
  else if ((27 <= uop_idx) && (uop_idx <= 34))
    vs1_idx = 6;
  else if ((35 <= uop_idx) && (uop_idx <= 42))
    vs1_idx = 7;
  else { printf("VGM Permutation vcompress, bad uop_idx %d\n", uop_idx); exit(1); }
  int mask_start_idx = vs1_idx * elements_per_reg;

  uint16_t mask_selected = (mask_start_idx >= 64) ? input.src1[1] >> (mask_start_idx - 64) : input.src1[0] >> mask_start_idx;
  if (sew == 3)
    mask_selected = mask_selected & 0x3;
  else if (sew == 2)
    mask_selected = mask_selected & 0xf;
  else if (sew == 1)
    mask_selected = mask_selected & 0xff;

  int vd_idx;
  if ((uop_idx == 0) || (uop_idx == 2) || (uop_idx == 5) || (uop_idx == 9) || (uop_idx == 14) || (uop_idx == 20) || (uop_idx == 27) || (uop_idx == 35))
    vd_idx = 0;
  else if ((uop_idx == 3) || (uop_idx == 6) || (uop_idx == 10) || (uop_idx == 15) || (uop_idx == 21) || (uop_idx == 28) || (uop_idx == 36))
    vd_idx = 1;
  else if ((uop_idx == 7) || (uop_idx == 11) || (uop_idx == 16) || (uop_idx == 22) || (uop_idx == 29) || (uop_idx == 37))
    vd_idx = 2;
  else if ((uop_idx == 12) || (uop_idx == 17) || (uop_idx == 23) || (uop_idx == 30) || (uop_idx == 38))
    vd_idx = 3;
  else if ((uop_idx == 18) || (uop_idx == 24) || (uop_idx == 31) || (uop_idx == 39))
    vd_idx = 4;
  else if ((uop_idx == 25) || (uop_idx == 32) || (uop_idx == 40))
    vd_idx = 5;
  else if ((uop_idx == 33) || (uop_idx == 41))
    vd_idx = 6;
  else if (uop_idx == 42)
    vd_idx = 7;
  else if (uop_idx <= 42)
    vd_idx = 0;
  else { printf("VGM Permutation vcompress, bad uop_idx %d\n", uop_idx); exit(1); }

  int ones_sum_base = vd_idx * elements_per_reg;

  bool output_ones_sum_res = (uop_idx == 1) || (uop_idx == 4) || (uop_idx == 8) || (uop_idx == 13) || (uop_idx == 19) || (uop_idx == 26) || (uop_idx == 34);

  VCompressInput vcompress_input;
  vcompress_input.src_data = (uint64_t *)(input.src2);
  vcompress_input.prev_data = (uint64_t *)(input.src3);
  vcompress_input.mask = mask_selected;
  vcompress_input.elements = elements;
  vcompress_input.os_base = ones_sum_base;
  vcompress_input.pmos = input.src4[0] & 0xff;
  vcompress_input.vinfo = &(input.vinfo);

  int ones_sum_res;
  switch(input.sew) {
    case 0: {
      VecOutputE8 output_e8 = vcompress_calculation_e8(&vcompress_input);
      output.result[0] = *(uint64_t *)(&output_e8.result[0]);
      output.result[1] = *(uint64_t *)(&output_e8.result[8]);
      ones_sum_res = output_e8.vxsat[0];
      break;
    }
    case 1: {
      VecOutputE16 output_e16 = vcompress_calculation_e16(&vcompress_input);
      output.result[0] = *(uint64_t *)(&output_e16.result[0]);
      output.result[1] = *(uint64_t *)(&output_e16.result[4]);
      ones_sum_res = output_e16.vxsat[0];
      break;
    }
    case 2: {
      VecOutputE32 output_e32 = vcompress_calculation_e32(&vcompress_input);
      output.result[0] = *(uint64_t *)(&output_e32.result[0]);
      output.result[1] = *(uint64_t *)(&output_e32.result[2]);
      ones_sum_res = output_e32.vxsat[0];
      break;
    }
    case 3: {
      VecOutput output_e64 = vcompress_calculation_e64(&vcompress_input);
      output.result[0] = output_e64.result[0];
      output.result[1] = output_e64.result[1];
      ones_sum_res = output_e64.vxsat;
      break;
    }
    default: {
      printf("VGM Permutation vcompress, bad sew %d\n", input.sew);
      exit(1);
    }
  }

  if (input.vinfo.vstart >= input.vinfo.vl) {
    output.result[0] = input.src3[0];
    output.result[1] = input.src3[1];
  }
  if (output_ones_sum_res) {
    output.result[0] = ones_sum_res;
    output.result[1] = 0;
  }
  output.fflags[0] = output.fflags[1] = 0;
  output.vxsat = 0;

  if (verbose) {
    printf("%s::%s  src_data: %016lx_%016lx prev: %016lx_%016lx mask: %x\n", typeid(this).name(), __func__, vcompress_input.src_data[1], vcompress_input.src_data[0], vcompress_input.prev_data[1], vcompress_input.prev_data[0], vcompress_input.mask);
    printf("%s::%s  os_base:%d pmos:%d mask_start_idx:%d elements:%d output_ones_sum_res:%d ones_sum_res:%d\n", typeid(this).name(), __func__, vcompress_input.os_base, vcompress_input.pmos, mask_start_idx, vcompress_input.elements, output_ones_sum_res, ones_sum_res);
  }

  return output;
}
VecOutputE8 VGMPermutation::vcompress_calculation_e8(VCompressInput *input) {
  VecOutputE8 output;
  uint8_t *src_data = (uint8_t *)(input->src_data);
  uint8_t *prev_data = (uint8_t *)(input->prev_data);
  uint16_t mask = input->mask;
  int os_base = input->os_base;
  int pmos = input->pmos;
  int elements = input->elements;
  uint8_t vlmul = input->vinfo->vlmul;
  int vl = input->vinfo->vl;
  bool ta = input->vinfo->ta;

  int vlmax = (vlmul > 4) ? elements : (elements << vlmul);
  int vl_valid = (vl <= vlmax) ? vl : vlmax;

  int cmos[16];
  int n = 16;

  for (int i = 0; i < n; i++) {
    int elements_idx = os_base + i;
    int mask_i = (mask >> i) & 0x1;
    bool res_agnostic = (elements_idx >= vl_valid) && ta;

    if(res_agnostic)
      output.result[i] = 0xff;
    else
      output.result[i] = prev_data[i];

    if (i == 0)
      cmos[i] = pmos + mask_i;
    else
      cmos[i] = cmos[i-1] + mask_i;
  }
  // os_base + n >= pmos

  for (int i = 0; i < n; i++) {
    int res_idx = cmos[i] - os_base - 1;
    int mask_i = (mask >> i) & 0x1;
    if (mask_i && (os_base < cmos[i]) && (cmos[i] < (os_base + n+1)) && (cmos[i] <= vl_valid))
      output.result[res_idx] = src_data[i];
  }
  output.vxsat[0] = cmos[n-1];

  return output;
}
VecOutputE16 VGMPermutation::vcompress_calculation_e16(VCompressInput *input) {
  VecOutputE16 output;
  uint16_t *src_data = (uint16_t *)(input->src_data);
  uint16_t *prev_data = (uint16_t *)(input->prev_data);
  uint16_t mask = input->mask;
  int os_base = input->os_base;
  int pmos = input->pmos;
  int elements = input->elements;
  uint8_t vlmul = input->vinfo->vlmul;
  int vl = input->vinfo->vl;
  bool ta = input->vinfo->ta;

  int vlmax = (vlmul > 4) ? elements : (elements << vlmul);
  int vl_valid = (vl <= vlmax) ? vl : vlmax;

  int cmos[8];
  int n = 8;

  for (int i = 0; i < n; i++) {
    int elements_idx = os_base + i;
    int mask_i = (mask >> i) & 0x1;
    bool res_agnostic = (elements_idx >= vl_valid) && ta;

    if(res_agnostic)
      output.result[i] = 0xffff;
    else
      output.result[i] = prev_data[i];

    if (i == 0)
      cmos[i] = pmos + mask_i;
    else
      cmos[i] = cmos[i-1] + mask_i;
  }
  // os_base + n >= pmos

  for (int i = 0; i < n; i++) {
    int res_idx = cmos[i] - os_base - 1;
    int mask_i = (mask >> i) & 0x1;
    if (mask_i && (os_base < cmos[i]) && (cmos[i] < (os_base + n+1)) && (cmos[i] <= vl_valid))
      output.result[res_idx] = src_data[i];
  }
  output.vxsat[0] = cmos[n-1];

  return output;
}
VecOutputE32 VGMPermutation::vcompress_calculation_e32(VCompressInput *input) {
  VecOutputE32 output;
  uint32_t *src_data = (uint32_t *)(input->src_data);
  uint32_t *prev_data = (uint32_t *)(input->prev_data);
  uint16_t mask = input->mask;
  int os_base = input->os_base;
  int pmos = input->pmos;
  int elements = input->elements;
  uint8_t vlmul = input->vinfo->vlmul;
  int vl = input->vinfo->vl;
  bool ta = input->vinfo->ta;

  int vlmax = (vlmul > 4) ? elements : (elements << vlmul);
  int vl_valid = (vl <= vlmax) ? vl : vlmax;

  int cmos[4];
  int n = 4;

  for (int i = 0; i < n; i++) {
    int elements_idx = os_base + i;
    int mask_i = (mask >> i) & 0x1;
    bool res_agnostic = (elements_idx >= vl_valid) && ta;

    if(res_agnostic)
      output.result[i] = 0xffffffff;
    else
      output.result[i] = prev_data[i];

    if (i == 0)
      cmos[i] = pmos + mask_i;
    else
      cmos[i] = cmos[i-1] + mask_i;
  }
  // os_base + n >= pmos

  for (int i = 0; i < n; i++) {
    int res_idx = cmos[i] - os_base - 1;
    int mask_i = (mask >> i) & 0x1;
    if (mask_i && (os_base < cmos[i]) && (cmos[i] < (os_base + n+1)) && (cmos[i] <= vl_valid))
      output.result[res_idx] = src_data[i];
  }
  output.vxsat[0] = cmos[n-1];

  return output;
}
VecOutput VGMPermutation::vcompress_calculation_e64(VCompressInput *input) {
  VecOutput output;
  uint64_t *src_data = (uint64_t *)(input->src_data);
  uint64_t *prev_data = (uint64_t *)(input->prev_data);
  uint16_t mask = input->mask;
  int os_base = input->os_base;
  int pmos = input->pmos;
  int elements = input->elements;
  uint8_t vlmul = input->vinfo->vlmul;
  int vl = input->vinfo->vl;
  bool ta = input->vinfo->ta;

  int vlmax = (vlmul > 4) ? elements : (elements << vlmul);
  int vl_valid = (vl <= vlmax) ? vl : vlmax;

  int cmos[2];
  int n = 2;

  for (int i = 0; i < n; i++) {
    int elements_idx = os_base + i;
    int mask_i = (mask >> i) & 0x1;
    bool res_agnostic = (elements_idx >= vl_valid) && ta;

    if(res_agnostic)
      output.result[i] = 0xffffffffffffffff;
    else
      output.result[i] = prev_data[i];

    if (i == 0)
      cmos[i] = pmos + mask_i;
    else
      cmos[i] = cmos[i-1] + mask_i;
  }
  // os_base + n >= pmos

  for (int i = 0; i < n; i++) {
    int res_idx = cmos[i] - os_base - 1;
    int mask_i = (mask >> i) & 0x1;
    if (mask_i && (os_base < cmos[i]) && (cmos[i] < (os_base + n+1)) && (cmos[i] <= vl_valid))
      output.result[res_idx] = src_data[i];
  }
  output.vxsat = cmos[n-1];

  return output;
}

ElementOutput VGMPermutation::calculation_e8(ElementInput  input) {ElementOutput rs; return rs;}
ElementOutput VGMPermutation::calculation_e16(ElementInput input) {ElementOutput rs; return rs;}
ElementOutput VGMPermutation::calculation_e32(ElementInput input) {ElementOutput rs; return rs;}
ElementOutput VGMPermutation::calculation_e64(ElementInput input) {ElementOutput rs; return rs;}
