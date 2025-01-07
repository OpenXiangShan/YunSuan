#include <stdio.h>
#include <cstdlib>
#include <stdint.h>
#include <algorithm>
extern "C" {
#include <softfloat.h>
}
#include "VSimTop.h"
#include "verilated_vcd_c.h"
#include "include/vpu_constant.h"
#include "include/test_driver.h"
#include "include/fp.h"

#define ASSIGN_SRC(dut_ptr, src_in, index, src_index) \
    dut_ptr->io_in_bits_src_##src_in##_##index = input.src##src_index[index];

#define ASSIGN_OUT(out, src, index) \
    dut_output.##out[##index] = dut_ptr->io_out_bits_##out##_##index;

TestDriver::TestDriver():
  issued(false), verbose(false), keepinput(false)
{
  // aviod random value
  set_test_type();
  gen_next_test_case();
}

TestDriver::~TestDriver() {
}

void TestDriver::set_default_value(VSimTop *dut_ptr) {
  dut_ptr->io_in_valid = false;
  dut_ptr->io_out_ready = true;
}
// fix set_test_type to select fuType
// TODO:
void TestDriver::set_test_type() {
  // test_type.pick_fuType = false;
  // test_type.pick_fuOpType = false;
  test_type.pick_fuType = true;
  test_type.pick_fuOpType = false;
  test_type.fuType =  NewVrgather;
  test_type.fuOpType = VRG_VRG;
  printf("Set Test Type Res: fuType:%d fuOpType:%d\n", test_type.fuType, test_type.fuOpType);
}

void TestDriver::gen_next_test_case() {
  issued = false;
  get_random_input();
  if (verbose) { display_ref_input(); }
  get_expected_output();
  if (verbose) { display_ref_output(); }
  // printf("\n-----new sample-----\n");
  // display();
  // printf("--------------------\n");
}

uint8_t TestDriver::gen_random_sew() {
  switch (input.fuType)
  {
    case VIntegerALU: return 0;
    case VIntegerALUV2: return 0;
    case VIntegerDivider : return 0;  
    case VFloatCvt: {
      if(input.fuOpType == VFNCVT_XFW){
        return 0;
      }
      else if(input.fuOpType == VFWCVT_FXV){
        return 0;
      }
      else return 1;
    }
    case NewVrgather: return 1;
    default: return 1;
  }
  return 1;
}

uint8_t TestDriver::gen_random_futype(std::initializer_list<uint8_t> futype_list) {
  return *(futype_list.begin() + (rand() % futype_list.size()));
}

uint8_t TestDriver::gen_random_optype() {
  switch (input.fuType)
  {
    case VFloatAdder: {
      uint8_t vfadd_all_optype[VFA_NUM] = VFA_ALL_OPTYPES;
      return vfadd_all_optype[rand() % VFA_NUM];
      break;
    }
    case VFloatFMA: {
      uint8_t vffma_all_optype[VFF_NUM] = VFF_ALL_OPTYPES;
      return vffma_all_optype[rand() % VFF_NUM];
      break;
      }
    case VFloatDivider: {
      uint8_t vfd_all_optype[VFD_NUM] = VFD_ALL_OPTYPES;
      return vfd_all_optype[rand() % VFD_NUM];
      break;
    }
    case VIntegerALU: {
      //TODO: add other type
      uint8_t via_all_optype[VIALU_NUM] = VIA_ALL_OPTYPES;
      return via_all_optype[rand() % VIALU_NUM];
      break;
    }
    case VPermutation: { //TODO: add other type
      uint8_t vperm_all_optype[VPERM_NUM-1] = VPERM_ALL_OPTYPES;
      return vperm_all_optype[rand() % (VPERM_NUM-1)];
      break;
    }
    case VIntegerALUV2: {
      uint8_t viaf_all_optype[VIAF_NUM] = VIAF_ALL_OPTYPES;
      return viaf_all_optype[rand() % VIAF_NUM];
      break;
    }
    case VIntegerDivider:{
      uint8_t vid_all_optype[VID_NUM] = VID_ALL_OPTYPES;
      return vid_all_optype[rand() % VID_NUM];
      break;
    }
    case VFloatCvt:{
      if (input.sew == 0) {
        uint8_t vfcvt_8_optype[VFCVT_8_NUM] = VFCVT_8_OPTYPES;
        return vfcvt_8_optype[rand() % VFCVT_8_NUM];
        break;
      } else if (input.sew == 1) {
        uint8_t vfcvt_16_optype[VFCVT_16_NUM] = VFCVT_16_OPTYPES;
        return vfcvt_16_optype[rand() % VFCVT_16_NUM];
        break;
      } else if (input.sew == 2) {
        uint8_t vfcvt_32_optype[VFCVT_32_NUM] = VFCVT_32_OPTYPES;
        return vfcvt_32_optype[rand() % VFCVT_32_NUM];
        break;
      } else {
        uint8_t vfcvt_64_optype[VFCVT_64_NUM] = VFCVT_64_OPTYPES;
        return vfcvt_64_optype[rand() % VFCVT_64_NUM];
        break;
      }
    }
    case FloatCvtF2X:{
      if(input.sew == 1){
        uint8_t fcvt_16_optype[FCVT_16_NUM] = FCVT_16_OPTYPES;
        return fcvt_16_optype[rand() % FCVT_16_NUM];
        break;
      }else if(input.sew == 2){
        uint8_t fcvt_32_optype[FCVT_32_NUM] = FCVT_32_OPTYPES;
        return fcvt_32_optype[rand() % FCVT_32_NUM];
        break;
      }else if(input.sew == 3){
        uint8_t fcvt_64_optype[FCVT_64_NUM] = FCVT_64_OPTYPES;
        return fcvt_64_optype[rand() % FCVT_64_NUM];
        break;
      }
    }
    case FloatCvtI2F:{
        uint8_t i2fcvt_64_optype[I2FCVT_64_NUM] = I2FCVT_64_OPTYPES;
        return i2fcvt_64_optype[rand() % I2FCVT_64_NUM];
        break;
    }
    case NewVrgather:{
        uint8_t vrg_64_optype[VRGATHER_NUM] = NEWVRGATHER_OPTYPES;
        return  vrg_64_optype[rand() % VRGATHER_NUM];
        break;   
    }
    default:
      printf("Unsupported FuType %d\n", input.fuType);
      exit(1);
      return 0;
  }
  return 0;
}


bool TestDriver::gen_random_widen() {
  if(input.sew > 1){
    switch (input.fuType)
    {
      case VFloatAdder: {
        if( input.fuOpType == VFADD || input.fuOpType == VFSUB )  return rand()%2 == 1; 
        else return false;
        break;
      }
      case VFloatFMA: {
        if(input.fuOpType==VFMUL || input.fuOpType==VFMACC || input.fuOpType==VFNMACC || input.fuOpType==VFMSAC || input.fuOpType==VFNMSAC) 
          return rand()%2 == 1;
        else return false;
        break;
      }
      default: return false; break;
    }
  }
  else return false;
}

bool TestDriver::gen_random_src_widen() {
  if (input.widen) {
    switch (test_type.fuType)
    {
      case VFloatAdder: return rand()%2 == 1; break;
      default: return false; break;
    }
  }
  else return false;
}

bool TestDriver::gen_random_is_frs1() {
  switch(input.fuType){
    case VFloatAdder: {
      uint8_t need_frs1_ops[] = VFA_NEED_FRS1_OPTYPES;
      uint8_t must_frs1_ops[] = VFA_MUST_FRS1_OPTYPES;
      bool need_frs1 = std::find(std::begin(need_frs1_ops), std::end(need_frs1_ops), input.fuOpType) != std::end(need_frs1_ops);
      bool must_frs1 = std::find(std::begin(must_frs1_ops), std::end(must_frs1_ops), input.fuOpType) != std::end(must_frs1_ops);
      if (must_frs1) {return true; break;}
      else if (need_frs1) {return rand() % 2 == 0; break;}
      else {return false; break;}
    }
    case VFloatFMA: {
      uint8_t need_frs1_ops[] = VFF_NEED_FRS1_OPTYPES;
      bool need_frs1 = std::find(std::begin(need_frs1_ops), std::end(need_frs1_ops), input.fuOpType) != std::end(need_frs1_ops);
      if (need_frs1) {return rand() % 2 == 0; break;}
      else {return false; break;}
    }
    case VFloatDivider: {
      if(input.fuOpType == VFDIV) {return rand() % 2 == 0; break;}
      else {return false; break;}
    }
    default: return false; break;
  }
}

bool TestDriver::gen_random_is_frs2() {
  switch(input.fuType){
    case VFloatDivider: {
      if(input.fuOpType == VFDIV && (!input.is_frs2)) {return rand() % 2 == 0; break;}
      else {return false; break;}
    }
    default: return false; break;
  }
}

void TestDriver::gen_random_vecinfo() {
  //               lmul =  8, 4, 2, 1,  1/2, 1/4, 1/8
  uint8_t vlmul_list[7] = {3, 2, 1, 0,  7,   6,   5};

  //TODO: modified
  input.vinfo.vlmul = 0;
  //sew: 01-> fp16, 10->fp32

  // input.vinfo.vlmul = vlmul_list[rand() % (7 - input.sew)];
  int elements_per_reg = (VLEN / 8) >> input.sew;
  int vlmax = (input.vinfo.vlmul > 4) ? (elements_per_reg >> (8 - input.vinfo.vlmul)) : (elements_per_reg << input.vinfo.vlmul);
  switch (input.fuType) {
    case VPermutation: {
      if (input.fuOpType == VCOMPRESS)
        input.vinfo.vstart = 0;
      else
        // input.vinfo.vstart = rand() % vlmax;
        input.vinfo.vstart = 0;
      break;
    }
    default: input.vinfo.vstart = 0; break;
  } // The vstart of an arithmetic instruction is generally equal to 0
  //TODO: need more test
  input.vinfo.vl = vlmax;
  input.vinfo.vm = 0;
  input.vinfo.ta = 0;
  input.vinfo.ma = 0;

  // input.vinfo.vl = rand() % vlmax + 1; // TODO: vl == 0 may be illegal
  // input.vinfo.vm = rand() % 2;
  // input.vinfo.ta = rand() % 2;
  // input.vinfo.ma = rand() % 2;
}

void TestDriver::gen_random_uopidx() {
  switch(input.fuType) {
    case VPermutation: {
      switch(input.fuOpType) {
        case VSLIDEUP: {
          if (input.vinfo.vlmul == 1) input.uop_idx = rand() % 3;
          else if (input.vinfo.vlmul == 2) input.uop_idx = rand() % 10;
          else if (input.vinfo.vlmul == 3) input.uop_idx = rand() % 36;
          else input.uop_idx = 0;
          break;
        }
        case VSLIDEDOWN: {
          if (input.vinfo.vlmul == 1) input.uop_idx = rand() % 3;
          else if (input.vinfo.vlmul == 2) input.uop_idx = rand() % 10;
          else if (input.vinfo.vlmul == 3) input.uop_idx = rand() % 36;
          else input.uop_idx = 0;
          break;
        }
        case VSLIDE1UP: {
          if (input.vinfo.vlmul == 1) input.uop_idx = rand() % 2;
          else if (input.vinfo.vlmul == 2) input.uop_idx = rand() % 4;
          else if (input.vinfo.vlmul == 3) input.uop_idx = rand() % 8;
          else input.uop_idx = 0;
          break;
        }
        case VSLIDE1DOWN: {
          if (input.vinfo.vlmul == 1) input.uop_idx = rand() % 3;
          else if (input.vinfo.vlmul == 2) input.uop_idx = rand() % 7;
          else if (input.vinfo.vlmul == 3) input.uop_idx = rand() % 15;
          else input.uop_idx = 0;
          break;
        }
        case VRGATHER: {
          if (input.vinfo.vlmul == 1) input.uop_idx = rand() % 4;
          else if (input.vinfo.vlmul == 2) input.uop_idx = rand() % 16;
          else if (input.vinfo.vlmul == 3) input.uop_idx = rand() % 64;
          else input.uop_idx = 0;
          break;
        }
        case VRGATHERRS1: {
          if (input.vinfo.vlmul == 1) input.uop_idx = rand() % 4;
          else if (input.vinfo.vlmul == 2) input.uop_idx = rand() % 16;
          else if (input.vinfo.vlmul == 3) input.uop_idx = rand() % 64;
          else input.uop_idx = 0;
          break;
        }
        case VCOMPRESS: {
          if (input.vinfo.vlmul == 1) input.uop_idx = rand() % 4;
          else if (input.vinfo.vlmul == 2) input.uop_idx = rand() % 13;
          else if (input.vinfo.vlmul == 3) input.uop_idx = rand() % 43;
          else input.uop_idx = 0;
          break;
        }
        default: input.uop_idx = 0;
      }
      break;
    }
    case VFloatAdder: input.uop_idx = input.widen ? rand() % 2 : 0; break;
    default: input.uop_idx = 0;
  }
}

void TestDriver::gen_input_vperm() {
  if(input.fuType == VPermutation) {
    if (input.fuOpType == VSLIDEUP || input.fuOpType == VSLIDEDOWN) {
      input.src1[1] = 0;
      input.src1[0] = input.src1[0] % (VLEN * 2);
    }
    else if (input.fuOpType == VSLIDE1UP) {
      uint64_t temp = (input.sew == 0) ? (input.src1[0] & 0xff) : \
                      (input.sew == 1) ? (input.src1[0] & 0xffff) : \
                      (input.sew == 2) ? (input.src1[0] & 0xffffffff) : input.src1[0];
      uint64_t res = temp;
      for (int i=0; i<(((VLEN / 8) >> (input.sew + 1)) - 1); i++) {
        res = (res << (8 << input.sew)) + temp;
      }
      input.src1[1] = input.src1[0] = res;
    }
    else if (input.fuOpType == VRGATHERRS1) {
      input.src1[1] = 0;
    }
    else if (input.fuOpType == VCOMPRESS) {
      int pmos;
      int elements_per_reg = (VLEN / 8) >> input.sew;
      int os_base = vperm.get_ones_sum_base(input.uop_idx, input.sew);
      if (os_base == -1 && input.uop_idx == 1)
        pmos = 0;
      else if (os_base == -1 && input.uop_idx != 1)
        pmos = rand() % (VLEN - 16 + 1);
      else
        pmos = rand() % (os_base + elements_per_reg);
      input.src4[1] = 0;
      input.src4[0] = pmos & 0xff;
    }
  }
}
void TestDriver::gen_random_idiv_input() {
  int sew_num[4] = {8,16,32,64};
  int lzc_num[4] = {5,12,25,48};
  int slice_num[4] = {8,4,2,1};
  uint64_t mask[4] = {0xFF, 0xFFFF, 0xFFFFFFFF,0xFFFFFFFFFFFFFFFF };
  uint64_t newsrc1[2]={0};
  uint64_t newsrc2[2]={0};
  for (int i = 0; i< slice_num[input.sew]; i++ ){
    int lzc_x_num, lzc_d_num;
    lzc_x_num = rand()%lzc_num[input.sew];
    int random_lzc = rand()%16;
    if (random_lzc < 14) {
      lzc_d_num = rand()%(lzc_num[input.sew] - lzc_x_num) + lzc_x_num;
    } else {
      lzc_d_num = 0;
    }
    uint64_t slice1[2]={0};
    uint64_t slice2[2]={0};
    for (int j = 0; j<2; j++) {
      slice1[j] = input.src1[j] >> (i * sew_num[input.sew]) & mask[input.sew];
      slice2[j] = input.src2[j] >> (i * sew_num[input.sew]) & mask[input.sew];;
      slice1[j] >>= lzc_x_num;
      slice2[j] >>= lzc_d_num;
      newsrc1[j] |= (uint64_t)slice1[j] << (i * sew_num[input.sew]);
      newsrc2[j] |= (uint64_t)slice2[j] << (i * sew_num[input.sew]);
    }
  }
  input.src1[0] = newsrc1[0];
  input.src1[1] = newsrc1[1];
  input.src2[0] = newsrc2[0];
  input.src2[1] = newsrc2[1];
}

void TestDriver::get_random_input() {
  if (keepinput) { return; }

  for(int i = 0; i < VLEN/XLEN; i++){
    input.src1[i] = rand64();
    input.src2[i] = rand64();
    input.src3[i] = rand64();
    input.src4[i] = rand64();
  }

  if (!test_type.pick_fuType) { input.fuType = gen_random_futype(ALL_FUTYPES); }
  else { input.fuType = test_type.fuType; }
  if(input.fuType == VFloatCvt){
    input.sew = gen_random_sew();
    input.is_frs1 = false;
    input.is_frs2 = false;
    input.widen = false;
    if (!test_type.pick_fuOpType) { input.fuOpType = gen_random_optype(); }
    else { input.fuOpType = test_type.fuOpType; }
  }else if(input.fuType == FloatCvtF2X){
    input.sew = gen_random_sew();
    input.is_frs1 = false;
    input.is_frs2 = false;
    input.widen = false;
    if (!test_type.pick_fuOpType) { input.fuOpType = gen_random_optype(); }
    else { input.fuOpType = test_type.fuOpType; }
  }else if(input.fuType == FloatCvtI2F){
    input.sew = gen_random_sew();
    input.is_frs1 = false;
    input.is_frs2 = false;
    input.widen = false;
    if (!test_type.pick_fuOpType) { input.fuOpType = gen_random_optype(); }
    else { input.fuOpType = test_type.fuOpType; }
  }else{
    if (!test_type.pick_fuOpType) { input.fuOpType = gen_random_optype(); }
    else { input.fuOpType = test_type.fuOpType; }
    input.sew = gen_random_sew();
    input.widen = gen_random_widen();
    input.src_widen = gen_random_src_widen();
    input.is_frs1 = false;
    input.is_frs2 = false;
    gen_random_vecinfo();
    gen_random_uopidx();
    gen_input_vperm();
  }

  if(input.fuType == VFloatCvt && input.fuOpType == VFNCVT_ROD_FFW){
    input.rm = 6;
  }else if((input.fuType == VFloatCvt && input.fuOpType == VFCVT_RTZ_XUFV)  ||
           (input.fuType == VFloatCvt && input.fuOpType == VFCVT_RTZ_XFV)   ||
           (input.fuType == VFloatCvt && input.fuOpType == VFWCVT_RTZ_XUFV) ||
           (input.fuType == VFloatCvt && input.fuOpType == VFWCVT_RTZ_XFV)  ||
           (input.fuType == VFloatCvt && input.fuOpType == VFNCVT_RTZ_XUFW) ||
           (input.fuType == VFloatCvt && input.fuOpType == VFNCVT_RTZ_XFW)
  ){
    input.rm = 1;
  }else{
    input.rm = rand() % 5;
  }

  if (input.fuType == VIntegerDivider) {
    gen_random_idiv_input();
  }
  // input.is_frs1 = false;
  // input.sew = 3;
  // input.widen = true;
  // input.src_widen = false;
  // input.uop_idx = 0;
}

void TestDriver::get_expected_output() {
  switch (input.fuType) {
    case VIntegerALU:
      if (verbose) { printf("FuType:%d, choose VIntegerALU %d\n", input.fuType, VIntegerALU); }
      expect_output = valu.get_expected_output(input); return;
    case VFloatAdder:
      if (verbose) { printf("FuType:%d, choose VFloatAdder %d\n", input.fuType, VFloatAdder); }
      expect_output = vfa.get_expected_output(input); return;
    case VFloatFMA:
      if (verbose) { printf("FuType:%d, choose VFloatFMA %d\n", input.fuType, VFloatFMA); }
      expect_output = vff.get_expected_output(input); return;
    case VFloatDivider:
      if (verbose) { printf("FuType:%d, choose VFloatDivider %d\n", input.fuType, VFloatDivider); }
      expect_output = vfd.get_expected_output(input); return;
    case VPermutation:
      if (verbose) { printf("FuType:%d, choose VPermutation %d\n", input.fuType, VPermutation); }
      expect_output = vperm.get_expected_output(input); return;
    case VIntegerALUV2:
      if (verbose) { printf("FuType:%d, choose VIntegerALUV2 %d\n", input.fuType, VIntegerALUV2); }
      expect_output = vialuF.get_expected_output(input); return;
    case VIntegerDivider:
      if (verbose) { printf("FuType:%d, choose VIntegerDivider %d\n", input.fuType, VIntegerDivider); }
      expect_output = vid.get_expected_output(input); return;
    case VFloatCvt:
      if (verbose) { printf("FuType:%d, choose VFloatCvt %d\n", input.fuType, VFloatCvt); }
      expect_output = vcvt.get_expected_output(input); return;     
    case FloatCvtF2X:
      if (verbose) { printf("FuType:%d, choose FloatCvtF2X %d\n", input.fuType, FloatCvtF2X); }
      expect_output = scvt.get_expected_output(input); return;   
    case FloatCvtI2F:
      if (verbose) { printf("FuType:%d, choose FloatCvtI2F %d\n", input.fuType, FloatCvtI2F); }
      expect_output = scvt.get_expected_output(input); return; 
    case NewVrgather:
      if (verbose) { printf("FuType:%d, choose NewVrgather %d\n", input.fuType, NewVrgather); }
      expect_output = vrg.get_expected_output(input); return;
    default:
      printf("Unsupported FuType %d\n", input.fuType);
      exit(1);
      return;
  }
}
uint64_t TestDriver::rand64() {
  uint64_t tmp = rand();
  tmp = (tmp << 32) + (uint32_t) rand();
  return tmp;
}
// dut io check, return fire or not
bool TestDriver::assign_input_raising(VSimTop *dut_ptr) {
  if (!issued) {
    dut_ptr->io_in_valid = true;
    if (dut_ptr->io_in_ready) {
      issued = true;
      stuck_count = 0;
    }
  } else {
    dut_ptr->io_in_valid = false;
  }
  dut_ptr->io_in_bits_src_0_0 = input.src1[0];
  dut_ptr->io_in_bits_src_0_1 = input.src1[1];
  dut_ptr->io_in_bits_src_0_2 = input.src1[2];
  dut_ptr->io_in_bits_src_0_3 = input.src1[3];
  dut_ptr->io_in_bits_src_0_4 = input.src1[4];
  dut_ptr->io_in_bits_src_0_5 = input.src1[5];
  dut_ptr->io_in_bits_src_0_6 = input.src1[6];
  dut_ptr->io_in_bits_src_0_7 = input.src1[7];
  dut_ptr->io_in_bits_src_0_8 = input.src1[8];
  dut_ptr->io_in_bits_src_0_9 = input.src1[9];
  dut_ptr->io_in_bits_src_0_10 = input.src1[10];
  dut_ptr->io_in_bits_src_0_11 = input.src1[11];
  dut_ptr->io_in_bits_src_0_12 = input.src1[12];
  dut_ptr->io_in_bits_src_0_13 = input.src1[13];
  dut_ptr->io_in_bits_src_0_14 = input.src1[14];
  dut_ptr->io_in_bits_src_0_15 = input.src1[15];
  dut_ptr->io_in_bits_src_0_16 = input.src1[16];
  dut_ptr->io_in_bits_src_0_17 = input.src1[17];
  dut_ptr->io_in_bits_src_0_18 = input.src1[18];
  dut_ptr->io_in_bits_src_0_19 = input.src1[19];
  dut_ptr->io_in_bits_src_0_20 = input.src1[20];
  dut_ptr->io_in_bits_src_0_21 = input.src1[21];
  dut_ptr->io_in_bits_src_0_22 = input.src1[22];
  dut_ptr->io_in_bits_src_0_23 = input.src1[23];
  dut_ptr->io_in_bits_src_0_24 = input.src1[24];
  dut_ptr->io_in_bits_src_0_25 = input.src1[25];
  dut_ptr->io_in_bits_src_0_26 = input.src1[26];
  dut_ptr->io_in_bits_src_0_27 = input.src1[27];
  dut_ptr->io_in_bits_src_0_28 = input.src1[28];
  dut_ptr->io_in_bits_src_0_29 = input.src1[29];
  dut_ptr->io_in_bits_src_0_30 = input.src1[30];
  dut_ptr->io_in_bits_src_0_31 = input.src1[31];

  dut_ptr->io_in_bits_src_1_0 = input.src2[0];
  dut_ptr->io_in_bits_src_1_1 = input.src2[1];
  dut_ptr->io_in_bits_src_1_2 = input.src2[2];
  dut_ptr->io_in_bits_src_1_3 = input.src2[3];
  dut_ptr->io_in_bits_src_1_4 = input.src2[4];
  dut_ptr->io_in_bits_src_1_5 = input.src2[5];
  dut_ptr->io_in_bits_src_1_6 = input.src2[6];
  dut_ptr->io_in_bits_src_1_7 = input.src2[7];
  dut_ptr->io_in_bits_src_1_8 = input.src2[8];
  dut_ptr->io_in_bits_src_1_9 = input.src2[9];
  dut_ptr->io_in_bits_src_1_10 = input.src2[10];
  dut_ptr->io_in_bits_src_1_11 = input.src2[11];
  dut_ptr->io_in_bits_src_1_12 = input.src2[12];
  dut_ptr->io_in_bits_src_1_13 = input.src2[13];
  dut_ptr->io_in_bits_src_1_14 = input.src2[14];
  dut_ptr->io_in_bits_src_1_15 = input.src2[15];
  dut_ptr->io_in_bits_src_1_16 = input.src2[16];
  dut_ptr->io_in_bits_src_1_17 = input.src2[17];
  dut_ptr->io_in_bits_src_1_18 = input.src2[18];
  dut_ptr->io_in_bits_src_1_19 = input.src2[19];
  dut_ptr->io_in_bits_src_1_20 = input.src2[20];
  dut_ptr->io_in_bits_src_1_21 = input.src2[21];
  dut_ptr->io_in_bits_src_1_22 = input.src2[22];
  dut_ptr->io_in_bits_src_1_23 = input.src2[23];
  dut_ptr->io_in_bits_src_1_24 = input.src2[24];
  dut_ptr->io_in_bits_src_1_25 = input.src2[25];
  dut_ptr->io_in_bits_src_1_26 = input.src2[26];
  dut_ptr->io_in_bits_src_1_27 = input.src2[27];
  dut_ptr->io_in_bits_src_1_28 = input.src2[28];
  dut_ptr->io_in_bits_src_1_29 = input.src2[29];
  dut_ptr->io_in_bits_src_1_30 = input.src2[30];
  dut_ptr->io_in_bits_src_1_31 = input.src2[31];

  dut_ptr->io_in_bits_src_2_0 = input.src3[0];
  dut_ptr->io_in_bits_src_2_1 = input.src3[1];
  dut_ptr->io_in_bits_src_2_2 = input.src3[2];
  dut_ptr->io_in_bits_src_2_3 = input.src3[3];
  dut_ptr->io_in_bits_src_2_4 = input.src3[4];
  dut_ptr->io_in_bits_src_2_5 = input.src3[5];
  dut_ptr->io_in_bits_src_2_6 = input.src3[6];
  dut_ptr->io_in_bits_src_2_7 = input.src3[7];
  dut_ptr->io_in_bits_src_2_8 = input.src3[8];
  dut_ptr->io_in_bits_src_2_9 = input.src3[9];
  dut_ptr->io_in_bits_src_2_10 = input.src3[10];
  dut_ptr->io_in_bits_src_2_11 = input.src3[11];
  dut_ptr->io_in_bits_src_2_12 = input.src3[12];
  dut_ptr->io_in_bits_src_2_13 = input.src3[13];
  dut_ptr->io_in_bits_src_2_14 = input.src3[14];
  dut_ptr->io_in_bits_src_2_15 = input.src3[15];
  dut_ptr->io_in_bits_src_2_16 = input.src3[16];
  dut_ptr->io_in_bits_src_2_17 = input.src3[17];
  dut_ptr->io_in_bits_src_2_18 = input.src3[18];
  dut_ptr->io_in_bits_src_2_19 = input.src3[19];
  dut_ptr->io_in_bits_src_2_20 = input.src3[20];
  dut_ptr->io_in_bits_src_2_21 = input.src3[21];
  dut_ptr->io_in_bits_src_2_22 = input.src3[22];
  dut_ptr->io_in_bits_src_2_23 = input.src3[23];
  dut_ptr->io_in_bits_src_2_24 = input.src3[24];
  dut_ptr->io_in_bits_src_2_25 = input.src3[25];
  dut_ptr->io_in_bits_src_2_26 = input.src3[26];
  dut_ptr->io_in_bits_src_2_27 = input.src3[27];
  dut_ptr->io_in_bits_src_2_28 = input.src3[28];
  dut_ptr->io_in_bits_src_2_29 = input.src3[29];
  dut_ptr->io_in_bits_src_2_30 = input.src3[30];
  dut_ptr->io_in_bits_src_2_31 = input.src3[31];

  dut_ptr->io_in_bits_src_3_0 = input.src4[0];
  dut_ptr->io_in_bits_src_3_1 = input.src4[1];
  dut_ptr->io_in_bits_src_3_2 = input.src4[2];
  dut_ptr->io_in_bits_src_3_3 = input.src4[3];
  dut_ptr->io_in_bits_src_3_4 = input.src4[4];
  dut_ptr->io_in_bits_src_3_5 = input.src4[5];
  dut_ptr->io_in_bits_src_3_6 = input.src4[6];
  dut_ptr->io_in_bits_src_3_7 = input.src4[7];
  dut_ptr->io_in_bits_src_3_8 = input.src4[8];
  dut_ptr->io_in_bits_src_3_9 = input.src4[9];
  dut_ptr->io_in_bits_src_3_10 = input.src4[10];
  dut_ptr->io_in_bits_src_3_11 = input.src4[11];
  dut_ptr->io_in_bits_src_3_12 = input.src4[12];
  dut_ptr->io_in_bits_src_3_13 = input.src4[13];
  dut_ptr->io_in_bits_src_3_14 = input.src4[14];
  dut_ptr->io_in_bits_src_3_15 = input.src4[15];
  dut_ptr->io_in_bits_src_3_16 = input.src4[16];
  dut_ptr->io_in_bits_src_3_17 = input.src4[17];
  dut_ptr->io_in_bits_src_3_18 = input.src4[18];
  dut_ptr->io_in_bits_src_3_19 = input.src4[19];
  dut_ptr->io_in_bits_src_3_20 = input.src4[20];
  dut_ptr->io_in_bits_src_3_21 = input.src4[21];
  dut_ptr->io_in_bits_src_3_22 = input.src4[22];
  dut_ptr->io_in_bits_src_3_23 = input.src4[23];
  dut_ptr->io_in_bits_src_3_24 = input.src4[24];
  dut_ptr->io_in_bits_src_3_25 = input.src4[25];
  dut_ptr->io_in_bits_src_3_26 = input.src4[26];
  dut_ptr->io_in_bits_src_3_27 = input.src4[27];
  dut_ptr->io_in_bits_src_3_28 = input.src4[28];
  dut_ptr->io_in_bits_src_3_29 = input.src4[29];
  dut_ptr->io_in_bits_src_3_30 = input.src4[30];
  dut_ptr->io_in_bits_src_3_31 = input.src4[31];

  dut_ptr->io_in_bits_fuType  = input.fuType;
  dut_ptr->io_in_bits_fuOpType = input.fuOpType;
  dut_ptr->io_in_bits_sew     = input.sew;
  dut_ptr->io_in_bits_uop_idx = input.uop_idx;
  dut_ptr->io_in_bits_src_widen = input.src_widen;
  dut_ptr->io_in_bits_widen   = input.widen;
  dut_ptr->io_in_bits_is_frs1 = input.is_frs1;
  dut_ptr->io_in_bits_is_frs2 = input.is_frs2;
  dut_ptr->io_in_bits_rm      = input.rm;
  dut_ptr->io_in_bits_vinfo_vstart = input.vinfo.vstart;
  dut_ptr->io_in_bits_vinfo_vl     = input.vinfo.vl;
  dut_ptr->io_in_bits_vinfo_vlmul  = input.vinfo.vlmul;
  dut_ptr->io_in_bits_vinfo_vm     = input.vinfo.vm;
  dut_ptr->io_in_bits_vinfo_ta     = input.vinfo.ta;
  dut_ptr->io_in_bits_vinfo_ma     = input.vinfo.ma;
  return  dut_ptr->io_in_valid;
}

int TestDriver::diff_output_falling(VSimTop *dut_ptr) {
  bool finish = dut_ptr->io_out_valid;
  if (finish) {
    // printf("Finished\n");
    dut_output.result[0] = dut_ptr->io_out_bits_result_0;
    dut_output.result[1] = dut_ptr->io_out_bits_result_1;
    dut_output.result[2] = dut_ptr->io_out_bits_result_2;
    dut_output.result[3] = dut_ptr->io_out_bits_result_3;
    dut_output.result[4] = dut_ptr->io_out_bits_result_4;
    dut_output.result[5] = dut_ptr->io_out_bits_result_5;
    dut_output.result[6] = dut_ptr->io_out_bits_result_6;
    dut_output.result[7] = dut_ptr->io_out_bits_result_7;
    dut_output.result[8] = dut_ptr->io_out_bits_result_8;
    dut_output.result[9] = dut_ptr->io_out_bits_result_9;
    dut_output.result[10] = dut_ptr->io_out_bits_result_10;
    dut_output.result[11] = dut_ptr->io_out_bits_result_11;
    dut_output.result[12] = dut_ptr->io_out_bits_result_12;
    dut_output.result[13] = dut_ptr->io_out_bits_result_13;
    dut_output.result[14] = dut_ptr->io_out_bits_result_14;
    dut_output.result[15] = dut_ptr->io_out_bits_result_15;
    dut_output.result[16] = dut_ptr->io_out_bits_result_16;
    dut_output.result[17] = dut_ptr->io_out_bits_result_17;
    dut_output.result[18] = dut_ptr->io_out_bits_result_18;
    dut_output.result[19] = dut_ptr->io_out_bits_result_19;
    dut_output.result[20] = dut_ptr->io_out_bits_result_20;
    dut_output.result[21] = dut_ptr->io_out_bits_result_21;
    dut_output.result[22] = dut_ptr->io_out_bits_result_22;
    dut_output.result[23] = dut_ptr->io_out_bits_result_23;
    dut_output.result[24] = dut_ptr->io_out_bits_result_24;
    dut_output.result[25] = dut_ptr->io_out_bits_result_25;
    dut_output.result[26] = dut_ptr->io_out_bits_result_26;
    dut_output.result[27] = dut_ptr->io_out_bits_result_27;
    dut_output.result[28] = dut_ptr->io_out_bits_result_28;
    dut_output.result[29] = dut_ptr->io_out_bits_result_29;
    dut_output.result[30] = dut_ptr->io_out_bits_result_30;
    dut_output.result[31] = dut_ptr->io_out_bits_result_31;
    dut_output.fflags[0] = dut_ptr->io_out_bits_fflags_0;
    dut_output.fflags[1] = dut_ptr->io_out_bits_fflags_1;
    dut_output.fflags[2] = dut_ptr->io_out_bits_fflags_2;
    dut_output.fflags[3] = dut_ptr->io_out_bits_fflags_3;
    dut_output.fflags[4] = dut_ptr->io_out_bits_fflags_4;
    dut_output.fflags[5] = dut_ptr->io_out_bits_fflags_5;
    dut_output.fflags[6] = dut_ptr->io_out_bits_fflags_6;
    dut_output.fflags[7] = dut_ptr->io_out_bits_fflags_7;
    dut_output.fflags[8] = dut_ptr->io_out_bits_fflags_8;
    dut_output.fflags[9] = dut_ptr->io_out_bits_fflags_9;
    dut_output.fflags[10] = dut_ptr->io_out_bits_fflags_10;
    dut_output.fflags[11] = dut_ptr->io_out_bits_fflags_11;
    dut_output.fflags[12] = dut_ptr->io_out_bits_fflags_12;
    dut_output.fflags[13] = dut_ptr->io_out_bits_fflags_13;
    dut_output.fflags[14] = dut_ptr->io_out_bits_fflags_14;
    dut_output.fflags[15] = dut_ptr->io_out_bits_fflags_15;
    dut_output.fflags[16] = dut_ptr->io_out_bits_fflags_16;
    dut_output.fflags[17] = dut_ptr->io_out_bits_fflags_17;
    dut_output.fflags[18] = dut_ptr->io_out_bits_fflags_18;
    dut_output.fflags[19] = dut_ptr->io_out_bits_fflags_19;
    dut_output.fflags[20] = dut_ptr->io_out_bits_fflags_20;
    dut_output.fflags[21] = dut_ptr->io_out_bits_fflags_21;
    dut_output.fflags[22] = dut_ptr->io_out_bits_fflags_22;
    dut_output.fflags[23] = dut_ptr->io_out_bits_fflags_23;
    dut_output.fflags[24] = dut_ptr->io_out_bits_fflags_24;
    dut_output.fflags[25] = dut_ptr->io_out_bits_fflags_25;
    dut_output.fflags[26] = dut_ptr->io_out_bits_fflags_26;
    dut_output.fflags[27] = dut_ptr->io_out_bits_fflags_27;
    dut_output.fflags[28] = dut_ptr->io_out_bits_fflags_28;
    dut_output.fflags[29] = dut_ptr->io_out_bits_fflags_29;
    dut_output.fflags[30] = dut_ptr->io_out_bits_fflags_30;
    dut_output.fflags[31] = dut_ptr->io_out_bits_fflags_31;

    dut_output.vxsat = dut_ptr->io_out_bits_vxsat;

    if (memcmp(&dut_output.result, &expect_output.result, sizeof(dut_output.result))) {
      printf("\n!!!!!!!!!Error, compare failed!!!!!!!!!!!!!\n");
      
      display();
      // for(int i = 0; i < VLEN/64; i++){
      //   if(dut_output.result[i] != expect_output.result[i]){
      //     printf("dut_output.result[%d] = %016lx_%016lx\n", i, dut_output.result[i] >> 64, dut_output.result[i]);
      //     printf("expect_output.result[%d] = %016lx_%016lx\n", i, expect_output.result[i] >> 64, expect_output.result[i]);
      //   }
      // }
      return STATE_BADTRAP;
    } else {
      gen_next_test_case();
    }
    return STATE_FINISH_OPERATION;
  } else {
    stuck_count ++;
    if (stuck_count >= stuck_limit) {
      printf("DUT stucked. Not finished in %lu cycles\n", stuck_limit);
      stuck_count = 0;
      return STATE_BADTRAP;
    }
    return STATE_RUNNING;
  }
}

void TestDriver::display_ref_input() {
  printf("REF Input:\n");
  // printf("  src1 %016lx_%016lx src2 %016lx_%016lx src3 %016lx_%016lx src4 %016lx_%016lx\n", input.src1[1], input.src1[0], input.src2[1], input.src2[0], input.src3[1], input.src3[0], input.src4[1], input.src4[0]);
  if(input.fuType == VFloatCvt){
    if(input.fuOpType == VFNCVT_XFW){
      printf("src1: \n");
      fp16_print(input.src1, VLEN, XLEN);
      // printf("src2: \n");
      // fp16_print(input.src2, VLEN, XLEN);
      // printf("src3: \n");
      // fp16_print(input.src3, VLEN, XLEN);
      // printf("src4: \n");
      // fp16_print(input.src4, VLEN, XLEN);
    }
    else if(input.fuOpType == VFWCVT_FXV){
      printf("src1: \n");
      int8_print(input.src1, VLEN, XLEN);
      // printf("src2: \n");
      // int8_print(input.src2, VLEN, XLEN);
      // printf("src3: \n");
      // int8_print(input.src3, VLEN, XLEN);
      // printf("src4: \n");
      // int8_print(input.src4, VLEN, XLEN);
    }
  }
  else if(input.fuType == NewVrgather){
    if(input.sew == 0){
      printf("src1: \n");
      int8_print(input.src1, VLEN, XLEN);
      printf("src2: \n");
      int8_print(input.src2, VLEN, XLEN);
    }
    else if(input.sew == 1){
      printf("src1: \n");
      int16_print(input.src1, VLEN, XLEN);
      printf("src2: \n");
      int16_print(input.src2, VLEN, XLEN);
    }
  }
  else if(input.sew == 1){
    printf("src1: \n");
    fp16_print(input.src1, VLEN, XLEN);
    printf("src2: \n");
    fp16_print(input.src2, VLEN, XLEN);
    // printf("src3: \n");
    // fp16_print(input.src3, VLEN, XLEN);
    // printf("src4: \n");
    // fp16_print(input.src4, VLEN, XLEN);
  }
  else if(input.sew == 0){
    printf("src1: \n");
    int8_print(input.src1, VLEN, XLEN);
    printf("src2: \n");
    int8_print(input.src2, VLEN, XLEN);
    // printf("src3: \n");
    // int8_print(input.src3, VLEN, XLEN);
    // printf("src4: \n");
    // int8_print(input.src4, VLEN, XLEN);
  }
  else printf("  src1 %016lx_%016lx_%016lx_%016lx src2 %016lx_%016lx_%016lx_%016lx src3 %016lx_%016lx_%016lx_%016lx src4 %016lx_%016lx_%016lx_%016lx\n", 
  input.src1[3], input.src1[2], input.src1[1], input.src1[0], 
  input.src2[3], input.src2[2], input.src2[1], input.src2[0], 
  input.src3[3], input.src3[2], input.src3[1], input.src3[0], 
  input.src4[3], input.src4[2], input.src4[1], input.src4[0]);

  printf("  fuType %x fuOpType %x sew %x uop_idx %d src_widen %d widen %d is_frs1 %d rm %d\n", input.fuType, input.fuOpType, input.sew, input.uop_idx, input.src_widen, input.widen, input.is_frs1, input.rm);
  printf("  vstart %d vl %d vlmul %x vm %d ta %d ma %d\n", input.vinfo.vstart, input.vinfo.vl, input.vinfo.vlmul, input.vinfo.vm, input.vinfo.ta, input.vinfo.ma);
}

void TestDriver::display_ref_output() {
  printf("Expected Output: \n");
  if(input.fuType == VFloatCvt){
    if(input.fuOpType == VFNCVT_XFW){
      printf("res : \n");
      int8_print(expect_output.result, VLEN, XLEN);
      printf("  fflags: %x_%x_%x_%x  vxsat: %lx\n", expect_output.fflags[3], expect_output.fflags[2], expect_output.fflags[1], expect_output.fflags[0], expect_output.vxsat);
    }
    else if(input.fuOpType == VFWCVT_FXV){
      printf("res : \n");
      fp16_print(expect_output.result, VLEN, XLEN);
      printf("  fflags: %x_%x_%x_%x  vxsat: %lx\n", expect_output.fflags[3], expect_output.fflags[2], expect_output.fflags[1], expect_output.fflags[0], expect_output.vxsat);
    }
  }
  else if(input.fuType == NewVrgather){
    printf("res : \n");
    if(input.sew == 0) int8_print(expect_output.result, VLEN, XLEN);
    else int16_print(expect_output.result, VLEN, XLEN);
    printf("  fflags: %x_%x_%x_%x  vxsat: %lx\n", expect_output.fflags[3], expect_output.fflags[2], expect_output.fflags[1], expect_output.fflags[0], expect_output.vxsat);    
  }
  else if(input.sew == 1){
    printf("res : \n");
    fp16_print(expect_output.result, VLEN, XLEN);
    printf("  fflags: %x_%x_%x_%x  vxsat: %lx\n", expect_output.fflags[3], expect_output.fflags[2], expect_output.fflags[1], expect_output.fflags[0], expect_output.vxsat);    
  }
  else if(input.sew == 0){
    printf("res : \n");
    int8_print(expect_output.result, VLEN, XLEN);
    printf("  fflags: %x_%x_%x_%x  vxsat: %lx\n", expect_output.fflags[3], expect_output.fflags[2], expect_output.fflags[1], expect_output.fflags[0], expect_output.vxsat);
  }
  else printf("  result  %016lx_%016lx_%016lx_%016lx fflags: %x_%x_%x_%x  vxsat: %lx\n", 
  expect_output.result[3], expect_output.result[2], expect_output.result[1], expect_output.result[0], 
  expect_output.fflags[3], expect_output.fflags[2], expect_output.fflags[1], expect_output.fflags[0], expect_output.vxsat);
}

void TestDriver::display_dut() {
  printf("DUT Output:\n");
  if(input.fuType == VFloatCvt){
    if(input.fuOpType == VFNCVT_XFW){
      printf("res : \n");
      int8_print(dut_output.result, VLEN, XLEN);
      printf("  fflags: %x_%x_%x_%x  vxsat: %lx\n", dut_output.fflags[3], dut_output.fflags[2], dut_output.fflags[1], dut_output.fflags[0], dut_output.vxsat);
    }
    else if(input.fuOpType == VFWCVT_FXV){
      printf("res : \n");
      fp16_print(dut_output.result, VLEN, XLEN);
      printf("  fflags: %x_%x_%x_%x  vxsat: %lx\n", dut_output.fflags[3], dut_output.fflags[2], dut_output.fflags[1], dut_output.fflags[0], dut_output.vxsat);
    }
  }
  else if(input.fuType == NewVrgather){
    printf("res : \n");
    if(input.sew == 0) int8_print(dut_output.result, VLEN, XLEN);
    else int16_print(dut_output.result, VLEN, XLEN);
    printf("  fflags: %x_%x_%x_%x  vxsat: %lx\n", dut_output.fflags[3], dut_output.fflags[2], dut_output.fflags[1], dut_output.fflags[0], dut_output.vxsat);    
  }
  else if(input.sew == 1){
    printf("res : \n");
    fp16_print(dut_output.result, VLEN, XLEN);
    printf("  fflags: %x_%x_%x_%x  vxsat: %lx\n", dut_output.fflags[3], dut_output.fflags[2], dut_output.fflags[1], dut_output.fflags[0], dut_output.vxsat);    
  }
  else if(input.sew == 0){
    printf("res : \n");
    int8_print(dut_output.result, VLEN/2, XLEN);
    printf("  fflags: %x_%x_%x_%x  vxsat: %lx\n", dut_output.fflags[3], dut_output.fflags[2], dut_output.fflags[1], dut_output.fflags[0], dut_output.vxsat);
  }
  else printf("  result  %016lx_%016lx_%016lx_%016lx fflags: %x_%x_%x_%x  vxsat: %lx\n", 
  dut_output.result[3], dut_output.result[2], dut_output.result[1], dut_output.result[0], 
  dut_output.fflags[3], dut_output.fflags[2], dut_output.fflags[1], dut_output.fflags[0], dut_output.vxsat);
}

void TestDriver::display() {
  display_ref_input();
  display_ref_output();
  display_dut();
}