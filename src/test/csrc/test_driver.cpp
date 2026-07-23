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

namespace {

uint64_t width_mask(int width) {
  return width == 64 ? ~0ULL : ((1ULL << width) - 1ULL);
}

int exp_bits_for_format(uint8_t format_mode) {
  return format_mode == VFEXP2_MODE_FP32 || format_mode == VFEXP2_MODE_BF16 ? 8 : 5;
}

int frac_bits_for_format(uint8_t format_mode) {
  switch (format_mode) {
    case VFEXP2_MODE_FP16: return 10;
    case VFEXP2_MODE_BF16: return 7;
    case VFEXP2_MODE_FP32: return 23;
    default:
      printf("Unsupported vfexp2 format mode %u\n", format_mode);
      exit(1);
  }
}

bool is_zero_bits(uint64_t bits, int width) {
  uint64_t payload_mask = (1ULL << (width - 1)) - 1ULL;
  return (bits & payload_mask) == 0;
}

bool is_nan_bits(uint64_t bits, int exp_bits, int frac_bits) {
  uint64_t exp_mask = (1ULL << exp_bits) - 1ULL;
  uint64_t frac_mask = (1ULL << frac_bits) - 1ULL;
  uint64_t exp = (bits >> frac_bits) & exp_mask;
  uint64_t frac = bits & frac_mask;
  return exp == exp_mask && frac != 0;
}

uint64_t ordered_bits(uint64_t bits, int width) {
  const uint64_t mask = width_mask(width);
  const uint64_t sign_mask = 1ULL << (width - 1);
  const uint64_t normalized = bits & mask;
  return (normalized & sign_mask) ? ((~normalized) & mask) : (normalized | sign_mask);
}

uint64_t ulp_distance(uint64_t a, uint64_t b, int width) {
  const uint64_t oa = ordered_bits(a, width);
  const uint64_t ob = ordered_bits(b, width);
  return oa > ob ? (oa - ob) : (ob - oa);
}

uint64_t log2_ulp_metric(uint64_t a, uint64_t b, int width) {
  const uint64_t ulp = ulp_distance(a, b, width);
  if (ulp == 0) return 0;
  return 64 - __builtin_clzll(ulp);
}

uint64_t vfexp2_budget(uint8_t format_mode, uint8_t rm) {
  if (format_mode == VFEXP2_MODE_FP32) return 13;
  if (rm == RM_RTO) return 4;
  return 1;
}

uint64_t vfexp2_metric(uint8_t format_mode, uint64_t a, uint64_t b, int width) {
  if (format_mode == VFEXP2_MODE_FP32) return log2_ulp_metric(a, b, width);
  return ulp_distance(a, b, width);
}

const char *vfexp2_metric_name(uint8_t format_mode) {
  return format_mode == VFEXP2_MODE_FP32 ? "log2ulp" : "ulp";
}

uint64_t extract_lane_bits(const VecOutput &output, int lane, int width) {
  const int lanes_per_word = 64 / width;
  const int word = lane / lanes_per_word;
  const int shift = (lane % lanes_per_word) * width;
  return (output.result[word] >> shift) & width_mask(width);
}

uint64_t extract_lane_input_bits(const VecInput &input, int lane, int width) {
  const int lanes_per_word = 64 / width;
  const int word = lane / lanes_per_word;
  const int shift = (lane % lanes_per_word) * width;
  return (input.src1[word] >> shift) & width_mask(width);
}

} // namespace

TestDriver::TestDriver():
  issued(false), vfexp2_only(false), vfexp2_format_mode(VFEXP2_MODE_MIXED), vfexp2_logged_diffs(0), verbose(false), keepinput(false)
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

void TestDriver::configure_vfexp2_test(bool enabled, uint8_t format_mode) {
  vfexp2_only = enabled;
  vfexp2_format_mode = format_mode;
  set_test_type();
  gen_next_test_case();
}
// fix set_test_type to select fuType
void TestDriver::set_test_type() {
  if (vfexp2_only) {
    test_type.pick_fuType = true;
    test_type.pick_fuOpType = false;
    test_type.fuType = VFloatCvt;
    test_type.fuOpType = VFEXP2;
  } else {
    test_type.pick_fuType = false;
    test_type.pick_fuOpType = false;
    test_type.fuType = VFloatCvt;
    test_type.fuOpType = VFREC7;
  }
  printf("Set Test Type Res: fuType:%d fuOpType:%d\n", test_type.fuType, test_type.fuOpType);
}

const char *TestDriver::vfexp2_format_name(uint8_t format_mode) {
  switch (format_mode) {
    case VFEXP2_MODE_FP16: return "fp16";
    case VFEXP2_MODE_BF16: return "bf16";
    case VFEXP2_MODE_FP32: return "fp32";
    default: return "mixed";
  }
}

uint8_t TestDriver::pick_vfexp2_format() const {
  if (vfexp2_format_mode != VFEXP2_MODE_MIXED) return vfexp2_format_mode;
  const uint8_t modes[3] = {VFEXP2_MODE_FP16, VFEXP2_MODE_BF16, VFEXP2_MODE_FP32};
  return modes[rand() % 3];
}

uint64_t TestDriver::gen_exp2_lane_bits(uint8_t format_mode) {
  if (format_mode == VFEXP2_MODE_FP32) {
    const uint32_t specials[] = {
      0x00000000U, 0x80000000U, 0x3f800000U, 0xbf800000U,
      0x7f800000U, 0xff800000U, 0x7fc00000U, 0x7f800001U,
      0x00000001U, 0x00800000U, 0x7f7fffffU, 0xff7fffffU
    };
    if (rand() % 5 == 0) return specials[rand() % (sizeof(specials) / sizeof(specials[0]))];
    uint32_t sign = (rand() & 1) ? 0x80000000U : 0U;
    uint32_t exp = ((rand() % 2) == 0 ? (120 + rand() % 15) : (1 + rand() % 254)) << 23;
    uint32_t frac = static_cast<uint32_t>(rand64() & 0x7fffffU);
    return sign | exp | frac;
  }

  if (format_mode == VFEXP2_MODE_BF16) {
    const uint16_t specials[] = {
      0x0000U, 0x8000U, 0x3f80U, 0xbf80U,
      0x7f80U, 0xff80U, 0x7fc0U, 0x7f81U,
      0x0001U, 0x0080U, 0x7f7fU, 0xff7fU
    };
    if (rand() % 5 == 0) return specials[rand() % (sizeof(specials) / sizeof(specials[0]))];
    uint16_t sign = (rand() & 1) ? 0x8000U : 0U;
    uint16_t exp = static_cast<uint16_t>((124 + rand() % 10) << 7);
    uint16_t frac = static_cast<uint16_t>(rand() & 0x7fU);
    return static_cast<uint16_t>(sign | exp | frac);
  }

  const uint16_t specials[] = {
    0x0000U, 0x8000U, 0x3c00U, 0xbc00U,
    0x7c00U, 0xfc00U, 0x7e00U, 0x7d00U,
    0x0001U, 0x0400U, 0x7bffU, 0xfbffU
  };
  if (rand() % 5 == 0) return specials[rand() % (sizeof(specials) / sizeof(specials[0]))];
  uint16_t sign = (rand() & 1) ? 0x8000U : 0U;
  uint16_t exp = static_cast<uint16_t>((11 + rand() % 10) << 10);
  uint16_t frac = static_cast<uint16_t>(rand() & 0x3ffU);
  return static_cast<uint16_t>(sign | exp | frac);
}

void TestDriver::get_random_exp2_input() {
  memset(&input, 0, sizeof(input));

  const uint8_t format_mode = pick_vfexp2_format();
  const int width = format_mode == VFEXP2_MODE_FP32 ? 32 : 16;
  const int lanes = 128 / width;
  const int lanes_per_word = 64 / width;

  input.fuType = VFloatCvt;
  input.fuOpType = format_mode == VFEXP2_MODE_BF16 ? VFEXP2BF16 : VFEXP2;
  input.sew = format_mode == VFEXP2_MODE_FP32 ? 2 : 1;
  input.widen = false;
  input.src_widen = false;
  input.is_frs1 = false;
  input.is_frs2 = false;
  input.uop_idx = 0;
  input.rm_s = 0;
  input.rm = (rand() % 6 == 5) ? RM_RTO : (rand() % 5);
  input.vinfo.vlmul = 0;
  input.vinfo.vl = lanes;
  input.vinfo.vstart = 0;
  input.vinfo.vm = true;
  input.vinfo.ta = false;
  input.vinfo.ma = false;

  for (int lane = 0; lane < lanes; lane++) {
    const int word = lane / lanes_per_word;
    const int shift = (lane % lanes_per_word) * width;
    input.src1[word] |= gen_exp2_lane_bits(format_mode) << shift;
  }
  input.src2[0] = rand64();
  input.src2[1] = rand64();
  input.src3[0] = rand64();
  input.src3[1] = rand64();
  input.src4[0] = rand64();
  input.src4[1] = rand64();
}

bool TestDriver::is_vfexp2_case() const {
  return input.fuType == VFloatCvt && (input.fuOpType == VFEXP2 || input.fuOpType == VFEXP2BF16);
}

void TestDriver::gen_next_test_case() {
  issued = false;
  get_random_input();
  if (verbose) { display_ref_input(); }
  get_expected_output();
  if (verbose) { display_ref_output(); }
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
    case VIntegerALU: break;
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
    default:
      printf("Unsupported FuType %d\n", input.fuType);
      exit(1);
      return 0;
  }
  return 0;
}

uint8_t TestDriver::gen_random_sew() {
  switch (input.fuType)
  {
    case VIntegerALU: return rand()%4; break;
    case VPermutation: return rand()%4; break;
    case VFloatCvt: return rand()%4; break;
    case FloatCvtF2X: return (rand()%3)+1 ; break;
    case FloatCvtI2F: return 0 ; break;
    default: return (rand()%3)+1; break;
  }
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

  input.vinfo.vlmul = vlmul_list[rand() % (7 - input.sew)];
  int elements_per_reg = (VLEN / 8) >> input.sew;
  int vlmax = (input.vinfo.vlmul > 4) ? (elements_per_reg >> (8 - input.vinfo.vlmul)) : (elements_per_reg << input.vinfo.vlmul);
  switch (input.fuType) {
    case VPermutation: {
      if (input.fuOpType == VCOMPRESS)
        input.vinfo.vstart = 0;
      else
        input.vinfo.vstart = rand() % vlmax;
      break;
    }
    default: input.vinfo.vstart = 0; break;
  } // The vstart of an arithmetic instruction is generally equal to 0
  input.vinfo.vl = rand() % vlmax + 1; // TODO: vl == 0 may be illegal

  input.vinfo.vm = rand() % 2;
  input.vinfo.ta = rand() % 2;
  input.vinfo.ma = rand() % 2;
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

  if (vfexp2_only) {
    get_random_exp2_input();
    return;
  }
 
  input.src1[0] = rand64();
  input.src1[1] = rand64();
  input.src2[0] = rand64();
  input.src2[1] = rand64();
  input.src3[0] = rand64();
  input.src3[1] = rand64();
  input.src4[0] = rand64();
  input.src4[1] = rand64();

  if (!test_type.pick_fuType) { input.fuType = gen_random_futype(ALL_FUTYPES); }
  else { input.fuType = test_type.fuType; }
  if(input.fuType == VFloatCvt){
    input.sew = gen_random_sew();
    input.is_frs1 = false;
    input.is_frs2 = false;
    input.widen = false;
    input.src_widen = false;
    input.uop_idx = 0;
    input.rm_s = 0;
    if (!test_type.pick_fuOpType) { input.fuOpType = gen_random_optype(); }
    else { input.fuOpType = test_type.fuOpType; }
    input.vinfo.vlmul = 0;
    input.vinfo.vl = (VLEN / 8) >> input.sew;
    input.vinfo.vstart = 0;
    input.vinfo.vm = true;
    input.vinfo.ta = false;
    input.vinfo.ma = false;
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
      expect_output = is_vfexp2_case() ? vexp2.get_expected_output(input) : vcvt.get_expected_output(input); return;
    case FloatCvtF2X:
      if (verbose) { printf("FuType:%d, choose FloatCvtF2X %d\n", input.fuType, FloatCvtF2X); }
      expect_output = scvt.get_expected_output(input); return;   
    case FloatCvtI2F:
      if (verbose) { printf("FuType:%d, choose FloatCvtI2F %d\n", input.fuType, FloatCvtI2F); }
      expect_output = scvt.get_expected_output(input); return; 
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

void TestDriver::record_vfexp2_match(uint8_t format_mode, int lane, uint64_t src_bits, uint64_t dut_bits, uint64_t ref_bits) {
  Vfexp2LaneStats &stats = vfexp2_stats[format_mode - 1];
  const int width = format_mode == VFEXP2_MODE_FP32 ? 32 : 16;
  const int exp_bits = exp_bits_for_format(format_mode);
  const int frac_bits = frac_bits_for_format(format_mode);

  if (dut_bits == ref_bits) {
    stats.exact++;
    if (format_mode == VFEXP2_MODE_FP32) stats.log2_ulp_le_13++;
    else stats.ulp_le_1++;
    return;
  }
  if (is_zero_bits(dut_bits, width) && is_zero_bits(ref_bits, width)) {
    stats.zero_equivalent++;
    if (format_mode == VFEXP2_MODE_FP32) stats.log2_ulp_le_13++;
    else stats.ulp_le_1++;
    return;
  }
  if (is_nan_bits(dut_bits, exp_bits, frac_bits) && is_nan_bits(ref_bits, exp_bits, frac_bits)) {
    stats.nan_equivalent++;
    if (format_mode == VFEXP2_MODE_FP32) stats.log2_ulp_le_13++;
    else stats.ulp_le_1++;
    return;
  }

  const uint64_t ulp = ulp_distance(dut_bits, ref_bits, width);
  if (input.rm < 8) {
    stats.max_ulp_by_rm[input.rm] = std::max(stats.max_ulp_by_rm[input.rm], ulp);
  }
  {
    Vfexp2WorstEntry (&w)[3] = vfexp2_worst[format_mode - 1];
    if (input.rm == RM_RNE && ulp > w[2].ulp) {
      int pos = 2;
      while (pos > 0 && ulp > w[pos - 1].ulp) pos--;
      for (int j = 2; j > pos; j--) w[j] = w[j - 1];
      w[pos].src_bits = src_bits;
      w[pos].dut_bits = dut_bits;
      w[pos].ref_bits = ref_bits;
      w[pos].ulp = ulp;
      w[pos].rm = input.rm;
    }
  }
  const uint64_t metric = vfexp2_metric(format_mode, dut_bits, ref_bits, width);
  const uint64_t budget = vfexp2_budget(format_mode, input.rm);
  if (format_mode == VFEXP2_MODE_FP32) {
    if (metric <= budget) stats.log2_ulp_le_13++;
  } else {
    if (metric <= 1) stats.ulp_le_1++;
    if (input.rm == RM_RTO && metric <= 4) stats.ulp_le_4_rto++;
  }
  const bool ulp_over_budget = metric > budget;
  if (ulp_over_budget) stats.ulp_over_budget++;

  if (ulp_over_budget && vfexp2_logged_diffs < vfexp2_log_limit) {
    printf("[vfexp2] %s lane %d rm=%u %s=%lu exceeds_budget src=0x%0*lx dut=0x%0*lx ref=0x%0*lx\n",
      vfexp2_format_name(format_mode),
      lane,
      input.rm,
      vfexp2_metric_name(format_mode),
      metric,
      width / 4, static_cast<unsigned long>(src_bits),
      width / 4, static_cast<unsigned long>(dut_bits),
      width / 4, static_cast<unsigned long>(ref_bits));
    vfexp2_logged_diffs++;
  }
}

bool TestDriver::compare_vfexp2_output() {
  const uint8_t format_mode = input.fuOpType == VFEXP2BF16 ? VFEXP2_MODE_BF16 : (input.sew == 2 ? VFEXP2_MODE_FP32 : VFEXP2_MODE_FP16);
  const int width = format_mode == VFEXP2_MODE_FP32 ? 32 : 16;
  const int lanes = 128 / width;

  for (int lane = 0; lane < lanes; lane++) {
    vfexp2_stats[format_mode - 1].total++;
    const uint64_t src_bits = extract_lane_input_bits(input, lane, width);
    const uint64_t dut_bits = extract_lane_bits(dut_output, lane, width);
    const uint64_t ref_bits = extract_lane_bits(expect_output, lane, width);
    record_vfexp2_match(format_mode, lane, src_bits, dut_bits, ref_bits);
  }
  return true;
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
  dut_ptr->io_in_bits_src_1_0 = input.src2[0];
  dut_ptr->io_in_bits_src_1_1 = input.src2[1];
  dut_ptr->io_in_bits_src_2_0 = input.src3[0];
  dut_ptr->io_in_bits_src_2_1 = input.src3[1];
  dut_ptr->io_in_bits_src_3_0 = input.src4[0];
  dut_ptr->io_in_bits_src_3_1 = input.src4[1];
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
    dut_output.fflags[0] = dut_ptr->io_out_bits_fflags_0;
    dut_output.fflags[1] = dut_ptr->io_out_bits_fflags_1;
    dut_output.vxsat = dut_ptr->io_out_bits_vxsat;

    bool matched = false;
    if (is_vfexp2_case()) {
      matched = compare_vfexp2_output();
    } else {
      matched = memcmp(&dut_output, &expect_output, sizeof(dut_output)) == 0;
    }

    if (!matched) {
      printf("Error, compare failed\n");
      display();
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
  printf("  src1 %016lx_%016lx src2 %016lx_%016lx src3 %016lx_%016lx src4 %016lx_%016lx\n", input.src1[1], input.src1[0], input.src2[1], input.src2[0], input.src3[1], input.src3[0], input.src4[1], input.src4[0]);
  printf("  fuType %x fuOpType %x sew %x uop_idx %d src_widen %d widen %d is_frs1 %d rm %d\n", input.fuType, input.fuOpType, input.sew, input.uop_idx, input.src_widen, input.widen, input.is_frs1, input.rm);
  printf("  vstart %d vl %d vlmul %x vm %d ta %d ma %d\n", input.vinfo.vstart, input.vinfo.vl, input.vinfo.vlmul, input.vinfo.vm, input.vinfo.ta, input.vinfo.ma);
}

void TestDriver::display_ref_output() {
  printf("Expected Output \n");
  printf("  result  %016lx_%016lx fflags: %x_%x  vxsat: %lx\n", expect_output.result[1], expect_output.result[0], expect_output.fflags[1], expect_output.fflags[0], expect_output.vxsat);
}

void TestDriver::display_dut() {
  printf("DUT Output:\n");
  printf("  result  %016lx_%016lx fflags: %x_%x  vxsat: %lx\n", dut_output.result[1], dut_output.result[0], dut_output.fflags[1], dut_output.fflags[0], dut_output.vxsat);
}

void TestDriver::display() {
  display_ref_input();
  display_ref_output();
  display_dut();
}

void TestDriver::print_summary() const {
  if (!vfexp2_only) return;

  printf("==== VFEXP2 comparison summary ====\n");
  for (uint8_t mode = VFEXP2_MODE_FP16; mode <= VFEXP2_MODE_FP32; mode++) {
    const Vfexp2LaneStats &stats = vfexp2_stats[mode - 1];
    if (stats.total == 0) continue;
    printf("  [%s] total=%lu exact=%lu zeroEq=%lu nanEq=%lu maxULP RNE=%lu RTZ=%lu RDN=%lu RUP=%lu RMM=%lu RTO=%lu\n",
      vfexp2_format_name(mode),
      stats.total,
      stats.exact,
      stats.zero_equivalent,
      stats.nan_equivalent,
      stats.max_ulp_by_rm[0],
      stats.max_ulp_by_rm[1],
      stats.max_ulp_by_rm[2],
      stats.max_ulp_by_rm[3],
      stats.max_ulp_by_rm[4],
      stats.max_ulp_by_rm[6]);
    const Vfexp2WorstEntry (&w)[3] = vfexp2_worst[mode - 1];
    const int hexw = (mode == VFEXP2_MODE_FP32) ? 8 : 4;
    for (int i = 0; i < 3 && w[i].ulp > 0; i++) {
      printf("    Top%u ULP=%lu  src=0x%0*lx dut=0x%0*lx ref=0x%0*lx\n",
        i + 1, w[i].ulp,
        hexw, w[i].src_bits,
        hexw, w[i].dut_bits,
        hexw, w[i].ref_bits);
    }
  }
}