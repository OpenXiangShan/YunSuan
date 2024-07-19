#ifndef __GM_COMMON_H
#define __GM_COMMON_H

extern "C" {
#include <softfloat.h>
}
#include "iotype.h"
#include "vpu_constant.h"
#include <typeinfo>

class VPUGoldenModel {
  private:
    // bool bad_fuType(VecInput input);
    // bool bad_fuOpType(VecInput input);
  public:
    VPUGoldenModel();
    VecOutput get_expected_output(VecInput input);
    ElementInput select_element(VecInput input, int idx);

    virtual ElementOutput calculation_e8(ElementInput input) = 0;
    virtual ElementOutput calculation_e16(ElementInput input) = 0;
    virtual ElementOutput calculation_e32(ElementInput input) = 0;
    virtual ElementOutput calculation_e64(ElementInput input) = 0;

    bool verbose;
    void verbose_exec() { verbose = true; }
    void display_calculation(const char *, const char *, ElementInput, ElementOutput);
};

class VGMFloatBase : public VPUGoldenModel {
  public:
    virtual ElementOutput calculation_e8(ElementInput input);
    virtual ElementOutput calculation_e16(ElementInput input) = 0;
    virtual ElementOutput calculation_e32(ElementInput input) = 0;
    virtual ElementOutput calculation_e64(ElementInput input) = 0;
  /*inline */void fp_set_rm(int rm);
  /*inline */uint32_t fp_get_exception();
  /*inline */void fp_clear_exception();
  /*inline */float16_t i2f16(uint16_t i);
  /*inline */float32_t i2f32(uint32_t i);
  /*inline */float64_t i2f64(uint64_t i);
};

class VGMFloatAdder : public VGMFloatBase {
  virtual ElementOutput calculation_e16(ElementInput input);
  virtual ElementOutput calculation_e32(ElementInput input);
  virtual ElementOutput calculation_e64(ElementInput input);
};

// VFCVT
class VGMFloatCvt : public VGMFloatBase {
  virtual ElementOutput calculation_e8(ElementInput input);
  virtual ElementOutput calculation_e16(ElementInput input);
  virtual ElementOutput calculation_e32(ElementInput input);
  virtual ElementOutput calculation_e64(ElementInput input);
};

// scalar cvt
class SGMFloatCvt : public VGMFloatBase {
  virtual ElementOutput calculation_e8(ElementInput input);
  virtual ElementOutput calculation_e16(ElementInput input);
  virtual ElementOutput calculation_e32(ElementInput input);
  virtual ElementOutput calculation_e64(ElementInput input);
};


class VGMFloatFMA : public VGMFloatBase {
  virtual ElementOutput calculation_e16(ElementInput input);
  virtual ElementOutput calculation_e32(ElementInput input);
  virtual ElementOutput calculation_e64(ElementInput input);
};

class VGMFloatDivider : public VGMFloatBase {
  virtual ElementOutput calculation_e16(ElementInput input);
  virtual ElementOutput calculation_e32(ElementInput input);
  virtual ElementOutput calculation_e64(ElementInput input);
};

class VGMIntegerALU : public VPUGoldenModel {
  virtual ElementOutput calculation_e8(ElementInput input);
  virtual ElementOutput calculation_e16(ElementInput input);
  virtual ElementOutput calculation_e32(ElementInput input);
  virtual ElementOutput calculation_e64(ElementInput input);
};

class VGMPermutation : public VPUGoldenModel {
  public:
    VecOutput get_expected_output(VecInput input);
    virtual ElementOutput calculation_e8(ElementInput input);
    virtual ElementOutput calculation_e16(ElementInput input);
    virtual ElementOutput calculation_e32(ElementInput input);
    virtual ElementOutput calculation_e64(ElementInput input);
    int get_ones_sum_base(int uop_idx, int sew);
  private:
    VecOutput get_output_vslideup(VecInput input);
    VecOutputE8  vslideup_calculation_e8(VSlideInput *input);
    VecOutputE16 vslideup_calculation_e16(VSlideInput *input);
    VecOutputE32 vslideup_calculation_e32(VSlideInput *input);
    VecOutput    vslideup_calculation_e64(VSlideInput *input);
    VecOutput get_output_vslidedown(VecInput input);
    VecOutputE8  vslidedown_calculation_e8(VSlideInput *input);
    VecOutputE16 vslidedown_calculation_e16(VSlideInput *input);
    VecOutputE32 vslidedown_calculation_e32(VSlideInput *input);
    VecOutput    vslidedown_calculation_e64(VSlideInput *input);
    VecOutput get_output_vslide1up(VecInput input);
    VecOutputE8  vslide1up_calculation_e8(VSlideOneInput *input);
    VecOutputE16 vslide1up_calculation_e16(VSlideOneInput *input);
    VecOutputE32 vslide1up_calculation_e32(VSlideOneInput *input);
    VecOutput    vslide1up_calculation_e64(VSlideOneInput *input);
    VecOutput get_output_vslide1down(VecInput input);
    VecOutputE8  vslide1down_calculation_e8(VSlideOneInput *input);
    VecOutputE16 vslide1down_calculation_e16(VSlideOneInput *input);
    VecOutputE32 vslide1down_calculation_e32(VSlideOneInput *input);
    VecOutput    vslide1down_calculation_e64(VSlideOneInput *input);
    VecOutput get_output_vrgather(VecInput input);
    VecOutputE8  vrgather_calculation_e8(VRGatherInput *input);
    VecOutputE16 vrgather_calculation_e16(VRGatherInput *input);
    VecOutputE32 vrgather_calculation_e32(VRGatherInput *input);
    VecOutput    vrgather_calculation_e64(VRGatherInput *input);
    VecOutput get_output_vcompress(VecInput input);
    VecOutputE8  vcompress_calculation_e8(VCompressInput *input);
    VecOutputE16 vcompress_calculation_e16(VCompressInput *input);
    VecOutputE32 vcompress_calculation_e32(VCompressInput *input);
    VecOutput    vcompress_calculation_e64(VCompressInput *input);
};

class VGMIntegerALUF : public VPUGoldenModel {
  public:
    VecOutput get_expected_output(VecInput input);
    virtual ElementOutput calculation_e8(ElementInput input);
    virtual ElementOutput calculation_e16(ElementInput input);
    virtual ElementOutput calculation_e32(ElementInput input);
    virtual ElementOutput calculation_e64(ElementInput input);
  private:
    VecOutput get_output_vialuF(VecInput input);
    VecOutput  vialuF_calculation_vvv(VecInput input);
    VecOutput  vialuF_calculation_vvw(VecInput input);
    VecOutput  vialuF_calculation_wvw(VecInput input);
    VecOutput  vialuF_calculation_wvv(VecInput input);
    VecOutput  vialuF_calculation_22v(VecInput input);
    VecOutput  vialuF_calculation_44v(VecInput input);
    VecOutput  vialuF_calculation_88v(VecInput input);
    VecOutput  vialuF_calculation_vvm(VecInput input);
    VecOutput  vialuF_calculation_mmm(VecInput input);
};
class VGMIntegerDividier : public VPUGoldenModel{
  virtual ElementOutput calculation_e8(ElementInput input);
  virtual ElementOutput calculation_e16(ElementInput input);
  virtual ElementOutput calculation_e32(ElementInput input);
  virtual ElementOutput calculation_e64(ElementInput input); 
};


#endif