#ifndef __GM_COMMON_H
#define __GM_COMMON_H

extern "C" {
#include <softfloat.h>
}
#include "iotype.h"
#include "vpu_constant.h"

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
  private:
    VecOutput get_output_vslideup(VecInput input);
    VecOutputE8  vslideup_calculation_e8(VSlideUpInput *input);
    VecOutputE16 vslideup_calculation_e16(VSlideUpInput *input);
    VecOutputE32 vslideup_calculation_e32(VSlideUpInput *input);
    VecOutput    vslideup_calculation_e64(VSlideUpInput *input);
};

#endif