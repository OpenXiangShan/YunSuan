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
    VecOutput get_expected_output(VecInput input);
    ElementInput select_element(VecInput input, int idx);

    virtual ElementOutput calculation_e8(ElementInput input) = 0;
    virtual ElementOutput calculation_e16(ElementInput input) = 0;
    virtual ElementOutput calculation_e32(ElementInput input) = 0;
    virtual ElementOutput calculation_e64(ElementInput input) = 0;
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

class VGMFloatDivider : public VGMFloatBase {
  virtual ElementOutput calculation_e16(ElementInput input);
  virtual ElementOutput calculation_e32(ElementInput input);
  virtual ElementOutput calculation_e64(ElementInput input);
};

class VGMIntegerALU : public VPUGoldenModel{
  virtual ElementOutput calculation_e8(ElementInput input);
  virtual ElementOutput calculation_e16(ElementInput input);
  virtual ElementOutput calculation_e32(ElementInput input);
  virtual ElementOutput calculation_e64(ElementInput input);
};

#endif