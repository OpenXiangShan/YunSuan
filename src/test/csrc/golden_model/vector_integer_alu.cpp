#include "../include/gm_common.h"

// bool VGMIntegerALU::bad_fuType(VecInput input) {
//   return false;
// }

// bool VGMIntegerALU::bad_fuOpType(VecInput input) {
//   return false;
// }

ElementOutput VGMIntegerALU::calculation_e8(ElementInput input) {
  ElementOutput output;
  output.result = (uint64_t)((uint8_t)input.src1 + (uint8_t)input.src2);
  output.fflags = 0;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMIntegerALU::calculation_e16(ElementInput input) {
  ElementOutput output;
  output.result = (uint64_t)((uint16_t)input.src1 + (uint16_t)input.src2);
  output.fflags = 0;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMIntegerALU::calculation_e32(ElementInput input) {
  ElementOutput output;
  output.result = (uint64_t)((uint32_t)input.src1 + (uint32_t)input.src2);
  output.fflags = 0;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMIntegerALU::calculation_e64(ElementInput input) {
  ElementOutput output;
  output.result = (uint64_t)(input.src1 + input.src2);
  output.fflags = 0;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}