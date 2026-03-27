#include "../include/gm_common.h"
#include "../include/vfpu_functions.h"
#include <typeinfo>
#include <stdint.h>

inline uint64_t mulhu(uint64_t a, uint64_t b) {
  uint64_t t;
  uint32_t y1, y2, y3;
  uint64_t a0 = (uint32_t)a, a1 = a >> 32;
  uint64_t b0 = (uint32_t)b, b1 = b >> 32;

  t = a1*b0 + ((a0*b0) >> 32);
  y1 = t;
  y2 = t >> 32;

  t = a0*b1 + y1;

  t = a1*b1 + y2 + (t >> 32);
  y2 = t;
  y3 = t >> 32;

  return ((uint64_t)y3 << 32) | y2;
}

inline int64_t mulh(int64_t a, int64_t b) {
  int negate = (a < 0) != (b < 0);
  uint64_t res = mulhu(a < 0 ? -a : a, b < 0 ? -b : b);
  return negate ? ~res + (a * b == 0) : res;
}

inline int64_t mulhsu(int64_t a, uint64_t b) {
  int negate = a < 0;
  uint64_t res = mulhu(a < 0 ? -a : a, b);
  return negate ? ~res + (a * b == 0) : res;
}

ElementOutput SGMIntegerMul::calculation_e16(ElementInput input) {
  printf("Integer Mul does not support e16");
  exit(1);
  ElementOutput output;
  output.result = 0;
  return output;
}

ElementOutput SGMIntegerMul::calculation_e32(ElementInput input) {
  printf("Integer Mul does not support e32");
  exit(1);
  ElementOutput output;
  output.result = 0;
  return output;
}

ElementOutput SGMIntegerMul::calculation_e64(ElementInput input) {
  ElementOutput output;
  switch(input.fuOpType) {
    case IMUL_MUL:
      output.result = sext_xlen(input.src1 * input.src2); break;
    case IMUL_MULH:
      output.result = mulh(input.src1, input.src2); break;
    case IMUL_MULHSU:
      output.result = mulhsu(input.src1, input.src2); break;
    case IMUL_MULHU:
      output.result = mulhu(input.src1, input.src2); break;
    case IMUL_MULW:
      output.result = sext32(input.src1 * input.src2); break;
    case IMUL_MULW7:
      output.result = sext32(GET_W7(input.src1) * input.src2); break;
    default:
      printf("Scalar Integer Mul Unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }
  output.fflags = 0;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

