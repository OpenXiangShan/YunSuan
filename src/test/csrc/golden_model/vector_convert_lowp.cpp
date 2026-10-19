#include "../include/gm_common.h"
#include <stdint.h>
#include <string.h>
#include <typeinfo>

// Golden model of the custom low precision conversions:
//   * vfncvtmxfp8_ffw / vfncvtmxfp4_ffw : FP32 -> MXFP8(E4M3) / MXFP4(E2M1), scaled by a
//     broadcast UE8M0 value (factor), see VGMLowPrecisionCvt::calculation_e32
//   * vfncvtxx8_int8 / vfncvtxx8_e4m3 / vfncvtxx8_e5m2 : FP32 -> INT8 / E4M3 / E5M2
//
// This is a faithful port of
//   src/main/scala/yunsuan/vector/VectorConvert/CVT_mxfp.scala
//   src/main/scala/yunsuan/vector/VectorConvert/CVT_xx8.scala
// together with the shared helpers
//   src/main/scala/yunsuan/vector/VectorConvert/util/{CLZ,Rounding,ShiftRightJam}.scala

namespace {

constexpr int kFp32FracWidth = 23;
constexpr int kFp32ExpWidth = 8;
constexpr int kFp32Bias = 127;

uint32_t bit_mask(int width) {
  if (width <= 0) return 0u;
  if (width >= 32) return 0xffffffffu;
  return (1u << width) - 1u;
}

// CLZ.scala: number of leading zeros of the 23 bit fraction of a subnormal value. A fraction of
// zero is reported as 23, which is what the RTL uses for the (unused) zero case.
int leading_zeros23(uint32_t value) {
  int lzc = 0;
  for (int i = kFp32FracWidth - 1; i >= 0; --i) {
    if ((value >> i) & 1u) break;
    ++lzc;
  }
  return lzc;
}

// Rounding.scala: the rounding decision of a single RoundingUnit, `in` only contributes its LSB
// (the guard bit).
struct RoundBit {
  bool inexact;
  bool round_up;
};

RoundBit round_bit(uint32_t in, bool round_in, bool sticky_in, bool sign, uint8_t rm) {
  const bool guard = in & 1u;
  const bool round = round_in;
  const bool sticky = sticky_in;
  const bool inexact = round | sticky;
  bool round_up = false;
  switch (rm) {
    case RM_RNE: round_up = (round && sticky) || (round && !sticky && guard); break;
    case RM_RTZ: round_up = false; break;
    case RM_RUP: round_up = inexact && !sign; break;
    case RM_RDN: round_up = inexact && sign; break;
    case RM_RMM: round_up = round; break;
    case RM_RTO: round_up = (round || sticky) && !guard; break;
    default: round_up = false; break; // MuxLookup default
  }
  RoundBit result;
  result.inexact = inexact;
  result.round_up = round_up;
  return result;
}

// ShiftRightJam.scala
void shift_right_jam(uint64_t value, int shamt, int len, uint64_t *out, bool *sticky) {
  const bool exceed = shamt > len;
  if (exceed || shamt >= 64) {
    *out = 0;
    *sticky = value != 0;
    return;
  }
  const uint64_t len_mask = len >= 64 ? ~0ull : ((1ull << len) - 1ull);
  const uint64_t jam_mask = (shamt == 0 ? 0ull : ((1ull << shamt) - 1ull)) & len_mask;
  *out = value >> shamt;
  *sticky = (value & jam_mask) != 0;
}

struct LowpResult {
  uint8_t result;
  uint8_t fflags; // {NV, -, OF, UF, NX}
};

uint8_t pack_fflags(bool invalid, bool overflow, bool underflow, bool inexact) {
  return (uint8_t)((invalid ? FFLAGS_NV : 0) | (overflow ? FFLAGS_OF : 0) |
                   (underflow ? FFLAGS_UF : 0) | (inexact ? FFLAGS_NX : 0));
}

// Common FP32 decoding shared by all low precision formats.
struct Fp32Parts {
  bool sign;
  bool is_zero;
  bool is_inf;
  bool is_nan;
  bool is_snan;
  uint32_t normalized_sig; // 24 bits, implicit 1 at bit 23
  int source_exp;          // unbiased exponent of the (normalized) input
};

Fp32Parts decode_fp32(uint32_t src) {
  Fp32Parts p;
  const uint32_t exp = (src >> kFp32FracWidth) & bit_mask(kFp32ExpWidth);
  const uint32_t frac = src & bit_mask(kFp32FracWidth);
  const bool exp_is_zero = exp == 0;
  const bool exp_is_ones = exp == bit_mask(kFp32ExpWidth);
  const bool frac_not_zero = frac != 0;
  p.sign = (src >> 31) & 1u;
  p.is_zero = exp_is_zero && !frac_not_zero;
  p.is_inf = exp_is_ones && !frac_not_zero;
  p.is_nan = exp_is_ones && frac_not_zero;
  p.is_snan = p.is_nan && !((frac >> (kFp32FracWidth - 1)) & 1u);
  const int lzc = leading_zeros23(frac);
  const uint32_t normalized_subnormal = (frac << (lzc + 1)) & 0xffffffu;
  p.normalized_sig = exp_is_zero ? normalized_subnormal : (0x800000u | frac);
  p.source_exp = exp_is_zero ? (-kFp32Bias - lzc) : ((int)exp - kFp32Bias);
  return p;
}

// CVT_xx8.scala: class FP32ToFloat8Format
LowpResult fp32_to_float8(uint32_t src, uint8_t rm, int exp_width, int frac_width, int bias,
                          int max_finite_exp, int max_finite_frac, bool has_infinity,
                          uint8_t canonical_nan) {
  const Fp32Parts p = decode_fp32(src);
  const int min_normal_exp = 1 - bias;
  const bool sign = p.sign;

  // normal path
  const uint32_t normal_in =
      (p.normalized_sig >> (kFp32FracWidth - frac_width)) & bit_mask(frac_width + 1);
  const bool normal_round_in = ((p.normalized_sig >> (kFp32FracWidth - 1 - frac_width)) & 1u) != 0;
  const bool normal_sticky =
      (p.normalized_sig & bit_mask(kFp32FracWidth - 1 - frac_width)) != 0;
  const RoundBit normal_rb = round_bit(normal_in, normal_round_in, normal_sticky, sign, rm);
  const uint32_t normal_rounded = normal_in + (normal_rb.round_up ? 1u : 0u);
  const bool normal_carry = ((normal_rounded >> (frac_width + 1)) & 1u) != 0;
  const uint32_t normal_frac = normal_rounded & bit_mask(frac_width);
  const int normal_exp = p.source_exp + bias + (normal_carry ? 1 : 0);
  // vfncvtxx8-spec-zh.md 5.2/5.3 + IEEE 754-2019 7.4: overflow is signalled if and only if the
  // value that would have been rounded with an unbounded exponent range exceeds the largest
  // finite number of the target format. A value that rounds down to the maximum finite value is
  // therefore only inexact (NX), never overflow.
  const bool rounded_overflows =
      normal_exp > max_finite_exp ||
      (normal_exp == max_finite_exp && (int)normal_frac > max_finite_frac);
  const bool use_normal = p.source_exp >= min_normal_exp;
  const uint32_t normal_payload = ((uint32_t)sign << (exp_width + frac_width)) |
      ((uint32_t)(normal_exp & bit_mask(exp_width)) << frac_width) | normal_frac;

  // subnormal path
  const int sub_shift_signed = (kFp32FracWidth + min_normal_exp - frac_width) - p.source_exp;
  const int sub_shift = sub_shift_signed > 31 ? 31 : (sub_shift_signed < 1 ? 1 : sub_shift_signed);
  uint64_t sub_with_round = 0;
  bool sub_sticky = false;
  shift_right_jam(p.normalized_sig, sub_shift - 1, 24, &sub_with_round, &sub_sticky);
  const uint32_t sub_in = (uint32_t)((sub_with_round >> 1) & bit_mask(frac_width));
  const bool sub_round_in = (sub_with_round & 1u) != 0;
  const RoundBit sub_rb = round_bit(sub_in, sub_round_in, sub_sticky, sign, rm);
  const uint32_t sub_rounded = sub_in + (sub_rb.round_up ? 1u : 0u);
  const bool sub_rounds_to_normal = ((sub_rounded >> frac_width) & 1u) != 0;
  const uint32_t sub_frac = sub_rounded & bit_mask(frac_width);
  const uint32_t sub_payload = ((uint32_t)sign << (exp_width + frac_width)) |
      (sub_rounds_to_normal ? (1u << frac_width) : 0u) | (sub_rounds_to_normal ? 0u : sub_frac);

  const uint32_t max_finite_payload =
      ((uint32_t)sign << (exp_width + frac_width)) | ((uint32_t)max_finite_exp << frac_width) |
      (uint32_t)max_finite_frac;
  const uint32_t infinity_payload =
      ((uint32_t)sign << (exp_width + frac_width)) | (bit_mask(exp_width) << frac_width);
  const bool to_infinity = rm == RM_RNE || rm == RM_RMM || (rm == RM_RUP && !sign) ||
      (rm == RM_RDN && sign);
  const uint32_t overflow_payload =
      has_infinity ? (to_infinity ? infinity_payload : max_finite_payload) : max_finite_payload;
  const uint32_t infinity_result = has_infinity ? infinity_payload : max_finite_payload;

  const bool finite_overflow =
      !p.is_zero && !p.is_inf && !p.is_nan && use_normal && rounded_overflows;
  const bool infinity_overflow = p.is_inf && !has_infinity;
  const bool overflow = finite_overflow || infinity_overflow;
  const bool rounded_inexact = use_normal ? normal_rb.inexact : sub_rb.inexact;
  const bool underflow = !p.is_nan && !p.is_inf && !p.is_zero && !use_normal &&
      sub_rb.inexact && !sub_rounds_to_normal;
  const bool inexact = overflow || (!p.is_nan && !p.is_inf && !p.is_zero && rounded_inexact);

  LowpResult out;
  out.result = p.is_nan ? canonical_nan
      : p.is_inf ? (uint8_t)infinity_result
      : finite_overflow ? (uint8_t)overflow_payload
      : p.is_zero ? (uint8_t)((uint32_t)sign << (exp_width + frac_width))
      : (uint8_t)(use_normal ? normal_payload : sub_payload);
  out.fflags = pack_fflags(p.is_snan, overflow, underflow, inexact);
  return out;
}

// CVT_mxfp.scala: class FP32ToMXFPFormat
LowpResult fp32_to_mxfp_format(uint32_t src, uint8_t factor, uint8_t rm, int exp_width,
                               int frac_width, int bias, int max_finite_frac, bool has_nan) {
  const Fp32Parts p = decode_fp32(src);
  const int min_normal_exp = 1 - bias;
  const int max_exp_field = (1 << exp_width) - 1;
  const bool sign = p.sign;

  // The UE8M0 scale value is subtracted from the FP32 exponent.
  const int scaled_exp = p.source_exp - (int)factor + kFp32Bias;
  const bool use_normal = scaled_exp >= min_normal_exp;

  // normal path
  const uint32_t normal_in =
      (p.normalized_sig >> (kFp32FracWidth - frac_width)) & bit_mask(frac_width + 1);
  const bool normal_round_in = ((p.normalized_sig >> (kFp32FracWidth - 1 - frac_width)) & 1u) != 0;
  const bool normal_sticky =
      (p.normalized_sig & bit_mask(kFp32FracWidth - 1 - frac_width)) != 0;
  const RoundBit normal_rb = round_bit(normal_in, normal_round_in, normal_sticky, sign, rm);
  const uint32_t normal_rounded = normal_in + (normal_rb.round_up ? 1u : 0u);
  const bool normal_carry = ((normal_rounded >> (frac_width + 1)) & 1u) != 0;
  const uint32_t normal_frac = normal_rounded & bit_mask(frac_width);
  const int normal_exp = scaled_exp + bias + (normal_carry ? 1 : 0);
  const bool normal_overflows = normal_exp > max_exp_field ||
      (normal_exp == max_exp_field && (int)normal_frac > max_finite_frac);
  const uint32_t normal_payload = ((uint32_t)sign << (exp_width + frac_width)) |
      ((uint32_t)(normal_exp & bit_mask(exp_width)) << frac_width) | normal_frac;

  // subnormal path
  const int sub_shift_signed = (kFp32FracWidth + min_normal_exp - frac_width) - scaled_exp;
  const int sub_shift = sub_shift_signed > 31 ? 31 : (sub_shift_signed < 1 ? 1 : sub_shift_signed);
  uint64_t sub_with_round = 0;
  bool sub_sticky = false;
  shift_right_jam(p.normalized_sig, sub_shift - 1, 24, &sub_with_round, &sub_sticky);
  const uint32_t sub_in = (uint32_t)((sub_with_round >> 1) & bit_mask(frac_width));
  const bool sub_round_in = (sub_with_round & 1u) != 0;
  const RoundBit sub_rb = round_bit(sub_in, sub_round_in, sub_sticky, sign, rm);
  const uint32_t sub_rounded = sub_in + (sub_rb.round_up ? 1u : 0u);
  const bool sub_rounds_to_normal = ((sub_rounded >> frac_width) & 1u) != 0;
  const uint32_t sub_frac = sub_rounded & bit_mask(frac_width);
  const uint32_t sub_payload = ((uint32_t)sign << (exp_width + frac_width)) |
      (sub_rounds_to_normal ? (1u << frac_width) : 0u) | (sub_rounds_to_normal ? 0u : sub_frac);

  const uint32_t max_finite_payload = ((uint32_t)sign << (exp_width + frac_width)) |
      ((uint32_t)max_exp_field << frac_width) | (uint32_t)max_finite_frac;
  const uint32_t positive_max_finite =
      ((uint32_t)max_exp_field << frac_width) | (uint32_t)max_finite_frac;
  const uint32_t nan_payload = has_nan
      ? ((uint32_t)max_exp_field << frac_width) | bit_mask(frac_width)
      : positive_max_finite;
  const bool factor_is_nan = factor == bit_mask(8);
  const bool nan_like = p.is_nan || factor_is_nan;
  const bool overflow = !nan_like && (p.is_inf || (!p.is_zero && use_normal && normal_overflows));
  const bool underflow = !nan_like && !p.is_inf && !p.is_zero && !use_normal && sub_rb.inexact &&
      !sub_rounds_to_normal;
  const bool rounded_inexact = use_normal ? normal_rb.inexact : sub_rb.inexact;
  const bool inexact =
      overflow || (!nan_like && !p.is_inf && !p.is_zero && rounded_inexact);
  const bool invalid = has_nan ? p.is_snan : nan_like;

  LowpResult out;
  out.result = nan_like ? (uint8_t)nan_payload
      : overflow ? (uint8_t)max_finite_payload
      : p.is_zero ? (uint8_t)((uint32_t)sign << (exp_width + frac_width))
      : (uint8_t)(use_normal ? normal_payload : sub_payload);
  out.fflags = pack_fflags(invalid, overflow, underflow, inexact);
  return out;
}

// CVT_xx8.scala: class FP32ToInt8
LowpResult fp32_to_int8(uint32_t src, uint8_t rm) {
  const Fp32Parts p = decode_fp32(src);
  const bool sign = p.sign;

  const int shift_signed = kFp32FracWidth - p.source_exp;
  const int shift = shift_signed > 31 ? 31 : (shift_signed < 1 ? 1 : shift_signed);
  uint64_t with_round = 0;
  bool sticky = false;
  shift_right_jam(p.normalized_sig, shift - 1, 24, &with_round, &sticky);
  const uint32_t rounder_in = (uint32_t)((with_round >> 1) & bit_mask(8));
  const bool round_in = (with_round & 1u) != 0;
  const RoundBit rb = round_bit(rounder_in, round_in, sticky, sign, rm);
  const uint32_t rounded_magnitude = rounder_in + (rb.round_up ? 1u : 0u);

  const uint32_t magnitude_limit = sign ? 128u : 127u;
  const bool exponent_overflow = p.source_exp > 7;
  const bool finite_invalid = !p.is_zero && !p.is_inf && !p.is_nan &&
      (exponent_overflow || rounded_magnitude > magnitude_limit);
  const bool invalid = p.is_inf || p.is_nan || finite_invalid;
  const uint8_t signed_result = sign
      ? (uint8_t)(((~rounded_magnitude) & 0xffu) + 1u)
      : (uint8_t)(rounded_magnitude & 0xffu);

  LowpResult out;
  out.result = invalid ? (uint8_t)((sign && !p.is_nan) ? 0x80u : 0x7fu)
      : p.is_zero ? 0u
      : signed_result;
  out.fflags = pack_fflags(invalid, false, false, !invalid && !p.is_zero && rb.inexact);
  return out;
}

} // namespace

VecOutput VGMLowPrecisionCvt::get_expected_output(VecInput input) {
  const VecInputE32 *input32 = (const VecInputE32 *)&input;
  const bool is_fp4 = input.fuOpType == VFNCVT_MXFP4_FFW;
  // Both low precision families convert two FP32 source elements per 64 bit source word, the
  // packed result uses 8 (E4M3/E5M2/INT8) or 4 (E2M1) bit per element.
  const int lane_bits = is_fp4 ? 4 : 8;
  const uint32_t lane_mask = bit_mask(lane_bits);
  const int src_lanes_per_word = 2;

  VecOutput output;
  for (int i = 0; i < 2; i++) {
    output.result[i] = 0;
    output.fflags[i] = 0;
    for (int j = 0; j < src_lanes_per_word; j++) {
      ElementInput element;
      memset(&element, 0, sizeof(element));
      element.src1 = (uint64_t)input32->src1[i * src_lanes_per_word + j];
      element.fuOpType = input.fuOpType;
      element.rm = input.rm;
      element.factor = input.factor;
      element.isMXFP = input.isMXFP;
      const ElementOutput part = calculation_e32(element);
      output.result[i] |= (uint64_t)(part.result & lane_mask) << (j * lane_bits);
      output.fflags[i] |= (uint32_t)(part.fflags & 0x1f) << (j * 5);
    }
  }
  return output;
}

ElementOutput VGMLowPrecisionCvt::calculation_e8(ElementInput input) {
  printf("LowPrecisionCvt does not support e8 input element (fuOpType %d)\n", input.fuOpType);
  exit(1);
}

ElementOutput VGMLowPrecisionCvt::calculation_e16(ElementInput input) {
  printf("LowPrecisionCvt does not support e16 input element (fuOpType %d)\n", input.fuOpType);
  exit(1);
}

ElementOutput VGMLowPrecisionCvt::calculation_e32(ElementInput input) {
  LowpResult lowp;
  switch (input.fuOpType) {
    case VFNCVT_MXFP8_FFW:
      lowp = fp32_to_mxfp_format((uint32_t)input.src1, input.factor, input.rm, 4, 3, 7, 6, true);
      break;
    case VFNCVT_MXFP4_FFW:
      lowp = fp32_to_mxfp_format((uint32_t)input.src1, input.factor, input.rm, 2, 1, 1, 1, false);
      break;
    case VFNCVTXX8_INT8:
      lowp = fp32_to_int8((uint32_t)input.src1, input.rm);
      break;
    case VFNCVTXX8_E4M3:
      lowp = fp32_to_float8((uint32_t)input.src1, input.rm, 4, 3, 7, 15, 6, false, 0x7f);
      break;
    case VFNCVTXX8_E5M2:
      lowp = fp32_to_float8((uint32_t)input.src1, input.rm, 5, 2, 15, 30, 3, true, 0x7e);
      break;
    default:
      printf("LowPrecisionCvt unsupported fuOpType %d\n", input.fuOpType);
      exit(1);
  }
  ElementOutput output;
  output.result = lowp.result;
  output.fflags = lowp.fflags;
  output.vxsat = false;
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMLowPrecisionCvt::calculation_e64(ElementInput input) {
  printf("LowPrecisionCvt does not support e64 input element (fuOpType %d)\n", input.fuOpType);
  exit(1);
}
