#include "../include/gm_common.h"
#include "../include/vpu_constant.h"

#include <cstring>
#include <gmp.h>
#include <mpfr.h>
#include <stdint.h>
#include <typeinfo>

namespace {

struct Exp2Format {
  const char *name;
  int width;
  int exp_bits;
  int frac_bits;
  int precision;
  int bias;
  int ieee_emin;
  int ieee_emax;
  uint64_t inf_bits;
  uint64_t qnan_bits;
  uint64_t max_finite_bits;
};

struct RoundedValue {
  uint64_t bits = 0;
  uint32_t fflags = 0;
};

constexpr int kExactPrec = 256;

Exp2Format fp16_format() {
  return {"fp16", 16, 5, 10, 11, 15, -14, 15, 0x7c00, 0x7e00, 0x7bff};
}

Exp2Format bf16_format() {
  return {"bf16", 16, 8, 7, 8, 127, -126, 127, 0x7f80, 0x7fc0, 0x7f7f};
}

Exp2Format fp32_format() {
  return {"fp32", 32, 8, 23, 24, 127, -126, 127, 0x7f800000ULL, 0x7fc00000ULL, 0x7f7fffffULL};
}

const Exp2Format &select_format(uint16_t fuOpType, int sew) {
  static const Exp2Format fp16 = fp16_format();
  static const Exp2Format bf16 = bf16_format();
  static const Exp2Format fp32 = fp32_format();
  if (fuOpType == VFEXP2BF16) return bf16;
  if (fuOpType == VFEXP2 && sew == 1) return fp16;
  if (fuOpType == VFEXP2 && sew == 2) return fp32;
  printf("VFEXP2 Unsupported format: fuOpType=%u sew=%d\n", fuOpType, sew);
  exit(1);
}

uint64_t bitmask(int width) {
  return width == 64 ? ~0ULL : ((1ULL << width) - 1ULL);
}

bool is_nan_bits(uint64_t bits, const Exp2Format &fmt) {
  uint64_t exp_mask = (1ULL << fmt.exp_bits) - 1ULL;
  uint64_t frac_mask = (1ULL << fmt.frac_bits) - 1ULL;
  uint64_t exp = (bits >> fmt.frac_bits) & exp_mask;
  uint64_t frac = bits & frac_mask;
  return exp == exp_mask && frac != 0;
}

bool is_snan_bits(uint64_t bits, const Exp2Format &fmt) {
  if (!is_nan_bits(bits, fmt)) return false;
  return ((bits >> (fmt.frac_bits - 1)) & 1ULL) == 0;
}

bool is_inf_bits(uint64_t bits, const Exp2Format &fmt) {
  uint64_t exp_mask = (1ULL << fmt.exp_bits) - 1ULL;
  uint64_t frac_mask = (1ULL << fmt.frac_bits) - 1ULL;
  uint64_t exp = (bits >> fmt.frac_bits) & exp_mask;
  return exp == exp_mask && (bits & frac_mask) == 0;
}

bool is_subnormal_bits(uint64_t bits, const Exp2Format &fmt) {
  uint64_t exp_mask = (1ULL << fmt.exp_bits) - 1ULL;
  uint64_t frac_mask = (1ULL << fmt.frac_bits) - 1ULL;
  uint64_t exp = (bits >> fmt.frac_bits) & exp_mask;
  return exp == 0 && (bits & frac_mask) != 0;
}

void mpfr_set_from_bits(mpfr_t out, uint64_t bits, const Exp2Format &fmt) {
  if ((bits & bitmask(fmt.width)) == 0) {
    mpfr_set_zero(out, 0);
    return;
  }

  const bool negative = ((bits >> (fmt.width - 1)) & 1ULL) != 0;
  const uint64_t frac_mask = (1ULL << fmt.frac_bits) - 1ULL;
  const uint64_t exp_mask = (1ULL << fmt.exp_bits) - 1ULL;
  const uint64_t frac = bits & frac_mask;
  const uint64_t exp = (bits >> fmt.frac_bits) & exp_mask;

  mpz_t mant;
  mpz_init(mant);
  if (exp == 0) {
    mpz_set_ui(mant, frac);
    mpfr_set_z_2exp(out, mant, fmt.ieee_emin - fmt.frac_bits, MPFR_RNDN);
  } else {
    mpz_set_ui(mant, (1ULL << fmt.frac_bits) | frac);
    mpfr_set_z_2exp(out, mant, static_cast<long>(static_cast<int>(exp) - fmt.bias - fmt.frac_bits), MPFR_RNDN);
  }
  if (negative) mpfr_neg(out, out, MPFR_RNDN);
  mpz_clear(mant);
}

void compute_exact_exp2(mpfr_t y, const mpfr_t x) {
  mpfr_exp2(y, x, MPFR_RNDN);
}

uint64_t overflow_result_bits(const Exp2Format &fmt, int rm) {
  return (rm == RM_RTZ || rm == RM_RDN) ? fmt.max_finite_bits : fmt.inf_bits;
}

uint64_t underflow_tiny_result_bits(int rm) {
  switch (rm) {
    case RM_RUP:
    case RM_RTO:
      return 1ULL;
    default:
      return 0ULL;
  }
}

uint64_t round_scaled_positive(const mpfr_t scaled, int rm, bool *inexact) {
  mpz_t floor_z;
  mpz_t ceil_z;
  mpz_init(floor_z);
  mpz_init(ceil_z);
  mpfr_get_z(floor_z, scaled, MPFR_RNDD);

  mpfr_t floor_v;
  mpfr_t frac;
  mpfr_t half;
  mpfr_inits2(kExactPrec, floor_v, frac, half, (mpfr_ptr) 0);
  mpfr_set_z(floor_v, floor_z, MPFR_RNDN);
  mpfr_sub(frac, scaled, floor_v, MPFR_RNDN);
  *inexact = mpfr_zero_p(frac) == 0;

  uint64_t result = mpz_get_ui(floor_z);
  if (!*inexact) {
    mpz_clears(floor_z, ceil_z, (mpz_ptr) 0);
    mpfr_clears(floor_v, frac, half, (mpfr_ptr) 0);
    return result;
  }

  switch (rm) {
    case RM_RTZ:
    case RM_RDN:
      break;
    case RM_RUP:
      mpz_add_ui(ceil_z, floor_z, 1);
      result = mpz_get_ui(ceil_z);
      break;
    case RM_RTO:
      break;
    case RM_RNE:
    case RM_RMM: {
      mpfr_set_d(half, 0.5, MPFR_RNDN);
      const int cmp = mpfr_cmp(frac, half);
      if (cmp > 0 || (cmp == 0 && (rm == RM_RMM || (result & 1ULL)))) {
        mpz_add_ui(ceil_z, floor_z, 1);
        result = mpz_get_ui(ceil_z);
      }
      break;
    }
    default:
      printf("VFEXP2 Unsupported RM:%d\n", rm);
      exit(1);
  }

  mpz_clears(floor_z, ceil_z, (mpz_ptr) 0);
  mpfr_clears(floor_v, frac, half, (mpfr_ptr) 0);
  return result;
}

RoundedValue compute_rounded_exp2(const mpfr_t x, const Exp2Format &fmt, int rm) {
  RoundedValue out;
  mpfr_t exact;
  mpfr_init2(exact, kExactPrec);

  if (mpfr_cmp_si(x, 512) > 0) {
    out.bits = overflow_result_bits(fmt, rm);
    out.fflags = FFLAGS_OF | FFLAGS_NX;
    mpfr_clear(exact);
    return out;
  }
  if (mpfr_cmp_si(x, -512) < 0) {
    out.bits = underflow_tiny_result_bits(rm);
    out.fflags = FFLAGS_UF | FFLAGS_NX;
    mpfr_clear(exact);
    return out;
  }

  compute_exact_exp2(exact, x);

  if (mpfr_zero_p(exact)) {
    out.bits = 0;
    out.fflags = 0;
    mpfr_clear(exact);
    return out;
  }

  const int exp_unbiased = static_cast<int>(mpfr_get_exp(exact)) - 1;
  if (exp_unbiased > fmt.ieee_emax) {
    out.bits = overflow_result_bits(fmt, rm);
    out.fflags = FFLAGS_OF | FFLAGS_NX;
    mpfr_clear(exact);
    return out;
  }

  const bool normal_path = exp_unbiased >= fmt.ieee_emin;
  const int shift = normal_path ? (fmt.frac_bits - exp_unbiased) : (fmt.frac_bits - fmt.ieee_emin);
  mpfr_t scaled;
  mpfr_init2(scaled, kExactPrec);
  mpfr_mul_2si(scaled, exact, shift, MPFR_RNDN);

  bool inexact = false;
  uint64_t rounded = round_scaled_positive(scaled, rm, &inexact);
  mpfr_clear(scaled);

  if (normal_path) {
    if (rounded >= (1ULL << (fmt.frac_bits + 1))) {
      rounded >>= 1;
      if (exp_unbiased + 1 > fmt.ieee_emax) {
        out.bits = overflow_result_bits(fmt, rm);
        out.fflags = FFLAGS_OF | FFLAGS_NX;
        mpfr_clear(exact);
        return out;
      }
      out.bits = (static_cast<uint64_t>(exp_unbiased + 1 + fmt.bias) << fmt.frac_bits) |
                 (rounded & ((1ULL << fmt.frac_bits) - 1ULL));
    } else {
      out.bits = (static_cast<uint64_t>(exp_unbiased + fmt.bias) << fmt.frac_bits) |
                 ((rounded - (1ULL << fmt.frac_bits)) & ((1ULL << fmt.frac_bits) - 1ULL));
    }
    if (rm == RM_RTO && inexact) out.bits |= 1ULL;
    out.fflags = inexact ? FFLAGS_NX : 0;
    mpfr_clear(exact);
    return out;
  }

  if (rounded >= (1ULL << fmt.frac_bits)) {
    out.bits = static_cast<uint64_t>(1ULL << fmt.frac_bits);
    if (rm == RM_RTO && inexact) out.bits |= 1ULL;
    out.fflags = inexact ? FFLAGS_NX : 0;
    mpfr_clear(exact);
    return out;
  }

  out.bits = rounded;
  if (rm == RM_RTO && inexact) out.bits |= 1ULL;
  out.fflags = inexact ? (FFLAGS_UF | FFLAGS_NX) : 0;
  mpfr_clear(exact);
  return out;
}

ElementOutput compute_exp2_reference(ElementInput input, const Exp2Format &fmt) {
  const uint64_t src_bits = input.src1 & bitmask(fmt.width);

  ElementOutput output{};
  if (is_nan_bits(src_bits, fmt)) {
    output.result = fmt.qnan_bits;
    output.fflags = is_snan_bits(src_bits, fmt) ? FFLAGS_NV : 0;
    return output;
  }
  if (is_inf_bits(src_bits, fmt)) {
    output.result = ((src_bits >> (fmt.width - 1)) & 1ULL) ? 0 : fmt.inf_bits;
    output.fflags = 0;
    return output;
  }
  if (is_subnormal_bits(src_bits, fmt)) {
    output.result = static_cast<uint64_t>(fmt.bias) << fmt.frac_bits;
    output.fflags = FFLAGS_NX;
    return output;
  }

  mpfr_t x;
  mpfr_init2(x, kExactPrec);
  mpfr_set_from_bits(x, src_bits, fmt);

  RoundedValue rounded = compute_rounded_exp2(x, fmt, input.rm);
  output.result = rounded.bits;
  output.fflags = rounded.fflags & 0x1f;

  mpfr_clear(x);
  return output;
}

} // namespace

ElementOutput VGMFloatExp2::calculation_e16(ElementInput input) {
  const Exp2Format &fmt = select_format(input.fuOpType, 1);
  ElementOutput output = compute_exp2_reference(input, fmt);
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMFloatExp2::calculation_e32(ElementInput input) {
  const Exp2Format &fmt = select_format(input.fuOpType, 2);
  ElementOutput output = compute_exp2_reference(input, fmt);
  if (verbose) { display_calculation(typeid(this).name(), __func__, input, output); }
  return output;
}

ElementOutput VGMFloatExp2::calculation_e64(ElementInput input) {
  (void) input;
  printf("VFEXP2 does not support e64\n");
  exit(1);
}
