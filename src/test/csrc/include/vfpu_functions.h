#include "gm_common.h"
#include <softfloat.h>
#include "assert.h"

extern "C"{
#include <specialize.h>
#include <internals.h>
}

#define ui8_fromPosOverflow  0xFF
#define ui8_fromNegOverflow  0
#define ui8_fromNaN          0xFF
#define i8_fromPosOverflow   0x7F
#define i8_fromNegOverflow   (-0x7F - 1)
#define i8_fromNaN           0x7F

#define ui16_fromPosOverflow 0xFFFF
#define ui16_fromNegOverflow 0
#define ui16_fromNaN         0xFFFF
#define i16_fromPosOverflow  0x7FFF
#define i16_fromNegOverflow  (-0x7FFF - 1)
#define i16_fromNaN          0x7FFF


const  uint16_t defaultNaN_ui16 = defaultNaNF16UI;
const  uint32_t defaultNaN_ui32 = defaultNaNF32UI;
const  uint64_t defaultNaN_ui64 = defaultNaNF64UI;
const inline float16_t *defaultNaN_f16 = (float16_t *) &defaultNaN_ui16;
const inline float32_t *defaultNaN_f32 = (float32_t *) &defaultNaN_ui32;
const inline float64_t *defaultNaN_f64 = (float64_t *) &defaultNaN_ui64;

static inline float16_t f16_min(float16_t a, float16_t b) {
  bool less = f16_lt_quiet(a, b) || (f16_eq(a, b) && (a.v & F16_SIGN));
  if(isNaNF16UI(a.v) && isNaNF16UI(b.v)) return *defaultNaN_f16;
  else return(less || isNaNF16UI(b.v) ? a : b);
}

static inline float32_t f32_min(float32_t a, float32_t b) {
  bool less = f32_lt_quiet(a, b) || (f32_eq(a, b) && (a.v & F32_SIGN));
  if(isNaNF32UI(a.v) && isNaNF32UI(b.v)) return *defaultNaN_f32;
  else return(less || isNaNF32UI(b.v) ? a : b);
}

static inline float64_t f64_min(float64_t a, float64_t b) {
  bool less = f64_lt_quiet(a, b) || (f64_eq(a, b) && (a.v & F64_SIGN));
  if(isNaNF64UI(a.v) && isNaNF64UI(b.v)) return *defaultNaN_f64;
  else return(less || isNaNF64UI(b.v) ? a : b);
}

static inline float16_t f16_max(float16_t a, float16_t b){
  bool greater = f16_lt_quiet(b, a) || (f16_eq(b, a) && (b.v & F16_SIGN));
  if(isNaNF16UI(a.v) && isNaNF16UI(b.v)) return *defaultNaN_f16;
  else return(greater || isNaNF16UI(b.v) ? a : b);
}

static inline float32_t f32_max(float32_t a, float32_t b){
  bool greater = f32_lt_quiet(b, a) || (f32_eq(b, a) && (b.v & F32_SIGN));
  if(isNaNF32UI(a.v) && isNaNF32UI(b.v)) return *defaultNaN_f32;
  else return(greater || isNaNF32UI(b.v) ? a : b);
}

static inline float64_t f64_max(float64_t a, float64_t b){
  bool greater = f64_lt_quiet(b, a) || (f64_eq(b, a) && (b.v & F64_SIGN));
  if(isNaNF64UI(a.v) && isNaNF64UI(b.v)) return *defaultNaN_f64;
  else return(greater || isNaNF64UI(b.v) ? a : b);
}

static inline float16_t f16(uint16_t v) { return { v }; }
static inline float32_t f32(uint32_t v) { return { v }; }
static inline float64_t f64(uint64_t v) { return { v }; }
#define f16_sgnj(a, b, n, x) \
  f16(((a).v & ~F16_SIGN) | ((((x) ? (a).v : (n) ? F16_SIGN : 0) ^ (b).v) & F16_SIGN))
#define f32_sgnj(a, b, n, x) \
  f32(((a).v & ~F32_SIGN) | ((((x) ? (a).v : (n) ? F32_SIGN : 0) ^ (b).v) & F32_SIGN))
#define f64_sgnj(a, b, n, x) \
  f64(((a).v & ~F64_SIGN) | ((((x) ? (a).v : (n) ? F64_SIGN : 0) ^ (b).v) & F64_SIGN))

uint_fast16_t inline f16_classify( float16_t a )
{
    union ui16_f16 uA;
    uint_fast16_t uiA;

    uA.f = a;
    uiA = uA.ui;

    uint_fast16_t infOrNaN = expF16UI( uiA ) == 0x1F;
    uint_fast16_t subnormalOrZero = expF16UI( uiA ) == 0;
    bool sign = signF16UI( uiA );
    bool fracZero = fracF16UI( uiA ) == 0;
    bool isNaN = isNaNF16UI( uiA );
    bool isSNaN = softfloat_isSigNaNF16UI( uiA );

    return
        (  sign && infOrNaN && fracZero )          << 0 |
        (  sign && !infOrNaN && !subnormalOrZero ) << 1 |
        (  sign && subnormalOrZero && !fracZero )  << 2 |
        (  sign && subnormalOrZero && fracZero )   << 3 |
        ( !sign && infOrNaN && fracZero )          << 7 |
        ( !sign && !infOrNaN && !subnormalOrZero ) << 6 |
        ( !sign && subnormalOrZero && !fracZero )  << 5 |
        ( !sign && subnormalOrZero && fracZero )   << 4 |
        ( isNaN &&  isSNaN )                       << 8 |
        ( isNaN && !isSNaN )                       << 9;
}

uint_fast16_t inline f32_classify( float32_t a )
{
    union ui32_f32 uA;
    uint_fast32_t uiA;

    uA.f = a;
    uiA = uA.ui;

    uint_fast16_t infOrNaN = expF32UI( uiA ) == 0xFF;
    uint_fast16_t subnormalOrZero = expF32UI( uiA ) == 0;
    bool sign = signF32UI( uiA );
    bool fracZero = fracF32UI( uiA ) == 0;
    bool isNaN = isNaNF32UI( uiA );
    bool isSNaN = softfloat_isSigNaNF32UI( uiA );

    return
        (  sign && infOrNaN && fracZero )          << 0 |
        (  sign && !infOrNaN && !subnormalOrZero ) << 1 |
        (  sign && subnormalOrZero && !fracZero )  << 2 |
        (  sign && subnormalOrZero && fracZero )   << 3 |
        ( !sign && infOrNaN && fracZero )          << 7 |
        ( !sign && !infOrNaN && !subnormalOrZero ) << 6 |
        ( !sign && subnormalOrZero && !fracZero )  << 5 |
        ( !sign && subnormalOrZero && fracZero )   << 4 |
        ( isNaN &&  isSNaN )                       << 8 |
        ( isNaN && !isSNaN )                       << 9;
}

uint_fast16_t inline f64_classify( float64_t a )
{
    union ui64_f64 uA;
    uint_fast64_t uiA;

    uA.f = a;
    uiA = uA.ui;

    uint_fast16_t infOrNaN = expF64UI( uiA ) == 0x7FF;
    uint_fast16_t subnormalOrZero = expF64UI( uiA ) == 0;
    bool sign = signF64UI( uiA );
    bool fracZero = fracF64UI( uiA ) == 0;
    bool isNaN = isNaNF64UI( uiA );
    bool isSNaN = softfloat_isSigNaNF64UI( uiA );

    return
        (  sign && infOrNaN && fracZero )          << 0 |
        (  sign && !infOrNaN && !subnormalOrZero ) << 1 |
        (  sign && subnormalOrZero && !fracZero )  << 2 |
        (  sign && subnormalOrZero && fracZero )   << 3 |
        ( !sign && infOrNaN && fracZero )          << 7 |
        ( !sign && !infOrNaN && !subnormalOrZero ) << 6 |
        ( !sign && subnormalOrZero && !fracZero )  << 5 |
        ( !sign && subnormalOrZero && fracZero )   << 4 |
        ( isNaN &&  isSNaN )                       << 8 |
        ( isNaN && !isSNaN )                       << 9;
}


uint_fast16_t inline f16_to_ui16( float16_t a, uint_fast8_t roundingMode, bool exact )
{
    uint_fast8_t old_flags = softfloat_exceptionFlags;

    uint_fast32_t sig32 = f16_to_ui32(a, roundingMode, exact);

    if (sig32 > UINT16_MAX) {
        softfloat_exceptionFlags = old_flags | softfloat_flag_invalid;
        return ui16_fromPosOverflow;
    } else {
        return sig32;
    }
}

int_fast16_t  inline f16_to_i16( float16_t a, uint_fast8_t roundingMode, bool exact )
{
    uint_fast8_t old_flags = softfloat_exceptionFlags;

    int_fast32_t sig32 = f16_to_i32(a, roundingMode, exact);

    if (sig32 > INT16_MAX) {
        softfloat_exceptionFlags = old_flags | softfloat_flag_invalid;
        return i16_fromPosOverflow;
    } else if (sig32 < INT16_MIN) {
        softfloat_exceptionFlags = old_flags | softfloat_flag_invalid;
        return i16_fromNegOverflow;
    } else {
        return sig32;
    }
}

uint_fast8_t  inline f16_to_ui8( float16_t a, uint_fast8_t roundingMode, bool exact )
{
    uint_fast8_t old_flags = softfloat_exceptionFlags;

    uint_fast32_t sig32 = f16_to_ui32(a, roundingMode, exact);

    if (sig32 > UINT8_MAX) {
        softfloat_exceptionFlags = old_flags | softfloat_flag_invalid;
        return ui8_fromPosOverflow;
    } else {
        return sig32;
    }
}

int_fast8_t  inline f16_to_i8( float16_t a, uint_fast8_t roundingMode, bool exact )
{
    uint_fast8_t old_flags = softfloat_exceptionFlags;

    int_fast32_t sig32 = f16_to_i32(a, roundingMode, exact);

    if (sig32 > INT8_MAX) {
        softfloat_exceptionFlags = old_flags | softfloat_flag_invalid;
        return i8_fromPosOverflow;
    } else if (sig32 < INT8_MIN) {
        softfloat_exceptionFlags = old_flags | softfloat_flag_invalid;
        return i8_fromNegOverflow;
    } else {
        return sig32;
    }
}

uint_fast16_t inline  f32_to_ui16( float32_t a, uint_fast8_t roundingMode, bool exact )
{
    uint_fast8_t old_flags = softfloat_exceptionFlags;

    uint_fast32_t sig32 = f32_to_ui32(a, roundingMode, exact);

    if (sig32 > UINT16_MAX) {
        softfloat_exceptionFlags = old_flags | softfloat_flag_invalid;
        return ui16_fromPosOverflow;
    } else {
        return sig32;
    }
}

int_fast16_t  inline f32_to_i16( float32_t a, uint_fast8_t roundingMode, bool exact )
{
    uint_fast8_t old_flags = softfloat_exceptionFlags;

    int_fast32_t sig32 = f32_to_i32(a, roundingMode, exact);

    if (sig32 > INT16_MAX) {
        softfloat_exceptionFlags = old_flags | softfloat_flag_invalid;
        return i16_fromPosOverflow;
    } else if (sig32 < INT16_MIN) {
        softfloat_exceptionFlags = old_flags | softfloat_flag_invalid;
        return i16_fromNegOverflow;
    } else {
        return sig32;
    }
}

static inline uint64_t extract64(uint64_t val, int pos, int len)
{
  assert(pos >= 0 && len > 0 && len <= 64 - pos);
  return (val >> pos) & (~UINT64_C(0) >> (64 - len));
}

static inline uint64_t make_mask64(int pos, int len)
{
    assert(pos >= 0 && len > 0 && pos < 64 && len <= 64);
    return (UINT64_MAX >> (64 - len)) << pos;
}


//user needs to truncate output to required length
inline uint64_t rsqrte7(uint64_t val, int e, int s, bool sub) {
  uint64_t exp = extract64(val, s, e);
  uint64_t sig = extract64(val, 0, s);
  uint64_t sign = extract64(val, s + e, 1);
  const int p = 7;

  static const uint8_t table[] = {
      52, 51, 50, 48, 47, 46, 44, 43,
      42, 41, 40, 39, 38, 36, 35, 34,
      33, 32, 31, 30, 30, 29, 28, 27,
      26, 25, 24, 23, 23, 22, 21, 20,
      19, 19, 18, 17, 16, 16, 15, 14,
      14, 13, 12, 12, 11, 10, 10, 9,
      9, 8, 7, 7, 6, 6, 5, 4,
      4, 3, 3, 2, 2, 1, 1, 0,
      127, 125, 123, 121, 119, 118, 116, 114,
      113, 111, 109, 108, 106, 105, 103, 102,
      100, 99, 97, 96, 95, 93, 92, 91,
      90, 88, 87, 86, 85, 84, 83, 82,
      80, 79, 78, 77, 76, 75, 74, 73,
      72, 71, 70, 70, 69, 68, 67, 66,
      65, 64, 63, 63, 62, 61, 60, 59,
      59, 58, 57, 56, 56, 55, 54, 53};

  if (sub) {
      while (extract64(sig, s - 1, 1) == 0)
          exp--, sig <<= 1;

      sig = (sig << 1) & make_mask64(0 ,s);
  }

  int idx = ((exp & 1) << (p-1)) | (sig >> (s-p+1));
  uint64_t out_sig = (uint64_t)(table[idx]) << (s-p);
  uint64_t out_exp = (3 * make_mask64(0, e - 1) + ~exp) / 2;

  return (sign << (s+e)) | (out_exp << s) | out_sig;
}

inline float16_t f16_rsqrte7(float16_t in)
{
    union ui16_f16 uA;

    uA.f = in;
    unsigned int ret = f16_classify(in);
    bool sub = false;
    switch(ret) {
    case 0x001: // -inf
    case 0x002: // -normal
    case 0x004: // -subnormal
    case 0x100: // sNaN
        softfloat_exceptionFlags |= softfloat_flag_invalid;
    case 0x200: //qNaN
        uA.ui = defaultNaNF16UI;
        break;
    case 0x008: // -0
        uA.ui = 0xfc00;
        softfloat_exceptionFlags |= softfloat_flag_infinite;
        break;
    case 0x010: // +0
        uA.ui = 0x7c00;
        softfloat_exceptionFlags |= softfloat_flag_infinite;
        break;
    case 0x080: //+inf
        uA.ui = 0x0;
        break;
    case 0x020: //+ sub
        sub = true;
    default: // +num
        uA.ui = rsqrte7(uA.ui, 5, 10, sub);
        break;
    }

    return uA.f;
}

inline float32_t f32_rsqrte7(float32_t in)
{
    union ui32_f32 uA;

    uA.f = in;
    unsigned int ret = f32_classify(in);
    bool sub = false;
    switch(ret) {
    case 0x001: // -inf
    case 0x002: // -normal
    case 0x004: // -subnormal
    case 0x100: // sNaN
        softfloat_exceptionFlags |= softfloat_flag_invalid;
    case 0x200: //qNaN
        uA.ui = defaultNaNF32UI;
        break;
    case 0x008: // -0
        uA.ui = 0xff800000;
        softfloat_exceptionFlags |= softfloat_flag_infinite;
        break;
    case 0x010: // +0
        uA.ui = 0x7f800000;
        softfloat_exceptionFlags |= softfloat_flag_infinite;
        break;
    case 0x080: //+inf
        uA.ui = 0x0;
        break;
    case 0x020: //+ sub
        sub = true;
    default: // +num
        uA.ui = rsqrte7(uA.ui, 8, 23, sub);
        break;
    }

    return uA.f;
}

inline float64_t f64_rsqrte7(float64_t in)
{
    union ui64_f64 uA;

    uA.f = in;
    unsigned int ret = f64_classify(in);
    bool sub = false;
    switch(ret) {
    case 0x001: // -inf
    case 0x002: // -normal
    case 0x004: // -subnormal
    case 0x100: // sNaN
        softfloat_exceptionFlags |= softfloat_flag_invalid;
    case 0x200: //qNaN
        uA.ui = defaultNaNF64UI;
        break;
    case 0x008: // -0
        uA.ui = 0xfff0000000000000ul;
        softfloat_exceptionFlags |= softfloat_flag_infinite;
        break;
    case 0x010: // +0
        uA.ui = 0x7ff0000000000000ul;
        softfloat_exceptionFlags |= softfloat_flag_infinite;
        break;
    case 0x080: //+inf
        uA.ui = 0x0;
        break;
    case 0x020: //+ sub
        sub = true;
    default: // +num
        uA.ui = rsqrte7(uA.ui, 11, 52, sub);
        break;
    }

    return uA.f;
}

//user needs to truncate output to required length
inline uint64_t recip7(uint64_t val, int e, int s, int rm, bool sub,
                              bool *round_abnormal)
{
    uint64_t exp = extract64(val, s, e);
    uint64_t sig = extract64(val, 0, s);
    uint64_t sign = extract64(val, s + e, 1);
    const int p = 7;

    static const uint8_t table[] = {
        127, 125, 123, 121, 119, 117, 116, 114,
        112, 110, 109, 107, 105, 104, 102, 100,
        99, 97, 96, 94, 93, 91, 90, 88,
        87, 85, 84, 83, 81, 80, 79, 77,
        76, 75, 74, 72, 71, 70, 69, 68,
        66, 65, 64, 63, 62, 61, 60, 59,
        58, 57, 56, 55, 54, 53, 52, 51,
        50, 49, 48, 47, 46, 45, 44, 43,
        42, 41, 40, 40, 39, 38, 37, 36,
        35, 35, 34, 33, 32, 31, 31, 30,
        29, 28, 28, 27, 26, 25, 25, 24,
        23, 23, 22, 21, 21, 20, 19, 19,
        18, 17, 17, 16, 15, 15, 14, 14,
        13, 12, 12, 11, 11, 10, 9, 9,
        8, 8, 7, 7, 6, 5, 5, 4,
        4, 3, 3, 2, 2, 1, 1, 0};

    if (sub) {
        while (extract64(sig, s - 1, 1) == 0)
            exp--, sig <<= 1;

        sig = (sig << 1) & make_mask64(0 ,s);

        if (exp != 0 && exp != UINT64_MAX) {
            *round_abnormal = true;
            if (rm == 1 ||
                (rm == 2 && !sign) ||
                (rm == 3 && sign))
                return ((sign << (s+e)) | make_mask64(s, e)) - 1;
            else
                return (sign << (s+e)) | make_mask64(s, e);
        }
    }

    int idx = sig >> (s-p);
    uint64_t out_sig = (uint64_t)(table[idx]) << (s-p);
    uint64_t out_exp = 2 * make_mask64(0, e - 1) + ~exp;
    if (out_exp == 0 || out_exp == UINT64_MAX) {
        out_sig = (out_sig >> 1) | make_mask64(s - 1, 1);
        if (out_exp == UINT64_MAX) {
            out_sig >>= 1;
            out_exp = 0;
        }
    }

    return (sign << (s+e)) | (out_exp << s) | out_sig;
}

inline float16_t f16_recip7(float16_t in)
{
    union ui16_f16 uA;

    uA.f = in;
    unsigned int ret = f16_classify(in);
    bool sub = false;
    bool round_abnormal = false;
    switch(ret) {
    case 0x001: // -inf
        uA.ui = 0x8000;
        break;
    case 0x080: //+inf
        uA.ui = 0x0;
        break;
    case 0x008: // -0
        uA.ui = 0xfc00;
        softfloat_exceptionFlags |= softfloat_flag_infinite;
        break;
    case 0x010: // +0
        uA.ui = 0x7c00;
        softfloat_exceptionFlags |= softfloat_flag_infinite;
        break;
    case 0x100: // sNaN
        softfloat_exceptionFlags |= softfloat_flag_invalid;
    case 0x200: //qNaN
        uA.ui = defaultNaNF16UI;
        break;
    case 0x004: // -subnormal
    case 0x020: //+ sub
        sub = true;
    default: // +- normal
        uA.ui = recip7(uA.ui, 5, 10,
                       softfloat_roundingMode, sub, &round_abnormal);
        if (round_abnormal)
            softfloat_exceptionFlags |= softfloat_flag_inexact |
                                        softfloat_flag_overflow;
        break;
    }

    return uA.f;
}

inline float32_t f32_recip7(float32_t in)
{
    union ui32_f32 uA;

    uA.f = in;
    unsigned int ret = f32_classify(in);
    bool sub = false;
    bool round_abnormal = false;
    switch(ret) {
    case 0x001: // -inf
        uA.ui = 0x80000000;
        break;
    case 0x080: //+inf
        uA.ui = 0x0;
        break;
    case 0x008: // -0
        uA.ui = 0xff800000;
        softfloat_exceptionFlags |= softfloat_flag_infinite;
        break;
    case 0x010: // +0
        uA.ui = 0x7f800000;
        softfloat_exceptionFlags |= softfloat_flag_infinite;
        break;
    case 0x100: // sNaN
        softfloat_exceptionFlags |= softfloat_flag_invalid;
    case 0x200: //qNaN
        uA.ui = defaultNaNF32UI;
        break;
    case 0x004: // -subnormal
    case 0x020: //+ sub
        sub = true;
    default: // +- normal
        uA.ui = recip7(uA.ui, 8, 23,
                       softfloat_roundingMode, sub, &round_abnormal);
        if (round_abnormal)
          softfloat_exceptionFlags |= softfloat_flag_inexact |
                                      softfloat_flag_overflow;
        break;
    }

    return uA.f;
}

inline float64_t f64_recip7(float64_t in)
{
    union ui64_f64 uA;

    uA.f = in;
    unsigned int ret = f64_classify(in);
    bool sub = false;
    bool round_abnormal = false;
    switch(ret) {
    case 0x001: // -inf
        uA.ui = 0x8000000000000000;
        break;
    case 0x080: //+inf
        uA.ui = 0x0;
        break;
    case 0x008: // -0
        uA.ui = 0xfff0000000000000;
        softfloat_exceptionFlags |= softfloat_flag_infinite;
        break;
    case 0x010: // +0
        uA.ui = 0x7ff0000000000000;
        softfloat_exceptionFlags |= softfloat_flag_infinite;
        break;
    case 0x100: // sNaN
        softfloat_exceptionFlags |= softfloat_flag_invalid;
    case 0x200: //qNaN
        uA.ui = defaultNaNF64UI;
        break;
    case 0x004: // -subnormal
    case 0x020: //+ sub
        sub = true;
    default: // +- normal
        uA.ui = recip7(uA.ui, 11, 52,
                       softfloat_roundingMode, sub, &round_abnormal);
        if (round_abnormal)
            softfloat_exceptionFlags |= softfloat_flag_inexact |
                                        softfloat_flag_overflow;
        break;
    }

    return uA.f;
}
