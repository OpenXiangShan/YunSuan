#include "gm_common.h"

extern "C"{
#include <specialize.h>
#include <internals.h>
}

const uint16_t defaultNaN_ui16 = defaultNaNF16UI;
const uint32_t defaultNaN_ui32 = defaultNaNF32UI;
const uint64_t defaultNaN_ui64 = defaultNaNF64UI;
const float16_t *defaultNaN_f16 = (float16_t *) &defaultNaN_ui16;
const float32_t *defaultNaN_f32 = (float32_t *) &defaultNaN_ui32;
const float64_t *defaultNaN_f64 = (float64_t *) &defaultNaN_ui64;

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

uint_fast16_t f16_classify( float16_t a )
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

uint_fast16_t f32_classify( float32_t a )
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

uint_fast16_t f64_classify( float64_t a )
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
