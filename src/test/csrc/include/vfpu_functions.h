#include "gm_common.h"

extern "C"{
#include <specialize.h>
#include <internals.h>
}

#define F16_SIGN ((uint64_t)1ul << 15)
#define F32_SIGN ((uint64_t)1ul << 31)
#define F64_SIGN ((uint64_t)1ul << 63)

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
