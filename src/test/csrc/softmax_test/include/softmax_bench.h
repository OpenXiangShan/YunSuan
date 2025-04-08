#ifndef _SOFTMAX_BENCH_H_
#define _SOFTMAX_BENCH_H_

#include <stddef.h>
/** generic type for a binary32/float softmax implementation */
typedef void(softmax_func_t)(float* dst, float* src, size_t n);


typedef struct {
    double max_abs_error;
    double max_rel_error;
    double mean_rel_error;
    double error_norm2;
} softmax_bench_result_t;



typedef softmax_bench_result_t (softmax_bench_func_t)(float* dst, float* src, double* golden, size_t n);

/** Descriptor structure for softmax benchmark */
typedef struct {
    softmax_bench_func_t* bench;
    softmax_bench_result_t result;
    char label[100];
} softmax_bench_t;

static softmax_bench_result_t accumulate_bench_result(softmax_bench_result_t res, softmax_bench_result_t new_result) {
    if (new_result.max_abs_error > res.max_abs_error) res.max_abs_error = new_result.max_abs_error;
    if (new_result.max_rel_error > res.max_rel_error) res.max_rel_error = new_result.max_rel_error;
    res.error_norm2 += new_result.max_rel_error * new_result.max_rel_error;
    res.mean_rel_error += new_result.mean_rel_error;
  
    return res;
  }
  

#endif