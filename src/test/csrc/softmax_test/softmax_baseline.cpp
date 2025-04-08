
#include <cmath>
#include <cstddef>
extern void softmax_golden_fp32_fp64(double* dst, float* src, size_t n);

void softmax_golden_fp32_fp64(double* dst, float* src, size_t n)
{
    int i;
    // looking for max input value
    double max_x = src[0];
    for (i = 1; i < n; ++i) {
        if (src[i] > max_x) max_x = src[i]; 
    }

    // computing the sum of exponentials
    double sum = 0.;
    for (i = 0; i < n; ++i) {
        dst[i] = exp((double) src[i] - max_x);
        sum += dst[i];
    }

    // normalizing each element: we use the division every single
    // type rather than hoisting the evaluation of the reciprocal 1 / sum
    // to improve accuracy since this function is intended to build golden
    // values and not to be fast.
    for (i = 0; i < n; ++i) dst[i] = (double) dst[i] / sum;
}