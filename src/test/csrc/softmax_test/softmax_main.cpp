#include <math.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <stdint.h>
#include <softmax_bench.h>
#include <reg.h>

#include <VVTopDebug.h>
#include "verilated.h"
#include "svdpi.h"
#include "VVTopDebug__Dpi.h"
#include <verilated_vcd_c.h>


#define VERBOSE 1
#define NUM_TESTS 1

softmax_bench_result_t softmax_stable_rvv_fp32_bench(float* dst, float* src, double* golden, size_t n);
void softmax_golden_fp32_fp64(double* dst, float* src, size_t n);
void reset(int n, VVTopDebug *top, VerilatedContext *contextp, VerilatedVcdC *wave);


CPU_STATE cpu={};
VCSR vcsr={.lmul=1, .sew=32, .vlenb=VLEN/8,.vstart=0};

VerilatedContext *contextp = NULL;
VVTopDebug *top = NULL;
VerilatedVcdC *wave = NULL;
int main(int argc, char **argv){
    int i;
    CPU_STATE cpu;

    contextp = new VerilatedContext;
    contextp->commandArgs(argc, argv);
    top = new VVTopDebug{contextp};
    wave = new VerilatedVcdC;

    contextp->traceEverOn(true);
    top->trace(wave, 99);
    wave->open("build/top.vcd");
    reset(3, top, contextp, wave);

    softmax_bench_t benchmarks[] = {
        (softmax_bench_t){.bench = softmax_stable_rvv_fp32_bench,              .label="rvv-based n-element stable softmax"},
    };
    size_t testSizes[] = {VLEN*LMUL/32};//{4, 16, 17, 32, 33, 128, 129, 511, 512, 1024, 2048};
    for (size_t testId = 0; testId < sizeof(testSizes) / sizeof(size_t); testId++)
    {
        size_t n = testSizes[testId];
#       ifdef VERBOSE 
        printf("--------------------------------------------------------------------------------\n");
        printf("--------------------------------------------------------------------------------\n");
        printf("Benchmarking softmax on a %ld-element array.\n", n);
#       endif
        //FloatUintUnion src;

        float* src = (float*)malloc(n * sizeof(float));
        float* dst = (float*)malloc(n * sizeof(float));
        double* golden = (double*)malloc(n * sizeof(double));
        assert(src);
        assert(dst);
        assert(golden);
        // reset benchmark results
        for (unsigned benchId=0; benchId < sizeof(benchmarks) / sizeof(softmax_bench_t); benchId++)
        {
            benchmarks[benchId].result.max_abs_error = 0.;
            benchmarks[benchId].result.max_rel_error = 0.;
            benchmarks[benchId].result.error_norm2 = 0.f;
        }

        int j;
        const float RAND_LOWER_BOUND = -2.f;
        const float RAND_RANGE = 4.f;
        for (j = 0; j < NUM_TESTS; ++j) {
            // random initialization of the input arrays
            for (i = 0; i < n; ++i) {
                src[i] = RAND_RANGE * rand() / (float) RAND_MAX + RAND_LOWER_BOUND;
            }

            // computing golden value
            softmax_golden_fp32_fp64(golden, src, n);


#           ifdef VERY_VERBOSE
            printf("source matrix:\n");
            array_dump_fp32(src, n);
            printf("golden result:\n");
            array_dump_fp64(golden, n);
#           endif // VERY_VERBOSE

            // softmax benchmarks. iterating over all existing implementation for this given input set
            for (unsigned benchId=0; benchId < sizeof(benchmarks) / sizeof(softmax_bench_t); benchId++)
            {
                memset(dst, 0, sizeof(float) * n); // resetting array in-between experiments

                softmax_bench_result_t local_result = benchmarks[benchId].bench(dst, src, golden, n);

                benchmarks[benchId].result = accumulate_bench_result(benchmarks[benchId].result, local_result);

#               ifdef VERY_VERBOSE
                printf("%s result:\n", benchmarks[benchId].label);
                array_dump_fp32(dst, n);
#               endif // VERY_VERBOSE

            }
        }

        // display results
        for (unsigned benchId=0; benchId < sizeof(benchmarks) / sizeof(softmax_bench_t); benchId++)
        {
            softmax_bench_result_t bench_result = benchmarks[benchId].result;
            bench_result.mean_rel_error = bench_result.mean_rel_error / NUM_TESTS;
            bench_result.error_norm2 = sqrt(bench_result.error_norm2);


#           ifdef VERBOSE 
            printf("--------------------------------------------------------------------------------\n");
            printf("%s evaluate softmax on a %ld-element array.\n",
                benchmarks[benchId].label,  n);
            printf("  max absolute error:  %.4a\n", bench_result.max_abs_error);
            printf("  max relative error:  %.4a\n", bench_result.max_rel_error);
            printf("  mean relative error: %.4a\n", bench_result.mean_rel_error);
            printf("  error norm 2:       %.4a\n", bench_result.error_norm2);
#           else
            // condensed display
            printf("%s, %d, %d, %.3e, %.3e, %.3e %.3e\n", 
                   benchmarks[benchId].label, n, bench_result.perf_count,
                   bench_result.max_abs_error, bench_result.max_rel_error, bench_result.error_norm2, bench_result.mean_rel_error);
#           endif
        }

        free(src);
        free(dst);
        free(golden);

    }

    wave->close();
    delete top;
    delete contextp;
    return 0;
}