
#include <math.h>
#include <stddef.h>
#include <fenv.h>
#include <string.h>
#include <inttypes.h>
#include <riscv_vector.h>
#include <bench_softmax_utils.h>

softmax_bench_result_t softmax_bench(float* dst, float* src, softmax_func_t func, double* golden, size_t n); 

#ifndef POLY_DEGREE
#define POLY_DEGREE 6
#elif (POLY_DEGREE > 6)
#error "POLY_DEGREE MUST NOT EXCEED 6"
#endif

float diff_vreg[32][VLEN/32]={};

/** Quick and dirty implementation of exponential function on a binary32 input */
float quick_dirty_expf(float x) {
    // values determined using (python)sollya
    // >>> iln2 = sollya.round(1/sollya.log(2), sollya.binary32, sollya.RN)
    // >>> ln2 = sollya.round(sollya.log(2), sollya.binary32, sollya.RN)
    const float ln2 = 0x1.62e43p-1;    
    const float iln2 = 0x1.715476p0f;

    // argument reduction
    const int k = nearbyintf(x * iln2);
    const float r = fmaf(- k, ln2, x);

    // polynomial approximation exp(r)
    // coefficients determined using (python)sollya
    // >>> ln2ov2 = sollya.round(sollya.log(2)/2, sollya.binary32, sollya.RN)
    // >>> approxInterval = sollya.Interval(-ln2ov2, ln2ov2)
    // >>> approxFunc = sollya.exp(sollya.x)
    // >>> degree = 6
    // >>> poly = sollya.fpminimax(approxFunc,
    //                             degree,
    //                             [1] + [sollya.binary32] * degree,
    //                             approxInterval)
    const float poly_coeffs[] = {
        0x1p0, 
        0x1.fffff8p-2, 
        0x1.55548ep-3, 
        0x1.555b98p-5, 
        0x1.123bccp-7, 
        0x1.6850e4p-10, 
    };

    const int poly_degree = POLY_DEGREE;

    float poly_r = poly_coeffs[poly_degree-1];
    int i = 0;
    for (i = poly_degree - 2; i >= 0; i--) {
        // poly_r = poly_r * r + poly_coeffs[i];
        poly_r = fmaf(poly_r, r, poly_coeffs[i]);
    }
    // poly_r = 1.f + r * poly_r;
    poly_r = fmaf(r, poly_r, 1.f);

    // typedef union { float f; uint32_t u; } f_u32_t;
    // NOTE: a proper cast should be done through memcopy and not an union
    // as I think accessing two different fields from the same variable
    // of a union type is undefined behavior.

    // quick and dirty (does not manage overflow/underflow/special values)
    // way to compute 2^k by injecting the biased exponent in the proper place
    // for IEEE-754 binary32 encoding.
    uint32_t exp2_k_u = (127 + k) << 23;
    float exp2_k;
    memcpy(&exp2_k, &exp2_k_u, sizeof(float)); // hopefully this memcpy is removed by the compiler   

    // result reconstruction
    float exp_x = poly_r * exp2_k;

    return exp_x;
}

/** RVV-based vectorized implementation of binary32 exponential with 
 *  result reduction (sum).
*/
float quick_dirty_vector_expf(float* dst, float* src, float max_x, size_t n) {
    // values determined using (python)sollya
    const float ln2 = 0x1.62e43p-1;    
    const float iln2 = 0x1.715476p0f;

    const size_t vlmax = __riscv_vsetvlmax_e32m1(); //0d0079d7          	vsetvli	s3,zero,e32,m1,ta,ma
    const vfloat32m1_t vln2 = __riscv_vfmv_v_f_f32m1(ln2, vlmax);//5e0052d7          	vfmv.v.f	v5,ft0
    const vfloat32m1_t viln2 = __riscv_vfmv_v_f_f32m1(iln2, vlmax);//5e05d3d7          	vfmv.v.f	v7,fa1

    // element-wise reduction accumulator
    vfloat32m1_t vsum = __riscv_vfmv_v_f_f32m1(0.f, vlmax);//5e0	vfmv.v.f	v4,fa2

    const vfloat32m1_t poly_c_0 = __riscv_vfmv_v_f_f32m1(0x1.p0, vlmax);//5e0          	vfmv.v.f	v9,fa3
    const vfloat32m1_t poly_c_1 = __riscv_vfmv_v_f_f32m1(0x1p0, vlmax);//5e0          	vfmv.v.f	v5,fa4
    const vfloat32m1_t poly_c_2 = __riscv_vfmv_v_f_f32m1(0x1.fffff8p-2, vlmax);//5e0        	vfmv.v.f	v7,fa5
    const vfloat32m1_t poly_c_3 = __riscv_vfmv_v_f_f32m1(0x1.55548ep-3, vlmax);//5e0          	vfmv.v.f	v8,fa6
    const vfloat32m1_t poly_c_4 = __riscv_vfmv_v_f_f32m1(0x1.555b98p-5, vlmax);//5e0          	vfmv.v.f	v10,fa7
    const vfloat32m1_t poly_c_5 = __riscv_vfmv_v_f_f32m1(0x1.123bccp-7, vlmax);//5e0         	vfmv.v.f	v11,fa8
    const vfloat32m1_t poly_c_6 = __riscv_vfmv_v_f_f32m1(0x1.6850e4p-10, vlmax);//5e0         	vfmv.v.f	v6,fa9
  
    // we need to make sure round-to-nearest is set, because we need
    // it to be enforced for the conversion from vxiln2 to vk.
    fesetround(FE_TONEAREST);

    size_t avl = n;
    while (avl > 0) {
        size_t vl = __riscv_vsetvl_e32m1(avl);//090477d7          	vsetvli	a5,s0,e32,m1,tu,ma
        vfloat32m1_t vx = __riscv_vle32_v_f32m1(src, vl);//02096107          	vle32.v	v2,(s2)
        vx = __riscv_vfsub(vx, max_x, vl);//0a245157          	vfsub.vf	v2,v2,fs0

        // argument reduction
        vfloat32m1_t vxiln2 = __riscv_vfmul(vx, iln2, vl);//922750d7          	vfmul.vf	v1,v2,fa4
        vint32m1_t       vk = __riscv_vfcvt_x_f_v_i32m1(vxiln2, vl); // require round to nearest mode::4a1090d7          	vfcvt.x.f.v	v1,v1
        vfloat32m1_t    vfk = __riscv_vfcvt_f_x_v_f32m1(vk, vl);//4a1191d7          	vfcvt.f.x.v	v3,v1
        // using vfnmsac.vf to evaluate r = x - k * log(2)
        vfloat32m1_t     vr = __riscv_vfnmsac(vx, ln2, vfk, vl);//be37d157          	vfnmsac.vf	v2,fa5,v3

        // polynomial approximation exp(r)
        vfloat32m1_t poly_vr = poly_c_6;//9e6030d7          	vmv1r.v	v1,v6
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_5, vl);//a2a110d7          	vfmadd.vv	v1,v2,v111
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_4, vl);//a29110d7          	vfmadd.vv	v1,v2,v10
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_3, vl);//a28110d7          	vfmadd.vv	v1,v2,v8
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_2, vl);//a27110d7          	vfmadd.vv	v1,v2,v7
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_1, vl);//a25110d7          	vfmadd.vv	v1,v2,v5
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_0, vl);//a25110d7          	vfmadd.vv	v1,v2,v9

        // reconstruction
        const int exp_bias = 127;
        vint32m1_t vbiased_exp = __riscv_vadd(vk, exp_bias, vl);//0216c0d7          	vadd.vx	v1,v1,a3
        vint32m1_t vexp2_vk    = __riscv_vsll(vbiased_exp, 23, vl);//961bb1d7          	vsll.vi	v3,v1,23
        vfloat32m1_t vfexp2_vk;
        vfexp2_vk = __riscv_vreinterpret_v_i32m1_f32m1(vexp2_vk);

        vfloat32m1_t vexp_vx  = __riscv_vfmul(poly_vr, vfexp2_vk, vl);//921190d7          	vfmul.vv	v1,v1,v3

        // element-size reduction with redution accumulator
        // tail-undisturbed is mandatory here to ensure that if vl is less
        // than VLMAX then unaffacted sum terms are not changed.
        vsum = __riscv_vfadd_vv_f32m1_tu(vsum, vsum, vexp_vx, vl);//02409257          	vfadd.vv	v4,v4,v1

        __riscv_vse32(dst, vexp_vx, vl);//0204e0a7          	vse32.v	v1,(s1)
        avl -= vl;
        src += vl;
        dst += vl;
    }

    vfloat32m1_t vredsum = __riscv_vfmv_v_f_f32m1(0.f, vlmax);//5e0030d7          	vmv.v.i	v1,0
    vredsum = __riscv_vfredusum_vs_f32m1_f32m1(vsum, vredsum, vlmax);//06409257          	vfredusum.vs	v4,v4,v1

    return __riscv_vfmv_f_s_f32m1_f32(vredsum);//42401557          	vfmv.f.s	fa0,v4
}


/** implementation of softmax for binary32 with RVV-based normalization
 * 
 *  @param dst destination array
 *  @param src source array
 *  @param n   number of element(s)
*/
void softmax_rvv_norm_fp32(float* dst, float* src, size_t n)
{
    int i;

    // computing the sum of exponentials
    float sum = 0.f;
    for (i = 0; i < n; ++i) {
        dst[i] = expf(src[i]);
        sum += dst[i];
    }

    // computing the reciprocal of the sum of exponentials, once and for all
    float inv_sum = 1.f / sum;

    // normalizing each element
    size_t avl = n;
    while (avl > 0) {
        size_t vl = __riscv_vsetvl_e32m1(avl);
        vfloat32m1_t row = __riscv_vle32_v_f32m1(dst, vl);
        row = __riscv_vfmul(row, inv_sum, vl);
        __riscv_vse32(dst, row, vl);
        avl -= vl;
        dst += vl;
    }
}


softmax_bench_result_t softmax_rvv_norm_fp32_bench(float* dst, float* src, double* golden, size_t n) {
    return softmax_bench(dst, src, softmax_rvv_norm_fp32, golden, n);
}


/** implementation of softmax for binary32 RVV-based
 * 
 *  @param dst destination array
 *  @param src source array
 *  @param n   number of element(s)
*/
void softmax_rvv_fp32(float* dst, float* src, size_t n)
{
    // computing element-wise exponentials and their seum
    float sum = quick_dirty_vector_expf(dst, src, 0.f, n);

    // computing the reciprocal of the sum of exponentials, once and for all
    float inv_sum = 1.f / sum;

    // normalizing each element
    size_t avl = n;
    while (avl > 0) {
        size_t vl = __riscv_vsetvl_e32m1(avl);
        vfloat32m1_t row = __riscv_vle32_v_f32m1(dst, vl);
        row = __riscv_vfmul_vf_f32m1(row, inv_sum, vl);
        __riscv_vse32(dst, row, vl);
        avl -= vl;
        dst += vl;
    }
}


softmax_bench_result_t softmax_rvv_fp32_bench(float* dst, float* src, double* golden, size_t n) {
    return softmax_bench(dst, src, softmax_rvv_fp32, golden, n);
}


/** More numerically stable implementation of softmax for binary32 RVV-based
 * 
 *  @param dst destination array
 *  @param src source array
 *  @param n   number of element(s)
*/
void softmax_stable_rvv_fp32(float* dst, float* src, size_t n)
{
    // initializing temporary maximum vector
    // vlmax initialization is required in case the first vsetvl does
    // not return VLMAX while avl > vl: in this case we need to
    // avoid some uninitialized values in vmax
    const size_t vlmax = __riscv_vsetvlmax_e32m1(); //0d0078d7          	vsetvli	a7,zero,e32,m1,ta,ma
    vfloat32m1_t vmax = __riscv_vfmv_v_f_f32m1(-INFINITY, vlmax);//5e07d0d7          	vfmv.v.f	v1,fa5

    size_t avl = n;

    while (avl > 0) {
        size_t vl = __riscv_vsetvl_e32m1(avl);//090777d7          	vsetvli	a5,a4,e32,m1,tu,ma
        vfloat32m1_t vx = __riscv_vle32_v_f32m1(src, vl);//0206e107          	vle32.v	v2,(a3)
        vmax = __riscv_vfmax_tu(vmax, vx, vmax, vl);//1a2090d7          	vfmax.vv	v1,v2,v1
        avl -= vl;
        src += vl;
    }
    src -= n; // reseting source pointer

    // final maximum reduction
    vfloat32m1_t vredmax = __riscv_vfmv_v_f_f32m1(-INFINITY, vlmax);//5e07d157          	vfmv.v.f	v2,fa5
    vredmax = __riscv_vfredmax(vmax, vredmax, vlmax);//1e1110d7          	vfredmax.vs	v1,v1,v2
    float max_x = __riscv_vfmv_f_s_f32m1_f32(vredmax);//42101557          	vfmv.f.s	fa0,v1

#ifdef VERY_VERBOSE
    printf("max_x=%a\n", max_x );
#endif

    // Computing element-wise exponentials and their sum.
    // max_x is subtracted from each element before computing the element-wise exponential.
    float sum = quick_dirty_vector_expf(dst, src, max_x, n);

    // computing the reciprocal of the sum of exponentials, once and for all
    float inv_sum = 1.f / sum;

    // normalizing each element
    avl = n;
    while (avl > 0) {
        size_t vl = __riscv_vsetvl_e32m1(avl);//0d04f7d7          	vsetvli	a5,s1,e32,m1,ta,ma
        vfloat32m1_t row = __riscv_vle32_v_f32m1(dst, vl);//02046087          	vle32.v	v1,(s0)
        row = __riscv_vfmul_vf_f32m1(row, inv_sum, vl);//921550d7          	vfmul.vf	v1,v1,fa0
        __riscv_vse32(dst, row, vl);//020460a7          	vse32.v	v1,(s0)
        avl -= vl;
        dst += vl;
    }
}


softmax_bench_result_t softmax_stable_rvv_fp32_bench(float* dst, float* src, double* golden, size_t n) {
    return softmax_bench(dst, src, softmax_stable_rvv_fp32, golden, n);
}
