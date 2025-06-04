#define VLEN 1024
#define LMUL 4
#include <riscv_vector.h>
#define INFINITY (__builtin_inff())

void softmax_stable_rvv_fp16(_Float16* dst, _Float16* src, size_t n);
_Float16 quick_dirty_vector_expf(_Float16* dst, _Float16* src, _Float16 max_x, size_t n) ;
void *memset(void *s, int c, size_t n);
void halt(int code);
void _trm_init();
int main();
extern void __am_asm_trap(void);

_Float16 src[VLEN*LMUL/16]={-0.501953,1.802734,0.927734,0.394531,-1.375977,-1.375977,-1.767578,1.464844,0.404541,0.832520,-1.917969,1.879883,1.330078,-1.150391,-1.272461,-1.266602,-0.783203,0.098999,-0.272217,-0.834961,0.447510,-1.442383,-0.831543,-0.534668,-0.175659,1.140625,-1.201172,0.056946,0.369629,-1.814453,0.430176,-1.318359,-1.740234,1.795898,1.862305,1.233398,-0.781738,-1.609375,0.736816,-0.239380,-1.511719,-0.019287,-1.862305,1.637695,-0.964844,0.649902,-0.752930,0.080261,0.186890,-1.260742,1.877930,1.100586,1.757812,1.579102,0.391602,1.687500,-1.646484,-1.215820,-1.819336,-0.698730,-0.445312,-0.914551,1.315430,-0.572754,-0.876465,0.170776,-1.436523,1.208984,-1.702148,1.947266,1.088867,-1.205078,-1.977539,1.261719,0.827637,0.916016,1.084961,-1.704102,-0.565918,-1.536133,1.452148,0.493164,-0.676270,-1.746094,-0.755859,-0.699219,0.918457,0.550293,1.548828,-0.111145,-1.521484,0.853027,1.042969,0.245117,1.083984,-0.024811,0.090942,-0.289795,-1.898438,-1.568359,-1.874023,0.545410,-0.742676,0.034271,1.629883,-1.002930,-0.358398,1.022461,-1.084961,-1.692383,-0.840820,-1.355469,1.718750,1.232422,0.533691,1.486328,1.214844,-1.253906,1.570312,0.157349,1.229492,1.583984,-0.728027,-1.559570,-1.087891,-0.291504,1.272461,1.443359,-1.972656,0.042999,-0.330322,-1.111328,-1.520508,-0.649414,1.771484,-0.707031,0.075134,0.812012,-0.545410,1.886719,1.849609,-0.992676,-0.011009,-0.796387,-0.860840,-1.852539,0.438232,0.010719,-1.793945,-0.885254,1.632812,-1.041992,-1.420898,-0.042175,1.942383,-1.032227,0.688477,1.046875,-1.049805,0.913086,-0.528809,0.529297,0.534180,0.143066,-1.638672,1.340820,-0.716797,-1.253906,-1.836914,0.363525,0.710449,-1.933594,0.048370,-1.093750,0.580566,-1.302734,0.763672,-0.453125,1.747070,-1.450195,-0.635742,-1.545898,1.699219,1.509766,-0.968262,0.640137,1.268555,0.220825,0.118591,-1.032227,-1.627930,1.588867,1.601562,0.532227,-0.644043,-0.603027,0.903809,1.588867,1.548828,1.119141,0.568359,-1.663086,-1.353516,1.593750,0.425781,-1.962891,-1.593750,0.653809,-1.979492,-1.356445,0.194946,0.767578,0.607910,-1.102539,0.848633,-1.050781,-0.698242,0.985840,0.598633,1.396484,0.630371,0.273193,-1.625000,-0.529297,-0.938965,-1.024414,1.891602,-0.427490,1.568359,0.524414,1.179688,0.010551,0.307617,-0.029922,-1.218750,0.889648,-0.876953,-1.902344,0.582031,-1.291992,1.761719,1.815430,1.659180,-0.519531,-1.938477,1.712891,-0.287354,1.866211,1.854492,1.412109,-0.822266,-0.459717,1.404297,-0.732422,-1.322266,0.227173};
_Float16 dst[VLEN*LMUL/16]={0};

// float golden[VLEN*LMUL/32]={0.01340518f,0.13433519f,0.05600587f,0.03285535f,0.00559322f,0.00559268f,0.00378034f,0.09579259f,0.03317978f,0.05089533f,0.00325378f,0.14505604f,0.08370104f,0.00700649f,0.00620143f,0.00624074f,0.01011935f,0.02444698f,0.01686534f,0.00960609f,0.03463596f,0.00523547f,0.00964133f,0.01297375f,0.01857396f,0.06928197f,0.00666038f,0.02343940f,0.03204493f,0.00360847f,0.03404422f,0.00592735f};
int main(){
    softmax_stable_rvv_fp16(dst,src,VLEN*LMUL/16);
    return 0;

}


void softmax_stable_rvv_fp16(_Float16* dst, _Float16* src, size_t n)
{
    // initializing temporary maximum vector
    // vlmax initialization is required in case the first vsetvl does
    // not return VLMAX while avl > vl: in this case we need to
    // avoid some uninitialized values in vmax
    const size_t vlmax = __riscv_vsetvlmax_e16m4(); 
    vfloat16m4_t vmax = __riscv_vfmv_v_f_f16m4(-INFINITY, vlmax);

    size_t avl = n;

    while (avl > 0) {
        size_t vl = __riscv_vsetvl_e16m4(avl);
        vfloat16m4_t vx = __riscv_vle16_v_f16m4(src, vl);
        vmax = __riscv_vfmax_tu(vmax, vx, vmax, vl);
        avl -= vl;
        src += vl;
    }
    src -= n; // reseting source pointer

    // final maximum reduction
    vfloat16m1_t vredmax = __riscv_vfmv_v_f_f16m1(-INFINITY, vlmax);//TODO maybe 
    vredmax = __riscv_vfredmax(vmax, vredmax, vlmax);
    _Float16 max_x = __riscv_vfmv_f_s_f16m1_f16(vredmax);


    // Computing element-wise exponentials and their sum.
    // max_x is subtracted from each element before computing the element-wise exponential.
    _Float16 sum = quick_dirty_vector_expf(dst, src, max_x, n);

    // computing the reciprocal of the sum of exponentials, once and for all
    _Float16 inv_sum = 1.f / sum;

    // normalizing each element
    avl = n;
    while (avl > 0) {
        size_t vl = __riscv_vsetvl_e16m4(avl);
        vfloat16m4_t row = __riscv_vle16_v_f16m4(dst, vl);
        row = __riscv_vfmul_vf_f16m4(row, inv_sum, vl);
        __riscv_vse16(dst, row, vl);
        avl -= vl;
        dst += vl;
    }
}

/** RVV-based vectorized implementation of binary16 exponential with 
 *  result reduction (sum).
*/
_Float16 quick_dirty_vector_expf(_Float16* dst, _Float16* src, _Float16 max_x, size_t n) {
    // values determined using (python)sollya
    const _Float16 ln2  = (_Float16)0.69314718056f;  // ln(2) ≈ 0.693147
    const _Float16 iln2 = (_Float16)1.44269504089f;  // 1/ln(2) ≈ 1.442695

    const size_t vlmax = __riscv_vsetvlmax_e16m4(); 
    const vfloat16m4_t vln2 = __riscv_vfmv_v_f_f16m4(ln2, vlmax);
    const vfloat16m4_t viln2 = __riscv_vfmv_v_f_f16m4(iln2, vlmax);

    // element-wise reduction accumulator
    vfloat16m4_t vsum = __riscv_vfmv_v_f_f16m4(0.f, vlmax);

    const vfloat16m4_t poly_c_0 = __riscv_vfmv_v_f_f16m4(0x1.p0, vlmax);
    const vfloat16m4_t poly_c_1 = __riscv_vfmv_v_f_f16m4(0x1p0, vlmax);
    const vfloat16m4_t poly_c_2 = __riscv_vfmv_v_f_f16m4(0x1.fffff8p-2, vlmax);
    const vfloat16m4_t poly_c_3 = __riscv_vfmv_v_f_f16m4(0x1.55548ep-3, vlmax);
    const vfloat16m4_t poly_c_4 = __riscv_vfmv_v_f_f16m4(0x1.555b98p-5, vlmax);
    const vfloat16m4_t poly_c_5 = __riscv_vfmv_v_f_f16m4(0x1.123bccp-7, vlmax);
    const vfloat16m4_t poly_c_6 = __riscv_vfmv_v_f_f16m4(0x1.6850e4p-10, vlmax);
  
    // we need to make sure round-to-nearest is set, because we need
    // it to be enforced for the conversion from vxiln2 to vk.
    //fesetround(FE_TONEAREST);
    asm volatile ("csrw frm, %0" : : "i" (0x0)); 
    size_t avl = n;
    while (avl > 0) {
        size_t vl = __riscv_vsetvl_e16m4(avl);
        vfloat16m4_t vx = __riscv_vle16_v_f16m4(src, vl);
        vx = __riscv_vfsub(vx, max_x, vl);

        // argument reduction
        vfloat16m4_t vxiln2 = __riscv_vfmul(vx, iln2, vl);
        vint16m4_t       vk = __riscv_vfcvt_x_f_v_i16m4(vxiln2, vl); // require round to nearest mode
        vfloat16m4_t    vfk = __riscv_vfcvt_f_x_v_f16m4(vk, vl);
        // using vfnmsac.vf to evaluate r = x - k * log(2)
        vfloat16m4_t     vr = __riscv_vfnmsac(vx, ln2, vfk, vl);

        // polynomial approximation exp(r)
        vfloat16m4_t poly_vr = poly_c_6;
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_5, vl);
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_4, vl);
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_3, vl);
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_2, vl);
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_1, vl);
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_0, vl);

        // reconstruction
        const int exp_bias = 15;
        vint16m4_t vbiased_exp = __riscv_vadd(vk, exp_bias, vl);
        vint16m4_t vexp2_vk    = __riscv_vsll(vbiased_exp, 10, vl);
        vfloat16m4_t vfexp2_vk;
        vfexp2_vk = __riscv_vreinterpret_v_i16m4_f16m4(vexp2_vk);

        vfloat16m4_t vexp_vx  = __riscv_vfmul(poly_vr, vfexp2_vk, vl);

        // element-size reduction with redution accumulator
        // tail-undisturbed is mandatory here to ensure that if vl is less
        // than VLMAX then unaffacted sum terms are not changed.
        vsum = __riscv_vfadd_vv_f16m4_tu(vsum, vsum, vexp_vx, vl);

        __riscv_vse16(dst, vexp_vx, vl);
        avl -= vl;
        src += vl;
        dst += vl;
    }
    // size_t vl = __riscv_vsetvlmax_e32m8();
    vfloat32m1_t vredsum_fp32 = __riscv_vfmv_v_f_f32m1(0.f, vlmax);
    vfloat32m8_t vsum_fp32 = __riscv_vfwcvt_f_f_v_f32m8(vsum, vlmax);
    vredsum_fp32 = __riscv_vfredusum_vs_f32m8_f32m1(vsum_fp32, vredsum_fp32, vlmax);
    vfloat16mf2_t vredsum = __riscv_vfncvt_f_f_w_f16mf2(vredsum_fp32, vlmax);

    return __riscv_vfmv_f_s_f16mf2_f16(vredsum);
}
void *memset(void *s, int c, size_t n) {
    char *schar=s;
    while(n--){
      *schar++=c;
    }
    return s;
  }

  
void _trm_init() {
    asm volatile("csrw mtvec, %0" : : "r"(__am_asm_trap));
    int ret = main();
    asm volatile("mv a0, %0; ebreak;" : :"r"(ret));
    while(1);
}
  