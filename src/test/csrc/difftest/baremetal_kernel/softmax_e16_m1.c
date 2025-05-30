#define VLEN 1024
#define LMUL 1
#include <riscv_vector.h>
#define INFINITY (__builtin_inff())

void softmax_stable_rvv_fp16(_Float16* dst, _Float16* src, size_t n);
_Float16 quick_dirty_vector_expf(_Float16* dst, _Float16* src, _Float16 max_x, size_t n) ;
void *memset(void *s, int c, size_t n);
void halt(int code);
void _trm_init();
int main();
extern void __am_asm_trap(void);

_Float16 src[VLEN*LMUL/16]={-0.501953,1.802734,0.927734,0.394531,-1.375977,-1.375977,-1.767578,1.464844,0.404541,0.832520,-1.917969,1.879883,1.330078,-1.150391,-1.272461,-1.266602,-0.783203,0.098999,-0.272217,-0.834961,0.447510,-1.442383,-0.831543,-0.534668,-0.175659,1.140625,-1.201172,0.056946,0.369629,-1.814453,0.430176,-1.318359,-1.740234,1.795898,1.862305,1.233398,-0.781738,-1.609375,0.736816,-0.239380,-1.511719,-0.019287,-1.862305,1.637695,-0.964844,0.649902,-0.752930,0.080261,0.186890,-1.260742,1.877930,1.100586,1.757812,1.579102,0.391602,1.687500,-1.646484,-1.215820,-1.819336,-0.698730,-0.445312,-0.914551,1.315430,-0.572754};
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
    const size_t vlmax = __riscv_vsetvlmax_e16m1(); 
    vfloat16m1_t vmax = __riscv_vfmv_v_f_f16m1(-INFINITY, vlmax);

    size_t avl = n;

    while (avl > 0) {
        size_t vl = __riscv_vsetvl_e16m1(avl);
        vfloat16m1_t vx = __riscv_vle16_v_f16m1(src, vl);
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
        size_t vl = __riscv_vsetvl_e16m1(avl);
        vfloat16m1_t row = __riscv_vle16_v_f16m1(dst, vl);
        row = __riscv_vfmul_vf_f16m1(row, inv_sum, vl);
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

    const size_t vlmax = __riscv_vsetvlmax_e16m1(); 
    const vfloat16m1_t vln2 = __riscv_vfmv_v_f_f16m1(ln2, vlmax);
    const vfloat16m1_t viln2 = __riscv_vfmv_v_f_f16m1(iln2, vlmax);

    // element-wise reduction accumulator
    vfloat16m1_t vsum = __riscv_vfmv_v_f_f16m1(0.f, vlmax);

    const vfloat16m1_t poly_c_0 = __riscv_vfmv_v_f_f16m1(0x1.p0, vlmax);
    const vfloat16m1_t poly_c_1 = __riscv_vfmv_v_f_f16m1(0x1p0, vlmax);
    const vfloat16m1_t poly_c_2 = __riscv_vfmv_v_f_f16m1(0x1.fffff8p-2, vlmax);
    const vfloat16m1_t poly_c_3 = __riscv_vfmv_v_f_f16m1(0x1.55548ep-3, vlmax);
    const vfloat16m1_t poly_c_4 = __riscv_vfmv_v_f_f16m1(0x1.555b98p-5, vlmax);
    const vfloat16m1_t poly_c_5 = __riscv_vfmv_v_f_f16m1(0x1.123bccp-7, vlmax);
    const vfloat16m1_t poly_c_6 = __riscv_vfmv_v_f_f16m1(0x1.6850e4p-10, vlmax);
  
    // we need to make sure round-to-nearest is set, because we need
    // it to be enforced for the conversion from vxiln2 to vk.
    //fesetround(FE_TONEAREST);
    asm volatile ("csrw frm, %0" : : "i" (0x0)); 
    size_t avl = n;
    while (avl > 0) {
        size_t vl = __riscv_vsetvl_e16m1(avl);
        vfloat16m1_t vx = __riscv_vle16_v_f16m1(src, vl);
        vx = __riscv_vfsub(vx, max_x, vl);

        // argument reduction
        vfloat16m1_t vxiln2 = __riscv_vfmul(vx, iln2, vl);
        vint16m1_t       vk = __riscv_vfcvt_x_f_v_i16m1(vxiln2, vl); // require round to nearest mode
        vfloat16m1_t    vfk = __riscv_vfcvt_f_x_v_f16m1(vk, vl);
        // using vfnmsac.vf to evaluate r = x - k * log(2)
        vfloat16m1_t     vr = __riscv_vfnmsac(vx, ln2, vfk, vl);

        // polynomial approximation exp(r)
        vfloat16m1_t poly_vr = poly_c_6;
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_5, vl);
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_4, vl);
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_3, vl);
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_2, vl);
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_1, vl);
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_0, vl);

        // reconstruction
        const int exp_bias = 15;
        vint16m1_t vbiased_exp = __riscv_vadd(vk, exp_bias, vl);
        vint16m1_t vexp2_vk    = __riscv_vsll(vbiased_exp, 10, vl);
        vfloat16m1_t vfexp2_vk;
        vfexp2_vk = __riscv_vreinterpret_v_i16m1_f16m1(vexp2_vk);

        vfloat16m1_t vexp_vx  = __riscv_vfmul(poly_vr, vfexp2_vk, vl);

        // element-size reduction with redution accumulator
        // tail-undisturbed is mandatory here to ensure that if vl is less
        // than VLMAX then unaffacted sum terms are not changed.
        vsum = __riscv_vfadd_vv_f16m1_tu(vsum, vsum, vexp_vx, vl);

        __riscv_vse16(dst, vexp_vx, vl);
        avl -= vl;
        src += vl;
        dst += vl;
    }

    vfloat16m1_t vredsum = __riscv_vfmv_v_f_f16m1(0.f, vlmax);
    vredsum = __riscv_vfredusum_vs_f16m1_f16m1(vsum, vredsum, vlmax);

    return __riscv_vfmv_f_s_f16m1_f16(vredsum);
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
  