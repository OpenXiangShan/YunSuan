#define VLEN 1024
#include <riscv_vector.h>
#define INFINITY (__builtin_inff())

void softmax_stable_rvv_fp32(float* dst, float* src, size_t n);
float quick_dirty_vector_expf(float* dst, float* src, float max_x, size_t n) ;
void *memset(void *s, int c, size_t n);
void halt(int code);
void _trm_init();
int main();

float src[VLEN/32]={-0.50183952f,1.80285728f,0.92797577f,0.39463395f,-1.37592542f,-1.37602186f,-1.76766551f,1.46470463f,0.40446004f,0.83229029f,-1.91766202f,1.87963939f,1.32977057f,-1.15064359f,-1.27270019f,-1.26638198f,-0.78303105f,0.09902573f,-0.27221993f,-0.83508343f,0.44741157f,-1.44202459f,-0.83142143f,-0.53455263f,-0.17572007f,1.14070380f,-1.20130491f,0.05693775f,0.36965826f,-1.81419837f,0.43017942f,-1.31790352f};
float dst[VLEN/32]={0};

float golden[VLEN/32]={0.01340518f,0.13433519f,0.05600587f,0.03285535f,0.00559322f,0.00559268f,0.00378034f,0.09579259f,0.03317978f,0.05089533f,0.00325378f,0.14505604f,0.08370104f,0.00700649f,0.00620143f,0.00624074f,0.01011935f,0.02444698f,0.01686534f,0.00960609f,0.03463596f,0.00523547f,0.00964133f,0.01297375f,0.01857396f,0.06928197f,0.00666038f,0.02343940f,0.03204493f,0.00360847f,0.03404422f,0.00592735f};
float diff_mem[VLEN/32]={0};
int main(){
    softmax_stable_rvv_fp32(dst,src,VLEN/32);
    // size_t vl = __riscv_vsetvlmax_e32m1();
    // vfloat32m1_t vx = __riscv_vle32_v_f32m1(gloden, vl);
    // vfloat32m1_t vy = __riscv_vle32_v_f32m1(dst, vl);
    // vfloat32m1_t diff = __riscv_vfsub_vv_f32m1(vx, vy, vl);

    // __riscv_vse32(diff_mem, diff, vl);
    return 0;

}


void softmax_stable_rvv_fp32(float* dst, float* src, size_t n)
{
    // initializing temporary maximum vector
    // vlmax initialization is required in case the first vsetvl does
    // not return VLMAX while avl > vl: in this case we need to
    // avoid some uninitialized values in vmax
    const size_t vlmax = __riscv_vsetvlmax_e32m1(); 
    vfloat32m1_t vmax = __riscv_vfmv_v_f_f32m1(-INFINITY, vlmax);

    size_t avl = n;

    while (avl > 0) {
        size_t vl = __riscv_vsetvl_e32m1(avl);
        vfloat32m1_t vx = __riscv_vle32_v_f32m1(src, vl);
        vmax = __riscv_vfmax_tu(vmax, vx, vmax, vl);
        avl -= vl;
        src += vl;
    }
    src -= n; // reseting source pointer

    // final maximum reduction
    vfloat32m1_t vredmax = __riscv_vfmv_v_f_f32m1(-INFINITY, vlmax);
    vredmax = __riscv_vfredmax(vmax, vredmax, vlmax);
    float max_x = __riscv_vfmv_f_s_f32m1_f32(vredmax);

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
        size_t vl = __riscv_vsetvl_e32m1(avl);
        vfloat32m1_t row = __riscv_vle32_v_f32m1(dst, vl);
        row = __riscv_vfmul_vf_f32m1(row, inv_sum, vl);
        volatile vfloat32m1_t vx = __riscv_vle32_v_f32m1(golden, vl);
        __riscv_vse32(dst, row, vl);
        avl -= vl;
        dst += vl;
    }
}

/** RVV-based vectorized implementation of binary32 exponential with 
 *  result reduction (sum).
*/
float quick_dirty_vector_expf(float* dst, float* src, float max_x, size_t n) {
    // values determined using (python)sollya
    const float ln2 = 0x1.62e43p-1;    
    const float iln2 = 0x1.715476p0f;

    const size_t vlmax = __riscv_vsetvlmax_e32m1(); 
    const vfloat32m1_t vln2 = __riscv_vfmv_v_f_f32m1(ln2, vlmax);
    const vfloat32m1_t viln2 = __riscv_vfmv_v_f_f32m1(iln2, vlmax);

    // element-wise reduction accumulator
    vfloat32m1_t vsum = __riscv_vfmv_v_f_f32m1(0.f, vlmax);

    const vfloat32m1_t poly_c_0 = __riscv_vfmv_v_f_f32m1(0x1.p0, vlmax);
    const vfloat32m1_t poly_c_1 = __riscv_vfmv_v_f_f32m1(0x1p0, vlmax);
    const vfloat32m1_t poly_c_2 = __riscv_vfmv_v_f_f32m1(0x1.fffff8p-2, vlmax);
    const vfloat32m1_t poly_c_3 = __riscv_vfmv_v_f_f32m1(0x1.55548ep-3, vlmax);
    const vfloat32m1_t poly_c_4 = __riscv_vfmv_v_f_f32m1(0x1.555b98p-5, vlmax);
    const vfloat32m1_t poly_c_5 = __riscv_vfmv_v_f_f32m1(0x1.123bccp-7, vlmax);
    const vfloat32m1_t poly_c_6 = __riscv_vfmv_v_f_f32m1(0x1.6850e4p-10, vlmax);
  
    // we need to make sure round-to-nearest is set, because we need
    // it to be enforced for the conversion from vxiln2 to vk.
    //fesetround(FE_TONEAREST);
    asm volatile ("csrw frm, %0" : : "i" (0x0)); 
    size_t avl = n;
    while (avl > 0) {
        size_t vl = __riscv_vsetvl_e32m1(avl);
        vfloat32m1_t vx = __riscv_vle32_v_f32m1(src, vl);
        vx = __riscv_vfsub(vx, max_x, vl);

        // argument reduction
        vfloat32m1_t vxiln2 = __riscv_vfmul(vx, iln2, vl);
        vint32m1_t       vk = __riscv_vfcvt_x_f_v_i32m1(vxiln2, vl); // require round to nearest mode
        vfloat32m1_t    vfk = __riscv_vfcvt_f_x_v_f32m1(vk, vl);
        // using vfnmsac.vf to evaluate r = x - k * log(2)
        vfloat32m1_t     vr = __riscv_vfnmsac(vx, ln2, vfk, vl);

        // polynomial approximation exp(r)
        vfloat32m1_t poly_vr = poly_c_6;
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_5, vl);
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_4, vl);
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_3, vl);
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_2, vl);
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_1, vl);
        poly_vr = __riscv_vfmadd(poly_vr, vr, poly_c_0, vl);

        // reconstruction
        const int exp_bias = 127;
        vint32m1_t vbiased_exp = __riscv_vadd(vk, exp_bias, vl);
        vint32m1_t vexp2_vk    = __riscv_vsll(vbiased_exp, 23, vl);
        vfloat32m1_t vfexp2_vk;
        vfexp2_vk = __riscv_vreinterpret_v_i32m1_f32m1(vexp2_vk);

        vfloat32m1_t vexp_vx  = __riscv_vfmul(poly_vr, vfexp2_vk, vl);

        // element-size reduction with redution accumulator
        // tail-undisturbed is mandatory here to ensure that if vl is less
        // than VLMAX then unaffacted sum terms are not changed.
        vsum = __riscv_vfadd_vv_f32m1_tu(vsum, vsum, vexp_vx, vl);

        __riscv_vse32(dst, vexp_vx, vl);
        avl -= vl;
        src += vl;
        dst += vl;
    }

    vfloat32m1_t vredsum = __riscv_vfmv_v_f_f32m1(0.f, vlmax);
    vredsum = __riscv_vfredusum_vs_f32m1_f32m1(vsum, vredsum, vlmax);

    return __riscv_vfmv_f_s_f32m1_f32(vredsum);
}
void *memset(void *s, int c, size_t n) {
    char *schar=s;
    while(n--){
      *schar++=c;
    }
    return s;
  }

  
void _trm_init() {
    int ret = main();
    asm volatile("mv a0, %0; " : :"r"(ret));
    while(1);
}
  