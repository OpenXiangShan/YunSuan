
#include "softmax_bench.h"
#include <stdio.h>
#include <cmath>
#define LMUL 1
#define SEW 32
#include <reg.h>
#include <rvv.h>
#include <mem.h>

#include <VVTopDebug.h>
#include "verilated.h"
#include "svdpi.h"
#include "VVTopDebug__Dpi.h"
#include <verilated_vcd_c.h>

extern VVTopDebug *top;
extern VerilatedContext *contextp;
extern VerilatedVcdC *wave ;
extern FloatUintUnion  pmem[CONFIG_MSIZE];

#define CLK_PERIOD 10    // 时钟周期（单位：ns）
vluint64_t main_time = 0;

bool check_vreg(int i);
void single_cycle(VVTopDebug *top, VerilatedContext *contextp, VerilatedVcdC *wave);
void reset(int n, VVTopDebug *top, VerilatedContext *contextp, VerilatedVcdC *wave);

float diff_vreg[32][VLEN/32]={};
const float poly_coeffs[] = {

    1.0f,                     // 0x1p0f
    0.49999997019767761f,     // 0x1.fffff8p-2f
    0.1666666716337204f,      // 0x1.55548ep-3f
    0.041666667908430099f,    // 0x1.555b98p-5f
    0.0083333337697553482f,   // 0x1.123bccp-7f
    0.0013888889251995273f,   // 0x1.6850e4p-10f
}; 


float quick_dirty_vector_expf(float* dst, float* src, float max_x, size_t n) {
    // values determined using (python)sollya
    const float ln2 = 0.69314718246459961f;  // 0x1.62e43p-1f;    
    const float iln2 = 1.4426950216293335f;  //0x1.715476p0f;
    
    cpu.fpr[0]=ln2;//store in ft0=0
    cpu.fpr[isa_freg_index("fa1")]=iln2;//store in fa1=11
    vsetvlmax_e32m1("s3",TA,MA);//0d0079d7          	vsetvli	s3,zero,e32,m1,ta,ma

    //initial sum of exponentials
    //vsum=0
    cpu.fpr[isa_freg_index("fa2")]=0;//store in fa1=11
    vfmv_v_f(4,"fa2", vcsr.vl);//vfmv.v.f	v4,fa2

    // element-wise reduction accumulator
    cpu.fpr[isa_freg_index("fa3")]=1.0f;
    vfmv_v_f(9,"fa3", vcsr.vl);//vfmv.v.f	v9,fa3
    cpu.fpr[isa_freg_index("fa4")]=1.0f;
    vfmv_v_f(5, "fa4", vcsr.vl);//vfmv.v.f	v5,fa4
    cpu.fpr[isa_freg_index("fa5")]=0.49999997019767761f;
    vfmv_v_f(7, "fa5", vcsr.vl);//vfmv.v.f	v7,fa5
    cpu.fpr[isa_freg_index("fa6")]=0.1666666716337204f;
    vfmv_v_f(8, "fa6", vcsr.vl);//vfmv.v.f	v8,fa6
    cpu.fpr[isa_freg_index("fa7")]=0.041666667908430099f;
    vfmv_v_f(10, "fa7", vcsr.vl);//vfmv.v.f	v10,fa7
    cpu.fpr[isa_freg_index("fa3")]=0.0083333337697553482f;
    vfmv_v_f(11, "fa3", vcsr.vl);//vfmv.v.f	v11,fa3
    cpu.fpr[isa_freg_index("fa4")]=0.0013888889251995273f;
    vfmv_v_f(6, "fa4", vcsr.vl);//vfmv.v.f	v6,fa4////9

    float x[VLEN/32];
    float xf[VLEN/32];
    int m[VLEN/32];
    float mf[VLEN/32];
    float r[VLEN/32];
    float poly_vr[VLEN/32];
    float exp2_vk[VLEN/32];
    float sum=0;
    for(int i=0;i<n;i++){
        x[i] = src[i] - max_x;
        xf[i] = x[i] * iln2;
        m[i] = round(xf[i]);
        mf[i]=(float)m[i];
        r[i] = x[i] - mf[i] * ln2;
        poly_vr[i] = poly_coeffs[5];
        for (int j = 4; j >= 0; j--) {
            poly_vr[i] = poly_vr[i] * r[i] + poly_coeffs[j];
        }
        poly_vr[i] = poly_vr[i] * r[i] + 0x1.p0;
        exp2_vk[i] =std::pow(2, m[i]);
        dst[i]=std::pow(2, m[i])*poly_vr[i];
        sum=sum+dst[i];   
    }
    
    // argument reduction
    vsetvlmax_e32m1("a5",TU,MA);//vsetvli	a5,zero,e32,m1,tu,ma
    cpu.gpr[isa_reg_index("a3")]=0;
    void *ptr=pmem+cpu.gpr[isa_reg_index("a3")];
    
    vle32(2, ptr, vcsr.vl);//0206e107          	vle32.v	v2,(a3)  ---src stored in v2

    vfsub_vf(2,cpu.fpr[isa_freg_index("fs0")],2,vcsr.vl);//x-max_x          	vfsub.vf	v2,v2,fs0
    vfmul_vf(1,2,cpu.fpr[isa_freg_index("fa1")],vcsr.vl);//vxiln2=(x-max_x )iln2  vfmul.vf	v1,v2,fa1
    
    //THIS is not implemented in this file, instead we use emulation results "mf[i]", mf is v3
    vfcvt_x_f_v_i32m1(1,1,vcsr.vl);//vfcvt.x.f.v	v1,v1
    vfcvt_f_x_v_f32m1(3,1,vcsr.vl);//vfcvt.f.x.v	v3,v1
    

    int addr=sizeof(float)*VLEN/32;
    for(int i=0;i<VLEN/32;i++){
        std::memcpy(&pmem[addr+i].as_float,&cpu.vreg[1][i].i,sizeof(float));
    }

    cpu.fpr[isa_freg_index("fa5")]=ln2;
    vfnmsac_vf(2,isa_freg_index("fa5"),3,vcsr.vl);//be37d157          	vfnmsac.vf	v2,fa5,v3 r = x - k * log(2)
    
    //polynomial approximation exp(r)
    vmv1r_v(1,6,vcsr.vl);//vmv1r.v	v1,v6
    vfmadd_vv(1,2,11,vcsr.vl);//vfmadd.vv	v1,v2,v11
    vfmadd_vv(1,2,10,vcsr.vl);//vfmadd.vv	v1,v2,v10
    vfmadd_vv(1,2,8,vcsr.vl);//vfmadd.vv	v1,v2,v8
    vfmadd_vv(1,2,7,vcsr.vl);//vfmadd.vv	v1,v2,v7
    vfmadd_vv(1,2,5,vcsr.vl);//vfmadd.vv	v1,v2,v5
    vfmadd_vv(1,2,9,vcsr.vl);//vfmadd.vv	v1,v2,v9
    

    //Reconstruction of integer is not implemented by emulation
    ptr=pmem+addr;
    cpu.gpr[isa_reg_index("a3")]=addr;
    vle32(2, ptr, vcsr.vl);//0206e107          	vle32.v	v2,(a3)
    
    cpu.gpr[isa_reg_index("a3")]=127;
    vadd_vx(4,2,cpu.gpr[isa_reg_index("a3")],vcsr.vl);//vadd.vx	v4,v2,a3
    
    vsll_vx(3,4,23,vcsr.vl);//vsll.vi	v3,v4,23
    
    vfmul_vv(2,1,3,vcsr.vl);//vfmul.vv	v2,v3,v1
    

 
    vfmv_v_i_f32m1(1,0,vcsr.vl);//vmv.v.i	v1,0
    vfredusum_vs(4,2,1,vcsr.vl);//vfredusum.vs	v4,v2,v1
    
    //v2 stores the result of expf
    //v4 stores the sum of expf
    return sum;
}


softmax_bench_result_t softmax_stable_rvv_fp32_bench(float* dst, float* src, double* golden, size_t n) {

    //PART I: calulating the MAX value in the input array 
    vsetvlmax_e32m1("a7",TA,MA);//0d0078d7          	vsetvli	a7,zero,e32,m1,ta,ma

    //TODO: issue instruction and wait result
    cpu.fpr[isa_freg_index("fa5")]=-INFINITY;//fa5=-INFINITY
    vfmv_v_f(1,"fa5", vcsr.vl);//5e07d0d7          	vfmv.v.f	v1,fa5

    //TODO: issue instruction and wait result
    //TODO: compare dut and golden model: v1 

    vsetvlmax_e32m1("a5",TU,MA);//vsetvli	a5,ZERO,e32,m1,tu,ma

    //TODO: issue instruction and wait result

    gpr_write(isa_reg_index("a3"),0);
    
    // top->instr=0x0206e107;
    for(int i=0;i<VLEN/32;i++){
        std::memcpy(&pmem[0+i].as_float,&src[i],sizeof(float));
    }

    vle32(0, &pmem->as_float, vcsr.vl);//0206e107          	vle32.v	v2,(a3)  ---src stored in v2

    top->io_dispatch_s2v_valid=1; // @[src/main/scala/vpu/debug/VTopDebug.scala 9:14]
    top->io_dispatch_s2v_bits_robIdx_flag=0; // @[src/main/scala/vpu/debug/VTopDebug.scala 9:14]
    top->io_dispatch_s2v_bits_robIdx_value=0;, // @[src/main/scala/vpu/debug/VTopDebug.scala 9:14]
    top->io_dispatch_s2v_bits_inst=0x0206e107; // @[src/main/scala/vpu/debug/VTopDebug.scala 9:14]
    top->io_dispatch_s2v_bits_vcsr_vstart=0; // @[src/main/scala/vpu/debug/VTopDebug.scala 9:14]
    top->io_dispatch_s2v_bits_vcsr_vl=vcsr.vl; // @[src/main/scala/vpu/debug/VTopDebug.scala 9:14]
    top->io_dispatch_s2v_bits_vcsr_vxrm=0; // @[src/main/scala/vpu/debug/VTopDebug.scala 9:14]
    top->io_dispatch_s2v_bits_vcsr_=0; // @[src/main/scala/vpu/debug/VTopDebug.scala 9:14]
    top->io_dispatch_s2v_bits_vcsr_vlmul=vcsr.lmul, // @[src/main/scala/vpu/debug/VTopDebug.scala 9:14]
    top->io_dispatch_s2v_bits_vcsr_vsew=vcsr.sew; // @[src/main/scala/vpu/debug/VTopDebug.scala 9:14]
    top->io_dispatch_s2v_bits_vcsr_vill=0; // @[src/main/scala/vpu/debug/VTopDebug.scala 9:14]
    top->io_dispatch_s2v_bits_vcsr_ma=vcsr.vma; // @[src/main/scala/vpu/debug/VTopDebug.scala 9:14]
    top->io_dispatch_s2v_bits_vcsr_ta=vcsr.vta; // @[src/main/scala/vpu/debug/VTopDebug.scala 9:14]
    top->io_dispatch_s2v_bits_rs1=cpu.gpr[isa_reg_index("a3")]; // @[src/main/scala/vpu/debug/VTopDebug.scala 9:14]
    top->io_dispatch_s2v_bits_rs2=0; // @[src/main/scala/vpu/debug/VTopDebug.scala 9:14]
    // top->vdst=2;
    while(top->io_dispatch_s2v_ready==0){
        single_cycle(top, contextp, wave);
    }
    bool check=check_vreg(0);
    printf("The result of difftest is :%s\n",check?"True":"False");

    vfmax_vv(1,2,1,vcsr.vl);//1a2090d7          	vfmax.vv	v1,v2,v1
    cpu.fpr[isa_freg_index("fa5")]=-INFINITY;//fa5=15
    vfmv_v_f(2, "fa5", vcsr.vl);//   vfmv.v.f	v2,fa5
    vfredmax_vs(1,1,2,vcsr.vl);//vfredmax.vs	v1,v1,v2  -----max_x stored in v1
    cpu.fpr[isa_freg_index("fs0")]=cpu.vreg[1][0].f;//v[1][0]
    double max_x = src[0];
    for (int i = 1; i < n; ++i) {
        if (src[i] > max_x) max_x = src[i]; 
    }
    
    //TODO: wait result; compare dut and golden model: v1  and max_x
    float sum = 0.;
    sum =quick_dirty_vector_expf(dst, src, max_x, n);

    // computing the reciprocal of the sum of exponentials, once and for all
    float inv_sum = 1.f / sum;
    cpu.fpr[10]=inv_sum;

    

    //PART III: normalizing each element

    vsetvlmax_e32m1("a5",TU,MA);//vsetvli	a5,zero,e32,m1,tu,ma
    cpu.fpr[isa_freg_index("fa0")]=1/cpu.vreg[4][0].f;

    vfmul_vf(2,2,cpu.fpr[isa_freg_index("fa0")],vcsr.vl);//vfmul.vf	v2,v2,fa0
    
    for (int i = 0; i < n; ++i) {
        dst[i] =dst[i] *inv_sum ;
    } 
    
    //TODO: wait result and compare dut and golden model: v1 and dst
    
    softmax_bench_result_t bench_result;
    // initializing bench result error values
    bench_result.max_abs_error = 0.0;
    bench_result.max_rel_error = 0.0;

    unsigned i;
    double sum_square_errors = 0.0;
    double sum_rel_errors = 0.0;
#       ifdef VERY_VERBOSE
        printf("relative errors: ");
#       endif
    for (i = 0; i < n; ++i) {
        double abs_error = fabs(dst[i] - cpu.vreg[2][i].f);
        double rel_error = abs_error / (double) dst[i];
#       ifdef VERY_VERBOSE
        printf("%.8e ", rel_error);
#       endif
        sum_square_errors += rel_error * rel_error;
        sum_rel_errors += rel_error;

        if (abs_error > bench_result.max_abs_error) bench_result.max_abs_error = abs_error;
        if (rel_error > bench_result.max_rel_error) bench_result.max_rel_error = rel_error;
    }

#   ifdef VERY_VERBOSE
    printf("\n");
#   endif

    bench_result.error_norm2 = sqrt(sum_square_errors);
    bench_result.mean_rel_error = sum_rel_errors / (double) n;
    return bench_result;
}


bool check_vreg(int i){
    for(int idx=0;idx<VLEN/32;idx++){
        if(cpu.vreg[i][idx].f!=diff_vreg[i][idx]) {
            printf("the different id is %d",idx);
            printf("d vreg=%f,\tv vreg=%f\n\n",diff_vreg[2][idx],cpu.vreg[2][idx].f);
            return false;
        }
    }
    return true;
}


void reset(int n, VVTopDebug *top, VerilatedContext *contextp, VerilatedVcdC *wave)
{
  top->reset = 1;
  n--;
  while (n-- > 0)
  {
    single_cycle(top, contextp, wave);
  }

  //add another cycle to make sure the reset is effective
  top->clock = 1;
  top->eval();
  wave->dump(main_time); // simulation time
  main_time += CLK_PERIOD / 2;
  top->clock = 0;
  top->reset = 0;
  top->eval();
  wave->dump(main_time); // simulation time
  main_time += CLK_PERIOD / 2;
}

void single_cycle(VVTopDebug *top, VerilatedContext *contextp, VerilatedVcdC *wave)
{
  top->clock = 1;
  top->eval();
  //wave->dump(contextp->time()); // simulation time
  //contextp->timeInc(1);
  wave->dump(main_time); // simulation time
  main_time += CLK_PERIOD / 2;
  top->clock = 0;
  top->eval();
  //wave->dump(contextp->time()); // simulation time
  //contextp->timeInc(1);
  wave->dump(main_time); // simulation time
  main_time += CLK_PERIOD / 2;
}

