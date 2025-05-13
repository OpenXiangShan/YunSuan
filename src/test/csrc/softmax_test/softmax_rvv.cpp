
#include "softmax_bench.h"
#include <stdio.h>

#define SEW 32
#include <reg.h>
#include <rvv.h>
#include <mem.h>

#include <VVTopDebug.h>
#include "verilated.h"
#include "svdpi.h"
#include "VVTopDebug__Dpi.h"
#include <verilated_vcd_c.h>
#include <cmath>
#include <half.hpp>
#include <random>
#include <iostream>

extern VVTopDebug *top;
extern VerilatedContext *contextp;
extern VerilatedVcdC *wave ;
extern FloatUintUnion  pmem[CONFIG_MSIZE];
extern bool commit_global;
#define CLK_PERIOD 10    // 时钟周期（单位：ps）
vluint64_t main_time = 0;
static uint8_t robIdx=0;

bool check_vreg(uint8_t rf_addr);
bool check_vreg_i(uint8_t rf_addr);
bool check_vreg_rec(uint8_t rf_addr, float diff_threshold);
bool check_vreg_uint16(uint8_t rf_addr);
void single_cycle(VVTopDebug *top, VerilatedContext *contextp, VerilatedVcdC *wave);
void reset(int n, VVTopDebug *top, VerilatedContext *contextp, VerilatedVcdC *wave);
void dut_input_execute(uint32_t instr, int sew, int lmul, uint64_t rs1, uint64_t rs2, bool robIdx_flag, uint8_t robIdx);
void kill_sim(bool abort_flag);

FloatUintUnion diff_vreg[32][VLEN/32]={0};
const float poly_coeffs[] = {

    1.0f,                     // 0x1p0f
    0.49999997019767761f,     // 0x1.fffff8p-2f
    0.1666666716337204f,      // 0x1.55548ep-3f
    0.041666667908430099f,    // 0x1.555b98p-5f
    0.0083333337697553482f,   // 0x1.123bccp-7f
    0.0013888889251995273f,   // 0x1.6850e4p-10f
}; 


float quick_dirty_vector_expf(float* dst, float* src, float max_x, size_t n) {
    bool check=false;
    // values determined using (python)sollya
    const float ln2 = 0.69314718246459961f;  // 0x1.62e43p-1f;    
    const float iln2 = 1.4426950216293335f;  //0x1.715476p0f;
    
    cpu.fpr[0].as_fp32=ln2;//store in ft0=0
    cpu.fpr[0].as_uint32x2.high=0xffffffff;
    cpu.fpr[isa_freg_index("fa1")].as_fp32=iln2;//store in fa1=11
    cpu.fpr[isa_freg_index("fa1")].as_uint32x2.high=0xffffffff;
    vsetvlmax_e32("s3",TA,MA);//0d0079d7          	vsetvli	s3,zero,e32,m1,ta,ma

    //Instr 3:vfmv.v.f	v8,fa2
    cpu.fpr[isa_freg_index("fa2")].as_uint64=0;
    //model execution
    vfmv_v_f(8,"fa2", vcsr.vl);//vfmv.v.f	v8,fa2
    //dut execution
    dut_input_execute(0x5e065457 , vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa2")].as_uint64,   0, false, robIdx++);
    check=check_vreg(8);
    printf("The result of \"Instr 3:vfmv.v.f	v8,fa2\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);


    // element-wise reduction accumulator
    //Instr 4:vfmv.v.f	v18,fa3
    cpu.fpr[isa_freg_index("fa3")].as_fp32=1.0f;
    cpu.fpr[isa_freg_index("fa3")].as_uint32x2.high=0xffffffff;
    //model execution
    vfmv_v_f(18,"fa3", vcsr.vl);//vfmv.v.f	v18,fa3
    //dut execution
    dut_input_execute(0x5e06d957 , vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa3")].as_uint64,   0, false, robIdx++);
    check=check_vreg(18);
    printf("The result of \"Instr 4:vfmv.v.f	v18,fa3\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 5: vfmv.v.f	v10,fa4
    cpu.fpr[isa_freg_index("fa4")].as_fp32=1.0f;
    cpu.fpr[isa_freg_index("fa4")].as_uint32x2.high=0xffffffff;
    //model execution
    vfmv_v_f(10, "fa4", vcsr.vl);//vfmv.v.f	v10,fa4
    //dut execution
    dut_input_execute(0x5e075557 , vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa4")].as_uint64,   0, false, robIdx++);
    check=check_vreg(10);
    printf("The result of \"Instr 5: vfmv.v.f	v10,fa4\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 6: vfmv.v.f	v14,fa5
    cpu.fpr[isa_freg_index("fa5")].as_fp32=0.49999997019767761f;
    cpu.fpr[isa_freg_index("fa5")].as_uint32x2.high=0xffffffff;
    //model execution
    vfmv_v_f(14, "fa5", vcsr.vl);//vfmv.v.f	v14,fa5
    //dut execution
    dut_input_execute(0x5e07d757, vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa5")].as_uint64,   0, false, robIdx++);
    check=check_vreg(14);
    printf("The result of \"Instr 6: vfmv.v.f	v14,fa5\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 7:vfmv.v.f	v16,fa6
    cpu.fpr[isa_freg_index("fa6")].as_fp32=0.1666666716337204f;
    cpu.fpr[isa_freg_index("fa6")].as_uint32x2.high=0xffffffff;
    //model execution
    vfmv_v_f(16, "fa6", vcsr.vl);//vfmv.v.f	v16,fa6
    //dut execution
    dut_input_execute(0x5e085857 , vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa6")].as_uint64,   0, false, robIdx++);
    check=check_vreg(16);
    printf("The result of \"Instr 7:vfmv.v.f	v16,fa6\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 8: vfmv.v.f	v20,fa7
    cpu.fpr[isa_freg_index("fa7")].as_fp32=0.041666667908430099f;
    cpu.fpr[isa_freg_index("fa7")].as_uint32x2.high=0xffffffff;
    //model execution
    vfmv_v_f(20, "fa7", vcsr.vl);//vfmv.v.f	v20,fa7
    //dut excecution
    dut_input_execute(0x5e08da57 , vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa7")].as_uint64,   0, false, robIdx++);
    check=check_vreg(20);
    printf("The result of \"Instr 8: vfmv.v.f	v20,fa7\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 9:vfmv.v.f	v22,fa3
    cpu.fpr[isa_freg_index("fa3")].as_fp32=0.0083333337697553482f;
    cpu.fpr[isa_freg_index("fa3")].as_uint32x2.high=0xffffffff;
    //model execution
    vfmv_v_f(22, "fa3", vcsr.vl);//vfmv.v.f	v22,fa3
    //dut excecution
    dut_input_execute(0x5e06db57, vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa3")].as_uint64,   0, false, robIdx++);
    check=check_vreg(22);
    printf("The result of \"Instr 9:vfmv.v.f	v22,fa3\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 10: vfmv.v.f	v12,fa4
    cpu.fpr[isa_freg_index("fa4")].as_fp32=0.0013888889251995273f;
    cpu.fpr[isa_freg_index("fa4")].as_uint32x2.high=0xffffffff;
    //model execution
    vfmv_v_f(12, "fa4", vcsr.vl);//vfmv.v.f	v12,fa4////9
    // dut excecution
    dut_input_execute(0x5e075657, vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa4")].as_uint64, 0, false, robIdx++);
    check = check_vreg(12);
    printf("The result of \"Instr 10: vfmv.v.f	v12,fa4\" difftest is :%s\n", check ? "True" : "False");
    kill_sim(check);

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
    vsetvlmax_e32("a5",TU,MA);//vsetvli	a5,zero,e32,m1,tu,ma
    cpu.gpr[isa_reg_index("a3")]=0;
    void *ptr=pmem+cpu.gpr[isa_reg_index("a3")];
    
    // //Instr 11: vle32.v	v2,(a3)
    // vle32(2, ptr, vcsr.vl);//0206e107          	vle32.v	v2,(a3)  ---src stored in v2
    // //dut execution
    // dut_input_execute(0x0206e107, vcsr.sew, vcsr.lmul, cpu.gpr[isa_reg_index("a3")], 0, false, robIdx++);
    // check=check_vreg(2);
    // printf("The result of \"Instr 14: vle32.v	v2,(a3)\" difftest is :%s\n",check?"True":"False");
    // kill_sim(check);



    //Instr 11: vfsub.vf	v6,v4,fs0
    //model execution
    vfsub_vf(6,cpu.fpr[isa_freg_index("fs0")].as_fp32,4,vcsr.vl);//x-max_x          	vfsub.vf	v6,v4,fs0
    //dut execution
    dut_input_execute(0x0a445357, vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fs0")].as_uint64, 0, false, robIdx++);
    check=check_vreg(6);
    printf("The result of \"Instr 11: vfsub.vf	v6,v4,fs0\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 12: vfmul.vf	v2,v6,fa1 
    //model execution
    vfmul_vf(2,6,cpu.fpr[isa_freg_index("fa1")].as_fp32,vcsr.vl);//vxiln2=(x-max_x )iln2  vfmul.vf	v2,v6,fa1 
    // dut execution
    dut_input_execute(0x9265d157, vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa1")].as_uint64, 0, false, robIdx++);
    check = check_vreg(2);
    printf("The result of \"Instr 12: vfmul.vf	v2,v6,fa1 \" difftest is :%s\n", check ? "True" : "False");
    kill_sim(check);

    //Instr 13: vfcvt.x.f.v	v24,v2
    //model execution
    vfcvt_x_f_v_i32m1(24,2,vcsr.vl);//vfcvt.x.f.v	v24,v2
    //dut execution
    dut_input_execute(0x4a209c57, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg_i(24);
    printf("The result of \"Instr 13: vfcvt.x.f.v	v24,v2\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 14: vfcvt.f.x.v	v28,v24
    //model execution
    vfcvt_f_x_v_f32m1(28,24,vcsr.vl);//vfcvt.f.x.v	v28,v24
    //dut execution
    dut_input_execute(0x4b819e57, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(28);
    printf("The result of \"Instr 14: vfcvt.f.x.v	v28,v24\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);



    uint64_t addr=sizeof(float)*LMUL*VLEN/32;
    for(int i=0;i<LMUL*VLEN/32;i++){
        std::memcpy(&pmem[addr+i].as_float,&cpu.vreg[1+i/(VLEN/vcsr.sew)][i%(VLEN/vcsr.sew)].i,sizeof(float));
    }

    //Instr 15: vfnmsac.vf	v6,fa5,v14
    cpu.fpr[isa_freg_index("fa5")].as_fp32=ln2;
    cpu.fpr[isa_freg_index("fa5")].as_uint32x2.high=0xffffffff;
    //model execution
    vfnmsac_vf(6,isa_freg_index("fa5"),28,vcsr.vl);//bee7d1d7          	vfnmsac.vf	v6,fa5,v28 r = x - k * log(2)
    //dut execution
    dut_input_execute(0xbfc7d357, vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa5")].as_uint64, 0, false, robIdx++);
    check=check_vreg(6);
    printf("The result of \"Instr 15: vfnmsac.vf	v6,fa5,v28\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);
    
    //polynomial approximation exp(r)

    //Instr 16: vmv2r.v	v2,v12
    //model execution
    #if LMUL == 2
    vmv2r_v(2,12);//vmv2r.v	v2,v12
    dut_input_execute(0x9ec0b157, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(2);
    printf("The result of \"Instr 16: vmv2r.v	v2,v12\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);
    #elif LMUL == 1
    vmv1r_v(2,12);//vmv1r.v	v2,v12
    dut_input_execute(0x9ec03157, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(2);
    printf("The result of \"Instr 16: vmv1r.v	v2,v12\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);
    #endif

    //dut execution
    

    //Instr 17: vfmadd.vv	v2,v6,v22
    //model execution
    vfmadd_vv(2,6,22,vcsr.vl);//vfmadd.vv	v2,v6,v22
    //dut execution
    dut_input_execute(0xa3631157, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(2);
    printf("The result of \"Instr 17: vfmadd.vv	v2,v6,v22\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 18: vfmadd.vv	v2,v6,v20
    //model execution
    vfmadd_vv(2,6,20,vcsr.vl);//vfmadd.vv	v2,v6,v20
    //dut execution
    dut_input_execute(0xa3431157, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(2);
    printf("The result of \"Instr 18: vfmadd.vv	v2,v6,v20\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 19: vfmadd.vv	v2,v6,v16
    //model execution
    vfmadd_vv(2,6,16,vcsr.vl);//vfmadd.vv	v2,v6,v16
    // dut execution
    dut_input_execute(0xa3031157, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check = check_vreg(2);
    printf("The result of \"Instr 19: vfmadd.vv	v2,v6,v16\" difftest is :%s\n", check ? "True" : "False");
    kill_sim(check);

    //Instr 20: vfmadd.vv	v2,v6,v14
    //model execution
    vfmadd_vv(2,6,14,vcsr.vl);//vfmadd.vv	v2,v6,v14
    // dut execution
    dut_input_execute(0xa2e31157, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check = check_vreg(2);
    printf("The result of \"Instr 20: vfmadd.vv	v2,v6,v14\" difftest is :%s\n", check ? "True" : "False");
    kill_sim(check);

    //Instr 21: vfmadd.vv	v2,v6,v10
    //model execution
    vfmadd_vv(2,6,10,vcsr.vl);//vfmadd.vv	v2,v6,v10
    // dut execution
    dut_input_execute(0xa2a31157, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check = check_vreg(2);
    printf("The result of \"Instr 21: vfmadd.vv	v2,v6,v10\" difftest is :%s\n", check ? "True" : "False");
    kill_sim(check);



    //Instr 22: vfmadd.vv	v2,v6,v18
    vfmadd_vv(2,6,18,vcsr.vl);//vfmadd.vv	v2,v6,v18
    // dut execution
    dut_input_execute(0xa3231157, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check = check_vreg(2);
    printf("The result of \"Instr 22: vfmadd.vv	v2,v6,v18\" difftest is :%s\n", check ? "True" : "False");

    kill_sim(check);

    // //Instr 23: vle32.v	v2,(a3)
    // ptr=pmem+addr;
    // cpu.gpr[isa_reg_index("a3")]=addr;
    // //model execution
    // vle32(2, ptr, vcsr.vl);//0206e107          	vle32.v	v2,(a3)
    // //dut execution
    // dut_input_execute(0x0206e107, vcsr.sew, vcsr.lmul, cpu.gpr[isa_reg_index("a3")], 0, false, robIdx++);
    // check=check_vreg(2);
    // printf("The result of \"Instr 27: vle32.v	v2,(a3)\" difftest is :%s\n",check?"True":"False");
    // kill_sim(check);
    
    //Instr 23: vadd.vx	v8,v24,a3
    cpu.gpr[isa_reg_index("a3")]=127;
    //model execution
    vadd_vx(8,24,cpu.gpr[isa_reg_index("a3")],vcsr.vl);//vadd.vx	v8,v24,a3
    //dut execution
    dut_input_execute(0x0386c457, vcsr.sew, vcsr.lmul, cpu.gpr[isa_reg_index("a3")], 0, false, robIdx++);
    check=check_vreg(8);
    printf("The result of \"Instr 23: vadd.vx	v8,v24,a3\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 24: vsll.vi	v6,v8,23
    //model execution
    vsll_vx(6,8,23,vcsr.vl);//vsll.vi	v3,v4,23
    //dut execution
    dut_input_execute(0x968bb357, vcsr.sew, vcsr.lmul, 23, 0, false, robIdx++);
    check=check_vreg(6);
    printf("The result of \"Instr 24: vsll.vi	v6,v8,23\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);
    
    //Instr 25: vfmul.vv	v4,v6,v2
    //model execution
    vfmul_vv(4,6,2,vcsr.vl);//vfmul.vv	v4,v6,v2
    //dut execution
    dut_input_execute(0x92611257, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(4);
    printf("The result of \"Instr 25: vfmul.vv	v4,v6,v2\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);
    
    //Instr 26: vmv.v.i	v1,0
    vfmv_v_i_f32m1(2,0,vcsr.vl);//vmv.v.i	v1,0
    //dut execution
    dut_input_execute(0x5e003157, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(2);
    printf("The result of \"Instr 26: vmv.v.i	v2,0\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 27: vfredusum.vs	v8,v4,v2
    vfredusum_vs(8,4,2,vcsr.vl);//vfredusum.vs	v8,v4,v2
    //dut execution
    dut_input_execute(0x06411457, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(8);
    printf("The result of \"Instr 27: vfredusum.vs	v8,v4,v2\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);
    //v4 stores the result of expf
    //v8 stores the sum of expf
    return sum;
}

softmax_bench_result_t softmax_stable_rvv_fp32_bench(float* dst, float* src, double* golden, size_t n) {
    bool check=true;

    //PART I: calulating the MAX value in the input array 
    // vsetvlmax_e32m1("a7",TA,MA);//0d0078d7          	vsetvli	a7,zero,e32,m1,ta,ma

    // //Instr1:vfmv.v.f	v1,fa5
    // cpu.fpr[isa_freg_index("fa5")].as_fp32=-INFINITY;//fa5=-INFINITY
    // cpu.fpr[isa_freg_index("fa5")].as_uint32x2.high=0xFFFFFFFF;
    // //moodel execution
    // vfmv_v_f(1,"fa5", vcsr.vl);//5e07d0d7          	vfmv.v.f	v1,fa5
    // //dut execution
    // dut_input_execute(0x5e07d0d7, vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa5")].as_uint64,   0, false, robIdx++);
    // check=check_vreg(1);
    // printf("The result of \"Instr 1: vfmv.v.f	v1,fa5\" difftest is :%s\n",check?"True":"False");
    // kill_sim(check);

    vsetvlmax_e32("a5",TU,MA);//vsetvli	a5,ZERO,e32,m1,tu,m

    //Instr0:vle32.v	v2,(a3)
    gpr_write(isa_reg_index("a3"),0);
    for(int i=0;i<VLEN*LMUL/32;i++){
        std::memcpy(&pmem[0+i].as_float,&src[i],sizeof(float));
    }
    //model execution
    vle32(4, &pmem->as_float, vcsr.vl);//vle32.v	v4,(a3)  
    //dut execution
    dut_input_execute(0x0206e207, vcsr.sew, vcsr.lmul, cpu.gpr[isa_reg_index("a3")], 0, false, robIdx++);
    check=check_vreg(4);
    printf("The result of \"Instr 0: vle32.v	v4,(a3)\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    // //Instr3:vfmax.vv	v1,v2,v1
    // //model execution
    // vfmax_vv(1,2,1,vcsr.vl);//vfmax.vv	v1,v2,v1
    // dut_input_execute(0x1a2090d7, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    // check=check_vreg(1);
    // printf("The result of \"Instr 3: vfmax.vv	v1,v2,v1\" difftest is :%s\n",check?"True":"False");
    // kill_sim(check);

    //Instr1:vfmv.v.f	v2,fa5
    cpu.fpr[isa_freg_index("fa5")].as_fp32=-INFINITY;//
    cpu.fpr[isa_freg_index("fa5")].as_uint32x2.high=0xFFFFFFFF;
    //model execution
    vfmv_v_f(2, "fa5", vcsr.vl);//vfmv.v.f	v2,fa5
    //dut execution
    dut_input_execute(0x5e07d157 , vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa5")].as_uint64, 0, false, robIdx++);
    check=check_vreg(2);
    printf("The result of \"Instr 1: vfmv.v.f	v2,fa5\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr2:vfredmax.vs	v2,v4,v2
    //model execution
    vfredmax_vs(2,4,2,vcsr.vl);//vfredmax.vs	v2,v4,v2  
    //dut execution
    dut_input_execute(0x1e411157, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(2);
    printf("The result of \"Instr 2: vfredmax.vs	v2,v4,v2\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    cpu.fpr[isa_freg_index("fs0")].as_fp32=diff_vreg[2][0].as_float;//v[2][0]
    double max_x = src[0];
    for (int i = 1; i < n; ++i) {
        if (src[i] > max_x) max_x = src[i]; 
    }
    
    float sum = 0.;
    sum =quick_dirty_vector_expf(dst, src, max_x, n);

    // computing the reciprocal of the sum of exponentials, once and for all
    float inv_sum = 1.f / sum;
    cpu.fpr[10].as_fp32=inv_sum;

    //PART III: normalizing each element
    vsetvlmax_e32("a5",TU,MA);//vsetvli	a5,zero,e32,m1,tu,ma

    
    //Instr 28:vrgather.vi	v10,v8,0
    //DIVISION HERE
    // printf("The value of v[8] is :%x\n",cpu.vreg[8][0].u);
    vrgather_vi(10,8,0,vcsr.vl);//vrgather.vi	v10,v8,0
    dut_input_execute(0x32803557, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(10);
    printf("The result of \"Instr 28: vrgather.vi	v10,v8,0\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);  
    
    //Instr 29:vfrec7.v	v12,v10
    dut_input_execute(0x4ea29657, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    // for (int i=0; i < VLEN*LMUL/vcsr.sew; i++){
    //     cpu.vreg[12][i].u=diff_vreg[12][i].as_uint32;
    // }
    for (int i=0; i < VLEN*LMUL/vcsr.sew; i++){
        cpu.vreg[12][i].f=1/cpu.vreg[8][0].f;
    }
    check=check_vreg_rec(12,1e-2f);
    printf("The result of \"Instr 29:vfrec7.v	v12,v10\" difftest is :%s\n",check?"True":"False");
    // printf("The result of \"Instr 29:vfrec7.v	v12,v10\" is copied to REF model!\n");
    kill_sim(check); 

    //Instr 30:vmv.v.x	v14,t0
    cpu.gpr[isa_reg_index("t0")]=0x40000000;
    vmv_v_x(14,"t0",vcsr.vl);//vmv.v.x	v14,t0
    dut_input_execute(0x5e02c757, vcsr.sew, vcsr.lmul, 0x40000000, 0, false, robIdx++);
    check=check_vreg(14);
    printf("The result of \"Instr 30:vmv.v.x	v14,t0\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);
    
    //Instr 31:vfnmsac.vv	v14,v10,v12
    vfnmsac_vv(14,10,12,vcsr.vl);//vfnmsac.vv	v14,v10,v12
    //dut execution
    dut_input_execute(0xbec51757, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg_rec(14,1e-2f);
    printf("The result of \"Instr 31:vfnmsac.vv	v14,v10,v12\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 32:vfmul.vv	v12,v12,v14
    vfmul_vv(12,12,14,vcsr.vl);//vfmul.vv	v12,v12,v14
    //dut execution
    dut_input_execute(0x92c71657, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg_rec(12,1e-2f);
    printf("The result of \"Instr 32:vfmul.vv	v12,v12,v14\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 33:vmv.v.x	v14,t0
    cpu.gpr[isa_reg_index("t0")]=0x40000000;
    vmv_v_x(14,"t0",vcsr.vl);//vmv.v.x	v14,t0
    dut_input_execute(0x5e02c757, vcsr.sew, vcsr.lmul, 0x40000000, 0, false, robIdx++);
    check=check_vreg(14);
    printf("The result of \"Instr 33:vmv.v.x	v14,t0\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);
    
    //Instr 34:vfnmsac.vv	v14,v10,v12
    vfnmsac_vv(14,10,12,vcsr.vl);//vfnmsac.vv	v14,v10,v12
    //dut execution
    dut_input_execute(0xbec51757, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg_rec(14,1e-2f);
    printf("The result of \"Instr 34:vfnmsac.vv	v14,v10,v12\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 35:vfmul.vv	v12,v12,v14
    vfmul_vv(12,12,14,vcsr.vl);//vfmul.vv	v12,v12,v14
    //dut execution
    dut_input_execute(0x92c71657, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(12);
    printf("The result of \"Instr 35:vfmul.vv	v12,v12,v14\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);
    
    //Instr 36: vfmul.vv	v4,v4,v12
    // cpu.fpr[isa_freg_index("fa0")].as_fp32=1/diff_vreg[8][0].as_float;
    // cpu.fpr[isa_freg_index("fa0")].as_uint32x2.high=0xffffffff;
    vfmul_vv(4,4,12,vcsr.vl);//vfmul.vv	v4,v4,v12
    //dut execution
    dut_input_execute(0x92461257, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(4);
    printf("The result of \"Instr 36: vfmul.vv	v4,v4,v12\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);    
    printf("Softmax final result:\n\n");
    for (int idx=4; idx < 4+LMUL; idx++ ){
        for (int i = 0; i < VLEN/32; i++){
         printf("cpu.vreg[%d][%d]=%x\t  diff.vreg[%d][%d]=%x\n", idx, i, cpu.vreg[idx][i].u, idx, i,diff_vreg[idx][i].as_uint32);
    }
    }

    //Widening/narrowing
    vcsr.sew=16;
    vcsr.vl=VLEN*vcsr.lmul/vcsr.sew;
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<float> dist(-4.0f, 4.0f);
    for (int i = 0; i < VLEN / 16; ++i)
    {
        size_t elem_idx = i / 2; // 每 2 个 FP16 存到一个 32 位单元
        size_t h_idx = i % 2;    // 0=低16位, 1=高16位
        auto get_half_bits = [](half_float::half h)
        {
            uint16_t bits;
            static_assert(sizeof(h) == sizeof(bits), "Size mismatch");
            std::memcpy(&bits, &h, sizeof(bits));
            return bits;
        };
        half_float::half val10 = half_float::half(dist(gen));
        cpu.vreg[10][elem_idx].h[h_idx] = get_half_bits(val10);
        pmem[0 + elem_idx].as_half[h_idx] = cpu.vreg[10][elem_idx].h[h_idx];

        half_float::half val12 = half_float::half(dist(gen));
        cpu.vreg[12][elem_idx].h[h_idx] = get_half_bits(val12);
        pmem[VLEN/16 + elem_idx].as_half[h_idx] = cpu.vreg[12][elem_idx].h[h_idx];

        half_float::half val14 = half_float::half(dist(gen));
        cpu.vreg[14][elem_idx].h[h_idx] = get_half_bits(val14);
        pmem[VLEN /16*2 + elem_idx].as_half[h_idx] = cpu.vreg[14][elem_idx].h[h_idx];
    }
    //Instr 37: vle32.v v10, (a3) a3=0
    dut_input_execute(0x0206e507, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(10);
    printf("The result of \"Instr 37: vle32.v v10, (a3)\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);
   
    //Instr 38: vle32.v v12, (a3) a3=VLEN / 16 * 4
    dut_input_execute(0x0206e607, vcsr.sew, vcsr.lmul, VLEN /16*4 , 0, false, robIdx++);
    check=check_vreg(12);
    printf("The result of \"Instr 38: vle32.v v12, (a3)\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 39: vle32.v v14, (a3) a3=VLEN /16*2*4
    dut_input_execute(0x0206e707, vcsr.sew, vcsr.lmul,VLEN /16*2 *4, 0, false, robIdx++);
    check=check_vreg(14);
    printf("The result of \"Instr 39: vle32.v v14, (a3)\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);
    // for(int i=0;i<VLEN/16;i++){
    //     printf("cpu.v[10][%d]=%x\tcpu.v[12][%d]=%x\tcpu.v[14][%d]=%x\n",i,cpu.vreg[10][i/2].h[i%2],i,cpu.vreg[12][i/2].h[i%2],i,cpu.vreg[14][i/2].h[i%2]);
    // }


    //Instr 40: vfwmacc.vv v14, v10, v12 lmul=1->emul=2
    
    vfwmacc_vv(14,10,12,vcsr.vl);
    dut_input_execute(0xf2c51757, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(14)&&check_vreg(15);
    printf("The result of \"Instr 40: vfwmacc.vv v14, v10, v12\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 41: vfncvt.f.f.w v16, v14 emul=2->lmul=1
    vfncvt_f_f(16,14,vcsr.vl);
    dut_input_execute(0x4aea1857, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg_uint16(16);
    printf("The result of \"Instr 41: vfncvt.f.f.w v16, v14\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    for (int i = 0; i < n; ++i) {
        dst[i] =dst[i] *inv_sum ;
    }
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
        double abs_error = fabs(diff_vreg[4+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].as_float - cpu.vreg[4+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f);
        double rel_error = abs_error / (double) cpu.vreg[4+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f;
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


bool check_vreg(uint8_t rf_addr){
    printf("rob index: %02d  ", robIdx-1);
    for(int i=rf_addr; i<rf_addr+LMUL;i++){
        for(int element=0;element<VLEN/32;element++){
            float abs_err=fabs(cpu.vreg[i][element].f - diff_vreg[i][element].as_float);
            float rel_err = fabs(cpu.vreg[i][element].f - diff_vreg[i][element].as_float) / (fabs(cpu.vreg[i][element].f) > 1e-6f ? fabs(cpu.vreg[i][element].f) : 1.0f);
            if(abs_err > 1e-5f && rel_err >  1e-6f) {
                printf("The different register id is %d\n",i);
                printf("The element index is : %d\n",element);
                for(int m=rf_addr; m < rf_addr+LMUL; m++){
                    for(int n=0;n<VLEN/32;n++){
                        printf("cpu.vreg[%d][%d]=%x\t  diff.vreg[%d][%d]=%x\n",m, n, cpu.vreg[m][n].u, m, n,diff_vreg[m][n].as_uint32);
                    }
                }
                return false;
            }
        }
    }
    return true;
}

bool check_vreg_i(uint8_t rf_addr){
    printf("rob index: %02d  ", robIdx-1);
    for(int i=rf_addr; i<rf_addr+1;i++){
        for(int element=0;element<VLEN/32;element++){
            if(cpu.vreg[i][element].u!=diff_vreg[i][element].as_uint32) {
                printf("The different register id is %d\n",i);
                printf("The element index is : %d\n",element);
                for(int m=rf_addr; m < rf_addr+LMUL; m++){
                    for(int n=0;n<VLEN/32;n++){
                        printf("cpu.vreg[%d][%d]=%d\t  diff.vreg[%d][%d]=%d\n",m, n, cpu.vreg[m][n].i, m, n,diff_vreg[m][n].as_int32);
                    }
                }
                return false;
            }
        }
    }
    // for(int element=0;element<VLEN/32;element++){
    //     printf("cpu.vreg[%d][%d]=%f\t  diff.vreg[%d][%d]=%f\n",rf_addr, element, cpu.vreg[rf_addr][element].f, rf_addr, element,diff_vreg[rf_addr][element].as_float); 
    // }
    // for(int idx=0;idx<VLEN/32;idx++){
    //     if(cpu.vreg[i][idx].f!=diff_vreg[i][idx]) {
    //         printf("the different id is %d",idx);
    //         printf("d vreg=%f,\tv vreg=%f\n\n",diff_vreg[2][idx],cpu.vreg[2][idx].f);
    //         return false;
    //     
    commit_global=false;
    return true;
}

bool check_vreg_rec(uint8_t rf_addr, float diff_threshold){
    printf("rob index: %02d  ", robIdx-1);
    for(int i=rf_addr; i<rf_addr+LMUL;i++){
        for(int element=0;element<VLEN/32;element++){
            float abs_err=fabs(cpu.vreg[i][element].f - diff_vreg[i][element].as_float);
            float rel_err = fabs(cpu.vreg[i][element].f - diff_vreg[i][element].as_float) / (fabs(cpu.vreg[i][element].f) > 1e-6f ? fabs(cpu.vreg[i][element].f) : 1.0f);

            // auto diff = cpu.vreg[i][element].f > diff_vreg[i][element].as_float ?
            // (cpu.vreg[i][element].f - diff_vreg[i][element].as_float)/cpu.vreg[i][element].f :
            // (diff_vreg[i][element].as_float - cpu.vreg[i][element].f)/cpu.vreg[i][element].f;
            if(abs_err>diff_threshold/10&&rel_err>diff_threshold) {
                printf("The different register id is %d\n",i);
                printf("The element index is : %d\n",element);
                for(int m=rf_addr; m < rf_addr+LMUL; m++){
                    for(int n=0;n<VLEN/32;n++){
                        printf("cpu.vreg[%d][%d]=%x\t  diff.vreg[%d][%d]=%x\n",m, n, cpu.vreg[m][n].u, m, n,diff_vreg[m][n].as_uint32);
                    }
                }
                return false;
            }
        }
    }
    commit_global=false;
    return true;
}

bool check_vreg_uint16(uint8_t rf_addr){
    printf("rob index: %02d  ", robIdx-1);
    for(int i=rf_addr; i<rf_addr+LMUL;i++){
        for(int element=0;element<VLEN/16;element++){
            auto diff = cpu.vreg[i][element/2].h[element%2] > diff_vreg[i][element/2].as_half[element%2] ?
            (cpu.vreg[i][element/2].h[element%2] - diff_vreg[i][element/2].as_half[element%2])/cpu.vreg[i][element/2].h[element%2] :
            (diff_vreg[i][element/2].as_half[element%2] - cpu.vreg[i][element/2].h[element%2])/cpu.vreg[i][element/2].h[element%2];
            if(diff>1e-2) {
                printf("The different register id is %d\n",i);
                printf("The element index is : %d\n",element);
                for(int m=rf_addr; m < rf_addr+LMUL; m++){
                    for(int n=0;n<VLEN/16;n++){
                        printf("cpu.vreg[%d][%d]=%x\t  diff.vreg[%d][%d]=%x\n",m, n, cpu.vreg[m][n/2].h[n%2], m, n,diff_vreg[m][n/2].as_half[n%2]);
                    }
                }
                return false;
            }
        }
    }
    commit_global=false;
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

void dut_input_execute(uint32_t instr, int sew, int lmul, uint64_t rs1, uint64_t rs2, bool robIdx_flag, uint8_t robIdx){
    int cycle_count=0;
    top->io_dispatch_s2v_valid=1; 
    top->io_dispatch_s2v_bits_robIdx_flag=robIdx_flag; 
    top->io_dispatch_s2v_bits_robIdx_value=robIdx; 
    top->io_dispatch_s2v_bits_inst=instr; 
    top->io_dispatch_s2v_bits_vcsr_vstart=0; 
    top->io_dispatch_s2v_bits_vcsr_vl=vcsr.vl; 
    top->io_dispatch_s2v_bits_vcsr_vxrm=0; 
    top->io_dispatch_s2v_bits_vcsr_frm=0; 
    if(lmul==1){
        top->io_dispatch_s2v_bits_vcsr_vlmul=0b000; 
    }else if(lmul==2){
        top->io_dispatch_s2v_bits_vcsr_vlmul=0b001; 
    }
    if(sew==32){
        top->io_dispatch_s2v_bits_vcsr_vsew=0b010; 
    }else if(sew==16){
        top->io_dispatch_s2v_bits_vcsr_vsew=0b001; 
    }
    top->io_dispatch_s2v_bits_vcsr_vill=0; 
    top->io_dispatch_s2v_bits_vcsr_ma=vcsr.vma; 
    top->io_dispatch_s2v_bits_vcsr_ta=vcsr.vta; 
    top->io_dispatch_s2v_bits_rs1=rs1; 
    top->io_dispatch_s2v_bits_rs2=rs2; 
    single_cycle(top, contextp, wave);
    top->io_dispatch_s2v_valid=0;
    while(cycle_count!=30){
        single_cycle(top, contextp, wave);
        cycle_count++;
    }
}

void kill_sim(bool abort_flag){
    if(abort_flag==false){
        wave->close();
        delete top;
        delete contextp;
        assert(0);
    }
}
