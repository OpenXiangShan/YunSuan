
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
extern bool commit_global;
#define CLK_PERIOD 10    // 时钟周期（单位：ns）
vluint64_t main_time = 0;
static uint8_t robIdx=0;

bool check_vreg(uint8_t rf_addr);
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
    vsetvlmax_e32m1("s3",TA,MA);//0d0079d7          	vsetvli	s3,zero,e32,m1,ta,ma

    //Instr 6:vfmv.v.f	v4,fa2
    cpu.fpr[isa_freg_index("fa2")].as_uint64=0;
    //model execution
    vfmv_v_f(4,"fa2", vcsr.vl);//vfmv.v.f	v4,fa2
    //dut execution
    dut_input_execute(0x5e065257 , vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa2")].as_uint64,   0, false, robIdx++);
    check=check_vreg(4);
    printf("The result of \"Instr 6:vfmv.v.f	v4,fa2\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);


    // element-wise reduction accumulator
    //Instr 7:vfmv.v.f	v9,fa3
    cpu.fpr[isa_freg_index("fa3")].as_fp32=1.0f;
    cpu.fpr[isa_freg_index("fa3")].as_uint32x2.high=0xffffffff;
    //model execution
    vfmv_v_f(9,"fa3", vcsr.vl);//vfmv.v.f	v9,fa3
    //dut execution
    dut_input_execute(0x5e06d4d7 , vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa3")].as_uint64,   0, false, robIdx++);
    check=check_vreg(9);
    printf("The result of \"Instr 7:vfmv.v.f	v9,fa3\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 8: vfmv.v.f	v5,fa4
    cpu.fpr[isa_freg_index("fa4")].as_fp32=1.0f;
    cpu.fpr[isa_freg_index("fa4")].as_uint32x2.high=0xffffffff;
    //model execution
    vfmv_v_f(5, "fa4", vcsr.vl);//vfmv.v.f	v5,fa4
    //dut execution
    dut_input_execute(0x5e0752d7 , vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa4")].as_uint64,   0, false, robIdx++);
    check=check_vreg(5);
    printf("The result of \"Instr 8: vfmv.v.f	v5,fa4\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 9: vfmv.v.f	v7,fa5
    cpu.fpr[isa_freg_index("fa5")].as_fp32=0.49999997019767761f;
    cpu.fpr[isa_freg_index("fa5")].as_uint32x2.high=0xffffffff;
    //model execution
    vfmv_v_f(7, "fa5", vcsr.vl);//vfmv.v.f	v7,fa5
    //dut execution
    dut_input_execute(0x5e07d3d7 , vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa5")].as_uint64,   0, false, robIdx++);
    check=check_vreg(7);
    printf("The result of \"Instr 9: vfmv.v.f	v7,fa5\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 10:vfmv.v.f	v8,fa6
    cpu.fpr[isa_freg_index("fa6")].as_fp32=0.1666666716337204f;
    cpu.fpr[isa_freg_index("fa6")].as_uint32x2.high=0xffffffff;
    //model execution
    vfmv_v_f(8, "fa6", vcsr.vl);//vfmv.v.f	v8,fa6
    //dut execution
    dut_input_execute(0x5e085457 , vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa6")].as_uint64,   0, false, robIdx++);
    check=check_vreg(8);
    printf("The result of \"Instr 10:vfmv.v.f	v8,fa6\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 11: vfmv.v.f	v10,fa7
    cpu.fpr[isa_freg_index("fa7")].as_fp32=0.041666667908430099f;
    cpu.fpr[isa_freg_index("fa7")].as_uint32x2.high=0xffffffff;
    //model execution
    vfmv_v_f(10, "fa7", vcsr.vl);//vfmv.v.f	v10,fa7
    //dut excecution
    dut_input_execute(0x5e08d557 , vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa7")].as_uint64,   0, false, robIdx++);
    check=check_vreg(10);
    printf("The result of \"Instr 11: vfmv.v.f	v10,fa7\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 12:vfmv.v.f	v11,fa3
    cpu.fpr[isa_freg_index("fa3")].as_fp32=0.0083333337697553482f;
    cpu.fpr[isa_freg_index("fa3")].as_uint32x2.high=0xffffffff;
    //model execution
    vfmv_v_f(11, "fa3", vcsr.vl);//vfmv.v.f	v11,fa3
    //dut excecution
    dut_input_execute(0x5e06d5d7, vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa3")].as_uint64,   0, false, robIdx++);
    check=check_vreg(11);
    printf("The result of \"Instr 12:vfmv.v.f	v11,fa3\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 13: vfmv.v.f	v6,fa4
    cpu.fpr[isa_freg_index("fa4")].as_fp32=0.0013888889251995273f;
    cpu.fpr[isa_freg_index("fa4")].as_uint32x2.high=0xffffffff;
    //model execution
    vfmv_v_f(6, "fa4", vcsr.vl);//vfmv.v.f	v6,fa4////9
    // dut excecution
    dut_input_execute(0x5e075357, vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa4")].as_uint64, 0, false, robIdx++);
    check = check_vreg(6);
    printf("The result of \"Instr 13: vfmv.v.f	v6,fa4\" difftest is :%s\n", check ? "True" : "False");
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
    vsetvlmax_e32m1("a5",TU,MA);//vsetvli	a5,zero,e32,m1,tu,ma
    cpu.gpr[isa_reg_index("a3")]=0;
    void *ptr=pmem+cpu.gpr[isa_reg_index("a3")];
    
    // //Instr 14: vle32.v	v2,(a3)
    // vle32(2, ptr, vcsr.vl);//0206e107          	vle32.v	v2,(a3)  ---src stored in v2
    // //dut execution
    // dut_input_execute(0x0206e107, vcsr.sew, vcsr.lmul, cpu.gpr[isa_reg_index("a3")], 0, false, robIdx++);
    // check=check_vreg(2);
    // printf("The result of \"Instr 14: vle32.v	v2,(a3)\" difftest is :%s\n",check?"True":"False");
    // kill_sim(check);



    //Instr 15: vfsub.vf	v3,v2,fs0
    //model execution
    vfsub_vf(3,cpu.fpr[isa_freg_index("fs0")].as_fp32,2,vcsr.vl);//x-max_x          	vfsub.vf	v3,v2,fs0
    //dut execution
    dut_input_execute(0x0a2451d7, vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fs0")].as_uint64, 0, false, robIdx++);
    check=check_vreg(3);
    printf("The result of \"Instr 15: vfsub.vf	v3,v2,fs0\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 16: vfmul.vf	v1,v3,fa1 
    //model execution
    vfmul_vf(1,3,cpu.fpr[isa_freg_index("fa1")].as_fp32,vcsr.vl);//vxiln2=(x-max_x )iln2  vfmul.vf	v1,v3,fa1 
    // dut execution
    dut_input_execute(0x9235d0d7, vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa1")].as_uint64, 0, false, robIdx++);
    check = check_vreg(1);
    printf("The result of \"Instr 16: vfmul.vf	v1,v3,fa1 \" difftest is :%s\n", check ? "True" : "False");
    kill_sim(check);

    //Instr 17: vfcvt.x.f.v	v12,v1
    //model execution
    vfcvt_x_f_v_i32m1(12,1,vcsr.vl);//vfcvt.x.f.v	v12,v1
    //dut execution
    dut_input_execute(0x4a109657, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg_i(12);
    printf("The result of \"Instr 17: vfcvt.x.f.v	v12,v1\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 18: vfcvt.f.x.v	v14,v12
    //model execution
    vfcvt_f_x_v_f32m1(14,12,vcsr.vl);//vfcvt.f.x.v	v14,v12
    //dut execution
    dut_input_execute(0x4ac19757, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(14);
    printf("The result of \"Instr 18: vfcvt.f.x.v	v14,v12\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);



    uint64_t addr=sizeof(float)*VLEN/32;
    for(int i=0;i<VLEN/32;i++){
        std::memcpy(&pmem[addr+i].as_float,&cpu.vreg[1][i].i,sizeof(float));
    }

    //Instr 19: vfnmsac.vf	v3,fa5,v14
    cpu.fpr[isa_freg_index("fa5")].as_fp32=ln2;
    cpu.fpr[isa_freg_index("fa5")].as_uint32x2.high=0xffffffff;
    //model execution
    vfnmsac_vf(3,isa_freg_index("fa5"),14,vcsr.vl);//bee7d1d7          	vfnmsac.vf	v3,fa5,v14 r = x - k * log(2)
    //dut execution
    dut_input_execute(0xbee7d1d7, vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa5")].as_uint64, 0, false, robIdx++);
    check=check_vreg(3);
    printf("The result of \"Instr 19: vfnmsac.vf	v3,fa5,v14\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);
    
    //polynomial approximation exp(r)

    //Instr 20: vmv1r.v	v1,v6
    //model execution
    vmv1r_v(1,6,vcsr.vl);//vmv1r.v	v1,v6
    //dut execution
    dut_input_execute(0x9e6030d7, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(1);
    printf("The result of \"Instr 20: vmv1r.v	v1,v6\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 21: vfmadd.vv	v1,v3,v11
    //model execution
    vfmadd_vv(1,3,11,vcsr.vl);//vfmadd.vv	v1,v3,v11
    //dut execution
    dut_input_execute(0xa2b190d7, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(1);
    printf("The result of \"Instr 21: vfmadd.vv	v1,v3,v11\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 22: vfmadd.vv	v1,v3,v10
    //model execution
    vfmadd_vv(1,3,10,vcsr.vl);//vfmadd.vv	v1,v3,v10
    //dut execution
    dut_input_execute(0xa2a190d7, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(1);
    printf("The result of \"Instr 22: vfmadd.vv	v1,v3,v10\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 23: vfmadd.vv	v1,v3,v8
    //model execution
    vfmadd_vv(1,3,8,vcsr.vl);//vfmadd.vv	v1,v3,v8
    // dut execution
    dut_input_execute(0xa28190d7, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check = check_vreg(1);
    printf("The result of \"Instr 23: vfmadd.vv	v1,v3,v8\" difftest is :%s\n", check ? "True" : "False");
    kill_sim(check);

    //Instr 24: vfmadd.vv	v1,v3,v7
    //model execution
    vfmadd_vv(1,3,7,vcsr.vl);//vfmadd.vv	v1,v3,v7
    // dut execution
    dut_input_execute(0xa27190d7, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check = check_vreg(1);
    printf("The result of \"Instr 24: vfmadd.vv	v1,v3,v7\" difftest is :%s\n", check ? "True" : "False");
    kill_sim(check);

    //Instr 25: vfmadd.vv	v1,v3,v5
    //model execution
    vfmadd_vv(1,3,5,vcsr.vl);//vfmadd.vv	v1,v3,v5
    // dut execution
    dut_input_execute(0xa25190d7, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check = check_vreg(1);
    printf("The result of \"Instr 25: vfmadd.vv	v1,v3,v5\" difftest is :%s\n", check ? "True" : "False");
    kill_sim(check);

    //Instr 26: vfmadd.vv	v1,v3,v9
    vfmadd_vv(1,3,9,vcsr.vl);//vfmadd.vv	v1,v3,v9
    // dut execution
    dut_input_execute(0xa29190d7, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check = check_vreg(1);
    printf("The result of \"Instr 26: vfmadd.vv	v1,v3,v9\" difftest is :%s\n", check ? "True" : "False");
    kill_sim(check);

    // //Instr 27: vle32.v	v2,(a3)
    // ptr=pmem+addr;
    // cpu.gpr[isa_reg_index("a3")]=addr;
    // //model execution
    // vle32(2, ptr, vcsr.vl);//0206e107          	vle32.v	v2,(a3)
    // //dut execution
    // dut_input_execute(0x0206e107, vcsr.sew, vcsr.lmul, cpu.gpr[isa_reg_index("a3")], 0, false, robIdx++);
    // check=check_vreg(2);
    // printf("The result of \"Instr 27: vle32.v	v2,(a3)\" difftest is :%s\n",check?"True":"False");
    // kill_sim(check);
    
    //Instr 28: vadd.vx	v4,v12,a3
    cpu.gpr[isa_reg_index("a3")]=127;
    //model execution
    vadd_vx(4,12,cpu.gpr[isa_reg_index("a3")],vcsr.vl);//vadd.vx	v4,v12,a3
    //dut execution
    dut_input_execute(0x02c6c257, vcsr.sew, vcsr.lmul, cpu.gpr[isa_reg_index("a3")], 0, false, robIdx++);
    check=check_vreg(4);
    printf("The result of \"Instr 28: vadd.vx	v4,v12,a3\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 29: vsll.vi	v3,v4,23
    //model execution
    vsll_vx(3,4,23,vcsr.vl);//vsll.vi	v3,v4,23
    //dut execution
    dut_input_execute(0x964bb1d7, vcsr.sew, vcsr.lmul, 23, 0, false, robIdx++);
    check=check_vreg(3);
    printf("The result of \"Instr 29: vsll.vi	v3,v4,23\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);
    
    //Instr 30: vfmul.vv	v2,v3,v1
    //model execution
    vfmul_vv(2,3,1,vcsr.vl);//vfmul.vv	v2,v3,v1
    //dut execution
    dut_input_execute(0x92309157, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(2);
    printf("The result of \"Instr 30: vfmul.vv	v2,v3,v1\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);
    
    //Instr 31: vmv.v.i	v1,0
    vfmv_v_i_f32m1(1,0,vcsr.vl);//vmv.v.i	v1,0
    //dut execution
    dut_input_execute(0x5e0030d7, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(1);
    printf("The result of \"Instr 31: vmv.v.i	v1,0\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr 32: vfredusum.vs	v4,v2,v1
    vfredusum_vs(4,2,1,vcsr.vl);//vfredusum.vs	v4,v2,v1
    //dut execution
    dut_input_execute(0x06209257, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(4);
    printf("The result of \"Instr 32: vfredusum.vs	v4,v2,v1\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);
    
    //v2 stores the result of expf
    //v4 stores the sum of expf
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

    vsetvlmax_e32m1("a5",TU,MA);//vsetvli	a5,ZERO,e32,m1,tu,m

    //Instr2:vle32.v	v2,(a3)
    gpr_write(isa_reg_index("a3"),0);
    // top->instr=0x0206e107;
    for(int i=0;i<VLEN/32;i++){
        std::memcpy(&pmem[0+i].as_float,&src[i],sizeof(float));
    }
    //model execution
    vle32(2, &pmem->as_float, vcsr.vl);//vle32.v	v2,(a3)  ---src stored in v2
    //dut execution
    dut_input_execute(0x0206e107, vcsr.sew, vcsr.lmul, cpu.gpr[isa_reg_index("a3")], 0, false, robIdx++);
    check=check_vreg(2);
    printf("The result of \"Instr 2: vle32.v	v2,(a3)\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    // //Instr3:vfmax.vv	v1,v2,v1
    // //model execution
    // vfmax_vv(1,2,1,vcsr.vl);//vfmax.vv	v1,v2,v1
    // dut_input_execute(0x1a2090d7, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    // check=check_vreg(1);
    // printf("The result of \"Instr 3: vfmax.vv	v1,v2,v1\" difftest is :%s\n",check?"True":"False");
    // kill_sim(check);

    //Instr4:vfmv.v.f	v1,fa5
    cpu.fpr[isa_freg_index("fa5")].as_fp32=-INFINITY;//
    cpu.fpr[isa_freg_index("fa5")].as_uint32x2.high=0xFFFFFFFF;
    //model execution
    vfmv_v_f(1, "fa5", vcsr.vl);//vfmv.v.f	v1,fa5
    //dut execution
    dut_input_execute(0x5e07d0d7 , vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa5")].as_uint64, 0, false, robIdx++);
    check=check_vreg(1);
    printf("The result of \"Instr 4: vfmv.v.f	v1,fa5\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    //Instr5:vfredmax.vs	v1,v2,v1
    //model execution
    vfredmax_vs(1,2,1,vcsr.vl);//vfredmax.vs	v1,v2,v1  -----max_x stored in v1
    //dut execution
    dut_input_execute(0x1e2090d7, vcsr.sew, vcsr.lmul, 0, 0, false, robIdx++);
    check=check_vreg(1);
    printf("The result of \"Instr 5: vfredmax.vs	v1,v2,v1\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);

    cpu.fpr[isa_freg_index("fs0")].as_fp32=cpu.vreg[1][0].f;//v[1][0]
    double max_x = src[0];
    for (int i = 1; i < n; ++i) {
        if (src[i] > max_x) max_x = src[i]; 
    }
    
    //TODO: wait result; compare dut and golden model: v1  and max_x
    float sum = 0.;
    sum =quick_dirty_vector_expf(dst, src, max_x, n);

    // computing the reciprocal of the sum of exponentials, once and for all
    float inv_sum = 1.f / sum;
    cpu.fpr[10].as_fp32=inv_sum;

    //PART III: normalizing each element
    vsetvlmax_e32m1("a5",TU,MA);//vsetvli	a5,zero,e32,m1,tu,ma

    //Instr 33: vfmul.vf	v2,v2,fa0
    cpu.fpr[isa_freg_index("fa0")].as_fp32=1/cpu.vreg[4][0].f;
    cpu.fpr[isa_freg_index("fa0")].as_uint32x2.high=0xffffffff;
    vfmul_vf(2,2,cpu.fpr[isa_freg_index("fa0")].as_fp32,vcsr.vl);//vfmul.vf	v2,v2,fa0
    //dut execution
    dut_input_execute(0x92255157, vcsr.sew, vcsr.lmul, cpu.fpr[isa_freg_index("fa0")].as_uint64, 0, false, robIdx++);
    check=check_vreg(2);
    printf("The result of \"Instr 33: vfmul.vf	v2,v2,fa0\" difftest is :%s\n",check?"True":"False");
    kill_sim(check);
    
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


bool check_vreg(uint8_t rf_addr){
    for(int i=rf_addr; i<rf_addr+1;i++){
        for(int element=0;element<VLEN/32;element++){
            if(cpu.vreg[i][element].u!=diff_vreg[i][element].as_uint32) {
                printf("The different register id is %d\n",i);
                printf("The element index is : %d\n",element);
                for(int i=0;i<VLEN/32;i++){
                    printf("cpu.vreg[%d][%d]=%f\t  diff.vreg[%d][%d]=%f\n",rf_addr, i, cpu.vreg[rf_addr][i].f, rf_addr, i,diff_vreg[rf_addr][i].as_float);
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
    //     }
    // }
    commit_global=false;
    return true;
}

bool check_vreg_i(uint8_t rf_addr){
    for(int i=rf_addr; i<rf_addr+1;i++){
        for(int element=0;element<VLEN/32;element++){
            if(cpu.vreg[i][element].u!=diff_vreg[i][element].as_uint32) {
                printf("The different register id is %d\n",i);
                printf("The element index is : %d\n",element);
                for(int i=0;i<VLEN/32;i++){
                    printf("cpu.vreg[%d][%d]=%d\t  diff.vreg[%d][%d]=%d\n",rf_addr, i, cpu.vreg[rf_addr][i].f, rf_addr, i,diff_vreg[rf_addr][i].as_int32);
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
    //     }
    // }
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
    }
    if(sew==32){
        top->io_dispatch_s2v_bits_vcsr_vsew=0b010; 
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
