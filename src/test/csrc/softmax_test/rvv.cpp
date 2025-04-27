
#include <stdio.h>
#include "reg.h"
#include <string.h>
#include <rvv.h>
#include <assert.h>
int isa_reg_index(const char *s);

const char *regs[] = {
    "$0", "ra", "sp", "gp", "tp", "t0", "t1", "t2",
    "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5",
    "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7",
    "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"
};

const char *fregs[] = {
    "ft0", "ft1", "ft2", "ft3",  "ft4",  "ft5", "ft6","ft7",
    "fs0", "fs1", "fa0", "fa1", "fa2", "fa3", "fa4", "fa5",
    "fa6", "fa7", "fs2",  "fs3", "fs4", "fs5", "fs6", "fs7", 
    "fs8", "fs9", "fs10", "fs11","ft8","ft9",  "ft10",  "ft11"
};

int isa_reg_index(const char *s) {
	int count=sizeof(regs)/sizeof(regs[0]);
	int i=0;
	for(i=0;i<count;i++){
		char name[3];
		strncpy(name,s,2);
		name[2]='\0';
		if(strcmp(name,regs[i])==0){
			break;
		}
	}
	if(i >= count) printf("Invalid register name\n");  
  return i;
}

int isa_freg_index(const char *s) {
	int count=sizeof(fregs)/sizeof(fregs[0]);
	int i=0;
    char name[5];
	for(i=0;i<count;i++){
		strcpy(name,s);
		if(strcmp(name,fregs[i])==0){
			break;
		}
	}
	if(i >= count) {
        printf("Invalid fregister name\n");
        printf("fregister name:s=%s, copy name:%s \n",s,name);
        assert(0); 
        
    } 
  return i;
}
void gpr_write(int idx, uint64_t val){
    cpu.gpr[idx]=val;
}

void vsetvlmax_e32(const char *s,int ta,int ma){
    vcsr.lmul=LMUL;
    vcsr.sew=32;
    vcsr.vta=ta;
    vcsr.vma=ma;
    vcsr.vstart=0;
    vcsr.vl=VLEN*vcsr.lmul/vcsr.sew;
    gpr_write(isa_reg_index(s),vcsr.vl);
}
void vfmv_v_f(int vreg_index, const char *s, int vl){
    for(int i=0;i<vl;i++){
        cpu.vreg[vreg_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f=cpu.fpr[isa_freg_index(s)].as_fp32;
    }
}

void vle32(int vreg_index, void *source_reg, int vl){
    void *ptr=source_reg;
    //4字节搬运
    for (int i = 0; i < vl; i++)
    {
        memcpy(&cpu.vreg[vreg_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].u, ptr, 4);
        ptr = (char *)ptr + 4;
    }
}
void vfsub_vf(int vreg_dst_index, float value, int vreg_src_index,int vl){
    for(int i=0;i<vl;i++){
        cpu.vreg[vreg_dst_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f=cpu.vreg[vreg_src_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f-value;
    }
}
void vfmul_vf(int vreg_dst_index, int vreg_src_index, float value, int vl){
    for(int i=0;i<vl;i++){
        cpu.vreg[vreg_dst_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f=cpu.vreg[vreg_src_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f*value;
    }
}
void vfcvt_x_f_v_i32m1(int vreg_dst_index, int vreg_src_index,int vl){
    for(int i=0;i<vl;i++){
        cpu.vreg[vreg_dst_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].i=(int32_t)round(cpu.vreg[vreg_src_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f);
    }
}
void vfcvt_f_x_v_f32m1(int vreg_dst_index, int vreg_src_index,int vl){
    for(int i=0;i<vl;i++){
        cpu.vreg[vreg_dst_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f=(float)cpu.vreg[vreg_src_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].i;
    }
}
void vfnmsac_vf(int vreg_dst_index,int vreg_fsrc1_index, int vreg_src2_index,int vl){
    for(int i=0;i<vl;i++){
        cpu.vreg[vreg_dst_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f=cpu.vreg[vreg_dst_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f \
        -cpu.fpr[vreg_fsrc1_index].as_fp32*cpu.vreg[vreg_src2_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f;
    }
}
void vfmadd_vv(int vreg_dst_index, int vreg_src1_index, int vreg_src2_index,int vl){
    for(int i=0;i<vl;i++){
        cpu.vreg[vreg_dst_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f=cpu.vreg[vreg_dst_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f*cpu.vreg[vreg_src1_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f+cpu.vreg[vreg_src2_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f;
    }
}
void vmv1r_v(int vreg_dst_index, int vreg_src_index,int vl){
    for(int i=0;i<vl;i++){
        cpu.vreg[vreg_dst_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f=cpu.vreg[vreg_src_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f;
    }
}

void vadd_vx(int vreg_dst_index, int vreg_src1_index, int value,int vl){
    for(int i=0;i<vl;i++){
        cpu.vreg[vreg_dst_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].i=cpu.vreg[vreg_src1_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].i+value;
    }
}
void vsll_vx(int vreg_dst_index, int vreg_src1_index, int value,int vl){
    for(int i=0;i<vl;i++){
        cpu.vreg[vreg_dst_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].u=(cpu.vreg[vreg_src1_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].u)<<value;
    }
}
void vfmul_vv(int vreg_dst_index, int vreg_src1_index, int vreg_src2_index,int vl){
    for(int i=0;i<vl;i++){
        cpu.vreg[vreg_dst_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f=cpu.vreg[vreg_src1_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f*cpu.vreg[vreg_src2_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f;
    }
}
void vfredusum_vs(int vreg_dst_index, int vreg_src2_index, int vreg_src1_index,int vl){
    float sum=cpu.vreg[vreg_src1_index][0].f;
    for(int i=0;i<vl;i++){
        sum=sum+cpu.vreg[vreg_src2_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f;
    }
    cpu.vreg[vreg_dst_index][0].f=sum;
}
void vfmv_v_i_f32m1(int vreg_dst_index, int value,int vl){
    for(int i=0;i<vl;i++){
        cpu.vreg[vreg_dst_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f=value;
    }
}
void vfmax_vv(int vreg_dst_index, int vreg_src1_index, int vreg_src2_index,int vl){
    for(int i=0;i<vl;i++){
        cpu.vreg[vreg_dst_index][i].f=cpu.vreg[vreg_src1_index][i].f>cpu.vreg[vreg_src2_index][i].f?cpu.vreg[vreg_src1_index][i].f:cpu.vreg[vreg_src2_index][i].f;
    }
}
// void vfredmax_vs(int vreg_dst_index, int vreg_src2_index, float value, int vl){
//     float max_x=value>cpu.vreg[vreg_src2_index][0].f?value:cpu.vreg[vreg_src2_index][0].f;
//     for(int i=0;i<vl;i++){
//         max_x=max_x>cpu.vreg[vreg_src2_index][i].f?max_x:cpu.vreg[vreg_src2_index][i].f;
//     }
//     cpu.vreg[vreg_dst_index][0].f=max_x;
//     if(vcsr.vta==TA){
//         //TODO: tail agnostic 
//     }
// }
void vfredmax_vs(int vreg_dst_index, int vreg_src2_index, int vreg_src1_index, int vl){
    float max_x=cpu.vreg[vreg_src1_index][0].f>cpu.vreg[vreg_src2_index][0].f?cpu.vreg[vreg_src1_index][0].f:cpu.vreg[vreg_src2_index][0].f;
    for(int i=0;i<vl;i++){
        max_x=max_x>cpu.vreg[vreg_src2_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f?max_x:cpu.vreg[vreg_src2_index+(i/(VLEN/vcsr.sew))][i%(VLEN/vcsr.sew)].f;
    }
    cpu.vreg[vreg_dst_index][0].f=max_x;
    if(vcsr.vta==TA){
        //TODO: tail agnostic 
    }
}