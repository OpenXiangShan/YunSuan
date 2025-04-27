#ifndef __RVV_H__
#define __RVV_H__
#include <stdint.h>
#include <math.h>

void vsetvlmax_e32(const char *s,int ta,int ma);
void vfmv_v_f(int vreg_index, const char *s, int vl);
void vle32(int vreg_index, void *source_reg, int vl);
void gpr_write(int idx, uint64_t val);
int isa_reg_index(const char *s);
int isa_freg_index(const char *s);
void vfsub_vf(int vreg_dst_index, float value, int vreg_src_index,int vl);
void vfmul_vf(int vreg_dst_index, int vreg_src_index, float value, int vl);
void vfcvt_x_f_v_i32m1(int vreg_dst_index, int vreg_src_index,int vl);
void vfcvt_f_x_v_f32m1(int vreg_dst_index, int vreg_src_index,int vl);
void vfnmsac_vf(int vreg_dst_index,int vreg_fsrc1_index, int vreg_src2_index,int vl);
void vfmadd_vv(int vreg_dst_index, int vreg_src1_index, int vreg_src2_index,int vl);
void vmv1r_v(int vreg_dst_index, int vreg_src_index,int vl);
void vadd_vx(int vreg_dst_index, int vreg_src1_index, int value,int vl);
void vsll_vx(int vreg_dst_index, int vreg_src1_index, int value,int vl);
void vfmul_vv(int vreg_dst_index, int vreg_src1_index, int vreg_src2_index,int vl);
void vfredusum_vs(int vreg_dst_index, int vreg_src2_index, int vreg_src1_index,int vl);
void vfmv_v_i_f32m1(int vreg_dst_index, int value,int vl);
void vfmax_vv(int vreg_dst_index, int vreg_src1_index, int vreg_src2_index,int vl);
void vfredmax_vs(int vreg_dst_index, int vreg_src2_index, int vreg_src1_index, int vl);
#endif
