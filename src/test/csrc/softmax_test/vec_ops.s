# vec_ops.s
.text
.global _start

_start:
    #vsetvli	a7,zero,e32,m1,ta,ma
    #vfmv.v.f	v1,fa5
    vsetvli	a5,zero,e32,m1,tu,ma
    vle32.v	v4,(a3) #src stored in v4 a3=0
    #vfmax.vv	v1,v2,v1#<-------
    vfmv.v.f	v2,fa5#<-------
    vfredmax.vs	v2,v4,v2  #-----max_x stored in v1#<-------
## START: quick_dirty_vector_expf
    vsetvli	s3,zero,e32,m1,ta,ma
    vfmv.v.f	v8,fa2
    vfmv.v.f	v18,fa3
    vfmv.v.f	v10,fa4
    vfmv.v.f	v14,fa5
    vfmv.v.f	v16,fa6
    vfmv.v.f	v20,fa7
    vfmv.v.f	v22,fa3
    vfmv.v.f	v12,fa4
    vsetvli	a5,zero,e32,m1,tu,ma
    #vle32.v	v2,(a3) #src stored in v2
    vfsub.vf	v6,v4,fs0
    vfmul.vf	v2,v6,fa1
    vfcvt.x.f.v	v24,v2
    vfcvt.f.x.v	v28,v24
    vfnmsac.vf	v6,fa5,v28
    vmv1r.v	v2,v12
    vfmadd.vv	v2,v6,v22
    vfmadd.vv	v2,v6,v20
    vfmadd.vv	v2,v6,v16
    vfmadd.vv	v2,v6,v14
    vfmadd.vv	v2,v6,v10
    vfmadd.vv	v2,v6,v18
    vadd.vx	v8,v24,a3
    vsll.vi	v6,v8,23
    vfmul.vv	v4,v6,v2
    vmv.v.i	v2,0
    vfredusum.vs	v8,v4,v2
 ## END: quick_dirty_vector_expf
    vsetvli	a5,zero,e32,m1,tu,ma
    #division
    vrgather.vi v10, v8,0 #v10=v8[0]
    vfrec7.v v12, v10 #v12=1/v10
    vmv.v.x v14,t0 #v14=0x40000000
    vfnmsac.vv v14, v10, v12 #v14=2-v10/est(v10)
    vfmul.vv v12, v12, v14
    vmv.v.x v14,t0 #v14=0x40000000
    vfnmsac.vv v14, v10, v12 #v14=2-v10/est(v10)
    vfmul.vv v12, v12, v14
    vfmul.vv	v4,v4,v12