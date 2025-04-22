# vec_ops.s
.text
.global _start

_start:
    #vsetvli	a7,zero,e32,m1,ta,ma
    #vfmv.v.f	v1,fa5
    vsetvli	a5,zero,e32,m1,tu,ma
    vle32.v	v2,(a3) #src stored in v2 a3=0
    #vfmax.vv	v1,v2,v1#<-------
    vfmv.v.f	v1,fa5#<-------
    vfredmax.vs	v1,v2,v1  #-----max_x stored in v1#<-------
## START: quick_dirty_vector_expf
    vsetvli	s3,zero,e32,m1,ta,ma
    vfmv.v.f	v4,fa2
    vfmv.v.f	v9,fa3
    vfmv.v.f	v5,fa4
    vfmv.v.f	v7,fa5
    vfmv.v.f	v8,fa6
    vfmv.v.f	v10,fa7
    vfmv.v.f	v11,fa3
    vfmv.v.f	v6,fa4
    vsetvli	a5,zero,e32,m1,tu,ma
    #vle32.v	v2,(a3) #src stored in v2
    vfsub.vf	v3,v2,fs0 #<-------
    vfmul.vf	v1,v3,fa1 #<-------
    vfcvt.x.f.v	v12,v1 #<-------
    vfcvt.f.x.v	v14,v12 #<-------
    vfnmsac.vf	v3,fa5,v14 # r = x - mf * log(2) #<-------
    vmv1r.v	v1,v6
    vfmadd.vv	v1,v3,v11#<-------
    vfmadd.vv	v1,v3,v10#<-------
    vfmadd.vv	v1,v3,v8#<-------
    vfmadd.vv	v1,v3,v7#<-------
    vfmadd.vv	v1,v3,v5#<-------
    vfmadd.vv	v1,v3,v9#<-------
    #vle32.v	v2,(a3)
    vadd.vx	v4,v12,a3 #<-------
    vsll.vi	v3,v4,23
    vfmul.vv	v2,v3,v1
    vmv.v.i	v1,0
    vfredusum.vs	v4,v2,v1
 ## END: quick_dirty_vector_expf
    vsetvli	a5,zero,e32,m1,tu,ma
    vfmul.vf	v2,v2,fa0