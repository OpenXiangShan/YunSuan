package yunsuan.vector

import chisel3._
import chisel3.util._
import yunsuan.util._
import yunsuan.VectorElementFormat
import yunsuan.vector.vpermutil._


// vslideup.vx/vi
class SlideUpLookup(n: Int) extends VPermModule {
    val io = IO(new VPermBundle() {
        val slide_base     = Input(UInt(7.W))
        val mask_start_idx = Input(UInt(7.W))

        val slide    = Input(UInt(XLEN.W))
        val elem_vld = Input(UInt(5.W))
        val vstart   = Input(UInt(7.W))
        val vl       = Input(UInt(8.W))
        val vm       = Input(Bool())
        val ta       = Input(Bool())
        val ma       = Input(Bool())

        val mask      = Input(UInt((VLEN/8).W))
        val src_data  = Input(UInt(VLEN.W))
        val prev_data = Input(UInt(VLEN.W))

        val res_data  = Output(UInt(VLEN.W))
    })

    // stage-0
    val index = Wire(Vec(n, UInt(log2Up(n).W)))
    val res_keep_old_vd = Wire(Vec(n, Bool()))
    val res_agnostic = Wire(Vec(n, Bool()))
    val res_update = Wire(Vec(n, Bool()))
    val src_data_vec  = Wire(Vec(n, UInt((VLEN/n).W)))
    val prev_data_vec = Wire(Vec(n, UInt((VLEN/n).W)))

    for(i <- 0 until n) {
        val elements_idx = io.mask_start_idx + i.U
        index(i) := RegNext(io.slide_base +& i.U + ~io.slide + 1.U)
        res_keep_old_vd(i) := RegNext((!io.vm && !io.mask(i).asBool && !io.ma) || (elements_idx < io.vstart) || ((elements_idx >= io.vl) && !io.ta))
        res_agnostic(i) := RegNext(((elements_idx >= io.vl) && io.ta) || (!io.vm && !io.mask(i).asBool && io.ma))
        res_update(i) := RegNext((io.slide <= (io.slide_base +& i.U)) && ((io.slide_base +& i.U) < (n.U + io.slide)))

        src_data_vec(i)  := RegNext(io.src_data((VLEN/n)*(i+1)-1, (VLEN/n)*i))
        prev_data_vec(i) := RegNext(io.prev_data((VLEN/n)*(i+1)-1, (VLEN/n)*i))
    }

    val ta_reg = RegNext(io.ta)
    val elem_vld_reg = RegNext(io.elem_vld)

    // stage-1
    val res_data_vec  = Wire(Vec(n, UInt((VLEN/n).W)))

    for(i <- 0 until n) {
        when (i.U < elem_vld_reg) {
            when (res_keep_old_vd(i)) {
                res_data_vec(i) := prev_data_vec(i)
            }.elsewhen (res_agnostic(i)) {
                res_data_vec(i) := Fill(VLEN/n, 1.U(1.W))
            }.elsewhen (res_update(i)) {
                res_data_vec(i) := src_data_vec(index(i))
            }.otherwise {
                res_data_vec(i) := prev_data_vec(i)
            }
        }.otherwise {
            res_data_vec(i) := Mux(ta_reg, Fill(VLEN/n, 1.U(1.W)), prev_data_vec(i))
        }
    }

    io.res_data := res_data_vec.reduce{ (a, b) => Cat(b, a) }
}

class SlideUpLookupModule extends VPermModule {
    val io = IO(new VPermIO)

    val vformat = io.vd_type(1,0)
    //val sew          = LookupTree(vformat, VFormat.VFormatTable.map(p => (p._1, p._2._1)))
    val elem_num     = LookupTree(vformat, VFormat.VFormatTable.map(p => (p._1, p._2._2)))
    val elem_num_pow = LookupTree(vformat, VFormat.VFormatTable.map(p => (p._1, p._2._3)))

    val vd_idx = Wire(UInt(3.W))
    when ( io.uop_idx === 0.U ) {
        vd_idx := 0.U
    }.elsewhen ( (1.U <= io.uop_idx) && (io.uop_idx <= 2.U) ) {
        vd_idx := 1.U
    }.elsewhen ( (3.U <= io.uop_idx) && (io.uop_idx <= 5.U) ) {
        vd_idx := 2.U
    }.elsewhen ( (6.U <= io.uop_idx) && (io.uop_idx <= 9.U) ) {
        vd_idx := 3.U
    }.elsewhen ( (10.U <= io.uop_idx) && (io.uop_idx <= 14.U) ) {
        vd_idx := 4.U
    }.elsewhen ( (15.U <= io.uop_idx) && (io.uop_idx <= 20.U) ) {
        vd_idx := 5.U
    }.elsewhen ( (21.U <= io.uop_idx) && (io.uop_idx <= 27.U) ) {
        vd_idx := 6.U
    }.otherwise {
        vd_idx := 7.U
    }
    val mask_start_idx = vd_idx << elem_num_pow

    val slide_base = Wire(UInt(7.W))
    when ( io.uop_idx === 28.U ) {
        slide_base := 7.U << elem_num_pow
    }.elsewhen ( io.uop_idx === 21.U || io.uop_idx === 29.U ) {
        slide_base := 6.U << elem_num_pow
    }.elsewhen ( io.uop_idx === 15.U || io.uop_idx === 22.U || io.uop_idx === 30.U ) {
        slide_base := 5.U << elem_num_pow
    }.elsewhen ( io.uop_idx === 10.U || io.uop_idx === 16.U || io.uop_idx === 23.U || io.uop_idx === 31.U ) {
        slide_base := 4.U << elem_num_pow
    }.elsewhen ( io.uop_idx === 6.U || io.uop_idx === 11.U || io.uop_idx === 17.U || io.uop_idx === 24.U || io.uop_idx === 32.U ) {
        slide_base := 3.U << elem_num_pow
    }.elsewhen ( io.uop_idx === 3.U || io.uop_idx === 7.U || io.uop_idx === 12.U || io.uop_idx === 18.U || io.uop_idx === 25.U || io.uop_idx === 33.U ) {
        slide_base := 2.U << elem_num_pow
    }.elsewhen ( io.uop_idx === 1.U || io.uop_idx === 4.U || io.uop_idx === 8.U || io.uop_idx === 13.U || io.uop_idx === 19.U || io.uop_idx === 26.U || io.uop_idx === 34.U ) {
        slide_base := 1.U << elem_num_pow
    }.otherwise {
        slide_base := 0.U << elem_num_pow
    }

    val elem_vld = LookupTreeDefault(io.vlmul, elem_num, List(
        "b101".U -> (elem_num >> 3),   //lmul=1/8
        "b110".U -> (elem_num >> 2),   //lmul=1/4
        "b111".U -> (elem_num >> 1)    //lmul=1/2
    ))

    val mask_selected = SelectMaskN(io.mask, 16, mask_start_idx)

    val slide_up_module_0 = Module(new SlideUpLookup(16)) //sew=8
    val slide_up_module_1 = Module(new SlideUpLookup(8))  //sew=16
    val slide_up_module_2 = Module(new SlideUpLookup(4))  //sew=32
    val slide_up_module_3 = Module(new SlideUpLookup(2))  //sew=64

    val slide_up_module = VecInit(Seq(slide_up_module_0.io, slide_up_module_1.io, slide_up_module_2.io, slide_up_module_3.io))
    for(i <- 0 until 4) {
        slide_up_module(i).slide_base     := slide_base
        slide_up_module(i).mask_start_idx := mask_start_idx
        slide_up_module(i).slide          := io.vs1(XLEN-1, 0)
        slide_up_module(i).elem_vld       := elem_vld
        slide_up_module(i).vstart         := io.vstart
        slide_up_module(i).vl             := io.vl
        slide_up_module(i).vm             := io.vm
        slide_up_module(i).ta             := io.ta
        slide_up_module(i).ma             := io.ma
        slide_up_module(i).mask           := mask_selected
        slide_up_module(i).src_data       := io.vs2
        slide_up_module(i).prev_data      := io.old_vd
    }

    io.res_vd := Mux(io.vstart >= io.vl, io.old_vd, LookupTree(vformat, List(
        VectorElementFormat.b -> slide_up_module_0.io.res_data,
        VectorElementFormat.h -> slide_up_module_1.io.res_data,
        VectorElementFormat.w -> slide_up_module_2.io.res_data,
        VectorElementFormat.d -> slide_up_module_3.io.res_data
    )))
}

// vslide1up.vx
class Slide1Up(n: Int) extends VPermModule {
    val io = IO(new VPermBundle() {
        val mask_start_idx = Input(UInt(7.W))

        val slide    = Input(UInt(5.W))
        val elem_vld = Input(UInt(5.W))
        val vstart   = Input(UInt(7.W))
        val vl       = Input(UInt(8.W))
        val vm       = Input(Bool())
        val ta       = Input(Bool())
        val ma       = Input(Bool())

        val mask        = Input(UInt((VLEN/8).W))
        val src_data_lo = Input(UInt(VLEN.W))     //vs2[i-1] / rs1
        val src_data_hi = Input(UInt(VLEN.W))     //vs2[i]
        val prev_data   = Input(UInt(VLEN.W))

        val res_data    = Output(UInt(VLEN.W))
    })

    // stage-0
    val index = Wire(Vec(n, UInt(log2Up(n).W)))
    val res_keep_old_vd = Wire(Vec(n, Bool()))
    val res_agnostic = Wire(Vec(n, Bool()))
    val res_update_hi = Wire(Vec(n, Bool()))
    val src_data_hi_vec  = Wire(Vec(n, UInt((VLEN/n).W)))
    val src_data_lo_vec  = Wire(Vec(n, UInt((VLEN/n).W)))
    val prev_data_vec = Wire(Vec(n, UInt((VLEN/n).W)))

    for(i <- 0 until n) {
        val elements_idx = io.mask_start_idx + i.U
        index(i) := RegNext(i.U + ~io.slide + 1.U)
        res_keep_old_vd(i) := RegNext((!io.vm && !io.mask(i).asBool && !io.ma) || (elements_idx < io.vstart) || ((elements_idx >= io.vl) && !io.ta))
        res_agnostic(i) := RegNext(((elements_idx >= io.vl) && io.ta) || (!io.vm && !io.mask(i).asBool && io.ma))
        res_update_hi(i) := RegNext(io.slide <= i.U)

        src_data_hi_vec(i) := RegNext(io.src_data_hi((VLEN/n)*(i+1)-1, (VLEN/n)*i))
        src_data_lo_vec(i) := RegNext(io.src_data_lo((VLEN/n)*(i+1)-1, (VLEN/n)*i))
        prev_data_vec(i)   := RegNext(io.prev_data((VLEN/n)*(i+1)-1, (VLEN/n)*i))
    }

    val ta_reg = RegNext(io.ta)
    val elem_vld_reg = RegNext(io.elem_vld)

    // stage-1
    val res_data_vec  = Wire(Vec(n, UInt((VLEN/n).W)))

    for(i <- 0 until n) {
        when (i.U < elem_vld_reg) {
            when (res_keep_old_vd(i)) {
                res_data_vec(i) := prev_data_vec(i)
            }.elsewhen (res_agnostic(i)) {
                res_data_vec(i) := Fill(VLEN/n, 1.U(1.W))
            }.elsewhen (res_update_hi(i)) {
                res_data_vec(i) := src_data_hi_vec(index(i))
            }.otherwise {
                res_data_vec(i) := src_data_lo_vec(index(i))
            }
        }.otherwise {
            res_data_vec(i) := Mux(ta_reg, Fill(VLEN/n, 1.U(1.W)), prev_data_vec(i))
        }
    }

    io.res_data := res_data_vec.reduce{ (a, b) => Cat(b, a) }
}

class Slide1UpModule extends VPermModule {
    val io = IO(new VPermIO)

    val vformat = io.vd_type(1,0)
    //val sew          = LookupTree(vformat, VFormat.VFormatTable.map(p => (p._1, p._2._1)))
    val elem_num     = LookupTree(vformat, VFormat.VFormatTable.map(p => (p._1, p._2._2)))
    val elem_num_pow = LookupTree(vformat, VFormat.VFormatTable.map(p => (p._1, p._2._3)))

    val mask_start_idx = io.uop_idx << elem_num_pow

    val elem_vld = LookupTreeDefault(io.vlmul, elem_num, List(
        "b101".U -> (elem_num >> 3),   //lmul=1/8
        "b110".U -> (elem_num >> 2),   //lmul=1/4
        "b111".U -> (elem_num >> 1)    //lmul=1/2
    ))

    val mask_selected = SelectMaskN(io.mask, 16, mask_start_idx)

    val slide_1_up_module_0 = Module(new Slide1Up(16)) //sew=8
    val slide_1_up_module_1 = Module(new Slide1Up(8))  //sew=16
    val slide_1_up_module_2 = Module(new Slide1Up(4))  //sew=32
    val slide_1_up_module_3 = Module(new Slide1Up(2))  //sew=64

    val slide_1_up_module = VecInit(Seq(slide_1_up_module_0.io, slide_1_up_module_1.io, slide_1_up_module_2.io, slide_1_up_module_3.io))
    for(i <- 0 until 4) {
        slide_1_up_module(i).mask_start_idx := mask_start_idx
        slide_1_up_module(i).slide          := 1.U
        slide_1_up_module(i).elem_vld       := elem_vld
        slide_1_up_module(i).vstart         := io.vstart
        slide_1_up_module(i).vl             := io.vl
        slide_1_up_module(i).vm             := io.vm
        slide_1_up_module(i).ta             := io.ta
        slide_1_up_module(i).ma             := io.ma
        slide_1_up_module(i).mask           := mask_selected
        slide_1_up_module(i).src_data_lo    := io.vs1
        slide_1_up_module(i).src_data_hi    := io.vs2
        slide_1_up_module(i).prev_data      := io.old_vd
    }

    io.res_vd := Mux(io.vstart >= io.vl, io.old_vd, LookupTree(vformat, List(
        VectorElementFormat.b -> slide_1_up_module_0.io.res_data,
        VectorElementFormat.h -> slide_1_up_module_1.io.res_data,
        VectorElementFormat.w -> slide_1_up_module_2.io.res_data,
        VectorElementFormat.d -> slide_1_up_module_3.io.res_data
    )))
}

// vslidedown.vx/vi
class SlideDownLookup(n: Int) extends VPermModule {
    val io = IO(new VPermBundle() {
        val slide_base      = Input(UInt(7.W))
        val mask_start_idx  = Input(UInt(7.W))
        val first_slidedown = Input(Bool())

        val slide    = Input(UInt(XLEN.W))
        val elem_vld = Input(UInt(5.W))
        val vstart   = Input(UInt(7.W))
        val vl       = Input(UInt(8.W))
        val vm       = Input(Bool())
        val ta       = Input(Bool())
        val ma       = Input(Bool())

        val mask      = Input(UInt((VLEN/8).W))
        val src_data  = Input(UInt(VLEN.W))
        val prev_data = Input(UInt(VLEN.W))

        val res_data  = Output(UInt(VLEN.W))
    })

    // stage-0
    val index = Wire(Vec(n, UInt(log2Up(n).W)))
    val res_keep_old_vd = Wire(Vec(n, Bool()))
    val res_agnostic = Wire(Vec(n, Bool()))
    val res_update = Wire(Vec(n, Bool()))
    val src_data_vec  = Wire(Vec(n, UInt((VLEN/n).W)))
    val prev_data_vec = Wire(Vec(n, UInt((VLEN/n).W)))

    for(i <- 0 until n) {
        val elements_idx = io.mask_start_idx + i.U
        index(i) := RegNext(io.slide +& i.U + ~io.slide_base + 1.U)
        res_keep_old_vd(i) := RegNext((!io.vm && !io.mask(i).asBool && !io.ma) || (elements_idx < io.vstart) || ((elements_idx >= io.vl) && !io.ta))
        res_agnostic(i) := RegNext(((elements_idx >= io.vl) && io.ta) || (!io.vm && !io.mask(i).asBool && io.ma))
        res_update(i) := RegNext((io.slide_base <= (io.slide +& i.U)) && ((io.slide +& i.U) < (io.elem_vld +& io.slide_base)))

        src_data_vec(i)  := RegNext(io.src_data((VLEN/n)*(i+1)-1, (VLEN/n)*i))
        prev_data_vec(i) := RegNext(io.prev_data((VLEN/n)*(i+1)-1, (VLEN/n)*i))
    }

    val ta_reg = RegNext(io.ta)
    val elem_vld_reg = RegNext(io.elem_vld)
    val first_slide_reg = RegNext(io.first_slidedown)

    // stage-1
    val res_data_vec  = Wire(Vec(n, UInt((VLEN/n).W)))

    for(i <- 0 until n) {
        when (i.U < elem_vld_reg) {
            when (res_keep_old_vd(i)) {
                res_data_vec(i) := prev_data_vec(i)
            }.elsewhen (res_agnostic(i)) {
                res_data_vec(i) := Fill(VLEN/n, 1.U(1.W))
            }.elsewhen (res_update(i)) {
                res_data_vec(i) := src_data_vec(index(i))
            }.elsewhen (first_slide_reg) {
                res_data_vec(i) := 0.U((VLEN/n).W)
            }.otherwise {
                res_data_vec(i) := prev_data_vec(i)
            }
        }.otherwise {
            res_data_vec(i) := Mux(ta_reg, Fill(VLEN/n, 1.U(1.W)), prev_data_vec(i))
        }
    }

    io.res_data := res_data_vec.reduce{ (a, b) => Cat(b, a) }
}

class SlideDownLookupModule extends VPermModule {
    val io = IO(new VPermIO)

    val vformat = io.vd_type(1,0)
    //val sew          = LookupTree(vformat, VFormat.VFormatTable.map(p => (p._1, p._2._1)))
    val elem_num     = LookupTree(vformat, VFormat.VFormatTable.map(p => (p._1, p._2._2)))
    val elem_num_pow = LookupTree(vformat, VFormat.VFormatTable.map(p => (p._1, p._2._3)))

    val vd_idx = Wire(UInt(3.W))
    when ( io.uop_idx === 35.U ) {
        vd_idx := 7.U
    }.elsewhen ( (33.U <= io.uop_idx) && (io.uop_idx <= 34.U) ) {
        vd_idx := 6.U
    }.elsewhen ( (30.U <= io.uop_idx) && (io.uop_idx <= 32.U) ) {
        vd_idx := 5.U
    }.elsewhen ( (26.U <= io.uop_idx) && (io.uop_idx <= 29.U) ) {
        vd_idx := 4.U
    }.elsewhen ( ((io.vlmul === "b010".U) && (io.uop_idx === 9.U)) || ((io.vlmul === "b011".U) && (21.U <= io.uop_idx) && (io.uop_idx <= 25.U)) ) {
        vd_idx := 3.U
    }.elsewhen ( ((io.vlmul === "b010".U) && (7.U <= io.uop_idx) && (io.uop_idx <= 8.U)) || ((io.vlmul === "b011".U) && (15.U <= io.uop_idx) && (io.uop_idx <= 20.U)) ) {
        vd_idx := 2.U
    }.elsewhen ( ((io.vlmul === "b001".U) && (io.uop_idx === 2.U)) || ((io.vlmul === "b010".U) && (4.U <= io.uop_idx) && (io.uop_idx <= 6.U)) || ((io.vlmul === "b011".U) && (8.U <= io.uop_idx) && (io.uop_idx <= 14.U)) ) {
        vd_idx := 1.U
    }.otherwise {
        vd_idx := 0.U
    }
    val mask_start_idx = vd_idx << elem_num_pow

    val slide_base = WireInit(0.U(7.W))
    switch (io.vlmul) {
        /*is( "b101".U ) {  //lmul=1/8
            slide_base := 0.U
        }
        is( "b110".U ) {  //lmul=1/4
            slide_base := 0.U
        }
        is( "b111".U ) {  //lmul=1/2
            slide_base := 0.U
        }
        is( "b000".U ) {  //lmul=1
            slide_base := 0.U
        }*/
        is( "b001".U ) {  //lmul=2
            when ( io.uop_idx === 0.U ) {
                slide_base := 1.U << elem_num_pow
            }.otherwise {
                slide_base := 0.U
            }
        }
        is( "b010".U ) {  //lmul=4
            when ( io.uop_idx === 0.U ) {
                slide_base := 3.U << elem_num_pow
            }.elsewhen ( (io.uop_idx === 1.U) || (io.uop_idx === 4.U) ) {
                slide_base := 2.U << elem_num_pow
            }.elsewhen ( (io.uop_idx === 2.U) || (io.uop_idx === 5.U) || (io.uop_idx === 7.U) ) {
                slide_base := 1.U << elem_num_pow
            }.otherwise {
                slide_base := 0.U
            }
        }
        is( "b011".U ) {  //lmul=8
            when ( io.uop_idx === 0.U ) {
                slide_base := 7.U << elem_num_pow
            }.elsewhen ( (io.uop_idx === 1.U) || (io.uop_idx === 8.U) ) {
                slide_base := 6.U << elem_num_pow
            }.elsewhen ( (io.uop_idx === 2.U) || (io.uop_idx === 9.U) || (io.uop_idx === 15.U) ) {
                slide_base := 5.U << elem_num_pow
            }.elsewhen ( (io.uop_idx === 3.U) || (io.uop_idx === 10.U) || (io.uop_idx === 16.U) || (io.uop_idx === 21.U) ) {
                slide_base := 4.U << elem_num_pow
            }.elsewhen ( (io.uop_idx === 4.U) || (io.uop_idx === 11.U) || (io.uop_idx === 17.U) || (io.uop_idx === 22.U) || (io.uop_idx === 26.U) ) {
                slide_base := 3.U << elem_num_pow
            }.elsewhen ( (io.uop_idx === 5.U) || (io.uop_idx === 12.U) || (io.uop_idx === 18.U) || (io.uop_idx === 23.U) || (io.uop_idx === 27.U) || (io.uop_idx === 30.U) ) {
                slide_base := 2.U << elem_num_pow
            }.elsewhen ( (io.uop_idx === 6.U) || (io.uop_idx === 13.U) || (io.uop_idx === 19.U) || (io.uop_idx === 24.U) || (io.uop_idx === 28.U) || (io.uop_idx === 31.U) || (io.uop_idx === 33.U) ) {
                slide_base := 1.U << elem_num_pow
            }.otherwise {
                slide_base := 0.U
            }
        }
    }

    val first_slidedown = (io.uop_idx === 0.U) || 
                          ((io.vlmul === "b001".U) && (io.uop_idx === 2.U)) || 
                          ((io.vlmul === "b010".U) && ((io.uop_idx === 4.U) || (io.uop_idx === 7.U) || (io.uop_idx === 9.U))) || 
                          ((io.vlmul === "b011".U) && ((io.uop_idx === 8.U) || (io.uop_idx === 15.U) || (io.uop_idx === 21.U) || (io.uop_idx === 26.U) || (io.uop_idx === 30.U) || (io.uop_idx === 33.U) || (io.uop_idx === 35.U)))

    val elem_vld = LookupTreeDefault(io.vlmul, elem_num, List(
        "b101".U -> (elem_num >> 3),   //lmul=1/8
        "b110".U -> (elem_num >> 2),   //lmul=1/4
        "b111".U -> (elem_num >> 1)    //lmul=1/2
    ))

    val mask_selected = SelectMaskN(io.mask, 16, mask_start_idx)

    val slide_down_module_0 = Module(new SlideDownLookup(16)) //sew=8
    val slide_down_module_1 = Module(new SlideDownLookup(8))  //sew=16
    val slide_down_module_2 = Module(new SlideDownLookup(4))  //sew=32
    val slide_down_module_3 = Module(new SlideDownLookup(2))  //sew=64

    val slide_down_module = VecInit(Seq(slide_down_module_0.io, slide_down_module_1.io, slide_down_module_2.io, slide_down_module_3.io))
    for(i <- 0 until 4) {
        slide_down_module(i).slide_base      := slide_base
        slide_down_module(i).mask_start_idx  := mask_start_idx
        slide_down_module(i).first_slidedown := first_slidedown
        slide_down_module(i).slide           := io.vs1(XLEN-1, 0)
        slide_down_module(i).elem_vld        := elem_vld
        slide_down_module(i).vstart          := io.vstart
        slide_down_module(i).vl              := io.vl
        slide_down_module(i).vm              := io.vm
        slide_down_module(i).ta              := io.ta
        slide_down_module(i).ma              := io.ma
        slide_down_module(i).mask            := mask_selected
        slide_down_module(i).src_data        := io.vs2
        slide_down_module(i).prev_data       := io.old_vd
    }

    io.res_vd := Mux(io.vstart >= io.vl, io.old_vd, LookupTree(vformat, List(
        VectorElementFormat.b -> slide_down_module_0.io.res_data,
        VectorElementFormat.h -> slide_down_module_1.io.res_data,
        VectorElementFormat.w -> slide_down_module_2.io.res_data,
        VectorElementFormat.d -> slide_down_module_3.io.res_data
    )))
}

// vslide1down.vx
class Slide1Down(n: Int) extends VPermModule {
    val io = IO(new VPermBundle() {
        val mask_start_idx          = Input(UInt(7.W))
        val ld_rs1_with_prev_res    = Input(Bool())
        val ld_rs1_without_prev_res = Input(Bool())
        val slide1down_from_vs1     = Input(Bool())

        val slide    = Input(UInt(5.W))
        val elem_vld = Input(UInt(5.W))
        val vstart   = Input(UInt(7.W))
        val vl       = Input(UInt(8.W))
        val vm       = Input(Bool())
        val ta       = Input(Bool())
        val ma       = Input(Bool())

        val mask        = Input(UInt((VLEN/8).W))
        val src_data_lo = Input(UInt(VLEN.W))     //vs2[i]
        val src_data_hi = Input(UInt(VLEN.W))     //vs2[i+1] / rs1
        val prev_data   = Input(UInt(VLEN.W))

        val res_data    = Output(UInt(VLEN.W))
    })

    // stage-0
    val index = Wire(Vec(n, UInt(log2Up(n).W)))
    val res_keep_old_vd = Wire(Vec(n, Bool()))
    val res_agnostic = Wire(Vec(n, Bool()))
    val res_update_hi = Wire(Vec(n, Bool()))
    val res_update_lo = Wire(Vec(n, Bool()))

    val src_data_hi_vec  = Wire(Vec(n, UInt((VLEN/n).W)))
    val src_data_lo_vec  = Wire(Vec(n, UInt((VLEN/n).W)))
    val prev_data_vec = Wire(Vec(n, UInt((VLEN/n).W)))

    for(i <- 0 until n) {
        val elements_idx = io.mask_start_idx + i.U
        index(i) := RegNext(i.U + io.slide)
        res_keep_old_vd(i) := RegNext((!io.vm && !io.mask(i).asBool && !io.ma) || (elements_idx < io.vstart) || ((elements_idx >= io.vl) && !io.ta) || (io.ld_rs1_with_prev_res && (elements_idx +& 1.U =/= io.vl)))
        res_agnostic(i) := RegNext(((elements_idx >= io.vl) && io.ta) || (!io.vm && !io.mask(i).asBool && io.ma))
        res_update_hi(i) := RegNext((io.ld_rs1_with_prev_res || io.ld_rs1_without_prev_res) && (elements_idx +& 1.U === io.vl))
        res_update_lo(i) := RegNext(io.slide < (n-i).U)

        src_data_hi_vec(i) := RegNext(io.src_data_hi((VLEN/n)*(i+1)-1, (VLEN/n)*i))
        src_data_lo_vec(i) := RegNext(io.src_data_lo((VLEN/n)*(i+1)-1, (VLEN/n)*i))
        prev_data_vec(i)   := RegNext(io.prev_data((VLEN/n)*(i+1)-1, (VLEN/n)*i))
    }

    val ta_reg = RegNext(io.ta)
    val elem_vld_reg = RegNext(io.elem_vld)
    val slide1down_from_vs1_reg = RegNext(io.slide1down_from_vs1)

    // stage-1
    val res_data_vec  = Wire(Vec(n, UInt((VLEN/n).W)))

    for(i <- 0 until n) {
        when (i.U < elem_vld_reg) {
            when (res_keep_old_vd(i)) {
                res_data_vec(i) := prev_data_vec(i)
            }.elsewhen (res_agnostic(i)) {
                res_data_vec(i) := Fill(VLEN/n, 1.U(1.W))
            }.elsewhen (res_update_hi(i)) {
                res_data_vec(i) := src_data_hi_vec(0)
            }.elsewhen (res_update_lo(i)) {
                res_data_vec(i) := src_data_lo_vec(index(i))
            }.elsewhen (slide1down_from_vs1_reg) {
                res_data_vec(i) := src_data_hi_vec(0)
            }.otherwise {
                res_data_vec(i) := 0.U((VLEN/n).W)
            }
        }.otherwise {
            res_data_vec(i) := Mux(ta_reg, Fill(VLEN/n, 1.U(1.W)), prev_data_vec(i))
        }
    }

    io.res_data := res_data_vec.reduce{ (a, b) => Cat(b, a) }
}

class Slide1DownModule extends VPermModule {
    val io = IO(new VPermIO)

    val vformat = io.vd_type(1,0)
    //val sew          = LookupTree(vformat, VFormat.VFormatTable.map(p => (p._1, p._2._1)))
    val elem_num     = LookupTree(vformat, VFormat.VFormatTable.map(p => (p._1, p._2._2)))
    val elem_num_pow = LookupTree(vformat, VFormat.VFormatTable.map(p => (p._1, p._2._3)))

    val mask_start_idx = (io.uop_idx >> 1) << elem_num_pow

    val ld_rs1_without_prev_res = ((io.vlmul(2) === 1.U || io.vlmul === "b000".U) && (io.uop_idx === 0.U)) || 
                                  ((io.vlmul === "b001".U) && (io.uop_idx === 2.U)) || 
                                  ((io.vlmul === "b010".U) && (io.uop_idx === 6.U)) || 
                                  (io.uop_idx === 14.U)

    val ld_rs1_with_prev_res = (io.uop_idx === 1.U) || (io.uop_idx === 3.U) || (io.uop_idx === 5.U) || (io.uop_idx === 7.U) || (io.uop_idx === 9.U) || (io.uop_idx === 11.U) || (io.uop_idx === 13.U)

    val slide1down_from_vs1 = ((io.vlmul >= "b001".U && io.vlmul <= "b011".U) && (io.uop_idx === 0.U)) || 
                              ((io.vlmul >= "b010".U && io.vlmul <= "b011".U) && (io.uop_idx === 2.U)) || 
                              ((io.vlmul >= "b010".U && io.vlmul <= "b011".U) && (io.uop_idx === 4.U)) || 
                              ((io.vlmul === "b011".U) && (io.uop_idx === 6.U)) || 
                              (io.uop_idx === 8.U) || (io.uop_idx === 10.U) || (io.uop_idx === 12.U)

    val elem_vld = LookupTreeDefault(io.vlmul, elem_num, List(
        "b101".U -> (elem_num >> 3),   //lmul=1/8
        "b110".U -> (elem_num >> 2),   //lmul=1/4
        "b111".U -> (elem_num >> 1)    //lmul=1/2
    ))

    val mask_selected = SelectMaskN(io.mask, 16, mask_start_idx)

    val slide_1_down_module_0 = Module(new Slide1Down(16)) //sew=8
    val slide_1_down_module_1 = Module(new Slide1Down(8))  //sew=16
    val slide_1_down_module_2 = Module(new Slide1Down(4))  //sew=32
    val slide_1_down_module_3 = Module(new Slide1Down(2))  //sew=64

    val slide_1_down_module = VecInit(Seq(slide_1_down_module_0.io, slide_1_down_module_1.io, slide_1_down_module_2.io, slide_1_down_module_3.io))
    for(i <- 0 until 4) {
        slide_1_down_module(i).mask_start_idx          := mask_start_idx
        slide_1_down_module(i).ld_rs1_with_prev_res    := ld_rs1_with_prev_res
        slide_1_down_module(i).ld_rs1_without_prev_res := ld_rs1_without_prev_res
        slide_1_down_module(i).slide1down_from_vs1     := slide1down_from_vs1
        slide_1_down_module(i).slide                   := 1.U
        slide_1_down_module(i).elem_vld                := elem_vld
        slide_1_down_module(i).vstart                  := io.vstart
        slide_1_down_module(i).vl                      := io.vl
        slide_1_down_module(i).vm                      := io.vm
        slide_1_down_module(i).ta                      := io.ta
        slide_1_down_module(i).ma                      := io.ma
        slide_1_down_module(i).mask                    := mask_selected
        slide_1_down_module(i).src_data_lo             := io.vs2
        slide_1_down_module(i).src_data_hi             := io.vs1
        slide_1_down_module(i).prev_data               := io.old_vd
    }

    io.res_vd := Mux(io.vstart >= io.vl, io.old_vd, LookupTree(vformat, List(
        VectorElementFormat.b -> slide_1_down_module_0.io.res_data,
        VectorElementFormat.h -> slide_1_down_module_1.io.res_data,
        VectorElementFormat.w -> slide_1_down_module_2.io.res_data,
        VectorElementFormat.d -> slide_1_down_module_3.io.res_data
    )))
}
