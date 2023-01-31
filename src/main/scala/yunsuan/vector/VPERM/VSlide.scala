import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._


// global io
class VSlideIO extends Bundle {
    //val src_0 = Input(UInt(VLEN.W))
    //val src_1 = Input(UInt(VLEN.W))
    //val src_2 = Input(UInt(VLEN.W))
    //val src_3 = Input(UInt(VLEN.W))
    val src      = Vec(4, Input(UInt(VLEN.W)))
    val uimm     = Input(UInt(5.W))

    val format   = Input(UInt(2.W)) // 0->int8,1->int16,2->int32,3->int64
    val lmul     = Input(UInt(4.W)) // if LMUL>=1, lmul=LMUL, else lmul=1
    //val elem_num = Input(UInt((log2Up(VLEN/8)+1).W)) // = VLEN/SEW
    val vstart   = Input(UInt(log2Up(VLEN).W))
    val vl       = Input(UInt((log2Up(VLEN)+1).W))
    val vlmax    = Input(UInt((log2Up(VLEN)+1).W))

    val vd_idx   = Input(UInt(4.W))
    val uop_idx  = Input(UInt(4.W))

    val is_slide1up = Input(Bool())

    val res_data = Output(UInt(VLEN.W))
}

// slideup_lookup
class SlideLookupIO extends Bundle {
    val base    = Input(UInt((log2Up(VLEN)+1).W))
    val base_vd = Input(UInt((log2Up(VLEN)+1).W))

    val slide   = Input(UInt(XLEN.W))
    val vstart  = Input(UInt(log2Up(VLEN).W))
    val vl      = Input(UInt((log2Up(VLEN)+1).W))

    val mask      = Input(UInt((VLEN/8).W))
    val src_data  = Input(UInt(VLEN.W))
    val prev_data = Input(UInt(VLEN.W))

    val first_op  = Input(Bool())

    val res_data  = Output(UInt(VLEN.W))
}

class SlideUpLookup(n: Int)(implicit p: Parameters) extends XSModule {
    val io = IO(new SlideLookupIO)

    val src_data_vec  = Wire(Vec(n, UInt((VLEN/n).W)))
    val prev_data_vec = Wire(Vec(n, UInt((VLEN/n).W)))
    val res_data_vec  = Wire(Vec(n, UInt((VLEN/n).W)))

    for(i <- 0 until n) {
        src_data_vec(i)  := io.src_data((VLEN/n)*(i+1)-1, (VLEN/n)*i)
        prev_data_vec(i) := io.prev_data((VLEN/n)*(i+1)-1, (VLEN/n)*i)
    }

    for(i <- 0 until n) {
        val index = io.base +& i.U + ~io.slide + 1.U
        when( (io.slide <= (io.base +& i.U)) && ((io.base +& i.U) <= (io.slide +& (n-1).U)) && ((io.base_vd +& i.U) >= io.vstart ) && ((io.base_vd +& i.U) < io.vl) && io.mask(i).asBool ) {
            res_data_vec(i) := src_data_vec(index)
        }.otherwise {
            res_data_vec(i) := prev_data_vec(i)
        }
    }

    io.res_data := res_data_vec.reduce{ (a, b) => Cat(b, a) }
}

class SlideUpLookupModule(implicit p: Parameters) extends XSModule {
    val io = IO(new VSlideIO)

    val VFormatTable = List(
        "b00".U -> (8.U,  16.U, 4.U),
        "b01".U -> (16.U, 8.U,  3.U),
        "b10".U -> (32.U, 4.U,  2.U),
        "b11".U -> (64.U, 2.U,  1.U)
    )
    //val sew          = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._1)))
    //val elem_num     = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._2)))
    val elem_num_pow = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._3)))

    val base_vd = io.vd_idx << elem_num_pow
    val base    = (io.vd_idx + ~io.uop_idx + 1.U) << elem_num_pow
    val vl_final = Mux(io.vl <= io.vlmax, io.vl, io.vlmax)

    val slide_up_module_0 = Module(new SlideUpLookup(16))
    val slide_up_module_1 = Module(new SlideUpLookup(8))
    val slide_up_module_2 = Module(new SlideUpLookup(4))
    val slide_up_module_3 = Module(new SlideUpLookup(2))

    val slide_up_module = VecInit(Seq(slide_up_module_0.io, slide_up_module_1.io, slide_up_module_2.io, slide_up_module_3.io))
    for(i <- 0 until 4) {
        slide_up_module(i).base      := base
        slide_up_module(i).base_vd   := base_vd
        slide_up_module(i).slide     := io.src(2)
        slide_up_module(i).vstart    := io.vstart
        slide_up_module(i).vl        := vl_final
        slide_up_module(i).mask      := io.src(3)(base_vd + 15.U, base_vd)
        slide_up_module(i).src_data  := io.src(0)
        slide_up_module(i).prev_data := io.src(1)
        slide_up_module(i).first_op  := false.B
    }

    io.res_data := LookupTree(io.format, List(
        "b00".U -> slide_up_module_0.io.res_data,
        "b01".U -> slide_up_module_1.io.res_data,
        "b10".U -> slide_up_module_2.io.res_data,
        "b11".U -> slide_up_module_3.io.res_data
    ))
}

// slideup_lookup_fsm
class SlideLookupFsmIO extends Bundle {
    val base_vd  = Input(UInt((log2Up(VLEN)+1).W))

    val slide    = Input(UInt(4.W))
    val vstart   = Input(UInt(log2Up(VLEN).W))
    val vl       = Input(UInt((log2Up(VLEN)+1).W))

    val mask            = Input(UInt((VLEN/8).W))
    val exceed_limit_hi = Input(Bool())
    val exceed_limit_lo = Input(Bool())
    val src_data_hi     = Input(UInt(VLEN.W))
    val src_data_lo     = Input(UInt(VLEN.W))
    val prev_data       = Input(UInt(VLEN.W))

    val res_data        = Output(UInt(VLEN.W))
}

class SlideUpLookupFsm(n: Int)(implicit p: Parameters) extends XSModule {
    val io = IO(new SlideLookupFsmIO)

    val src_data_hi_vec  = Wire(Vec(n, UInt((VLEN/n).W)))
    val src_data_lo_vec  = Wire(Vec(n, UInt((VLEN/n).W)))
    val prev_data_vec = Wire(Vec(n, UInt((VLEN/n).W)))
    val res_data_vec  = Wire(Vec(n, UInt((VLEN/n).W)))

    for(i <- 0 until n) {
        src_data_hi_vec(i)  := io.src_data_hi((VLEN/n)*(i+1)-1, (VLEN/n)*i)
        src_data_lo_vec(i)  := io.src_data_lo((VLEN/n)*(i+1)-1, (VLEN/n)*i)
        prev_data_vec(i) := io.prev_data((VLEN/n)*(i+1)-1, (VLEN/n)*i)
    }

    for(i <- 0 until n) {
        val index_1 = i.U + ~io.slide + 1.U
        //val index_2 = index_1 + n.U
        when( !io.mask(i).asBool || io.exceed_limit_hi || ((io.base_vd +& i.U) < io.vstart) || ((io.base_vd +& i.U) >= io.vl) ) {
            res_data_vec(i) := prev_data_vec(i)
        }.elsewhen( io.slide <= i.U ) {
            res_data_vec(i) := src_data_hi_vec(index_1)
        }.elsewhen( ((i+1).U <= io.slide) && !io.exceed_limit_lo ) {
            res_data_vec(i) := src_data_lo_vec(index_1)
        }.otherwise {
            res_data_vec(i) := prev_data_vec(i)
        }
    }

    io.res_data := res_data_vec.reduce{ (a, b) => Cat(b, a) }
}

class SlideUpLookupFsmModule(implicit p: Parameters) extends XSModule {
    val io = IO(new VSlideIO)

    val VFormatTable = List(
        "b00".U -> (8.U,  16.U, 4.U),
        "b01".U -> (16.U, 8.U,  3.U),
        "b10".U -> (32.U, 4.U,  2.U),
        "b11".U -> (64.U, 2.U,  1.U)
    )
    //val sew          = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._1)))
    //val elem_num     = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._2)))
    val elem_num_pow = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._3)))

    val base_vd = io.vd_idx << elem_num_pow
    //val slide_final = (io.uimm & ~(31.U(5.W) << elem_num_pow))(3,0)
    val slide_final = LookupTree(io.format, List(
        "b00".U -> io.uimm(3, 0),
        "b01".U -> ZeroExt(io.uimm(2, 0), 4),
        "b10".U -> ZeroExt(io.uimm(1, 0), 4),
        "b11".U -> ZeroExt(io.uimm(0), 4)
    ))
    val vl_final = Mux(io.vl <= io.vlmax, io.vl, io.vlmax)
    val exceed_limit_hi = ((io.uimm >> elem_num_pow) >= (io.vd_idx +& 1.U))
    val exceed_limit_lo = ((io.uimm >> elem_num_pow) >= io.vd_idx)

    val slide1up_first_uop = io.is_slide1up && io.vd_idx === 0.U
    val slide1up_data_lo = LookupTree(io.format, List(
        "b00".U -> Cat(io.src(0)( 7,0), 0.U(120.W)),
        "b01".U -> Cat(io.src(0)(15,0), 0.U(112.W)),
        "b10".U -> Cat(io.src(0)(31,0), 0.U(96.W)),
        "b11".U -> Cat(io.src(0)(63,0), 0.U(64.W))
    ))

    val slide_up_fsm_module_0 = Module(new SlideUpLookupFsm(16))
    val slide_up_fsm_module_1 = Module(new SlideUpLookupFsm(8))
    val slide_up_fsm_module_2 = Module(new SlideUpLookupFsm(4))
    val slide_up_fsm_module_3 = Module(new SlideUpLookupFsm(2))

    val slide_up_fsm_module = VecInit(Seq(slide_up_fsm_module_0.io, slide_up_fsm_module_1.io, slide_up_fsm_module_2.io, slide_up_fsm_module_3.io))
    for(i <- 0 until 4) {
        slide_up_fsm_module(i).base_vd         := base_vd
        slide_up_fsm_module(i).slide           := slide_final
        slide_up_fsm_module(i).vstart          := io.vstart
        slide_up_fsm_module(i).vl              := vl_final
        slide_up_fsm_module(i).mask            := io.src(3)(base_vd + 15.U, base_vd)
        slide_up_fsm_module(i).exceed_limit_hi := exceed_limit_hi
        slide_up_fsm_module(i).exceed_limit_lo := Mux(slide1up_first_uop, false.B, exceed_limit_lo)
        slide_up_fsm_module(i).src_data_hi     := io.src(1)
        slide_up_fsm_module(i).src_data_lo     := Mux(slide1up_first_uop, slide1up_data_lo, io.src(0))
        slide_up_fsm_module(i).prev_data       := io.src(2)
    }

    io.res_data := LookupTree(io.format, List(
        "b00".U -> slide_up_fsm_module_0.io.res_data,
        "b01".U -> slide_up_fsm_module_1.io.res_data,
        "b10".U -> slide_up_fsm_module_2.io.res_data,
        "b11".U -> slide_up_fsm_module_3.io.res_data
    ))
}

// slidedown_lookup
class SlideDownLookup(n: Int)(implicit p: Parameters) extends XSModule {
    val io = IO(new SlideLookupIO)

    val src_data_vec  = Wire(Vec(n, UInt((VLEN/n).W)))
    val prev_data_vec = Wire(Vec(n, UInt((VLEN/n).W)))
    val res_data_vec  = Wire(Vec(n, UInt((VLEN/n).W)))

    for(i <- 0 until n) {
        src_data_vec(i)  := io.src_data((VLEN/n)*(i+1)-1, (VLEN/n)*i)
        prev_data_vec(i) := io.prev_data((VLEN/n)*(i+1)-1, (VLEN/n)*i)
    }

    for(i <- 0 until n) {
        val index = io.slide +& i.U + ~io.base + 1.U
        when( !io.mask(i).asBool || ((io.base_vd +& i.U) < io.vstart) || ((io.base_vd +& i.U) >= io.vl) ) {
            res_data_vec(i) := prev_data_vec(i)
        }.elsewhen( (io.base <= (i.U +& io.slide)) && ((i.U +& io.slide) <= (io.base + (n-1).U)) ) {
            res_data_vec(i) := src_data_vec(index)
        }.elsewhen( io.first_op ) {
            res_data_vec(i) := 0.U
        }.otherwise {
            res_data_vec(i) := prev_data_vec(i)
        }
    }

    io.res_data := res_data_vec.reduce{ (a, b) => Cat(b, a) }
}

class SlideDownLookupModule(implicit p: Parameters) extends XSModule {
    val io = IO(new VSlideIO)

    val VFormatTable = List(
        "b00".U -> (8.U,  16.U, 4.U),
        "b01".U -> (16.U, 8.U,  3.U),
        "b10".U -> (32.U, 4.U,  2.U),
        "b11".U -> (64.U, 2.U,  1.U)
    )
    //val sew          = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._1)))
    //val elem_num     = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._2)))
    val elem_num_pow = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._3)))

    val base_vd = io.vd_idx << elem_num_pow
    val base    = (io.lmul + ~io.vd_idx + ~io.uop_idx + 1.U) << elem_num_pow
    val vl_final = Mux(io.vl <= io.vlmax, io.vl, io.vlmax)
    val first_op = (io.uop_idx === 0.U)

    val slide_down_module_0 = Module(new SlideDownLookup(16))
    val slide_down_module_1 = Module(new SlideDownLookup(8))
    val slide_down_module_2 = Module(new SlideDownLookup(4))
    val slide_down_module_3 = Module(new SlideDownLookup(2))

    val slide_down_module = VecInit(Seq(slide_down_module_0.io, slide_down_module_1.io, slide_down_module_2.io, slide_down_module_3.io))
    for(i <- 0 until 4) {
        slide_down_module(i).base      := base
        slide_down_module(i).base_vd   := base_vd
        slide_down_module(i).slide     := io.src(2)
        slide_down_module(i).vstart    := io.vstart
        slide_down_module(i).vl        := vl_final
        slide_down_module(i).mask      := io.src(3)(base_vd + 15.U, base_vd)
        slide_down_module(i).src_data  := io.src(0)
        slide_down_module(i).prev_data := io.src(1)
        slide_down_module(i).first_op  := first_op
    }

    io.res_data := LookupTree(io.format, List(
        "b00".U -> slide_down_module_0.io.res_data,
        "b01".U -> slide_down_module_1.io.res_data,
        "b10".U -> slide_down_module_2.io.res_data,
        "b11".U -> slide_down_module_3.io.res_data
    ))
}

// slidedown_lookup_fsm
class SlideDownLookupFsm(n: Int)(implicit p: Parameters) extends XSModule {
    val io = IO(new SlideLookupFsmIO)

    val src_data_hi_vec  = Wire(Vec(n, UInt((VLEN/n).W)))
    val src_data_lo_vec  = Wire(Vec(n, UInt((VLEN/n).W)))
    val prev_data_vec = Wire(Vec(n, UInt((VLEN/n).W)))
    val res_data_vec  = Wire(Vec(n, UInt((VLEN/n).W)))

    for(i <- 0 until n) {
        src_data_hi_vec(i)  := io.src_data_hi((VLEN/n)*(i+1)-1, (VLEN/n)*i)
        src_data_lo_vec(i)  := io.src_data_lo((VLEN/n)*(i+1)-1, (VLEN/n)*i)
        prev_data_vec(i) := io.prev_data((VLEN/n)*(i+1)-1, (VLEN/n)*i)
    }

    for(i <- 0 until n) {
        val index_1 = i.U + io.slide
        //val index_2 = index_1 - n.U
        when( !io.mask(i).asBool || ((io.base_vd +& i.U) < io.vstart) || ((io.base_vd +& i.U) >= io.vl) ) {
            res_data_vec(i) := prev_data_vec(i)
        }.elsewhen( io.exceed_limit_lo ){
            res_data_vec(i) := 0.U
        }.elsewhen( (i.U +& io.slide) <= (n-1).U ) {
            res_data_vec(i) := src_data_lo_vec(index_1)
        }.elsewhen( (n.U <= (i.U +& io.slide)) && !io.exceed_limit_hi ) {
            res_data_vec(i) := src_data_hi_vec(index_1)
        }.otherwise {
            res_data_vec(i) := 0.U
        }
    }

    io.res_data := res_data_vec.reduce{ (a, b) => Cat(b, a) }
}

class SlideDownLookupFsmModule(implicit p: Parameters) extends XSModule {
    val io = IO(new VSlideIO)

    val VFormatTable = List(
        "b00".U -> (8.U,  16.U, 4.U),
        "b01".U -> (16.U, 8.U,  3.U),
        "b10".U -> (32.U, 4.U,  2.U),
        "b11".U -> (64.U, 2.U,  1.U)
    )
    //val sew          = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._1)))
    //val elem_num     = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._2)))
    val elem_num_pow = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._3)))

    val base_vd = io.vd_idx << elem_num_pow
    //val slide_final = (io.uimm & ~(31.U(5.W) << elem_num_pow))(3,0)
    val slide_final = LookupTree(io.format, List(
        "b00".U -> io.uimm(3, 0),
        "b01".U -> ZeroExt(io.uimm(2, 0), 4),
        "b10".U -> ZeroExt(io.uimm(1, 0), 4),
        "b11".U -> ZeroExt(io.uimm(0), 4)
    ))
    val vl_final = Mux(io.vl <= io.vlmax, io.vl, io.vlmax)
    val exceed_limit_hi = ((io.uimm >> elem_num_pow) >= (io.lmul + ~io.vd_idx))
    val exceed_limit_lo = ((io.uimm >> elem_num_pow) >= (io.lmul + ~io.vd_idx + 1.U))

    val slide_down_fsm_module_0 = Module(new SlideDownLookupFsm(16))
    val slide_down_fsm_module_1 = Module(new SlideDownLookupFsm(8))
    val slide_down_fsm_module_2 = Module(new SlideDownLookupFsm(4))
    val slide_down_fsm_module_3 = Module(new SlideDownLookupFsm(2))

    val slide_down_fsm_module = VecInit(Seq(slide_down_fsm_module_0.io, slide_down_fsm_module_1.io, slide_down_fsm_module_2.io, slide_down_fsm_module_3.io))
    for(i <- 0 until 4) {
        slide_down_fsm_module(i).base_vd         := base_vd
        slide_down_fsm_module(i).slide           := slide_final
        slide_down_fsm_module(i).vstart          := io.vstart
        slide_down_fsm_module(i).vl              := vl_final
        slide_down_fsm_module(i).mask            := io.src(3)(base_vd + 15.U, base_vd)
        slide_down_fsm_module(i).exceed_limit_hi := exceed_limit_hi
        slide_down_fsm_module(i).exceed_limit_lo := exceed_limit_lo
        slide_down_fsm_module(i).src_data_hi     := io.src(1)
        slide_down_fsm_module(i).src_data_lo     := io.src(0)
        slide_down_fsm_module(i).prev_data       := io.src(2)
    }

    io.res_data := LookupTree(io.format, List(
        "b00".U -> slide_down_fsm_module_0.io.res_data,
        "b01".U -> slide_down_fsm_module_1.io.res_data,
        "b10".U -> slide_down_fsm_module_2.io.res_data,
        "b11".U -> slide_down_fsm_module_3.io.res_data
    ))
}

// slidedown_assign_single_data
class SlideDownAssignSingle(n: Int)(implicit p: Parameters) extends XSModule {
    val io = IO(new Bundle() {
        val vl          = Input(UInt((log2Up(VLEN)+1).W))
        val vl_valid    = Input(Bool())
        val dest        = Input(UInt(4.U))
        val mask        = Input(UInt((VLEN/8).W))
        val src_data_rs = Input(UInt(XLEN.W))
        val prev_data   = Input(UInt(VLEN.W))
        val res_data    = Output(UInt(VLEN.W))
    })

    val prev_data_vec = Wire(Vec(n, UInt((VLEN/n).W)))
    val res_data_vec  = Wire(Vec(n, UInt((VLEN/n).W)))

    for(i <- 0 until n) {
        prev_data_vec(i) := io.prev_data((VLEN/n)*(i+1)-1, (VLEN/n)*i)
    }

    for(i <- 0 until n) {
        when( io.mask(i).asBool && (i.U === io.dest) && io.vl_valid ) {
            res_data_vec(i) := io.src_data_rs(VLEN/n-1, 0)
        }.otherwise {
            res_data_vec(i) := prev_data_vec(i)
        }
    }

    io.res_data := res_data_vec.reduce{ (a, b) => Cat(b, a) }
}

class SlideDownAssignSingleModule(implicit p: Parameters) extends XSModule {
    val io = IO(new VSlideIO)

    val VFormatTable = List(
        "b00".U -> (8.U,  16.U, 4.U),
        "b01".U -> (16.U, 8.U,  3.U),
        "b10".U -> (32.U, 4.U,  2.U),
        "b11".U -> (64.U, 2.U,  1.U)
    )
    //val sew          = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._1)))
    //val elem_num     = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._2)))
    val elem_num_pow = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._3)))

    val base_vd = io.vd_idx << elem_num_pow
    val vl_valid = (io.vl <= io.vlmax)
    val vl_1 = io.vl - 1.U
    val dest = LookupTree(io.format, List(
        "b00".U -> vl_1(3, 0),
        "b01".U -> ZeroExt(vl_1(2, 0), 4),
        "b10".U -> ZeroExt(vl_1(1, 0), 4),
        "b11".U -> ZeroExt(vl_1(0), 4)
    ))

    val slide_down_assign_single_module_0 = Module(new SlideDownAssignSingle(16))
    val slide_down_assign_single_module_1 = Module(new SlideDownAssignSingle(8))
    val slide_down_assign_single_module_2 = Module(new SlideDownAssignSingle(4))
    val slide_down_assign_single_module_3 = Module(new SlideDownAssignSingle(2))

    val slide_down_assign_single_module = VecInit(Seq(slide_down_assign_single_module_0.io, slide_down_assign_single_module_1.io, slide_down_assign_single_module_2.io, slide_down_assign_single_module_3.io))
    for(i <- 0 until 4) {
        slide_down_assign_single_module(i).vl          := io.vl
        slide_down_assign_single_module(i).vl_valid    := vl_valid
        slide_down_assign_single_module(i).dest        := dest
        slide_down_assign_single_module(i).mask        := io.src(3)(base_vd + 15.U, base_vd)
        slide_down_assign_single_module(i).src_data_rs := io.src(0)(XLEN-1, 0)
        slide_down_assign_single_module(i).prev_data   := io.src(1)
    }

    io.res_data := LookupTree(io.format, List(
        "b00".U -> slide_down_assign_single_module_0.io.res_data,
        "b01".U -> slide_down_assign_single_module_1.io.res_data,
        "b10".U -> slide_down_assign_single_module_2.io.res_data,
        "b11".U -> slide_down_assign_single_module_3.io.res_data
    ))
}
