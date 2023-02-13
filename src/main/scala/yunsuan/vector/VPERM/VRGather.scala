import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._


// global io
class VRGatherIO extends Bundle {
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

    val res_data = Output(UInt(VLEN.W))
}

// table_lookup
class TableLookupIO extends Bundle {
    val base_vd  = Input(UInt((log2Up(VLEN)+1).W))
    val min      = Input(UInt((log2Up(VLEN)+1).W))
    val max      = Input(UInt((log2Up(VLEN)+1).W))

    val vstart   = Input(UInt(log2Up(VLEN).W))
    val vl       = Input(UInt((log2Up(VLEN)+1).W))
    val first_op = Input(Bool())
    val half     = Input(UInt(1.W))

    val mask       = Input(UInt((VLEN/8).W))
    val index_data = Input(UInt(VLEN.W))
    val table_data = Input(UInt(VLEN.W))
    val prev_data  = Input(UInt(VLEN.W))

    val res_data   = Output(UInt(VLEN.W))
}

class TableLookup(n: Int)(implicit p: Parameters) extends XSModule {
    val io = IO(new TableLookupIO)

    val index_data_vec = Wire(Vec(n, UInt((VLEN/n).W)))
    val table_data_vec = Wire(Vec(n, UInt((VLEN/n).W)))
    val prev_data_vec  = Wire(Vec(n, UInt((VLEN/n).W)))
    val res_data_vec   = Wire(Vec(n, UInt((VLEN/n).W)))

    for(i <- 0 until n) {
        index_data_vec(i) := io.index_data((VLEN/n)*(i+1)-1, (VLEN/n)*i)
        table_data_vec(i) := io.table_data((VLEN/n)*(i+1)-1, (VLEN/n)*i)
        prev_data_vec(i)  := io.prev_data((VLEN/n)*(i+1)-1, (VLEN/n)*i)
    }

    for(i <- 0 until n) {
        val index = index_data_vec(i) + ~io.min + 1.U
        when( !io.mask(i).asBool || ((io.base_vd +& i.U) < io.vstart) || ((io.base_vd +& i.U) >= io.vl) ) {
            res_data_vec(i) := prev_data_vec(i)
        }.elsewhen( (io.min <= index_data_vec(i)) && (index_data_vec(i) < io.max) ) {
            res_data_vec(i) := table_data_vec(index)
        }.elsewhen( io.first_op ) {
            res_data_vec(i) := 0.U
        }.otherwise {
            res_data_vec(i) := prev_data_vec(i)
        }
    }

    io.res_data := res_data_vec.reduce{ (a, b) => Cat(b, a) }
}

class TableLookupModule(implicit p: Parameters) extends XSModule {
    val io = IO(new VRGatherIO)

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
    val min = io.uop_idx << elem_num_pow
    val max = (io.uop_idx + 1.U) << elem_num_pow
    val vl_final = Mux(io.vl <= io.vlmax, io.vl, io.vlmax)
    val first_op = (io.uop_idx === 0.U)

    val table_module_0 = Module(new TableLookup(16))
    val table_module_1 = Module(new TableLookup(8))
    val table_module_2 = Module(new TableLookup(4))
    val table_module_3 = Module(new TableLookup(2))

    val table_module = VecInit(Seq(table_module_0.io, table_module_1.io, table_module_2.io, table_module_3.io))
    for(i <- 0 until 4) {
        table_module(i).base_vd    := base_vd
        table_module(i).min        := min
        table_module(i).max        := max
        table_module(i).vstart     := io.vstart
        table_module(i).vl         := vl_final
        table_module(i).first_op   := first_op
        table_module(i).half       := 0.U
        table_module(i).mask       := io.src(3)(base_vd + 15.U, base_vd)
        table_module(i).index_data := io.src(0)
        table_module(i).table_data := io.src(1)
        table_module(i).prev_data  := io.src(2)
    }

    io.res_data := LookupTree(io.format, List(
        "b00".U -> table_module_0.io.res_data,
        "b01".U -> table_module_1.io.res_data,
        "b10".U -> table_module_2.io.res_data,
        "b11".U -> table_module_3.io.res_data
    ))
}

// table_lookup_16
class TableLookup16Type1(n: Int)(implicit p: Parameters) extends XSModule {
    val io = IO(new TableLookupIO)

    val index_data_vec = Wire(Vec(8, UInt((VLEN/8).W)))
    val table_data_vec = Wire(Vec(n, UInt((VLEN/n).W)))
    val prev_data_vec  = Wire(Vec(n, UInt((VLEN/n).W)))
    val res_data_vec   = Wire(Vec(n, UInt((VLEN/n).W)))

    for(i <- 0 until n) {
        index_data_vec(i) := io.index_data((VLEN/8)*(i+1)-1, (VLEN/8)*i)
        table_data_vec(i) := io.table_data((VLEN/n)*(i+1)-1, (VLEN/n)*i)
        prev_data_vec(i)  := io.prev_data((VLEN/n)*(i+1)-1, (VLEN/n)*i)
    }

    for(i <- 0 until n) {
        val idx_i = (i.U + io.base_vd)(2, 0)
        val index = index_data_vec(idx_i) + ~io.min + 1.U
        when( !io.mask(i).asBool || ((io.base_vd +& i.U) < io.vstart) || ((io.base_vd +& i.U) >= io.vl) ) {
            res_data_vec(i) := prev_data_vec(i)
        }.elsewhen( (io.min <= index_data_vec(idx_i)) && (index_data_vec(idx_i) < io.max) ) {
            res_data_vec(i) := table_data_vec(index)
        }.elsewhen( io.first_op ) {
            res_data_vec(i) := 0.U
        }.otherwise {
            res_data_vec(i) := prev_data_vec(i)
        }
    }

    io.res_data := res_data_vec.reduce{ (a, b) => Cat(b, a) }
}

class TableLookup16Type2(implicit p: Parameters) extends XSModule {
    val io = IO(new TableLookupIO)

    val index_data_vec = Wire(Vec(8, UInt((VLEN/8).W)))
    val table_data_vec = Wire(Vec(16, UInt((VLEN/16).W)))
    val prev_data_vec  = Wire(Vec(16, UInt((VLEN/16).W)))
    val res_data_vec   = Wire(Vec(16, UInt((VLEN/16).W)))

    for(i <- 0 until 16) {
        index_data_vec(i) := io.index_data((VLEN/8)*(i+1)-1, (VLEN/8)*i)
        table_data_vec(i) := io.table_data((VLEN/16)*(i+1)-1, (VLEN/16)*i)
        prev_data_vec(i)  := io.prev_data((VLEN/16)*(i+1)-1, (VLEN/16)*i)
    }

    for(i <- 0 until 16) {
        val idx_i = (i.U(4.W))(2, 0)
        val index = index_data_vec(idx_i) + ~io.min + 1.U
        when( !io.mask(i).asBool || ((io.base_vd +& i.U) < io.vstart) || ((io.base_vd +& i.U) >= io.vl) || (i.U < (io.half << 3) || (i.U >= ((io.half +& 1.U) << 3)) ) {
            res_data_vec(i) := prev_data_vec(i)
        }.elsewhen( (io.min <= index_data_vec(idx_i)) && (index_data_vec(idx_i) < io.max) ) {
            res_data_vec(i) := table_data_vec(index)
        }.elsewhen( io.first_op ) {
            res_data_vec(i) := 0.U
        }.otherwise {
            res_data_vec(i) := prev_data_vec(i)
        }
    }

    io.res_data := res_data_vec.reduce{ (a, b) => Cat(b, a) }
}

class TableLookup16Module(implicit p: Parameters) extends XSModule {
    val io = IO(new VRGatherIO)

    val VFormatTable = List(
        "b00".U -> (8.U,  16.U, 4.U),
        "b01".U -> (16.U, 8.U,  3.U),
        "b10".U -> (32.U, 4.U,  2.U),
        "b11".U -> (64.U, 2.U,  1.U)
    )
    //val sew          = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._1)))
    //val elem_num     = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._2)))
    val elem_num_pow = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._3)))
    val is_8bit = (io.format === 0.U)
    val uop_idx = Mux(is_8bit, Cat(0.U(1.W), io.uop_idx(3,1)), io.uop_idx)

    val base_vd = io.vd_idx << elem_num_pow
    val min = uop_idx << elem_num_pow
    val max = (uop_idx + 1.U) << elem_num_pow
    val vl_final = Mux(io.vl <= io.vlmax, io.vl, io.vlmax)
    val first_op = (uop_idx === 0.U)
    val half = Mux(is_8bit, io.uop_idx(0), 0.U)

    val table_16_module_0 = Module(new TableLookup16Type2)
    val table_16_module_1 = Module(new TableLookup(8))
    val table_16_module_2 = Module(new TableLookup16Type1(4))
    val table_16_module_3 = Module(new TableLookup16Type1(2))

    val table_16_module = VecInit(Seq(table_16_module_0.io, table_16_module_1.io, table_16_module_2.io, table_16_module_3.io))
    for(i <- 0 until 4) {
        table_16_module(i).base_vd    := base_vd
        table_16_module(i).min        := min
        table_16_module(i).max        := max
        table_16_module(i).vstart     := io.vstart
        table_16_module(i).vl         := vl_final
        table_16_module(i).first_op   := first_op
        table_16_module(i).half       := half
        table_16_module(i).mask       := io.src(3)(base_vd + 15.U, base_vd)
        table_16_module(i).index_data := io.src(0)
        table_16_module(i).table_data := io.src(1)
        table_16_module(i).prev_data  := io.src(2)
    }

    io.res_data := LookupTree(io.format, List(
        "b00".U -> table_16_module_0.io.res_data,
        "b01".U -> table_16_module_1.io.res_data,
        "b10".U -> table_16_module_2.io.res_data,
        "b11".U -> table_16_module_3.io.res_data
    ))
}

// single lookup
class SingleLookup(n: Int)(implicit p: Parameters) extends XSModule {
    val io = IO(new Bundle() {
        val min      = Input(UInt((log2Up(VLEN)+1).W))
        val max      = Input(UInt((log2Up(VLEN)+1).W))
        val first_op = Input(Bool())

        val rs_data    = Input(UInt(VLEN.W))
        val table_data = Input(UInt(VLEN.W))
        val prev_data  = Input(UInt(VLEN.W))

        val res_data   = Output(UInt(VLEN.W))
    })

    val table_data_vec = Wire(Vec(n, UInt((VLEN/n).W)))
    val prev_data_vec  = Wire(Vec(n, UInt((VLEN/n).W)))
    val res_data_vec   = Wire(Vec(n, UInt((VLEN/n).W)))

    for(i <- 0 until n) {
        table_data_vec(i) := io.table_data((VLEN/n)*(i+1)-1, (VLEN/n)*i)
        prev_data_vec(i)  := io.prev_data((VLEN/n)*(i+1)-1, (VLEN/n)*i)
    }

    for(i <- 0 until n) {
        val index = io.rs_data + ~io.min + 1.U
        when( (io.min <= io.rs_data) && (io.rs_data < io.max) ) {
            res_data_vec(i) := table_data_vec(index)
        }.elsewhen( io.first_op ) {
            res_data_vec(i) := 0.U
        }.otherwise {
            res_data_vec(i) := prev_data_vec(i)
        }
    }

    io.res_data := res_data_vec.reduce{ (a, b) => Cat(b, a) }
}

class SingleLookupModule(implicit p: Parameters) extends XSModule {
    val io = IO(new VRGatherIO)

    val VFormatTable = List(
        "b00".U -> (8.U,  16.U, 4.U),
        "b01".U -> (16.U, 8.U,  3.U),
        "b10".U -> (32.U, 4.U,  2.U),
        "b11".U -> (64.U, 2.U,  1.U)
    )
    //val sew          = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._1)))
    //val elem_num     = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._2)))
    val elem_num_pow = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._3)))

    val min = io.vd_idx << elem_num_pow
    val max = (io.vd_idx + 1.U) << elem_num_pow
    val first_op = (io.uop_idx === 0.U)

    val single_lookup_module_0 = Module(new SingleLookup(16))
    val single_lookup_module_1 = Module(new SingleLookup(8))
    val single_lookup_module_2 = Module(new SingleLookup(4))
    val single_lookup_module_3 = Module(new SingleLookup(2))

    val single_lookup_module = VecInit(Seq(single_lookup_module_0.io, single_lookup_module_1.io, single_lookup_module_2.io, single_lookup_module_3.io))
    for(i <- 0 until 4) {
        single_lookup_module(i).min        := min
        single_lookup_module(i).max        := max
        single_lookup_module(i).first_op   := first_op
        single_lookup_module(i).rs_data    := io.src(1)(XLEN-1, 0)
        single_lookup_module(i).table_data := io.src(0)
        single_lookup_module(i).prev_data  := io.src(2)
    }

    io.res_data := LookupTree(io.format, List(
        "b00".U -> single_lookup_module_0.io.res_data,
        "b01".U -> single_lookup_module_1.io.res_data,
        "b10".U -> single_lookup_module_2.io.res_data,
        "b11".U -> single_lookup_module_3.io.res_data
    ))
}

// single assign
class SingleAssign(n: Int)(implicit p: Parameters) extends XSModule {
    val io = IO(new Bundle() {
        val base_vd  = Input(UInt((log2Up(VLEN)+1).W))
        val vstart   = Input(UInt(log2Up(VLEN).W))
        val vl       = Input(UInt((log2Up(VLEN)+1).W))

        val mask        = Input(UInt((VLEN/8).W))
        val lookup_data = Input(UInt(VLEN.W))
        val prev_data   = Input(UInt(VLEN.W))

        val res_data    = Output(UInt(VLEN.W))
    })

    val lookup_data_vec = Wire(Vec(n, UInt((VLEN/n).W)))
    val prev_data_vec   = Wire(Vec(n, UInt((VLEN/n).W)))
    val res_data_vec    = Wire(Vec(n, UInt((VLEN/n).W)))

    for(i <- 0 until n) {
        lookup_data_vec(i) := io.lookup_data((VLEN/n)*(i+1)-1, (VLEN/n)*i)
        prev_data_vec(i)   := io.prev_data((VLEN/n)*(i+1)-1, (VLEN/n)*i)
    }

    for(i <- 0 until n) {
        when( !io.mask(i).asBool || ((io.base_vd +& i.U) < io.vstart) || ((io.base_vd +& i.U) >= io.vl) ) {
            res_data_vec(i) := prev_data_vec(i)
        }.otherwise {
            res_data_vec(i) := lookup_data_vec(i)
        }
    }

    io.res_data := res_data_vec.reduce{ (a, b) => Cat(b, a) }
}

class SingleAssignModule(implicit p: Parameters) extends XSModule {
    val io = IO(new VRGatherIO)

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
    val vl_final = Mux(io.vl <= io.vlmax, io.vl, io.vlmax)

    val single_assign_module_0 = Module(new SingleAssign(16))
    val single_assign_module_1 = Module(new SingleAssign(8))
    val single_assign_module_2 = Module(new SingleAssign(4))
    val single_assign_module_3 = Module(new SingleAssign(2))

    val single_assign_module = VecInit(Seq(single_assign_module_0.io, single_assign_module_1.io, single_assign_module_2.io, single_assign_module_3.io))
    for(i <- 0 until 4) {
        single_assign_module(i).base_vd     := base_vd
        single_assign_module(i).vstart      := io.vstart
        single_assign_module(i).vl          := vl_final
        single_assign_module(i).mask        := io.src(3)(base_vd + 15.U, base_vd)
        single_assign_module(i).lookup_data := io.src(0)
        single_assign_module(i).prev_data   := io.src(1)
    }

    io.res_data := LookupTree(io.format, List(
        "b00".U -> single_assign_module_0.io.res_data,
        "b01".U -> single_assign_module_1.io.res_data,
        "b10".U -> single_assign_module_2.io.res_data,
        "b11".U -> single_assign_module_3.io.res_data
    ))
}
