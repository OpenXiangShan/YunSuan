package yunsuan.vector

import chisel3._
import chisel3.util._
import yunsuan.util._
import yunsuan.VectorElementFormat
import yunsuan.vector.vpermutil._


// vrgather.vv vd, vs2, vs1, vm
// vrgather.vi vd, vs2, uimm, vm
class VRGatherLookup(n: Int) extends VPermModule {
    val io = IO(new VPermBundle() {
        val table_range_min = Input(UInt(7.W))
        val table_range_max = Input(UInt(8.W))
        val mask_start_idx  = Input(UInt(7.W))
        val first_gather    = Input(Bool())

        val vstart   = Input(UInt(7.W))
        val vl_valid = Input(UInt(8.W))
        val vm       = Input(Bool())
        val ta       = Input(Bool())
        val ma       = Input(Bool())

        val mask       = Input(UInt((VLEN/8).W))
        val index_data = Input(UInt(VLEN.W))    //vs1
        val table_data = Input(UInt(VLEN.W))    //vs2
        val prev_data  = Input(UInt(VLEN.W))

        val res_data   = Output(UInt(VLEN.W))
    })

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
        val index = index_data_vec(i) + ~io.table_range_min + 1.U
        val elements_idx = io.mask_start_idx +& i.U
        val res_keep_old_vd = (!io.vm && !io.mask(i).asBool && !io.ma) || (elements_idx < io.vstart) || ((elements_idx >= io.vl_valid) && !io.ta)
        val res_agnostic = ((elements_idx >= io.vl_valid) && io.ta) || (!io.vm && !io.mask(i).asBool && io.ma)

        when (res_keep_old_vd) {
            res_data_vec(i) := prev_data_vec(i)
        }.elsewhen (res_agnostic) {
            res_data_vec(i) := Fill(VLEN/n, 1.U(1.W))
        }.elsewhen ( (io.table_range_min <= index_data_vec(i)) & (index_data_vec(i) < io.table_range_max) ) {
            res_data_vec(i) := table_data_vec(index)
        }.elsewhen ( io.first_gather ) {
            res_data_vec(i) := 0.U((VLEN/n).W)
        }.otherwise {
            res_data_vec(i) := prev_data_vec(i)
        }
    }

    io.res_data := res_data_vec.reduce{ (a, b) => Cat(b, a) }
}

// vrgather.vx vd, vs2, rs1, vm
class VRGatherLookupVX(n: Int) extends VPermModule {
    val io = IO(new VPermBundle() {
        val table_range_min = Input(UInt(7.W))
        val table_range_max = Input(UInt(8.W))
        val mask_start_idx  = Input(UInt(7.W))
        val first_gather    = Input(Bool())

        val vstart   = Input(UInt(7.W))
        val vl_valid = Input(UInt(8.W))
        val vm       = Input(Bool())
        val ta       = Input(Bool())
        val ma       = Input(Bool())

        val mask       = Input(UInt((VLEN/8).W))
        val index_data = Input(UInt(XLEN.W))    //vs1[63:0]
        val table_data = Input(UInt(VLEN.W))    //vs2
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
        val index = io.index_data + ~io.table_range_min + 1.U
        val elements_idx = io.mask_start_idx +& i.U
        val res_keep_old_vd = (!io.vm && !io.mask(i).asBool && !io.ma) || (elements_idx < io.vstart) || ((elements_idx >= io.vl_valid) && !io.ta)
        val res_agnostic = ((elements_idx >= io.vl_valid) && io.ta) || (!io.vm && !io.mask(i).asBool && io.ma)

        when (res_keep_old_vd) {
            res_data_vec(i) := prev_data_vec(i)
        }.elsewhen (res_agnostic) {
            res_data_vec(i) := Fill(VLEN/n, 1.U(1.W))
        }.elsewhen ( (io.table_range_min <= io.index_data) & (io.index_data < io.table_range_max) ) {
            res_data_vec(i) := table_data_vec(index)
        }.elsewhen ( io.first_gather ) {
            res_data_vec(i) := 0.U((VLEN/n).W)
        }.otherwise {
            res_data_vec(i) := prev_data_vec(i)
        }
    }

    io.res_data := res_data_vec.reduce{ (a, b) => Cat(b, a) }
}

class VRGatherLookupModule extends VPermModule {
    val io = IO(new VIALUIO)

    val vformat = io.vd_type(1,0)
    //val sew          = LookupTree(vformat, VFormat.VFormatTable.map(p => (p._1, p._2._1)))
    val elem_num     = LookupTree(vformat, VFormat.VFormatTable.map(p => (p._1, p._2._2)))
    val elem_num_pow = LookupTree(vformat, VFormat.VFormatTable.map(p => (p._1, p._2._3)))

    val vd_idx = Wire(UInt(3.W))
    when ( (56.U <= io.uop_idx) && (io.uop_idx <= 63.U) ) {
        vd_idx := 7.U
    }.elsewhen ( (48.U <= io.uop_idx) && (io.uop_idx <= 55.U) ) {
        vd_idx := 6.U
    }.elsewhen ( (40.U <= io.uop_idx) && (io.uop_idx <= 47.U) ) {
        vd_idx := 5.U
    }.elsewhen ( (32.U <= io.uop_idx) && (io.uop_idx <= 39.U) ) {
        vd_idx := 4.U
    }.elsewhen ( ((io.vlmul === "b010".U) && (12.U <= io.uop_idx) && (io.uop_idx <= 15.U)) || ((io.vlmul === "b011".U) && (24.U <= io.uop_idx) && (io.uop_idx <= 31.U)) ) {
        vd_idx := 3.U
    }.elsewhen ( ((io.vlmul === "b010".U) && (8.U <= io.uop_idx) && (io.uop_idx <= 11.U)) || ((io.vlmul === "b011".U) && (16.U <= io.uop_idx) && (io.uop_idx <= 23.U)) ) {
        vd_idx := 2.U
    }.elsewhen ( ((io.vlmul === "b001".U) && (2.U <= io.uop_idx) && (io.uop_idx <= 3.U)) || ((io.vlmul === "b010".U) && (4.U <= io.uop_idx) && (io.uop_idx <= 7.U)) || ((io.vlmul === "b011".U) && (8.U <= io.uop_idx) && (io.uop_idx <= 15.U)) ) {
        vd_idx := 1.U
    }.otherwise {
        vd_idx := 0.U
    }
    val mask_start_idx = vd_idx << elem_num_pow

    val first_gather = (io.vlmul(2) === 1.U || io.vlmul === "b000".U) || 
                       ((io.vlmul === "b001".U) && (io.uop_idx(0) === 0.U)) || 
                       ((io.vlmul === "b010".U) && (io.uop_idx(1,0) === 0.U)) || 
                       ((io.vlmul === "b011".U) && (io.uop_idx(2,0) === 0.U))

    val table_idx = Wire(UInt(3.W))
    when ( (io.vlmul === "b011".U) && (io.uop_idx(2,0) === "b111".U) ) {
        table_idx := 7.U
    }.elsewhen ( (io.vlmul === "b011".U) && (io.uop_idx(2,0) === "b110".U) ) {
        table_idx := 6.U
    }.elsewhen ( (io.vlmul === "b011".U) && (io.uop_idx(2,0) === "b101".U) ) {
        table_idx := 5.U
    }.elsewhen ( (io.vlmul === "b011".U) && (io.uop_idx(2,0) === "b100".U) ) {
        table_idx := 4.U
    }.elsewhen ( ((io.vlmul === "b011".U) && (io.uop_idx(2,0) === "b011".U)) || ((io.vlmul === "b010".U) && (io.uop_idx(1,0) === "b11".U)) ) {
        table_idx := 3.U
    }.elsewhen ( ((io.vlmul === "b011".U) && (io.uop_idx(2,0) === "b010".U)) || ((io.vlmul === "b010".U) && (io.uop_idx(1,0) === "b10".U)) ) {
        table_idx := 2.U
    }.elsewhen ( ((io.vlmul === "b011".U) && (io.uop_idx(2,0) === "b001".U)) || ((io.vlmul === "b010".U) && (io.uop_idx(1,0) === "b01".U)) || ((io.vlmul === "b001".U) && (io.uop_idx(0) === "b1".U)) ) {
        table_idx := 1.U
    }.otherwise {
        table_idx := 0.U
    }
    val table_range_min = table_idx << elem_num_pow
    val table_range_max = (table_idx +& 1.U) << elem_num_pow

    val vlmax = LookupTree(io.vlmul, List(
        "b000".U -> elem_num,          //lmul=1
        "b001".U -> (elem_num << 1),   //lmul=2
        "b010".U -> (elem_num << 2),   //lmul=4
        "b011".U -> (elem_num << 3),   //lmul=8
        "b101".U -> (elem_num >> 3),   //lmul=1/8
        "b110".U -> (elem_num >> 2),   //lmul=1/4
        "b111".U -> (elem_num >> 1)    //lmul=1/2
    ))
    val vl_valid = Mux(io.vl <= vlmax, io.vl, vlmax)

    val mask_selected = SelectMaskN(io.mask, 16, mask_start_idx)

    // vrgather.vv/vi
    val gather_lookup_module_0 = Module(new VRGatherLookup(16)) //sew=8
    val gather_lookup_module_1 = Module(new VRGatherLookup(8))  //sew=16
    val gather_lookup_module_2 = Module(new VRGatherLookup(4))  //sew=32
    val gather_lookup_module_3 = Module(new VRGatherLookup(2))  //sew=64

    val gather_lookup_module = VecInit(Seq(gather_lookup_module_0.io, gather_lookup_module_1.io, gather_lookup_module_2.io, gather_lookup_module_3.io))
    for(i <- 0 until 4) {
        gather_lookup_module(i).table_range_min  := table_range_min
        gather_lookup_module(i).table_range_max  := table_range_max
        gather_lookup_module(i).mask_start_idx   := mask_start_idx
        gather_lookup_module(i).first_gather     := first_gather
        gather_lookup_module(i).vstart           := io.vstart
        gather_lookup_module(i).vl_valid         := vl_valid
        gather_lookup_module(i).vm               := io.vm
        gather_lookup_module(i).ta               := io.ta
        gather_lookup_module(i).ma               := io.ma
        gather_lookup_module(i).mask             := mask_selected
        gather_lookup_module(i).index_data       := io.vs1
        gather_lookup_module(i).table_data       := io.vs2
        gather_lookup_module(i).prev_data        := io.old_vd
    }
    val gather_res_data = LookupTree(vformat, List(
        VectorElementFormat.b -> gather_lookup_module_0.io.res_data,
        VectorElementFormat.h -> gather_lookup_module_1.io.res_data,
        VectorElementFormat.w -> gather_lookup_module_2.io.res_data,
        VectorElementFormat.d -> gather_lookup_module_3.io.res_data
    ))

    // vrgather.vx
    val gather_vx_lookup_module_0 = Module(new VRGatherLookupVX(16)) //sew=8
    val gather_vx_lookup_module_1 = Module(new VRGatherLookupVX(8))  //sew=16
    val gather_vx_lookup_module_2 = Module(new VRGatherLookupVX(4))  //sew=32
    val gather_vx_lookup_module_3 = Module(new VRGatherLookupVX(2))  //sew=64

    val gather_vx_lookup_module = VecInit(Seq(gather_vx_lookup_module_0.io, gather_vx_lookup_module_1.io, gather_vx_lookup_module_2.io, gather_vx_lookup_module_3.io))
    for(i <- 0 until 4) {
        gather_vx_lookup_module(i).table_range_min  := table_range_min
        gather_vx_lookup_module(i).table_range_max  := table_range_max
        gather_vx_lookup_module(i).mask_start_idx   := mask_start_idx
        gather_vx_lookup_module(i).first_gather     := first_gather
        gather_vx_lookup_module(i).vstart           := io.vstart
        gather_vx_lookup_module(i).vl_valid         := vl_valid
        gather_vx_lookup_module(i).vm               := io.vm
        gather_vx_lookup_module(i).ta               := io.ta
        gather_vx_lookup_module(i).ma               := io.ma
        gather_vx_lookup_module(i).mask             := mask_selected
        gather_vx_lookup_module(i).index_data       := io.vs1(XLEN-1, 0)
        gather_vx_lookup_module(i).table_data       := io.vs2
        gather_vx_lookup_module(i).prev_data        := io.old_vd
    }
    val gather_vx_res_data = LookupTree(vformat, List(
        VectorElementFormat.b -> gather_vx_lookup_module_0.io.res_data,
        VectorElementFormat.h -> gather_vx_lookup_module_1.io.res_data,
        VectorElementFormat.w -> gather_vx_lookup_module_2.io.res_data,
        VectorElementFormat.d -> gather_vx_lookup_module_3.io.res_data
    ))

    io.res_vd := Mux(io.vstart >= io.vl, io.old_vd, Mux(io.opcode === 50.U, gather_res_data, gather_vx_res_data))
}
