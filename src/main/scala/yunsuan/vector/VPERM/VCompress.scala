import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._


// global io
class VCompIO extends Bundle {
    //val src_0 = Input(UInt(VLEN.W))
    //val src_1 = Input(UInt(VLEN.W))
    //val src_2 = Input(UInt(VLEN.W))
    //val src_3 = Input(UInt(VLEN.W))
    val src      = Vec(4, Input(UInt(VLEN.W)))

    val format   = Input(UInt(2.W)) // 0->int8,1->int16,2->int32,3->int64
    val lmul     = Input(UInt(4.W)) // if LMUL>=1, lmul=LMUL, else lmul=1
    //val elem_num = Input(UInt((log2Up(VLEN/8)+1).W)) // = VLEN/SEW
    val vl       = Input(UInt((log2Up(VLEN)+1).W))
    val vlmax    = Input(UInt((log2Up(VLEN)+1).W))

    val vd_idx   = Input(UInt(4.W))
    val uop_idx  = Input(UInt(4.W))

    val res_data = Output(UInt(VLEN.W))
}

// vcpop
class CPopModule(implicit p: Parameters) extends XSModule {
    val io = IO(new VCompIO)

    val VFormatTable = List(
        "b00".U -> (8.U,  16.U, 4.U),
        "b01".U -> (16.U, 8.U,  3.U),
        "b10".U -> (32.U, 4.U,  2.U),
        "b11".U -> (64.U, 2.U,  1.U)
    )
    //val sew          = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._1)))
    //val elem_num     = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._2)))
    val elem_num_pow = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._3)))

    val base = io.vd_idx << elem_num_pow
    val mask = LookupTree(io.format, List(
        "b00".U -> io.src(0)(base + 15.U, base),
        "b01".U -> ZeroExt(io.src(0)(base + 7.U, base), 16),
        "b10".U -> ZeroExt(io.src(0)(base + 3.U, base), 16),
        "b11".U -> ZeroExt(io.src(0)(base + 1.U, base), 16)
    ))

    val pop = PopCount(mask)
    val res = io.src(1)(7,0) + pop

    io.res_data := ZeroExt(res(7,0), VLEN)
}

// vcompress
class CompressIO extends Bundle {
    val base = Input(UInt((log2Up(VLEN)+1).W))
    val vl   = Input(UInt((log2Up(VLEN)+1).W))
    val pmos = Input(UInt((log2Up(VLEN)+1).W))

    val mask      = Input(UInt((VLEN/8).W))
    val src_data  = Input(UInt(VLEN.W))
    val prev_data = Input(UInt(VLEN.W))

    val res_data  = Output(UInt(VLEN.W))
}

class Compress(n: Int)(implicit p: Parameters) extends XSModule {
    val io = IO(new CompressIO)

    val src_data_vec  = Wire(Vec(n, UInt((VLEN/n).W)))
    val prev_data_vec = Wire(Vec(n, UInt((VLEN/n).W)))
    val res_data_vec  = Wire(Vec(n, UInt((VLEN/n).W)))
    val cmos_vec      = Wire(Vec(n, UInt((log2Up(VLEN)+1).W)))

    for(i <- 0 until n) {
        src_data_vec(i)  := io.src_data((VLEN/n)*(i+1)-1, (VLEN/n)*i)
        prev_data_vec(i) := io.prev_data((VLEN/n)*(i+1)-1, (VLEN/n)*i)
        res_data_vec(i)  := prev_data_vec(i)
    }

    for(i <- 0 until n) {
        if (i == 0) {
            cmos_vec(i) := io.pmos + io.mask(i)
        }
        else {
            cmos_vec(i) := cmos_vec(i-1) + io.mask(i)
        }
        val res_idx = cmos_vec(i) + ~io.base // = cmos_vec(i) - base - 1
    
        when( io.mask(i).asBool && (io.base < cmos_vec(i)) && (cmos_vec(i) < (io.base +& (n+1).U)) && (cmos_vec(i) <= io.vl) ) {
            res_data_vec(res_idx) := src_data_vec(i)
        }
    }

    io.res_data := res_data_vec.reduce{ (a, b) => Cat(b, a) }
}

class CompressModule(implicit p: Parameters) extends XSModule {
    val io = IO(new VCompIO)

    val VFormatTable = List(
        "b00".U -> (8.U,  16.U, 4.U),
        "b01".U -> (16.U, 8.U,  3.U),
        "b10".U -> (32.U, 4.U,  2.U),
        "b11".U -> (64.U, 2.U,  1.U)
    )
    //val sew          = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._1)))
    //val elem_num     = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._2)))
    val elem_num_pow = LookupTreeDefault(io.format, 0.U, VFormatTable.map(p => (p._1, p._2._3)))

    val base = io.vd_idx << elem_num_pow
    val vl_final = Mux(io.vl <= io.vlmax, io.vl, io.vlmax)
    val base_uop = io.uop_idx << elem_num_pow
    val mask = LookupTree(io.format, List(
        "b00".U -> io.src(0)(base_uop + 15.U, base_uop),
        "b01".U -> ZeroExt(io.src(0)(base_uop + 7.U, base_uop), 16),
        "b10".U -> ZeroExt(io.src(0)(base_uop + 3.U, base_uop), 16),
        "b11".U -> ZeroExt(io.src(0)(base_uop + 1.U, base_uop), 16)
    ))

    val compress_module_0 = Module(new Compress(16))
    val compress_module_1 = Module(new Compress(8))
    val compress_module_2 = Module(new Compress(4))
    val compress_module_3 = Module(new Compress(2))

    val compress_module = VecInit(Seq(compress_module_0.io, compress_module_1.io, compress_module_2.io, compress_module_3.io))
    for(i <- 0 until 4) {
        compress_module(i).base      := base
        compress_module(i).vl        := vl_final
        compress_module(i).pmos      := io.src(3)(7,0)
        compress_module(i).mask      := mask
        compress_module(i).src_data  := io.src(1)
        compress_module(i).prev_data := io.src(2)
    }

    io.res_data := LookupTree(io.format, List(
        "b00".U -> compress_module_0.io.res_data,
        "b01".U -> compress_module_1.io.res_data,
        "b10".U -> compress_module_2.io.res_data,
        "b11".U -> compress_module_3.io.res_data
    ))
}
