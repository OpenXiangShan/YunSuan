package yunsuan.vector

import chisel3._
import chisel3.util._
import yunsuan.util._
import yunsuan.vector._

trait VParameter {
  val VLEN       : Int = 2048
  val VLMUL      : Int = 8  // vlmul max
}

// valid only for VLEN = 2048
class Vrgather() extends Module with VParameter {
  val XLEN = 64
  val io = IO(new Bundle {
    val vs1 = Input(UInt(VLEN.W))   //index data
    val vs2 = Input(UInt(VLEN.W))   //table data
    val res_vd = Output(UInt(VLEN.W))

    val sew       = Input(UInt(2.W))

    val vm        = Input(Bool())         // 0: masked, 1: unmasked
    val ta        = Input(Bool())         // 0: undisturbed, 1: agnostic
    val ma        = Input(Bool())         // 0: undisturbed, 1: agnostic

    val vstart    = Input(UInt(7.W))      // 0-127
    val vl        = Input(UInt(8.W))      // 0-128
    val vlmul     = Input(UInt(3.W))
  })

  // latency  = 1
  // width max = 64

  val vrg  = Module(new New_VRGather)

  vrg.io.index_data := io.vs1
  vrg.io.table_data := io.vs2
  vrg.io.sew       := io.sew
  
  io.res_vd := vrg.io.res_data

}

class vrg_index extends Bundle with VParameter {
  val index_e8 = Vec(VLEN / 8,   UInt((VLEN/8 ).W))
  val index_e16 = Vec(VLEN / 16, UInt((VLEN/16).W))
}

class vrg_lut extends Bundle with VParameter {
  val lut_e8 = Vec(VLEN / 8, UInt(8.W))
  val lut_e16 = Vec(VLEN / 16, UInt(16.W))
}

class vrg_res extends Bundle with VParameter {
  val res_e8 = Vec(VLEN / 8, UInt(8.W))
  val res_e16 = Vec(VLEN / 16, UInt(16.W))
}

class New_VRGather extends Module with VParameter{
  val io = IO(new Bundle {
    val index_data = Input(UInt(VLEN.W))
    val table_data = Input(UInt(VLEN.W))
    val res_data = Output(UInt(VLEN.W))
    val sew       = Input(UInt(2.W))
    // val vstart    = Input(UInt(7.W))
    // val vl        = Input(UInt(8.W))
    // val vm        = Input(Bool())
    // val ta        = Input(Bool())
    // val ma        = Input(Bool())
  })
    val XLEN = 8.U << io.sew
    val index = Wire(new vrg_index)
    val lut   = Wire(new vrg_lut)
    val res   = Wire(new vrg_res)

    for (n <- 0 until (VLEN / 8)) {
      lut.lut_e8(n)       := io.table_data((n + 1) * 8 - 1, n * 8)
      index.index_e8(n)   := 1.U << io.index_data((n + 1) * 8 - 1, n * 8) 
    }

    for (n <- 0 until (VLEN / 16)) {
      lut.lut_e16(n)      := io.table_data((n + 1) * 16 - 1, n * 16)
      index.index_e16(n)  := 1.U << io.index_data((n + 1) * 16 - 1, n * 16) 
    }

    for (n <- 0 until (VLEN / 8)) {
      res.res_e8(n)  := Mux1H(index.index_e8(n), lut.lut_e8)
    }
    for (n <- 0 until (VLEN / 16)) {
      res.res_e16(n) := Mux1H(index.index_e16(n), lut.lut_e16)
    }

    val res_reg = RegInit((0.U(VLEN.W)))
    
    res_reg :=  (Fill(VLEN, io.sew === 0.U) & Cat(res.res_e8.reverse)) |
                (Fill(VLEN, io.sew === 1.U) & Cat(res.res_e16.reverse))

    io.res_data := res_reg

}