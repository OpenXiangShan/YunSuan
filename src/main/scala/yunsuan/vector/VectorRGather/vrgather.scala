package yunsuan.vector

import chisel3._
import chisel3.util._
import yunsuan.util._
import yunsuan.vector._

trait VParameter {
  val VLEN       : Int = 2048
  val Vlmulmax   : Int = 8
  val Vlmax      : Int = 1024
}

// valid only for VLEN = 2048
class Vrgather() extends Module with VParameter {
  val XLEN = 64
  val io = IO(new Bundle {
    val vs1 = Input(UInt(VLEN.W))   //index data
    val vs2 = Input(UInt(VLEN.W))   //table data
    val res_vd = Output(UInt(VLEN.W))

    val sew       = Input(UInt(2.W))

    val mask      = Input(UInt(VLEN.W))
    val vm        = Input(Bool())         // 0: masked, 1: unmasked
    val ta        = Input(Bool())         // 0: undisturbed, 1: agnostic
    val ma        = Input(Bool())         // 0: undisturbed, 1: agnostic

    val vstart    = Input(UInt(7.W))      // 0-127
    val vl        = Input(UInt(8.W))      // 0-128
    val vlmul     = Input(UInt(3.W))
  })

  // latency  = 1
  // width max = 64
  // val width = Wire(UInt(6.W))
  // width := (1.U << io.sew) << 3.U

  val vrgather_e8  = Module(new VRGather_with_sew(SEW = 0))
  val vrgather_e16 = Module(new VRGather_with_sew(SEW = 1))


  vrgather_e8.io.vs1 := io.vs1
  vrgather_e8.io.vs2 := io.vs2

  vrgather_e16.io.vs1 := io.vs1
  vrgather_e16.io.vs2 := io.vs2

  // io.res_vd := Mux(io.sew === 0.U, vrgather_e8.io.res_data, vrgather_e16.io.res_data)
  io.res_vd := LookupTreeDefault(io.sew, 0.U(VLEN.W), List(
      0.U -> vrgather_e8.io.res_data,
      1.U -> vrgather_e16.io.res_data
  ) )
}


class VRGather_with_sew(
  val SEW: Int = 1
  ) 
  extends Module with VParameter{
  val XLEN = 8 << SEW
  val io = IO(new Bundle {
    val vs1 = Input(UInt(VLEN.W))
    val vs2 = Input(UInt(VLEN.W))
    val res_data = Output(UInt(VLEN.W))

    val sew       = Input(UInt(2.W))
    val vstart    = Input(UInt(7.W))
    val vl        = Input(UInt(8.W))
    val vm        = Input(Bool())
    val ta        = Input(Bool())
    val ma        = Input(Bool())
  })

    val index = Wire(Vec(VLEN / XLEN, UInt(XLEN.W)))

    // val index = Wire(UInt((VLEN/XLEN).W))
    val lut = Wire(Vec(VLEN / XLEN, UInt(XLEN.W)))

    for (n <- 0 until (VLEN / XLEN)) {
      index(n) := io.vs1((n + 1) * XLEN - 1, n * XLEN)  // 从 `index_data` 中提取每个切片
      lut(n)   := io.vs2((n + 1) * XLEN - 1, n * XLEN)    // 从 `table_data` 中提取每个切片
    }

    val res = RegInit(VecInit(Seq.fill(VLEN / XLEN)(0.U(XLEN.W))))

    for (n <- 0 until (VLEN / XLEN)) {
      res(n) := Mux(io.vs1(n) >= (VLEN/XLEN).U, 0.U, lut(index(n)))
    }
    
    io.res_data := Cat((0 until VLEN/XLEN).map(i => res(i)).reverse)

}