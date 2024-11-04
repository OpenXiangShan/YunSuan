package yunsuan.vector.VectorShuffle

import chisel3._
import chisel3.util._
import yunsuan.util.LiteralCat
import yunsuan.util.Reflect.{getUIntMaxWidthOfObject, getUIntWidthOfObject}
import yunsuan.vector.Common.VectorConfig

import scala.language.{existentials, postfixOps}

class VectorSlideUp(val vlen: Int) extends Module with VectorConfig {
  val io = IO(new Bundle {
    val in = Input(ValidIO(new Bundle {
      val uopIdx = UInt(3.W)
      val src1 = UInt(vlen.W)
      val src2 = UInt(vlen.W)
      val vl = UInt(VlWidth.W)
      val eew = UInt(2.W)
    }))

    val out = Output(ValidIO(new Bundle {
      val dest = UInt(vlen.W)
    }))

    val rdataVec = Input(Vec(8, UInt(vlen.W)))
    val wdataVec = Output(Vec(8, Vec(vlen / 8, ValidIO(UInt(8.W)))))
  })

  val valid = io.in.valid
  val uopIdx = io.in.bits.uopIdx

  val data8bVec = Wire(Vec(8, Vec(vlen / 8, UInt( 8.W))))
  data8bVec := io.rdataVec.asTypeOf(data8bVec)

  val i_e8vl = (io.in.bits.vl << io.in.bits.eew).asUInt(VlWidth - 1, 0)
  val i_e8offset = (io.in.bits.src1(VlWidth - 1, 0) << io.in.bits.eew).asUInt(VlWidth - 1, 0)
  val i_vregOffset = (i_e8offset >> ElemIdxWidth).asUInt
  val i_vs = Wire(Vec(vlen / 8, UInt(8.W)))
  val i_inBypass = i_vregOffset =/= 0.U
  val i_uopIdx = io.in.bits.uopIdx

  i_vs := io.in.bits.src2.asTypeOf(i_vs)

  val reg_e8vl        = RegEnable(i_e8vl,       valid && uopIdx === 0.U)
  val reg_e8offset    = RegEnable(i_e8offset,   valid && uopIdx === 0.U)
  val reg_vregOffset  = RegEnable(i_vregOffset, valid && uopIdx === 0.U)
  val reg_inBypass    = RegEnable(i_inBypass,   valid && uopIdx === 0.U)

  val e8vl        = Mux(valid && uopIdx === 0.U, i_e8vl, reg_e8vl)
  val e8offset    = Mux(valid && uopIdx === 0.U, i_e8offset, reg_e8offset)
  val vregOffset  = Mux(valid && uopIdx === 0.U, i_vregOffset, reg_vregOffset)
  val inBypass    = Mux(valid && uopIdx === 0.U, i_inBypass, reg_inBypass)

  val dataVecIdx = (vregOffset +& uopIdx)(VIdxWidth - 1, 0)

  val vsIdxVec = Wire(Vec(vlen, UInt(3.W)))
  val vsE8InVregIdxVec = WireInit(VecInit((0 until VLENB).map(i => (i.U - e8offset)(ElemIdxWidth - 1, 0))))
  val vsE8IdxVec = Wire(Vec(vlen, UInt(VstartWidth.W)))

  val wdataVec = Wire(chiselTypeOf(io.wdataVec))

  for (regIdx <- 0 until 8) {
    for (eIdx <- 0 until VLENB) {
      vsIdxVec(regIdx * VLENB + eIdx) := Mux(
        eIdx.U > e8offset(ElemIdxWidth - 1, 0),
        vregOffset + (regIdx + 1).U,
        vregOffset + regIdx.U,
      )
      vsE8IdxVec(regIdx * VLENB + eIdx) := Cat(vsIdxVec(regIdx * VLENB + eIdx), vsE8InVregIdxVec(eIdx))

      val vsEIdx = vsE8IdxVec(regIdx * VLENB + eIdx)
      val vsEIdxInVreg = vsEIdx(ElemIdxWidth - 1, 0)

      wdataVec(regIdx)(eIdx).valid := vsEIdx >= 0.U && vsEIdx < VLENB.U
      wdataVec(regIdx)(eIdx).bits := i_vs(vsEIdxInVreg)
    }
  }

  val vd = Wire(Vec(vlen / 8, UInt(8.W)))

  for (i <- 0 until VLENB) {
    val vsE8IdxInVreg = vsE8InVregIdxVec(i)
    val vsIdx = vsIdxVec(i)
    val dataVecEIdx = (i % VLENB).U
    when(!inBypass) {
      vd(i) := Mux1H(Seq(
        (i.U  < e8offset && i.U < e8vl) -> i_vs(i),
        (i.U >= e8offset && i.U < e8vl) -> data8bVec(dataVecIdx)(dataVecEIdx),
      ))
    }.otherwise {
      vd(i) := Mux1H(Seq(
        (i.U  < e8offset && i.U < e8vl) -> i_vs(i),
        (i.U >= e8offset && i.U < e8vl && vsIdx < i_uopIdx) -> data8bVec(dataVecIdx)(dataVecEIdx),
        (i.U >= e8offset && i.U < e8vl && vsIdx === i_uopIdx) -> i_vs(vsE8IdxInVreg),
      ))
    }
  }

  io.wdataVec := wdataVec

  io.out.valid := io.in.valid
  io.out.bits.dest := vd.asTypeOf(io.out.bits.dest)
}
