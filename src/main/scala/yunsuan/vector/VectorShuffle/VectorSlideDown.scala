package yunsuan.vector.VectorShuffle

import chisel3._
import chisel3.util._
import yunsuan.util.LiteralCat
import yunsuan.util.Reflect.{getUIntMaxWidthOfObject, getUIntWidthOfObject}
import yunsuan.vector.Common._


class VectorSlideDown(vlen: Int) extends VectorShuffleBaseModule(vlen) {
  ////////
  // stage 0: rotate down to be aligned in vreg
  //

  val s0_vs8bVec = WireInit(s0.bits.src2.to8bitVec)

  val s0_alignedVs8bVec: Vec[UInt] = s0_vs8bVec.rotateDown(e8offset)

  for (dregE8IdxHigh <- 0 until 8) {
    for (dregE8IdxLow <- 0 until VLENB) {
      wdataVec(dregE8IdxHigh)(dregE8IdxLow).valid := dregE8IdxHigh.U === s0.bits.uopIdx && s0.valid
      wdataVec(dregE8IdxHigh)(dregE8IdxLow).bits := s0_alignedVs8bVec(dregE8IdxLow)
    }
  }

  ////////
  // stage 1: rotate down to get right result
  //

  val zeroRegionFrom = vlen.U - e8offset
  val zeroRegionUntil = e8vl

  val vd = Wire(VecE8)

  val dregIdx = WireInit(VdIdx, s1.bits.uopIdx + e8offsetVreg)
  val dregGapOffset = 0.U - e8offsetInVreg

  for (i <- 0 until VLENB) {
    val vdE8Idx = Cat(s1.bits.uopIdx, i.U(ElemIdxWidth.W))
    vd(i) := Mux1H(Seq(
      (vdE8Idx < e8vl && vdE8Idx < zeroRegionFrom) ->
        Mux1H(Seq(
          (i.U <  dregGapOffset) -> dreg8bMatrix(dregIdx)(i),
          (i.U >= dregGapOffset) -> dreg8bMatrix(dregIdx + 1.U)(i),
        )),
      (vdE8Idx >= zeroRegionFrom && vdE8Idx < zeroRegionUntil) -> 0.U,
      (vdE8Idx >= e8vl) -> fill8b1s,
    ))
  }

  io.out.valid := s1.valid
  io.out.bits.dest := vd.asUInt
  io.out.bits.vdIdx := s1.bits.uopIdx
}
