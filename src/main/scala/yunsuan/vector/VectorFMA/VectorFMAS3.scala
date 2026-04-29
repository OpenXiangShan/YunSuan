package yunsuan.vector.VectorFMA

import chisel3._
import chisel3.util._

class VectorFMAS3 extends VectorFMA.Module {
  val in = IO(Input(new VectorFMAS2.S2ToS3))
  val out = IO(Output(new VectorFMA.Out))

  private val fflagsNv = "b10000".U(5.W)
  private val fflagsOverflow = "b00101".U(5.W)
  private val fflagsNone = 0.U(5.W)

  private val inCtrl = in.ctrl
  private val inData = in.data

  val isFp32 = inCtrl.resultIsFp32
  val isFp64 = inCtrl.resultIsFp64
  val isFp16 = !isFp32 && !isFp64

  private def expandFflags(
    fflagsF64: UInt,
    fflagsF32_0: UInt,
    fflagsF32_1: UInt,
    fflagsF16_0: UInt,
    fflagsF16_1: UInt,
    fflagsF16_2: UInt,
    fflagsF16_3: UInt
  ): Vec[UInt] = {
    val fflagsF16 = Seq(
      fflagsF16_0, fflagsF16_0,
      fflagsF16_1, fflagsF16_1,
      fflagsF16_2, fflagsF16_2,
      fflagsF16_3, fflagsF16_3
    )
    val fflagsF32 = Seq(
      fflagsF32_0, fflagsF32_0, fflagsF32_0, fflagsF32_0,
      fflagsF32_1, fflagsF32_1, fflagsF32_1, fflagsF32_1
    )

    VecInit(Seq.tabulate(floatWidth / 8) { i =>
      Mux1H(Seq(
        isFp16 -> fflagsF16(i),
        isFp32 -> fflagsF32(i),
        isFp64 -> fflagsF64,
      ))
    })
  }
  val fpResultF64 = Wire(UInt(64.W))
  val fflagsF64 = Wire(UInt(5.W))
  fpResultF64 := inData.normalResultF64
  fflagsF64 := inCtrl.normalFFlagsF64
  when(inCtrl.hasNanF64) {
    fpResultF64 := inData.nanResultF64
    fflagsF64 := Mux(inCtrl.hasNanIsNvF64, fflagsNv, fflagsNone)
  }.elsewhen(inCtrl.hasInfF64) {
    fpResultF64 := Mux(inCtrl.hasInfIsNvF64, inData.nanResultF64, inData.infResultF64)
    fflagsF64 := Mux(inCtrl.hasInfIsNvF64, fflagsNv, fflagsNone)
  }.elsewhen(inCtrl.isOverflowF64) {
    fpResultF64 := Mux(inCtrl.isOverflowDownF64, inData.overflowDownResultF64, inData.overflowUpResultF64)
    fflagsF64 := fflagsOverflow
  }.elsewhen(inCtrl.hasZeroF64) {
    fpResultF64 := Mux(
      inCtrl.fpAOrBIsZeroF64,
      inData.zeroFromOperandResultF64,
      Mux(inCtrl.normalResultIsZeroF64, inData.zeroNormalResultF64, inData.normalResultF64)
    )
    fflagsF64 := Mux(inCtrl.fpAOrBIsZeroF64 | inCtrl.normalResultIsZeroF64, fflagsNone, inCtrl.normalFFlagsF64)
  }

  val fpResultF32_0 = Wire(UInt(32.W))
  val fflagsF32_0 = Wire(UInt(5.W))
  fpResultF32_0 := inData.normalResultF32_0
  fflagsF32_0 := inCtrl.normalFFlagsF32_0
  when(inCtrl.hasNanF32_0) {
    fpResultF32_0 := inData.nanResultF32_0
    fflagsF32_0 := Mux(inCtrl.hasNanIsNvF32_0, fflagsNv, fflagsNone)
  }.elsewhen(inCtrl.hasInfF32_0) {
    fpResultF32_0 := Mux(inCtrl.hasInfIsNvF32_0, inData.nanResultF32_0, inData.infResultF32_0)
    fflagsF32_0 := Mux(inCtrl.hasInfIsNvF32_0, fflagsNv, fflagsNone)
  }.elsewhen(inCtrl.isOverflowF32_0) {
    fpResultF32_0 := Mux(inCtrl.isOverflowDownF32_0, inData.overflowDownResultF32_0, inData.overflowUpResultF32_0)
    fflagsF32_0 := fflagsOverflow
  }.elsewhen(inCtrl.hasZeroF32_0) {
    fpResultF32_0 := Mux(
      inCtrl.fpAOrBIsZeroF32_0,
      inData.zeroFromOperandResultF32_0,
      Mux(inCtrl.normalResultIsZeroF32_0, inData.zeroNormalResultF32_0, inData.normalResultF32_0)
    )
    fflagsF32_0 := Mux(inCtrl.fpAOrBIsZeroF32_0 | inCtrl.normalResultIsZeroF32_0, fflagsNone, inCtrl.normalFFlagsF32_0)
  }

  val fpResultF32_1 = Wire(UInt(32.W))
  val fflagsF32_1 = Wire(UInt(5.W))
  fpResultF32_1 := inData.normalResultF32_1
  fflagsF32_1 := inCtrl.normalFFlagsF32_1
  when(inCtrl.hasNanF32_1) {
    fpResultF32_1 := inData.nanResultF32_1
    fflagsF32_1 := Mux(inCtrl.hasNanIsNvF32_1, fflagsNv, fflagsNone)
  }.elsewhen(inCtrl.hasInfF32_1) {
    fpResultF32_1 := Mux(inCtrl.hasInfIsNvF32_1, inData.nanResultF32_1, inData.infResultF32_1)
    fflagsF32_1 := Mux(inCtrl.hasInfIsNvF32_1, fflagsNv, fflagsNone)
  }.elsewhen(inCtrl.isOverflowF32_1) {
    fpResultF32_1 := Mux(inCtrl.isOverflowDownF32_1, inData.overflowDownResultF32_1, inData.overflowUpResultF32_1)
    fflagsF32_1 := fflagsOverflow
  }.elsewhen(inCtrl.hasZeroF32_1) {
    fpResultF32_1 := Mux(
      inCtrl.fpAOrBIsZeroF32_1,
      inData.zeroFromOperandResultF32_1,
      Mux(inCtrl.normalResultIsZeroF32_1, inData.zeroNormalResultF32_1, inData.normalResultF32_1)
    )
    fflagsF32_1 := Mux(inCtrl.fpAOrBIsZeroF32_1 | inCtrl.normalResultIsZeroF32_1, fflagsNone, inCtrl.normalFFlagsF32_1)
  }

  val fpResultF16_0 = Wire(UInt(16.W))
  val fflagsF16_0 = Wire(UInt(5.W))
  fpResultF16_0 := inData.normalResultF16_0
  fflagsF16_0 := inCtrl.normalFFlagsF16_0
  when(inCtrl.hasNanF16_0) {
    fpResultF16_0 := inData.nanResultF16_0
    fflagsF16_0 := Mux(inCtrl.hasNanIsNvF16_0, fflagsNv, fflagsNone)
  }.elsewhen(inCtrl.hasInfF16_0) {
    fpResultF16_0 := Mux(inCtrl.hasInfIsNvF16_0, inData.nanResultF16_0, inData.infResultF16_0)
    fflagsF16_0 := Mux(inCtrl.hasInfIsNvF16_0, fflagsNv, fflagsNone)
  }.elsewhen(inCtrl.isOverflowF16_0) {
    fpResultF16_0 := Mux(inCtrl.isOverflowDownF16_0, inData.overflowDownResultF16_0, inData.overflowUpResultF16_0)
    fflagsF16_0 := fflagsOverflow
  }.elsewhen(inCtrl.hasZeroF16_0) {
    fpResultF16_0 := Mux(
      inCtrl.fpAOrBIsZeroF16_0,
      inData.zeroFromOperandResultF16_0,
      Mux(inCtrl.normalResultIsZeroF16_0, inData.zeroNormalResultF16_0, inData.normalResultF16_0)
    )
    fflagsF16_0 := Mux(inCtrl.fpAOrBIsZeroF16_0 | inCtrl.normalResultIsZeroF16_0, fflagsNone, inCtrl.normalFFlagsF16_0)
  }

  val fpResultF16_1 = Wire(UInt(16.W))
  val fflagsF16_1 = Wire(UInt(5.W))
  fpResultF16_1 := inData.normalResultF16_1
  fflagsF16_1 := inCtrl.normalFFlagsF16_1
  when(inCtrl.hasNanF16_1) {
    fpResultF16_1 := inData.nanResultF16_1
    fflagsF16_1 := Mux(inCtrl.hasNanIsNvF16_1, fflagsNv, fflagsNone)
  }.elsewhen(inCtrl.hasInfF16_1) {
    fpResultF16_1 := Mux(inCtrl.hasInfIsNvF16_1, inData.nanResultF16_1, inData.infResultF16_1)
    fflagsF16_1 := Mux(inCtrl.hasInfIsNvF16_1, fflagsNv, fflagsNone)
  }.elsewhen(inCtrl.isOverflowF16_1) {
    fpResultF16_1 := Mux(inCtrl.isOverflowDownF16_1, inData.overflowDownResultF16_1, inData.overflowUpResultF16_1)
    fflagsF16_1 := fflagsOverflow
  }.elsewhen(inCtrl.hasZeroF16_1) {
    fpResultF16_1 := Mux(
      inCtrl.fpAOrBIsZeroF16_1,
      inData.zeroFromOperandResultF16_1,
      Mux(inCtrl.normalResultIsZeroF16_1, inData.zeroNormalResultF16_1, inData.normalResultF16_1)
    )
    fflagsF16_1 := Mux(inCtrl.fpAOrBIsZeroF16_1 | inCtrl.normalResultIsZeroF16_1, fflagsNone, inCtrl.normalFFlagsF16_1)
  }

  val fpResultF16_2 = Wire(UInt(16.W))
  val fflagsF16_2 = Wire(UInt(5.W))
  fpResultF16_2 := inData.normalResultF16_2
  fflagsF16_2 := inCtrl.normalFFlagsF16_2
  when(inCtrl.hasNanF16_2) {
    fpResultF16_2 := inData.nanResultF16_2
    fflagsF16_2 := Mux(inCtrl.hasNanIsNvF16_2, fflagsNv, fflagsNone)
  }.elsewhen(inCtrl.hasInfF16_2) {
    fpResultF16_2 := Mux(inCtrl.hasInfIsNvF16_2, inData.nanResultF16_2, inData.infResultF16_2)
    fflagsF16_2 := Mux(inCtrl.hasInfIsNvF16_2, fflagsNv, fflagsNone)
  }.elsewhen(inCtrl.isOverflowF16_2) {
    fpResultF16_2 := Mux(inCtrl.isOverflowDownF16_2, inData.overflowDownResultF16_2, inData.overflowUpResultF16_2)
    fflagsF16_2 := fflagsOverflow
  }.elsewhen(inCtrl.hasZeroF16_2) {
    fpResultF16_2 := Mux(
      inCtrl.fpAOrBIsZeroF16_2,
      inData.zeroFromOperandResultF16_2,
      Mux(inCtrl.normalResultIsZeroF16_2, inData.zeroNormalResultF16_2, inData.normalResultF16_2)
    )
    fflagsF16_2 := Mux(inCtrl.fpAOrBIsZeroF16_2 | inCtrl.normalResultIsZeroF16_2, fflagsNone, inCtrl.normalFFlagsF16_2)
  }

  val fpResultF16_3 = Wire(UInt(16.W))
  val fflagsF16_3 = Wire(UInt(5.W))
  fpResultF16_3 := inData.normalResultF16_3
  fflagsF16_3 := inCtrl.normalFFlagsF16_3
  when(inCtrl.hasNanF16_3) {
    fpResultF16_3 := inData.nanResultF16_3
    fflagsF16_3 := Mux(inCtrl.hasNanIsNvF16_3, fflagsNv, fflagsNone)
  }.elsewhen(inCtrl.hasInfF16_3) {
    fpResultF16_3 := Mux(inCtrl.hasInfIsNvF16_3, inData.nanResultF16_3, inData.infResultF16_3)
    fflagsF16_3 := Mux(inCtrl.hasInfIsNvF16_3, fflagsNv, fflagsNone)
  }.elsewhen(inCtrl.isOverflowF16_3) {
    fpResultF16_3 := Mux(inCtrl.isOverflowDownF16_3, inData.overflowDownResultF16_3, inData.overflowUpResultF16_3)
    fflagsF16_3 := fflagsOverflow
  }.elsewhen(inCtrl.hasZeroF16_3) {
    fpResultF16_3 := Mux(
      inCtrl.fpAOrBIsZeroF16_3,
      inData.zeroFromOperandResultF16_3,
      Mux(inCtrl.normalResultIsZeroF16_3, inData.zeroNormalResultF16_3, inData.normalResultF16_3)
    )
    fflagsF16_3 := Mux(inCtrl.fpAOrBIsZeroF16_3 | inCtrl.normalResultIsZeroF16_3, fflagsNone, inCtrl.normalFFlagsF16_3)
  }

  out.fp_result := Mux1H(Seq(
    isFp64 -> fpResultF64,
    isFp32 -> Cat(fpResultF32_1, fpResultF32_0),
    isFp16 -> Cat(fpResultF16_3, fpResultF16_2, fpResultF16_1, fpResultF16_0),
  ))

  out.fflags := expandFflags(
    fflagsF64,
    fflagsF32_0,
    fflagsF32_1,
    fflagsF16_0,
    fflagsF16_1,
    fflagsF16_2,
    fflagsF16_3
  )
}
