package yunsuan.fpu.fmul.utils
import chisel3._
import chisel3.util._
import yunsuan.fpu._
import yunsuan.fpu.fmul.utils.{FMULS0ToS1Bundle, FMULS1ToS2Bundle, FMULToFADDCtrlBundle}

class FMULS1(val totalWidth: Int, val version: Int = 0) extends Module with FloatParams {
  override def desiredName = (this.getClass.getName + s"_$version").split("\\.").last
  val io = IO(new Bundle() {
    val inFromS0 = Input(new FMULS0ToS1Bundle(totalWidth))
    val outToS2 = Output(new FMULS1ToS2Bundle(totalWidth))
    val outResToFADD = Output(UInt((totalWidth + decimalWidth).W))
    val outCtrlToFADD = Output(new FMULToFADDCtrlBundle(totalWidth))
  })
  val isLeftShift = io.inFromS0.needLeftShift
  val fracMul = io.inFromS0.out_sum + io.inFromS0.out_car
  val leftShiftOverFlowMask = (((1.U << (2 * decimalWidth - 1)) >> io.inFromS0.shiftBits) >> (io.inFromS0.expc === 0.U)).asUInt
  val fracOverFlowLeftShift = (fracMul & leftShiftOverFlowMask)(2 * decimalWidth - 1, significandWidth).orR && !io.inFromS0.leftShiftNoOverFlow
  val fracOverFlowRightShift = fracMul.head(1).asBool
  val fracOverFlow = isLeftShift && fracOverFlowLeftShift || !isLeftShift && (io.inFromS0.shiftBits === 1.U) && fracOverFlowRightShift
  val fracMulLeftShift = Wire(UInt((2 * decimalWidth).W))
  val fracMulRightShift = Wire(UInt((2 * decimalWidth).W))
  val rightShiftBits = Mux(isLeftShift, 0.U, io.inFromS0.shiftBits)
  val rightShiftStickyMask = (Fill(totalWidth - 1, 1.U) << rightShiftBits)(2 * (totalWidth - 1) - 1, totalWidth - 1)
  val rightShiftSticky = (rightShiftStickyMask & fracMul(totalWidth - 2, 0)).orR
  io.outToS2.sticky := rightShiftSticky
  fracMulLeftShift := fracMul << io.inFromS0.shiftBits
  fracMulRightShift := fracMul >> io.inFromS0.shiftBits
  val outFracRaw = Mux(isLeftShift, fracMulLeftShift, fracMulRightShift)
  val outFrac = Mux(isLeftShift && fracOverFlow && (io.inFromS0.expc =/= 0.U), outFracRaw(2 * significandWidth, 0), Cat(outFracRaw(2 * significandWidth - 1, 0), 0.U))
  val exp = io.inFromS0.expc
  val expAdd1 = io.inFromS0.expc + 1.U
  val expIsRoundUp = fracOverFlow
  //  || exp(exponentWidth - 1, 1).andR && fracOverFlow locgic is useless
  val expIsOverFlow = exp.andR || exp(exponentWidth - 1, 1).andR && fracOverFlow
  val outExp = Mux(expIsOverFlow, Fill(exponentWidth, 1.U), Mux(expIsRoundUp, expAdd1, exp))
  io.outToS2.resForRounding := Cat(io.inFromS0.sign, outExp, outFrac)
  io.outToS2.rm := io.inFromS0.rm
  io.outToS2.resIsNAN := io.inFromS0.resIsNAN
  io.outToS2.resIsZero := io.inFromS0.resIsZero
  io.outToS2.resIsInf := io.inFromS0.resIsInf
  io.outToS2.flagsNV := io.inFromS0.flagsNV
  io.outResToFADD := io.outToS2.resForRounding
  io.outCtrlToFADD.isFMA := io.inFromS0.isFMA
  io.outCtrlToFADD.rm := io.inFromS0.rm
  io.outCtrlToFADD.sticky := rightShiftSticky
  io.outCtrlToFADD.resIsNaN := io.inFromS0.resIsNAN
  io.outCtrlToFADD.resIsZero := io.inFromS0.resIsZero
  io.outCtrlToFADD.resIsInf := io.inFromS0.resIsInf
  io.outCtrlToFADD.flagsNV := io.inFromS0.flagsNV
  io.outCtrlToFADD.flagsOF := io.inFromS0.flagsOFToFADD || exp.andR && fracOverFlow

  dontTouch(fracMul)
  dontTouch(leftShiftOverFlowMask)
  dontTouch(fracOverFlow)
  dontTouch(fracMulLeftShift)
  dontTouch(fracMulRightShift)
  dontTouch(outFracRaw)
  dontTouch(outFrac)
  dontTouch(exp)
  dontTouch(expAdd1)
  dontTouch(expIsRoundUp)
  dontTouch(expIsOverFlow)
}
