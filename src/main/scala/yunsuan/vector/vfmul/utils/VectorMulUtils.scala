package yunsuan.vector.vfmul.utils

import chisel3._
import chisel3.util._
import yunsuan.fpu.fmul.utils.FMULToFADDCtrlBundle
import yunsuan.vector.Common.{Fflags, VSew}
import yunsuan.vector.vfmul.VFMul2VFALUCtrlBundle

trait VFParas {
  val fpMaxWidth: Int = 64
  val byte: Int = 8

  val fpTypeSeq = Seq("fp16", "fp32", "fp64")

  private val floatWidthSeq = Seq(16, 32, 64)
  private val exponentWidthSeq  = Seq(5, 8, 11)
  private val shiftBitsWidthSeq = Seq(4, 5, 6)
  val rmWidth = 3
  val flagsWidth = 5
  val signWidth = 1

  def getIdx(implicit fpType: String) = {
    require(fpTypeSeq.contains(fpType))
    fpTypeSeq.indexOf(fpType)
  }
  def floatWidth(implicit fpType: String) = floatWidthSeq(getIdx)
  def exponentWidth(implicit fpType: String) = exponentWidthSeq(getIdx)
  def significandWidth(implicit fpType: String) = floatWidth - signWidth - exponentWidth // 10, 23, 52
  def quiteBit(implicit fpType: String) = significandWidth(fpType) - 1 // 9, 22, 51
  def decimalWidth(implicit fpType: String) = significandWidth + 1 // 11, 24, 53
  def exponentBias(implicit fpType: String) = (1 << (exponentWidth - 1)) - 1
  def shiftBitsWidth(implicit fpType: String) = shiftBitsWidthSeq(getIdx)

  def fpAAppendWidth(implicit fpType: String) = decimalWidth
  val fpAAppendMaxWidth = fpAAppendWidth("fp64")
  def elemNum(implicit fpType: String) = fpMaxWidth / floatWidth

  def getNanOfFp(fpNum: UInt)(implicit fpType: String) = fpNum(floatWidth - signWidth - 1, significandWidth).andR && fpNum(significandWidth - 1, 0).orR
  def getSign(fpNum: UInt)(implicit fpType: String) = fpNum(floatWidth - 1)
  def getAbs(fpNum: UInt)(implicit fpType: String) = fpNum(floatWidth - 2, 0)

  def canonicalNan(implicit fpType: String) = Cat(false.B, Fill(exponentWidth + 1, true.B), Fill(significandWidth - 1, false.B))
}

class VFBundle extends Bundle with VFParas
class VFModule extends Module with VFParas

object VFAlgoUtils extends VFParas {
  private val fpWidthList = List(16, 32, 64)

  private def srcWidthCheck(src: UInt): Unit = {
    val width = src.getWidth
    require(fpWidthList.contains(width), s"Unsupported float-point width: $width. Supported widths are: ${fpWidthList.mkString(", ")}")
  }

  def fpNeg(fpElem: UInt): UInt = {
    srcWidthCheck(fpElem)

    Cat(~fpElem.head(1), fpElem.tail(1))
  }

  def split2Vec(src: UInt, elemNum: Int): Vec[UInt] = {
    srcWidthCheck(src)
    require(Seq(2, 4, 8).contains(elemNum), s"Unsupported split number for 64-bits UInt: $elemNum")
    val elemWidth = src.getWidth / elemNum

    VecInit(Seq.tabulate(elemNum)(i => src(elemWidth * (i + 1) - 1, elemWidth * i)))
  }

  def split2VecManual(src: UInt, elemNum: Int, elemWidth: Int): Vec[UInt] = {
    require(src.getWidth >= elemWidth * elemNum)

    VecInit(Seq.tabulate(elemNum)(i => src(elemWidth * (i + 1) - 1, elemWidth * i)))
  }

  def getVfmacCtrlFromFmul(outToFADDs: Vec[FMULToFADDCtrlBundle]): VFMul2VFALUCtrlBundle = {
    val out2Vfalu = Wire(new VFMul2VFALUCtrlBundle)

    out2Vfalu.isFMA     := outToFADDs.head.isFMA // make sure valid control signals are put at outToFADDs(0)
    out2Vfalu.rm        := outToFADDs.head.rm
    out2Vfalu.sticky    := VecInit(outToFADDs.map(_.sticky))
    out2Vfalu.resIsNaN  := VecInit(outToFADDs.map(_.resIsNaN))
    out2Vfalu.resIsZero := VecInit(outToFADDs.map(_.resIsZero))
    out2Vfalu.resIsInf  := VecInit(outToFADDs.map(_.resIsInf))
    out2Vfalu.flagsNV   := VecInit(outToFADDs.map(_.flagsNV))
    out2Vfalu.flagsOF   := VecInit(outToFADDs.map(_.flagsOF))

    out2Vfalu
  }

  def getFpAFromResults(results: Seq[UInt])(implicit fpType: String): UInt = {
    require(results.length == elemNum)
    require(results.head.getWidth == floatWidth + decimalWidth)
    val fpAs = results.map(_.head(floatWidth))
    Cat(fpAs.reverse)
  }

  def getFpAAppendFromResults(results: Seq[UInt])(implicit fpType: String): UInt = {
    require(results.length == elemNum)
    require(results.head.getWidth == floatWidth + decimalWidth)
    val fpAAppends = results.map(_.tail(floatWidth))
    Cat(Fill(fpAAppendMaxWidth - fpAAppendWidth * elemNum, 1.U), Cat(fpAAppends.reverse))
  }

  def expandFflags(fflagsFp16: Seq[UInt],
                   fflagsFp32: Seq[UInt],
                   fflagsFp64: UInt,
                   isFp16: Bool,
                   isFp32: Bool,
                   isFp64: Bool
                  ): Vec[UInt] = {
    require(fflagsFp16.head.getWidth == flagsWidth)
    require(fflagsFp32.head.getWidth == flagsWidth)
    require(fflagsFp64.getWidth == flagsWidth)
    require(fflagsFp16.length == elemNum("fp16"))
    require(fflagsFp32.length == elemNum("fp32"))

    val fflagsFp16Idx = Seq(
      0, 0,
      1, 1,
      2, 2,
      3, 3
    )
    val fflagsFp32Idx = Seq(
      0, 0, 0, 0,
      1, 1, 1, 1
    )

    VecInit(Seq.tabulate(fpMaxWidth / byte) { i =>
      Mux1H(Seq(
        isFp16 -> fflagsFp16(fflagsFp16Idx(i)),
        isFp32 -> fflagsFp32(fflagsFp32Idx(i)),
        isFp64 -> fflagsFp64,
      ))
    })
  }

  def expandFflags(fflagsEach16bit: Seq[UInt],
                   isFp16: Bool,
                   isFp32: Bool,
                   isFp64: Bool
                  ): Vec[UInt] = {
    expandFflags(fflagsEach16bit, fflagsEach16bit.take(2), fflagsEach16bit.head, isFp16, isFp32, isFp64)
  }
}
