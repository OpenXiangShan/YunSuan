package yunsuan.vector.VectorALU

import chisel3._
import chisel3.util._
import yunsuan.encoding.Opcode.FixedPointConst._
import yunsuan.vector.Common.Vxrm
import yunsuan.vector.VectorALU.VIAlu.{MaskGroup, NarrowMaskGroup}

class FixPoint extends Module with VIAlu.Config {

  val in = IO(Input(new FixPoint.In))
  val out = IO(Output(new FixPoint.Out))

  private val sel8 = in.ctrl.sel8
  private val sel16 = in.ctrl.sel16
  private val sel32 = in.ctrl.sel32
  private val sel64 = in.ctrl.sel64
  private val isSigned = in.ctrl.isSigned
  private val isSat = in.ctrl.isSat
  private val isMaxMin = in.ctrl.isMaxMin
  private val isMax = in.ctrl.isMax
  private val isNClip = in.ctrl.isNClip
  private val isTreeFirst = in.ctrl.isTreeFirst

  private val vs2 = in.data.vs2
  private val vs1 = in.data.vs1
  private val maskVs2 = in.data.maskVs2
  private val maskVs1 = in.data.maskVs1

  private val ltResult = in.adderData.lt
  private val vxsatAdder = in.adderData.vxsatAdder
  private val originAddResult = in.adderData.adder
  private val satUpOverflowUnSign = in.adderData.upOverflowUnSign
  private val satDownOverflowSign = in.adderData.downOverflowSign

  private val narrowVxsatMisc = in.miscData.narrowVxsatNClip
  private val signBitSel8  = in.miscData.signBitSel8
  private val signBitSel16 = in.miscData.signBitSel16
  private val signBitSel32 = in.miscData.signBitSel32
  private val overflowSignSel8  = in.miscData.overflowSignSel8
  private val overflowSignSel16 = in.miscData.overflowSignSel16
  private val overflowSignSel32 = in.miscData.overflowSignSel32
  private val upOverflowUnsignSel8  = in.miscData.upOverflowUnSignSel8
  private val upOverflowUnsignSel16 = in.miscData.upOverflowUnSignSel16
  private val upOverflowUnsignSel32 = in.miscData.upOverflowUnSignSel32
  private val nclipResultSel8Tmp  = in.miscData.nclipResultSel8Tmp
  private val nclipResultSel16Tmp = in.miscData.nclipResultSel16Tmp
  private val nclipResultSel32Tmp = in.miscData.nclipResultSel32Tmp

  def selectPos(in: UInt, i: Int): Bool = {
    Mux1H(Seq(
      sel8  -> in(i),
      sel16 -> in((i / 2) * 2 + 1),
      sel32 -> in((i / 4) * 4 + 3),
      sel64 -> in(7),
    ))
  }

  private val maxMinVec = Wire(Vec(8, UInt(8.W)))
  for (i <- 0 until 8) {
    val select = selectPos(ltResult, i)
    maxMinVec(i) := Mux(select ^ (isMax), vs2(i * 8 + 7, i * 8), vs1(i * 8 + 7, i * 8))
  }
  private val maxMinResult = Wire(UInt(dWidth.W))
  maxMinResult := maxMinVec.asUInt

  private val vs1Vec = (0 until maskWidth).map(i => vs1(8 * i + 7, 8 * i))
  private val vs2Vec = (0 until maskWidth).map(i => vs2(8 * i + 7, 8 * i))

  val bytesPerElem = Cat(sel64, sel32, sel16, sel8)
  val neutral = (0 until maskWidth).map { byte =>
    val isSignByte = (((byte + 1).U & (bytesPerElem - 1.U)) === 0.U)
    Mux1H(Seq(
      ( isMax &&  isSigned) -> Mux(isSignByte, "h80".U(8.W), 0.U(8.W)),
      (!isMax &&  isSigned) -> Mux(isSignByte, "h7f".U(8.W), "hff".U(8.W)),
      ( isMax && !isSigned) -> 0.U(8.W),
      (!isMax && !isSigned) -> "hff".U(8.W),
    ))
  }

  private val treeMaxMinResult = Wire(UInt(dWidth.W))
  treeMaxMinResult := Cat((0 until maskWidth).map { i =>
    Mux(!isTreeFirst || maskVs1(i) && maskVs2(i), maxMinVec(i),
      Mux1H(Seq(
        (!maskVs1(i) &&  maskVs2(i)) -> vs2Vec(i),
        ( maskVs1(i) && !maskVs2(i)) -> vs1Vec(i),
        (!maskVs1(i) && !maskVs2(i)) -> neutral(i),
      ))
    )
  }.reverse)

  private val satOverflowSign = Wire(UInt(8.W))
  satOverflowSign := Mux1H(Seq(
    sel8 -> "hff".U,
    sel16 -> "haa".U,
    sel32 -> "h88".U,
    sel64 -> "h80".U,
  ))

  private val wadd4SrcAdder = Module(new Wadd4SecondAdder)

  wadd4SrcAdder.in.sel8       := sel8
  wadd4SrcAdder.in.sel16      := sel16
  wadd4SrcAdder.in.sel32      := sel32
  wadd4SrcAdder.in.isSigned   := isSigned
  wadd4SrcAdder.in.adder      := in.adderData.adder
  wadd4SrcAdder.in.vxsatAdder := in.adderData.vxsatAdder

  private val satVec = Wire(Vec(8, UInt(8.W)))
  for (i <- 0 until 8) {
    satVec(i) := Mux(vxsatAdder(i),
      Mux(isSigned,
        Mux(satDownOverflowSign(i), Cat(satOverflowSign(i), 0.U(7.W)), Cat(~satOverflowSign(i), "h7f".U(7.W))),
        Mux(satUpOverflowUnSign(i), "hff".U, "h00".U)
      ),
      originAddResult(i * 8 + 7, i * 8))
  }
  private val satResult = Wire(UInt(dWidth.W))
  satResult := satVec.asUInt

  private val nClipResultSel8  = Wire(Vec(4, UInt(8.W)))
  private val nClipResultSel16 = Wire(Vec(2, UInt(16.W)))
  private val nClipResultSel32 = Wire(UInt((dWidth / 2).W))

  for (i <- 0 until 4) {
    nClipResultSel8(i) := Mux(isSigned,
      Mux(overflowSignSel8(i),
        Mux(signBitSel8(i), signedMin, signedMax),
        nclipResultSel8Tmp(i * 8 + 7, i * 8)),
      Mux(upOverflowUnsignSel8(i), unsignedMax, nclipResultSel8Tmp(i * 8 + 7, i * 8))
    )
  }
  for (i <- 0 until 2) {
    nClipResultSel16(i) := Mux(isSigned,
      Mux(overflowSignSel16(i),
        Mux(signBitSel16(i), Cat(signedMin, unsignedMin), Cat(signedMax, unsignedMax)),
        nclipResultSel16Tmp(i * 16 + 15, i * 16)),
      Mux(upOverflowUnsignSel16(i), Fill(2, unsignedMax), nclipResultSel16Tmp(i * 16 + 15, i * 16)))
  }
  nClipResultSel32 := Mux(isSigned,
    Mux(overflowSignSel32,
      Mux(signBitSel32, Cat(signedMin, Fill(3, unsignedMin)), Cat(signedMax, Fill(3, unsignedMax))),
      nclipResultSel32Tmp),
    Mux(upOverflowUnsignSel32, Fill(4, unsignedMax), nclipResultSel32Tmp))

  private val nClipResult = Wire(UInt((dWidth / 2).W))
  nClipResult := Mux1H(Seq(
    sel8  -> nClipResultSel8.asUInt,
    sel16 -> nClipResultSel16.asUInt,
    sel32 -> nClipResultSel32,
  ))

  out.res.vd := Mux1H(Seq(
    isSat -> satResult,
    isMaxMin -> treeMaxMinResult,
    in.ctrl.isWadd4Tree -> wadd4SrcAdder.out.vd,
  ))

  out.res.narrowVd := nClipResult
  out.res.vxsat := Mux(isSat, vxsatAdder, 0.U)
  out.res.narrowVxsat := narrowVxsatMisc
}

class Wadd4SecondAdder extends VIAlu.Module {
  class Input extends VIAlu.Bundle {
    val sel8 = Bool()
    val sel16 = Bool()
    val sel32 = Bool()
    val isSigned = Bool()
    val adder = WidthData()
    val vxsatAdder = MaskData()
  }

  class Output extends VIAlu.Bundle {
    val vd = WidthData()
  }

  val in = IO(Input(new Input))
  val out = IO(Output(new Output))

  // Four shared byte adders. Carry crosses bytes only within a narrow element.
  private val sums = Wire(Vec(4, UInt(9.W)))
  for (i <- 0 until 4) {
    val carryIn = if (i == 0) false.B
      else if (i == 2) in.sel32 && sums(i - 1)(8)
      else (in.sel16 || in.sel32) && sums(i - 1)(8)
    sums(i) := (in.adder(8 * i + 7, 8 * i) +& in.adder(8 * i + 39, 8 * i + 32)) + carryIn
  }

  // A signed pair's true sign is its truncated sign XOR signed overflow.
  // For unsigned pairs, overflow is the missing carry above the narrow sum.
  private def highPart(sign: Bool, overflow: Bool): UInt =
    Mux(in.isSigned, Fill(3, sign ^ overflow), Cat(0.U(2.W), overflow))

  private def result(width: Int): UInt = {
    val bytes = width / 8
    Cat((0 until 32 / width).reverse.map { i =>
      val last = (i + 1) * bytes - 1
      val high = highPart(in.adder(8 * last + 7), in.vxsatAdder(last)) +
        highPart(in.adder(8 * last + 39), in.vxsatAdder(last + 4)) + sums(last)(8)
      val extendedHigh = Cat(Fill(width - 3, in.isSigned && high(2)), high)
      val low = Cat((0 until bytes).reverse.map(b => sums(i * bytes + b)(7, 0)))
      Cat(extendedHigh, low)
    })
  }

  out.vd := Mux1H(Seq(in.sel8 -> result(8), in.sel16 -> result(16), in.sel32 -> result(32)))
}
object FixPoint {
  class In extends VIAlu.Bundle {
    val vxrm = Vxrm()
    val ctrl = new InCtrl
    val data = new InData
    val adderData = new Adder.OutMidRes
    val miscData = new Misc.OutMidRes
  }

  class Out extends VIAlu.Bundle {
    val res = new OutRes
  }

  class InCtrl extends VIAlu.Bundle {
    val sel8 = Bool()
    val sel16 = Bool()
    val sel32 = Bool()
    val sel64 = Bool()
    val isSigned = Bool()
    val isSat = Bool()
    val isMaxMin = Bool()
    val isMax = Bool()
    val isNClip = Bool()
    val isTreeFirst = Bool()
    val isWadd4Tree = Bool()
  }

  class InData extends VIAlu.Bundle {
    val vs2 = WidthData()
    val vs1 = WidthData()
    val maskVs2 = MaskData()
    val maskVs1 = MaskData()
  }

  class OutRes extends VIAlu.Bundle {
    val vd = WidthData()
    val narrowVd = NarrowData()
    val vxsat = MaskData()
    val narrowVxsat = NarrowMask()
  }
}
