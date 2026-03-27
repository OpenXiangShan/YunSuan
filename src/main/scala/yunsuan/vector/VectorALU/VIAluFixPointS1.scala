package yunsuan.vector.VectorALU

import chisel3._
import chisel3.util._
import yunsuan.encoding.Opcode.FixedPointConst._

class VIAluFixPointS1Input(xlen: Int) extends Bundle {
  val adderToS1 = new VIAluAdderToS1(xlen)
  val miscToS1 = new VIAluMiscToS1(xlen)
  val sel8 = Bool()
  val sel16 = Bool()
  val sel32 = Bool()
  val sel64 = Bool()
  val vs2 = UInt(xlen.W)
  val vs1 = UInt(xlen.W)
  val isSigned = Bool()
  val isMisc = Bool()
}

class VIAluFixPointS1Output(xlen: Int) extends Bundle {
  val vd = UInt(xlen.W)
  val addCarryCmpMask = UInt(8.W)
  val narrowVd = UInt((xlen / 2).W)
  val vxsat = UInt(8.W)
}

class VIAluFixPointS1(xlen: Int = 64) extends Module {
  val io = IO(new Bundle {
    val in = Input(new VIAluFixPointS1Input(xlen))
    val out = Output(new VIAluFixPointS1Output(xlen))
  })
  private val sel8 = io.in.sel8
  private val sel16 = io.in.sel16
  private val sel32 = io.in.sel32
  private val sel64 = io.in.sel64
  private val vs2 = io.in.vs2
  private val vs1 = io.in.vs1
  private val isSigned = io.in.isSigned
  private val isMisc = io.in.isMisc

  private val vdAdder = io.in.adderToS1.vd
  private val addCarryCmpMask = io.in.adderToS1.addCarryCmpMask
  private val vxsatAdder = io.in.adderToS1.vxsat
  private val isSat = io.in.adderToS1.isSat
  private val isAvg = io.in.adderToS1.isAvg
  private val isMaxMin = io.in.adderToS1.isMaxMin
  private val isMax = io.in.adderToS1.isMax
  private val ltResult = io.in.adderToS1.ltResult
  private val sat = io.in.adderToS1.sat
  private val originAddResult = io.in.adderToS1.originAddResult
  private val satUpOverflowUnSign = io.in.adderToS1.upOverflowUnSign
  private val satDownOverflowSign = io.in.adderToS1.downOverflowSign

  private val vdMisc = io.in.miscToS1.vd
  private val narrowVd = io.in.miscToS1.narrowVd
  private val vxsatMisc = io.in.miscToS1.vxsat
  private val isVcpop = io.in.miscToS1.isVcpop
  private val popResult = io.in.miscToS1.popResult
  private val isNClip = io.in.miscToS1.isNClip
  private val signBitSel8  = io.in.miscToS1.signBitSel8
  private val signBitSel16 = io.in.miscToS1.signBitSel16
  private val signBitSel32 = io.in.miscToS1.signBitSel32
  private val overflowSignSel8  = io.in.miscToS1.overflowSignSel8
  private val overflowSignSel16 = io.in.miscToS1.overflowSignSel16
  private val overflowSignSel32 = io.in.miscToS1.overflowSignSel32
  private val upOverflowUnsignSel8  = io.in.miscToS1.upOverflowUnSignSel8
  private val upOverflowUnsignSel16 = io.in.miscToS1.upOverflowUnSignSel16
  private val upOverflowUnsignSel32 = io.in.miscToS1.upOverflowUnSignSel32
  private val nclipResultSel8Tmp  = io.in.miscToS1.nclipResultSel8Tmp
  private val nclipResultSel16Tmp = io.in.miscToS1.nclipResultSel16Tmp
  private val nclipResultSel32Tmp = io.in.miscToS1.nclipResultSel32Tmp

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
    maxMinVec(i) := Mux(select ^ isMax, vs2(i * 8 + 7, i * 8), vs1(i * 8 + 7, i * 8))
  }
  private val maxMinResult = Wire(UInt(xlen.W))
  maxMinResult := maxMinVec.asUInt

  private val satOverflowSign = Wire(UInt(8.W))
  satOverflowSign := Mux1H(Seq(
    sel8 -> "hff".U,
    sel16 -> "haa".U,
    sel32 -> "h88".U,
    sel64 -> "h80".U,
  ))

  private val satVec = Wire(Vec(8, UInt(8.W)))
  for (i <- 0 until 8) {
    satVec(i) := Mux(sat(i),
      Mux(isSigned,
        Mux(satDownOverflowSign(i), Cat(satOverflowSign(i), 0.U(7.W)), Cat(~satOverflowSign(i), "h7f".U(7.W))),
        Mux(satUpOverflowUnSign(i), "hff".U, "h00".U)),
      originAddResult(i * 8 + 7, i * 8))
  }
  private val satResult = Wire(UInt(xlen.W))
  satResult := satVec.asUInt

  private val nClipResultSel8  = Wire(Vec(4, UInt(8.W)))
  private val nClipResultSel16 = Wire(Vec(2, UInt(16.W)))
  private val nClipResultSel32 = Wire(UInt((xlen / 2).W))

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

  private val nClipResult = Wire(UInt((xlen / 2).W))
  nClipResult := Mux1H(Seq(
    sel8  -> nClipResultSel8.asUInt,
    sel16 -> nClipResultSel16.asUInt,
    sel32 -> nClipResultSel32,
  ))

  io.out.vd := Mux(isMisc,
    Mux(isVcpop, popResult, vdMisc),
    MuxCase(originAddResult, Seq(
      isSat -> satResult,
      isAvg -> vdAdder,
      isMaxMin -> maxMinResult)))
  io.out.narrowVd := Mux(isNClip, nClipResult, narrowVd)
  io.out.addCarryCmpMask := addCarryCmpMask
  io.out.vxsat := Mux(isMisc, Mux(isNClip, vxsatMisc, 0.U), Mux(isSat, sat, vxsatAdder))
}
