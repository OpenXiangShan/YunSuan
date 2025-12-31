package yunsuan.vector.VectorALU

import chisel3._
import chisel3.util._
import yunsuan.encoding.Opcode.FixedPointRoundingMode._
import yunsuan.encoding.Opcode.Opcodes.VIAluOpcode
import yunsuan.util._
import yunsuan.vector.UIntSplit

import scala.math.pow

class VIAluMiscCtrl extends Bundle {
  val sel8 = Bool()
  val sel16 = Bool()
  val sel32 = Bool()
  val sel64 = Bool()
  val isExt = ValidIO(new ExtInfo)
  val widenVs2 = Bool()
  val widen = Bool()
  val isNarrow = Bool()
  val isSigned = Bool()
}

class VIAluMiscData(xlen: Int) extends Bundle {
  val vs2 = UInt(xlen.W)
  val vs1 = UInt(xlen.W)
  val vs2Widen = UInt(xlen.W)
  val vs1Widen = UInt(xlen.W)
  val vxrm = UInt(2.W)
}

class VIAluMiscToS1(xlen: Int) extends Bundle {
  val vd = UInt(xlen.W)
  val narrowVd = UInt((xlen / 2).W)
  val vxsat = UInt(8.W)
  val isVcpop = Bool()
  val popResult = UInt(xlen.W)
  val isNClip = Bool()
  val signBitSel8  = UInt(4.W)
  val signBitSel16 = UInt(2.W)
  val signBitSel32 = Bool()
  val overflowSignSel8 = UInt(4.W)
  val overflowSignSel16 = UInt(2.W)
  val overflowSignSel32 = Bool()
  val upOverflowUnSignSel8 = UInt(4.W)
  val upOverflowUnSignSel16 = UInt(2.W)
  val upOverflowUnSignSel32 = Bool()
  val nclipResultSel8Tmp = UInt((xlen / 2).W)
  val nclipResultSel16Tmp = UInt((xlen / 2).W)
  val nclipResultSel32Tmp = UInt((xlen / 2).W)
}


class VIAluMiscInput(xlen: Int) extends Bundle {
  val opcode = VIAluOpcode()
  val ctrl = new VIAluMiscCtrl
  val data = new VIAluMiscData(xlen)
}

class VIAluMiscOutput(xlen: Int) extends Bundle {
  val toS1 = new VIAluMiscToS1(xlen)
}

class VIAluMisc(xlen: Int = 64) extends Module {
  import VIAluOpcode._

  val io = IO(new Bundle {
    val in = Input(new VIAluMiscInput(xlen))
    val out = Output(new VIAluMiscOutput(xlen))
  })

  private val isVector: Boolean = true

  private implicit val opcode: UInt = io.in.opcode
  private val sel8  = io.in.ctrl.sel8
  private val sel16 = io.in.ctrl.sel16
  private val sel32 = io.in.ctrl.sel32
  private val sel64 = io.in.ctrl.sel64
  private val isExt = io.in.ctrl.isExt.valid
  private val isVf2 = io.in.ctrl.isExt.bits.isVf2
  private val isVf4 = io.in.ctrl.isExt.bits.isVf4
  private val isVf8 = io.in.ctrl.isExt.bits.isVf8
  private val widenVs2 = io.in.ctrl.widenVs2
  private val widen = io.in.ctrl.widen
  private val isNarrow = io.in.ctrl.isNarrow
  private val isSigned = io.in.ctrl.isSigned
  private val vs2 = io.in.data.vs2
  private val vs1 = io.in.data.vs1
  private val vs2Widen = io.in.data.vs2Widen
  private val vs1Widen = io.in.data.vs1Widen
  private val rm = io.in.data.vxrm

  private val ext = Wire(Vec(8, UInt(8.W)))
  ext(0) := vs2Widen(7, 0)
  ext(1) := Mux(isVf2 & !sel16 | isVf4 & sel64, vs2Widen(15, 8), Fill(8, isSigned & vs2Widen(7)))
  ext(2) := Mux1H(Seq(
    (isVf2 & sel16) -> vs2Widen(15, 8),
    (isVf2 & sel32 | isVf4 & sel64) -> Fill(8, isSigned & vs2Widen(15)),
    (isVf2 & sel64) -> vs2Widen(23, 16),
    (isVf4 & sel32 | isVf8) -> Fill(8, isSigned & vs2Widen(7)),
  ))
  ext(3) := Mux1H(Seq(
    (isVf2 & !sel64 | isVf4 & sel64) -> Fill(8, isSigned & vs2Widen(15)),
    (isVf2 & sel64) -> vs2Widen(31, 24),
    (isVf4 & sel32 | isVf8) -> Fill(8, isSigned & vs2Widen(7)),
  ))
  ext(4) := Mux1H(Seq(
    isVf2 -> Mux(sel64, Fill(8, isSigned & vs2Widen(31)), vs2Widen(23, 16)),
    isVf4 -> Mux1H(Seq(
      sel32 -> vs2Widen(15, 8),
      sel64 -> Fill(8, isSigned & vs2Widen(15)),
    )),
    isVf8 -> Fill(8, isSigned & vs2Widen(7)),
  ))
  ext(5) := Mux1H(Seq(
    isVf2 -> Mux1H(Seq(
      sel16 -> Fill(8, isSigned & vs2Widen(23)),
      sel32 -> vs2Widen(31, 24),
      sel64 -> Fill(8, isSigned & vs2Widen(31)),
    )),
    isVf4 -> Fill(8, isSigned & vs2Widen(15)),
    isVf8 -> Fill(8, isSigned & vs2Widen(7)),
  ))
  ext(6) := Mux1H(Seq(
    isVf2 -> Mux(sel16, vs2Widen(31, 24), Fill(8, isSigned & vs2Widen(31))),
    isVf4 -> Fill(8, isSigned & vs2Widen(15)),
    isVf8 -> Fill(8, isSigned & vs2Widen(7)),
  ))
  ext(7) := Mux1H(Seq(
    isVf2 -> Fill(8, isSigned & vs2Widen(31)),
    isVf4 -> Fill(8, isSigned & vs2Widen(15)),
    isVf8 -> Fill(8, isSigned & vs2Widen(7)),
  ))

  private val extResult = Wire(UInt(xlen.W))
  extResult := ext.asUInt

  private val andResult = vs2 & vs1
  private val orResult  = vs2 | vs1
  private val xorResult = vs2 ^ vs1

  private val bitLogicResult = Wire(UInt(xlen.W))
  bitLogicResult := Mux1H(Seq(
    isVand -> andResult,
    isVor  -> orResult,
    isVxor -> xorResult,
    isVnand -> ~andResult,
    isVandn -> (vs2 & (~vs1).asUInt),
    isVnor -> ~orResult,
    isVorn -> (vs2 | (~vs1).asUInt),
    isVxnor -> ~xorResult
  ))

  private val vs2WidenVec = Wire(Vec(8, UInt(8.W)))
  private val vs1WidenVec = Wire(Vec(8, UInt(8.W)))
  private val vs2WidenWire = Wire(UInt(xlen.W))
  private val vs1WidenWire = Wire(UInt(xlen.W))

  vs2WidenVec(0) := vs2Widen(7, 0)
  vs2WidenVec(1) := Mux(sel8, 0.U, vs2Widen(15, 8))
  vs2WidenVec(2) := Mux1H(Seq(
    sel8 -> vs2Widen(15, 8),
    sel32 -> vs2Widen(23, 16),
  ))
  vs2WidenVec(3) := Mux(sel32, vs2Widen(31, 24), 0.U)
  vs2WidenVec(4) := Mux(sel32, 0.U, vs2Widen(23, 16))
  vs2WidenVec(5) := Mux(sel16, vs2Widen(31, 24), 0.U)
  vs2WidenVec(6) := Mux(sel8, vs2Widen(31, 24), 0.U)
  vs2WidenVec(7) := 0.U

  vs1WidenVec(0) := vs1Widen(7, 0)
  vs1WidenVec(1) := 0.U
  vs1WidenVec(2) := Mux(sel8, vs1Widen(15, 8), 0.U)
  vs1WidenVec(3) := 0.U
  vs1WidenVec(4) := Mux(sel32, 0.U, vs1Widen(23, 16))
  vs1WidenVec(5) := 0.U
  vs1WidenVec(6) := Mux(sel8, vs1Widen(31, 24), 0.U)
  vs1WidenVec(7) := 0.U

  vs2WidenWire := vs2WidenVec.asUInt
  vs1WidenWire := vs1WidenVec.asUInt

  private val vs2RightShift = vs2
  private val vs1RightShift = Mux(isNarrow, Mux1H(Seq(
    sel8  -> Cat(0.U(8.W), vs1Widen(31, 24), 0.U(8.W), vs1Widen(23, 16), 0.U(8.W), vs1Widen(15, 8), 0.U(8.W), vs1Widen(7, 0)),
    sel16 -> Cat(0.U(16.W), vs1Widen(31, 16), 0.U(16.W), vs1Widen(15, 0)),
    sel32 -> Cat(0.U(32.W), vs1Widen(31, 0)),
  )), vs1)

  private val vs2LeftShift = Mux(widenVs2, vs2WidenWire, vs2)
  private val vs1LeftShift = Mux(widen, vs1WidenWire, vs1)

  def leftShiftOneElement(data: UInt, shift: UInt, sew: Int): UInt = {
    sew match {
      case 8  => doShiftLeft(data, shift(2, 0)).asUInt
      case 16 => doShiftLeft(data, shift(3, 0)).asUInt
      case 32 => doShiftLeft(data, shift(4, 0)).asUInt
      case 64 => doShiftLeft(data, shift(5, 0)).asUInt
    }
  }

  def leftShift(sew: Int): UInt = {
    Cat(UIntSplit(vs2LeftShift, sew).zip(UIntSplit(vs1LeftShift, sew)).map { case (v2, v1) =>
      leftShiftOneElement(v2, v1, sew)
    }.reverse)
  }


  def dynamicShift(data: UInt, shift: UInt, shiftRound: UInt): (UInt, UInt) = {
    val length = shift.getWidth
    val amount = pow(2, length - 1).intValue
    val tmp = data.getWidth - amount
    val shifted = Mux(shift.head(1).asBool, Cat(Fill(amount, isSigned & data.head(1).asBool), data.head(tmp)), data)
    val shiftData = Mux(shift.head(1).asBool, Cat(data.tail(tmp), shiftRound.head(tmp)), shiftRound)
    if (length == 1) (shifted, shiftData) else dynamicShift(shifted, shift.tail(1), shiftData)
  }

  // shift sew data
  def rightShiftOneElement(data: UInt, shift: UInt, sew: Int): (UInt, UInt) = {
    val shiftData = WireInit(0.U(data.getWidth.W))
    sew match {
      case 8  => dynamicShift(data, shift(2, 0), shiftData)
      case 16 => dynamicShift(data, shift(3, 0), shiftData)
      case 32 => dynamicShift(data, shift(4, 0), shiftData)
      case 64 => dynamicShift(data, shift(5, 0), shiftData)
    }
  }

  def rightShift(sew: Int): (UInt, UInt) = {
    val shift = UIntSplit(vs2RightShift, sew).zip(UIntSplit(vs1RightShift, sew)).map { case (v2, v1) =>
      rightShiftOneElement(v2, v1, sew)
    }
    val shifted = Cat(shift.map(_._1).reverse)
    val shiftData = Cat(shift.map(_._2).reverse)
    (shifted, shiftData)
  }

  private val leftShift8  = leftShift(8)
  private val leftShift16 = leftShift(16)
  private val leftShift32 = leftShift(32)
  private val leftShift64 = leftShift(64)

  private val (rightShift8, rightShiftData8)   = rightShift(8)
  private val (rightShift16, rightShiftData16) = rightShift(16)
  private val (rightShift32, rightShiftData32) = rightShift(32)
  private val (rightShift64, rightShiftData64) = rightShift(64)

  private val leftShiftResult = Wire(UInt(xlen.W))
  private val rightShiftResult = Wire(UInt(xlen.W))

  leftShiftResult := Mux1H(Seq(
    sel8  -> leftShift8,
    sel16 -> leftShift16,
    sel32 -> leftShift32,
    sel64 -> leftShift64,
  ))
  rightShiftResult := Mux1H(Seq(
    sel8  -> rightShift8,
    sel16 -> rightShift16,
    sel32 -> rightShift32,
    sel64 -> rightShift64,
  ))

  def genRound(in: UInt, rIn: UInt): UInt = {
    val (g, r, s) = (in(0), rIn.head(1).asBool, rIn.tail(1))
    val roundUp = MuxLookup(
      rm,
      false.B
    )(Seq(
      RNU -> r,
      RNE -> (r & (s.orR | g)),
      RDN -> false.B,
      ROD -> (!g & rIn.orR),
    ))
    val incOne = Wire(UInt((in.getWidth + 1).W))
    incOne := in + 1.U
    val out = Mux(roundUp, incOne, in)
    out.tail(1)
  }

  private val scalResultSel8 = Wire(Vec(8, UInt(8.W)))
  private val scalResultSel16 = Wire(Vec(4, UInt(16.W)))
  private val scalResultSel32 = Wire(Vec(2, UInt(32.W)))
  private val scalResultSel64 = Wire(UInt(xlen.W))

  for (i <- 0 until 8) {
    scalResultSel8(i) := genRound(rightShift8(i * 8 + 7, i * 8), rightShiftData8(i * 8 + 7, i * 8))
  }
  for (i <- 0 until 4) {
    scalResultSel16(i) := genRound(rightShift16(i * 16 + 15, i * 16), rightShiftData16(i * 16 + 15, i * 16))
  }
  for (i <- 0 until 2) {
    scalResultSel32(i) := genRound(rightShift32(i * 32 + 31, i * 32), rightShiftData32(i * 32 + 31, i * 32))
  }
  scalResultSel64 := genRound(rightShift64, rightShiftData64)

  private val shiftResult = Wire(UInt(xlen.W))
  shiftResult := Mux(isLeftShiftLogic, leftShiftResult, rightShiftResult)

  private val narrowResult = Wire(UInt((xlen / 2).W))
  narrowResult := Mux1H(Seq(
    sel8  -> Cat(rightShift16(55, 48), rightShift16(39, 32), rightShift16(23, 16), rightShift16(7, 0)),
    sel16 -> Cat(rightShift32(47, 32), rightShift32(15, 0)),
    sel32 -> rightShift64,
  ))

  private val scalResult = Wire(UInt(xlen.W))
  scalResult := Mux1H(Seq(
    sel8  -> scalResultSel8.asUInt,
    sel16 -> scalResultSel16.asUInt,
    sel32 -> scalResultSel32.asUInt,
    sel64 -> scalResultSel64,
  ))

  private val nClipSatSel8  = Wire(Vec(4, Bool()))
  private val nClipSatSel16 = Wire(Vec(2, Bool()))
  private val nClipSatSel32 = Wire(Bool())
  private val signBitSel8 = Wire(Vec(4, Bool()))
  private val overflowSignSel8 = Wire(Vec(4, Bool()))
  private val upOverflowUnSignSel8 = Wire(Vec(4, Bool()))
  private val signBitSel16 = Wire(Vec(2, Bool()))
  private val overflowSignSel16 = Wire(Vec(2, Bool()))
  private val upOverflowUnSignSel16 = Wire(Vec(2, Bool()))
  private val nclipResultsel8Tmp  = Wire(Vec(4, UInt(8.W)))
  private val nclipResultsel16Tmp = Wire(Vec(2, UInt(16.W)))
  for (i <- 0 until 4) {
    upOverflowUnSignSel8(i) := scalResultSel16(i).head(8).orR
    signBitSel8(i) := scalResultSel16(i).head(1).asBool
    val upOverflowSign = scalResultSel16(i).tail(1).head(8).orR
    val downOverflowSign = !scalResultSel16(i).tail(1).head(8).andR
    overflowSignSel8(i) := Mux(signBitSel8(i), downOverflowSign, upOverflowSign)
    nClipSatSel8(i) := Mux(isSigned, overflowSignSel8(i), upOverflowUnSignSel8(i))
    nclipResultsel8Tmp(i) := scalResultSel16(i).tail(8)
  }
  for (i <- 0 until 2) {
    upOverflowUnSignSel16(i) := scalResultSel32(i).head(16).orR
    signBitSel16(i) := scalResultSel32(i).head(1).asBool
    val upOverflowSign = scalResultSel32(i).tail(1).head(16).orR
    val downOverflowSign = !scalResultSel32(i).tail(1).head(16).andR
    overflowSignSel16(i) := Mux(signBitSel16(i), downOverflowSign, upOverflowSign)
    nClipSatSel16(i) := Mux(isSigned, overflowSignSel16(i), upOverflowUnSignSel16(i))
    nclipResultsel16Tmp(i) := scalResultSel32(i).tail(16)
  }
  private val upOverflowUnSign = scalResultSel64.head(32).orR
  private val signBit = scalResultSel64.head(1).asBool
  private val upOverflowSign = scalResultSel64.tail(1).head(32).orR
  private val downOverflowSign = !scalResultSel64.tail(1).head(32).andR
  private val overflowSign = Mux(signBit, downOverflowSign, upOverflowSign)
  nClipSatSel32 := Mux(isSigned, overflowSign, upOverflowUnSign)

  private val nClipSat = Wire(UInt(4.W))
  nClipSat := Mux1H(Seq(
    sel8  -> nClipSatSel8.asUInt,
    sel16 -> nClipSatSel16.asUInt,
    sel32 -> nClipSatSel32,
  ))

  def shiftRotateLeftOneElement(data: UInt, shift: UInt, sew: Int): UInt = {
    sew match {
      case 8   => doShiftRotateLeft(data, shift(2, 0)).asUInt
      case 16  => doShiftRotateLeft(data, shift(3, 0)).asUInt
      case 32  => doShiftRotateLeft(data, shift(4, 0)).asUInt
      case 64  => doShiftRotateLeft(data, shift(5, 0)).asUInt
    }
  }

  def shiftRotateRightOneElement(data: UInt, shift: UInt, sew: Int): UInt = {
    sew match {
      case 8  => doShiftRotateRight(data, shift(2, 0)).asUInt
      case 16 => doShiftRotateRight(data, shift(3, 0)).asUInt
      case 32 => doShiftRotateRight(data, shift(4, 0)).asUInt
      case 64 => doShiftRotateRight(data, shift(5, 0)).asUInt
    }
  }

  def shiftRotateLeft(data: UInt, shift: UInt, sew: Int): UInt = {
    Cat(UIntSplit(vs2, sew).zip(UIntSplit(vs1, sew)).map { case (vs2, vs1) =>
      shiftRotateLeftOneElement(vs2, vs1, sew)}.reverse)
  }

  def shiftRotateRight(data: UInt, shift: UInt, sew: Int): UInt = {
    Cat(UIntSplit(vs2, sew).zip(UIntSplit(vs1, sew)).map { case (vs2, vs1) =>
      shiftRotateRightOneElement(vs2, vs1, sew)}.reverse)
  }

  private val vroShiftRotateLeft64 = shiftRotateLeft(vs2, vs1, 64)
  private val vroShiftRotateLeft32 = shiftRotateLeft(vs2, vs1, 32)
  private val vroShiftRotateLeft16 = shiftRotateLeft(vs2, vs1, 16)
  private val vroShiftRotateLeft8  = shiftRotateLeft(vs2, vs1, 8)
  private val vroShiftRotateRight64 = shiftRotateRight(vs2, vs1, 64)
  private val vroShiftRotateRight32 = shiftRotateRight(vs2, vs1, 32)
  private val vroShiftRotateRight16 = shiftRotateRight(vs2, vs1, 16)
  private val vroShiftRotateRight8  = shiftRotateRight(vs2, vs1, 8)

  private val vroShift64 = Mux(isLeftShiftLogic, vroShiftRotateLeft64, vroShiftRotateRight64)
  private val vroShift32 = Mux(isLeftShiftLogic, vroShiftRotateLeft32, vroShiftRotateRight32)
  private val vroShift16 = Mux(isLeftShiftLogic, vroShiftRotateLeft16, vroShiftRotateRight16)
  private val vroShift8  = Mux(isLeftShiftLogic, vroShiftRotateLeft8,  vroShiftRotateRight8)

  private val vroShiftResult = Wire(UInt(xlen.W))
  vroShiftResult := Mux1H(Seq(
    sel8  -> vroShift8,
    sel16 -> vroShift16,
    sel32 -> vroShift32,
    sel64 -> vroShift64,
  ))

  def CSA3to1(a: UInt, b: UInt, cin: UInt): (UInt, UInt) = {
    val S = a ^ b ^ cin
    val C = a & b | a & cin | b & cin
    (S, C)
  }

  def PopCount8(in: UInt): UInt = {
    val (l1S1, l1C1) = CSA3to1(in(0), in(1), in(2))
    val (l1S2, l1C2) = CSA3to1(in(3), in(4), in(5))
    val l1S3 = in(6) ^ in(7)
    val l1C3 = in(6) & in(7)

    val (l2S1, l2C1) = CSA3to1(l1S1, l1S2, l1S3)
    val (l2S2, l2C2) = CSA3to1(l1C1, l1C2, l1C3)

    val count = Wire(Vec(4, Bool()))
    count(0) := l2S1
    count(1) := l2C1 ^ l2S2
    count(2) := !l2S2 & l2C2 | !l2C1 & l2C2 | l2C1 & l2S2 & !l2C2
    count(3) := l2C1 & l2S2 & l2C2
    count.asUInt
  }

  private val pop8  = Wire(Vec(8, UInt(8.W)))
  private val pop16 = Wire(Vec(4, UInt(16.W)))
  private val pop32 = Wire(Vec(2, UInt(32.W)))
  private val pop64 = Wire(UInt(xlen.W))
  for (i <- 0 until 8) {
    pop8(i) := PopCount8(vs2(i * 8 + 7, i * 8))
  }
  for (i <- 0 until 4) {
    pop16(i) := pop8(i * 2 + 1)(3, 0) +& pop8(i * 2)(3, 0)
  }
  for (i <- 0 until 2) {
    pop32(i) := pop16(i * 2 + 1)(4, 0) +& pop16(i * 2)(4, 0)
  }
  pop64 := pop32(1)(5, 0) +& pop32(0)(5, 0)

  private val popResult = Wire(UInt(xlen.W))
  popResult := Mux1H(Seq(
    sel8  -> pop8.asUInt,
    sel16 -> pop16.asUInt,
    sel32 -> pop32.asUInt,
    sel64 -> pop64
  ))

  private val brevResultSel8  = Wire(Vec(8, UInt(8.W)))
  private val brevResultSel16 = Wire(Vec(4, UInt(16.W)))
  private val brevResultSel32 = Wire(Vec(2, UInt(32.W)))
  private val brevResultSel64 = Wire(UInt(64.W))

  for (i <- 0 until 8) {
    brevResultSel8(i) := Cat(vs2(i * 8 + 7, i * 8).asBools)
  }
  for (i <- 0 until 4) {
    brevResultSel16(i) := Cat(vs2(i * 16 + 15, i * 16).asBools)
  }
  for (i <- 0 until 2) {
    brevResultSel32(i) := Cat(vs2(i * 32 + 31, i * 32).asBools)
  }
  brevResultSel64 := Cat(vs2.asBools)

  private val brevResult = Wire(UInt(xlen.W))
  brevResult := Mux1H(Seq(
    sel8  -> brevResultSel8.asUInt,
    sel16 -> brevResultSel16.asUInt,
    sel32 -> brevResultSel32.asUInt,
    sel64 -> brevResultSel64.asUInt,
  ))

  private val brev8Result = Wire(UInt(xlen.W))
  brev8Result := brevResultSel8.asUInt

  private val rev8ResultSel16 = Wire(Vec(4, Vec(2, UInt(8.W))))
  private val rev8ResultSel32 = Wire(Vec(2, Vec(4, UInt(8.W))))
  private val rev8ResultSel64 = Wire(Vec(8, UInt(8.W)))
  private val rev8ResultSel16Tmp = Wire(Vec(4, Vec(2, UInt(8.W))))
  private val rev8ResultSel32Tmp = Wire(Vec(2, Vec(4, UInt(8.W))))
  private val rev8ResultSel64Tmp = Wire(Vec(8, UInt(8.W)))
  rev8ResultSel16Tmp := vs2.asTypeOf(rev8ResultSel16Tmp)
  rev8ResultSel32Tmp := vs2.asTypeOf(rev8ResultSel32Tmp)
  rev8ResultSel64Tmp := vs2.asTypeOf(rev8ResultSel64Tmp)
  for (i <- 0 until 4) {
    for (j <- 0 until 2) {
      rev8ResultSel16(i)(1 - j) := rev8ResultSel16Tmp(i)(j)
    }
  }
  for (i <- 0 until 2) {
    for (j <- 0 until 4) {
      rev8ResultSel32(i)(3 - j) := rev8ResultSel32Tmp(i)(j)
    }
  }
  for (i <- 0 until 8) {
    rev8ResultSel64(7 - i) := rev8ResultSel64Tmp(i)
  }

  private val rev8Result = Wire(UInt(xlen.W))
  rev8Result := Mux1H(Seq(
    sel8  -> vs2,
    sel16 -> rev8ResultSel16.asUInt,
    sel32 -> rev8ResultSel32.asUInt,
    sel64 -> rev8ResultSel64.asUInt,
  ))

  private val revResult = Wire(UInt(xlen.W))
  revResult := Mux1H(Seq(
    isVbrev  -> brevResult,
    isVbrev8 -> brev8Result,
    isVrev8  -> rev8Result,
  ))

  private val leadZeroIn = Mux(isCtz, brevResult, vs2)
  private val lzc = Lzc(leadZeroIn, isVector)

  private val clzResultSel8  = Wire(Vec(8, UInt(8.W)))
  private val clzResultSel16 = Wire(Vec(4, UInt(16.W)))
  private val clzResultSel32 = Wire(Vec(2, UInt(32.W)))
  private val clzResultSel64 = Wire(UInt(64.W))

  for (i <- 0 until 8) {
    clzResultSel8(i) := lzc.io.vectorOut.get.out8(i)
  }
  for (i <- 0 until 4) {
    clzResultSel16(i) := lzc.io.vectorOut.get.out16(i)
  }
  for (i <- 0 until 2) {
    clzResultSel32(i) := lzc.io.vectorOut.get.out32(i)
  }
  clzResultSel64 := lzc.io.vectorOut.get.out64

  private val leadZeroResult = Wire(UInt(xlen.W))
  leadZeroResult := Mux1H(Seq(
    sel8  -> clzResultSel8.asUInt,
    sel16 -> clzResultSel16.asUInt,
    sel32 -> clzResultSel32.asUInt,
    sel64 -> clzResultSel64.asUInt,
  ))

  private val vwsllResult = Wire(UInt(xlen.W))
  vwsllResult := Mux1H(Seq(
    sel8  -> leftShift16,
    sel16 -> leftShift32,
    sel32 -> leftShift64,
  ))

  io.out.toS1.vd := MuxCase(bitLogicResult, Seq(
    isExt -> extResult,
    isShift -> Mux(isScalVro,
      Mux(isNotVro, scalResult, vroShiftResult),
      Mux(widen, vwsllResult, shiftResult)),
    isZvbbOthers -> Mux(isCountZero, leadZeroResult, revResult),
  ))
  io.out.toS1.narrowVd := narrowResult
  io.out.toS1.vxsat := nClipSat
  io.out.toS1.isVcpop := isZvbbOthers & !isCountZero & isVcpop
  io.out.toS1.popResult := popResult
  io.out.toS1.isNClip := isNClip
  io.out.toS1.signBitSel8  := signBitSel8.asUInt
  io.out.toS1.signBitSel16 := signBitSel16.asUInt
  io.out.toS1.signBitSel32 := signBit
  io.out.toS1.overflowSignSel8 := overflowSignSel8.asUInt
  io.out.toS1.overflowSignSel16 := overflowSignSel16.asUInt
  io.out.toS1.overflowSignSel32 := overflowSign
  io.out.toS1.upOverflowUnSignSel8 := upOverflowUnSignSel8.asUInt
  io.out.toS1.upOverflowUnSignSel16 := upOverflowUnSignSel16.asUInt
  io.out.toS1.upOverflowUnSignSel32 := upOverflowUnSign
  io.out.toS1.nclipResultSel8Tmp := nclipResultsel8Tmp.asUInt
  io.out.toS1.nclipResultSel16Tmp := nclipResultsel16Tmp.asUInt
  io.out.toS1.nclipResultSel32Tmp := scalResultSel64.tail(32)


  dontTouch(vs2WidenVec)
  dontTouch(vs1WidenVec)
  dontTouch(vwsllResult)
  dontTouch(vroShift8)
  dontTouch(vroShift16)
  dontTouch(vroShift32)
  dontTouch(vroShift64)
  dontTouch(vroShiftResult)
  dontTouch(nClipSatSel8)
  dontTouch(nClipSatSel16)
  dontTouch(nClipSatSel32)
  dontTouch(nClipSat)
  dontTouch(scalResultSel8)
  dontTouch(scalResultSel16)
  dontTouch(scalResultSel32)
  dontTouch(scalResultSel64)
  dontTouch(shiftResult)
  dontTouch(rightShift8)
  dontTouch(rightShift16)
  dontTouch(rightShift32)
  dontTouch(rightShift64)
  dontTouch(rightShiftData8)
  dontTouch(rightShiftData16)
  dontTouch(rightShiftData32)
  dontTouch(rightShiftData64)



}
