package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import yunsuan.util._
import yunsuan.encoding.Opcode.Opcodes.FCvtOpcode
import yunsuan.vector.Common._

object VectorCvt {
  class In(width: Int) extends Bundle {
    val fire = Bool()
    val ctrl = new InCtrl
    val data = new InData(width)
  }

  class Out(width: Int) extends Bundle {
    val ex1 = new OutData(width)
    val ex2 = new OutData(width)
  }

  class InCtrl extends Bundle {
    val opType = FCvtOpcode()
    val rm = Frm()
    val inSew1H  = Sew()
    val outSew1H = Sew()
    val cvt64UseWidenSrc2 = Bool()
    val cvt32UseWidenSrc2 = Bool()
    val cvt16UseWidenSrc2 = Bool()
  }

  class InData(width: Int) extends Bundle {
    val src2 = UInt(width.W)
    val widenSrc2 = UInt(width.W)
    val narrowSrc2 = UInt(width.W)
    val narrowSrc1 = UInt(width.W)
  }

  class OutData(width: Int) extends Bundle {
    val res = UInt(width.W)
    val fflagsE8 = Vec(width / 8, Fflags())
    val narrowFflagsE8 = Vec(width / 8, Fflags())
  }
}

class VectorCvtIO(width: Int) extends Bundle {
  val in  = Input(new VectorCvt.In(width))
  val out = Output(new VectorCvt.Out(width))

  def fire = in.fire
  def src2 = in.data.src2
  def widenSrc2 = in.data.widenSrc2
  def narrowSrc2 = in.data.narrowSrc2
  def narrowSrc1 = in.data.narrowSrc1
  def opType = in.ctrl.opType
  def rm = in.ctrl.rm
  def inSew1H = in.ctrl.inSew1H
  def outSew1H = in.ctrl.outSew1H
  def cvt64UseWidenSrc2 = in.ctrl.cvt64UseWidenSrc2
  def cvt32UseWidenSrc2 = in.ctrl.cvt32UseWidenSrc2
  def cvt16UseWidenSrc2 = in.ctrl.cvt16UseWidenSrc2

  def resEx1 = out.ex1.res
  def fflagsE8Ex1 = out.ex1.fflagsE8
  def narrowFflagsE8Ex1 = out.ex1.narrowFflagsE8
  def resEx2 = out.ex2.res
  def fflagsE8Ex2 = out.ex2.fflagsE8
  def narrowFflagsE8Ex2 = out.ex2.narrowFflagsE8
}

class VectorCvt(xlen :Int) extends Module{

  val io = IO(new VectorCvtIO(xlen))
  val in = io.in
  val out = io.out
  val (
    fire,
    src2,
    widenSrc2,
    narrowSrc2,
    narrowSrc1,
    opType,
    rm,
    inSew1H,
    outSew1H,
    cvt64UseWidenSrc2,
    cvt32UseWidenSrc2,
    cvt16UseWidenSrc2,
  ) = (
    in.fire,
    in.data.src2,
    in.data.widenSrc2,
    in.data.narrowSrc2,
    in.data.narrowSrc1,
    in.ctrl.opType,
    in.ctrl.rm,
    in.ctrl.inSew1H,
    in.ctrl.outSew1H,
    in.ctrl.cvt64UseWidenSrc2,
    in.ctrl.cvt32UseWidenSrc2,
    in.ctrl.cvt16UseWidenSrc2,
  )
  private val elemBitsE8 = 8
  private val elemBitsE16 = 16
  private val elemBitsE32 = 32
  private val xlenb = xlen / 8
  private val zeroFflags = 0.U.asTypeOf(Fflags())

  private val cvt64 = Module(new CVT64(64, isVectorCvt = true))
  private val cvt32 = Module(new CVT32(32))
  private val cvt16Slot0 = Module(new CVT16(16))
  private val cvt16Slot1 = Module(new CVT16(16))
  private val src2NarrowCvt64 = Module(new CVT64NarrowConvert(64))
  private val src2NarrowCvt32 = Module(new CVT32NarrowConvert(32))
  private val src2NarrowCvt16Slot0 = Module(new CVT16NarrowConvert(16))
  private val src2NarrowCvt16Slot1 = Module(new CVT16NarrowConvert(16))
  private val src1NarrowCvt64 = Module(new CVT64NarrowConvert(64))
  private val src1NarrowCvt32 = Module(new CVT32NarrowConvert(32))
  private val src1NarrowCvt16Slot0 = Module(new CVT16NarrowConvert(16))
  private val src1NarrowCvt16Slot1 = Module(new CVT16NarrowConvert(16))

  private def packLowElems(elemBits: Int, results: UInt*): UInt = {
    VecInit(results.map(_.splitToVecByWidth(elemBits)(0))).asUInt
  }

  private case class CvtStageOut(
    res64: UInt,
    res32: UInt,
    res16Slot0: UInt,
    res16Slot1: UInt,
    fflags64: UInt,
    fflags32: UInt,
    fflags16Slot0: UInt,
    fflags16Slot1: UInt,
  ) {
    def resSeq: Seq[UInt] = Seq(res64, res32, res16Slot0, res16Slot1)
    def fflagsSeq: Seq[UInt] = Seq(fflags64, fflags32, fflags16Slot0, fflags16Slot1)
  }

  private def collectStageOut(
    out64: VCVT.OutData,
    out32: VCVT.OutData,
    out16Slot0: VCVT.OutData,
    out16Slot1: VCVT.OutData,
  ): CvtStageOut = {
    CvtStageOut(
      out64.res, out32.res, out16Slot0.res, out16Slot1.res,
      out64.fflags, out32.fflags, out16Slot0.fflags, out16Slot1.fflags,
    )
  }

  private def genFflagsE8(
    outSew1H: UInt,
    stageOut: CvtStageOut,
    fflagsE64: UInt
  ): Vec[UInt] = {
    Mux1H(Seq(
      outSew1H(0) -> VecInit(stageOut.fflagsSeq ++ Seq.fill(xlenb - 4)(zeroFflags)),
      outSew1H(1) -> VecInit(Seq.fill(2)(stageOut.fflags64) ++ Seq.fill(2)(stageOut.fflags32) ++ Seq.fill(2)(stageOut.fflags16Slot0) ++ Seq.fill(2)(stageOut.fflags16Slot1)),
      outSew1H(2) -> VecInit(Seq.fill(4)(stageOut.fflags64) ++ Seq.fill(4)(stageOut.fflags32)),
      outSew1H(3) -> VecInit(Seq.fill(xlenb)(fflagsE64)),
    ))
  }

  private def genNarrowFflagsE8(
    outSew1H: UInt,
    lo: CvtStageOut,
    hi: CvtStageOut,
  ): Vec[UInt] = {
    Mux1H(Seq(
      outSew1H(0) -> VecInit(lo.fflagsSeq ++ hi.fflagsSeq),
      outSew1H(1) -> VecInit(Seq.fill(2)(lo.fflags64) ++ Seq.fill(2)(lo.fflags32) ++ Seq.fill(2)(hi.fflags64) ++ Seq.fill(2)(hi.fflags32)),
      outSew1H(2) -> VecInit(Seq.fill(4)(lo.fflags64) ++ Seq.fill(4)(hi.fflags64)),
      outSew1H(3) -> VecInit(Seq.fill(xlenb)(lo.fflags64)),
    ))
  }

  private def genStageResult(
    outSew1H: UInt,
    isNarrow: Bool,
    normal: CvtStageOut,
    narrowLo: CvtStageOut,
    narrowHi: CvtStageOut,
    resE64: UInt,
  ): UInt = {
    val resE8 = Mux(
      isNarrow,
      packLowElems(elemBitsE8, (narrowLo.resSeq ++ narrowHi.resSeq): _*),
      packLowElems(elemBitsE8, normal.resSeq: _*)
    )
    val resE16 = Mux(
      isNarrow,
      packLowElems(elemBitsE16, narrowLo.res64, narrowLo.res32, narrowHi.res64, narrowHi.res32),
      packLowElems(elemBitsE16, normal.resSeq: _*)
    )
    val resE32 = Mux(
      isNarrow,
      packLowElems(elemBitsE32, narrowLo.res64, narrowHi.res64),
      packLowElems(elemBitsE32, normal.res64, normal.res32)
    )
    Mux1H(Seq(
      outSew1H(0) -> resE8,
      outSew1H(1) -> resE16,
      outSew1H(2) -> resE32,
      outSew1H(3) -> resE64,
    ))
  }

  def connectCvt(cvtIn: VCVT.In, input: UInt, cvtFire: Bool): Unit = {
    cvtIn.fire := cvtFire
    cvtIn.data.src := input
    cvtIn.ctrl.opType := opType
    cvtIn.ctrl.rm := rm
    cvtIn.ctrl.inSew1H := inSew1H
    cvtIn.ctrl.outSew1H := outSew1H
    cvtIn.ctrl.isScalarFpInst := false.B
  }

  val fireDelay1 = RegEnable(fire, false.B, fire)
  val outSew1HDelay1 = RegEnable(outSew1H, fire)
  val outSew1HDelay2 = RegEnable(outSew1HDelay1, fireDelay1)
  val isNarrow = FCvtOpcode.getInputDataWidth(opType) > FCvtOpcode.getOutputDataWidth(opType)
  val isNarrowDelay1 = RegEnable(isNarrow, false.B, fire)
  val isNarrowDelay2 = RegEnable(isNarrowDelay1, false.B, fireDelay1)

  val elementsE8 = Wire(Vec(8, UInt(8.W)))
  val elementsE16 = Wire(Vec(4, UInt(16.W)))
  val elementsE32 = Wire(Vec(2, UInt(32.W)))
  val elementsE64 = Wire(Vec(1, UInt(64.W)))
  val widenElementsE8 = Wire(Vec(8, UInt(8.W)))
  val widenElementsE16 = Wire(Vec(4, UInt(16.W)))
  val src1ElementsE16 = Wire(Vec(4, UInt(16.W)))
  val narrowSrc2ElementsE8 = Wire(Vec(8, UInt(8.W)))
  val narrowSrc2ElementsE16 = Wire(Vec(4, UInt(16.W)))
  val narrowSrc2ElementsE32 = Wire(Vec(2, UInt(32.W)))
  val narrowSrc2ElementsE64 = Wire(Vec(1, UInt(64.W)))
  val narrowHiElementsE32 = Wire(Vec(2, UInt(32.W)))
  val narrowHiElementsE64 = Wire(Vec(1, UInt(64.W)))

  elementsE8 := src2.asTypeOf(elementsE8)
  elementsE16 := src2.asTypeOf(elementsE16)
  elementsE32 := src2.asTypeOf(elementsE32)
  elementsE64 := src2.asTypeOf(elementsE64)
  widenElementsE8 := widenSrc2.asTypeOf(widenElementsE8)
  widenElementsE16 := widenSrc2.asTypeOf(widenElementsE16)
  src1ElementsE16 := narrowSrc1.asTypeOf(src1ElementsE16)
  narrowSrc2ElementsE8 := narrowSrc2.asTypeOf(narrowSrc2ElementsE8)
  narrowSrc2ElementsE16 := narrowSrc2.asTypeOf(narrowSrc2ElementsE16)
  narrowSrc2ElementsE32 := narrowSrc2.asTypeOf(narrowSrc2ElementsE32)
  narrowSrc2ElementsE64 := narrowSrc2.asTypeOf(narrowSrc2ElementsE64)
  narrowHiElementsE32 := narrowSrc1.asTypeOf(narrowHiElementsE32)
  narrowHiElementsE64 := narrowSrc1.asTypeOf(narrowHiElementsE64)

  private val cvt64Input = Mux(cvt64UseWidenSrc2, widenSrc2, elementsE64(0))
  private val cvt32Input = Mux1H(Seq(
    inSew1H(0) -> Mux(cvt32UseWidenSrc2, widenElementsE8(1), elementsE8(1)),
    inSew1H(1) -> Mux(cvt32UseWidenSrc2, widenElementsE16(1), elementsE16(1)),
    inSew1H(2) -> elementsE32(1),
    inSew1H(3) -> 0.U,
  ))
  private val cvt16Slot0Input = Mux1H(Seq(
    inSew1H(0) -> Mux(cvt16UseWidenSrc2, widenElementsE8(2), elementsE8(2)),
    inSew1H(1) -> elementsE16(2),
    inSew1H(2) -> 0.U,
    inSew1H(3) -> 0.U,
  ))
  private val cvt16Slot1Input = Mux1H(Seq(
    inSew1H(0) -> Mux(cvt16UseWidenSrc2, widenElementsE8(3), elementsE8(3)),
    inSew1H(1) -> elementsE16(3),
    inSew1H(2) -> 0.U,
    inSew1H(3) -> 0.U,
  ))
  private val narrowLoCvt64Input = narrowSrc2ElementsE64(0)
  private val narrowLoCvt32Input = Mux1H(Seq(
    inSew1H(0) -> narrowSrc2ElementsE8(1),
    inSew1H(1) -> narrowSrc2ElementsE16(1),
    inSew1H(2) -> narrowSrc2ElementsE32(1),
    inSew1H(3) -> 0.U,
  ))
  private val narrowLoCvt16Slot0Input = Mux1H(Seq(
    inSew1H(0) -> narrowSrc2ElementsE8(2),
    inSew1H(1) -> narrowSrc2ElementsE16(2),
    inSew1H(2) -> 0.U,
    inSew1H(3) -> 0.U,
  ))
  private val narrowLoCvt16Slot1Input = Mux1H(Seq(
    inSew1H(0) -> narrowSrc2ElementsE8(3),
    inSew1H(1) -> narrowSrc2ElementsE16(3),
    inSew1H(2) -> 0.U,
    inSew1H(3) -> 0.U,
  ))
  private val narrowHiCvt64Input = narrowHiElementsE64(0)
  private val narrowHiCvt32Input = Mux(inSew1H(1), src1ElementsE16(1), narrowHiElementsE32(1))
  private val narrowHiCvt16Slot0Input = src1ElementsE16(2)
  private val narrowHiCvt16Slot1Input = src1ElementsE16(3)

  Seq(
    cvt64.io.in -> cvt64Input,
    cvt32.io.in -> cvt32Input,
    cvt16Slot0.io.in -> cvt16Slot0Input,
    cvt16Slot1.io.in -> cvt16Slot1Input,
  ).foreach { case (cvtIn, input) => connectCvt(cvtIn, input, fire && !isNarrow) }
  Seq(
    src2NarrowCvt64.io.in -> narrowLoCvt64Input,
    src2NarrowCvt32.io.in -> narrowLoCvt32Input,
    src2NarrowCvt16Slot0.io.in -> narrowLoCvt16Slot0Input,
    src2NarrowCvt16Slot1.io.in -> narrowLoCvt16Slot1Input,
    src1NarrowCvt64.io.in -> narrowHiCvt64Input,
    src1NarrowCvt32.io.in -> narrowHiCvt32Input,
    src1NarrowCvt16Slot0.io.in -> narrowHiCvt16Slot0Input,
    src1NarrowCvt16Slot1.io.in -> narrowHiCvt16Slot1Input,
  ).foreach { case (cvtIn, input) => connectCvt(cvtIn, input, fire && isNarrow) }

  private val normalEx1   = collectStageOut(cvt64.io.out.ex1,           cvt32.io.out.ex1,           cvt16Slot0.io.out.ex1,           cvt16Slot1.io.out.ex1)
  private val narrowLoEx1 = collectStageOut(src2NarrowCvt64.io.out.ex1, src2NarrowCvt32.io.out.ex1, src2NarrowCvt16Slot0.io.out.ex1, src2NarrowCvt16Slot1.io.out.ex1)
  private val narrowHiEx1 = collectStageOut(src1NarrowCvt64.io.out.ex1, src1NarrowCvt32.io.out.ex1, src1NarrowCvt16Slot0.io.out.ex1, src1NarrowCvt16Slot1.io.out.ex1)
  private val normalEx2   = collectStageOut(cvt64.io.out.ex2,           cvt32.io.out.ex2,           cvt16Slot0.io.out.ex2,           cvt16Slot1.io.out.ex2)
  private val narrowLoEx2 = collectStageOut(src2NarrowCvt64.io.out.ex2, src2NarrowCvt32.io.out.ex2, src2NarrowCvt16Slot0.io.out.ex2, src2NarrowCvt16Slot1.io.out.ex2)
  private val narrowHiEx2 = collectStageOut(src1NarrowCvt64.io.out.ex2, src1NarrowCvt32.io.out.ex2, src1NarrowCvt16Slot0.io.out.ex2, src1NarrowCvt16Slot1.io.out.ex2)

  private val ex1NormalFflagsE8 = genFflagsE8(outSew1HDelay1, normalEx1, zeroFflags)
  private val ex1NarrowFflagsE8 = genNarrowFflagsE8(outSew1HDelay1, narrowLoEx1, narrowHiEx1)
  private val ex2NormalFflagsE8 = genFflagsE8(outSew1HDelay2, normalEx2, normalEx2.fflags64)
  private val ex2NarrowFflagsE8 = genNarrowFflagsE8(outSew1HDelay2, narrowLoEx2, narrowHiEx2)
  out.ex1.fflagsE8 := ex1NormalFflagsE8
  out.ex1.narrowFflagsE8 := ex1NarrowFflagsE8
  out.ex2.fflagsE8 := ex2NormalFflagsE8
  out.ex2.narrowFflagsE8 := ex2NarrowFflagsE8

  out.ex1.res := genStageResult(outSew1HDelay1, isNarrowDelay1, normalEx1, narrowLoEx1, narrowHiEx1, 0.U(xlen.W))
  out.ex2.res := genStageResult(outSew1HDelay2, isNarrowDelay2, normalEx2, narrowLoEx2, narrowHiEx2, normalEx2.res64)
}