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
  }

  class InData(width: Int) extends Bundle {
    val src = UInt(width.W)
  }

  class OutData(width: Int) extends Bundle {
    val res = UInt(width.W)
    val fflagsE8 = Vec(width / 8, Fflags())
  }
}

class VectorCvtIO(width: Int) extends Bundle {
  val in  = Input(new VectorCvt.In(width))
  val out = Output(new VectorCvt.Out(width))

  def fire = in.fire
  def src = in.data.src
  def opType = in.ctrl.opType
  def rm = in.ctrl.rm
  def inSew1H = in.ctrl.inSew1H
  def outSew1H = in.ctrl.outSew1H

  def resEx1 = out.ex1.res
  def fflagsE8Ex1 = out.ex1.fflagsE8
  def resEx2 = out.ex2.res
  def fflagsE8Ex2 = out.ex2.fflagsE8
}

class VectorCvt(xlen :Int) extends Module{

  val io = IO(new VectorCvtIO(xlen))
  val in = io.in
  val out = io.out
  val (fire, src, opType, rm, inSew1H, outSew1H) = (in.fire, in.data.src, in.ctrl.opType, in.ctrl.rm, in.ctrl.inSew1H, in.ctrl.outSew1H)
  private val elemBitsE8 = 8
  private val elemBitsE16 = 16
  private val elemBitsE32 = 32
  private val xlenb = xlen / 8
  private val zeroFflags = 0.U.asTypeOf(Fflags())

  private val cvt64 = Module(new CVT64(64, isVectorCvt = true))
  private val cvt32 = Module(new CVT32(32))
  private val cvt16Slot0 = Module(new CVT16(16))
  private val cvt16Slot1 = Module(new CVT16(16))

  private def packLowElems(elemBits: Int, results: UInt*): UInt = {
    VecInit(results.map(_.splitToVecByWidth(elemBits)(0))).asUInt
  }

  private def genFflagsE8(
    outSew1H: UInt,
    fflags64: UInt,
    fflags32: UInt,
    fflags16Slot0: UInt,
    fflags16Slot1: UInt,
    fflagsE64: UInt
  ): Vec[UInt] = {
    Mux1H(Seq(
      (outSew1H === SewOH.e8)  -> VecInit(Seq(fflags64, fflags32, fflags16Slot0, fflags16Slot1) ++ Seq.fill(xlenb - 4)(zeroFflags)),
      (outSew1H === SewOH.e16) -> VecInit(Seq.fill(2)(fflags64) ++ Seq.fill(2)(fflags32) ++ Seq.fill(2)(fflags16Slot0) ++ Seq.fill(2)(fflags16Slot1)),
      (outSew1H === SewOH.e32) -> VecInit(Seq.fill(4)(fflags64) ++ Seq.fill(4)(fflags32)),
      (outSew1H === SewOH.e64) -> VecInit(Seq.fill(xlenb)(fflagsE64)),
    ))
  }

  val fireDelay1 = RegEnable(fire, false.B, fire)
  val outSew1HDelay1 = RegEnable(outSew1H, fire)
  val outSew1HDelay2 = RegEnable(outSew1HDelay1, fireDelay1)

  val elementsE8 = Wire(Vec(8, UInt(8.W)))
  val elementsE16 = Wire(Vec(4, UInt(16.W)))
  val elementsE32 = Wire(Vec(2, UInt(32.W)))
  val elementsE64 = Wire(Vec(1, UInt(64.W)))

  elementsE8 := src.asTypeOf(elementsE8)
  elementsE16 := src.asTypeOf(elementsE16)
  elementsE32 := src.asTypeOf(elementsE32)
  elementsE64 := src.asTypeOf(elementsE64)

  private val cvt64Input = elementsE64(0)
  private val cvt32Input = Mux1H(Seq(
    (inSew1H === SewOH.e8)  -> elementsE8(1),
    (inSew1H === SewOH.e16) -> elementsE16(1),
    (inSew1H === SewOH.e32) -> elementsE32(1),
    (inSew1H === SewOH.e64) -> 0.U,
  ))
  private val cvt16Slot0Input = Mux1H(Seq(
    (inSew1H === SewOH.e8)  -> elementsE8(2),
    (inSew1H === SewOH.e16) -> elementsE16(2),
    (inSew1H === SewOH.e32) -> 0.U,
    (inSew1H === SewOH.e64) -> 0.U,
  ))
  private val cvt16Slot1Input = Mux1H(Seq(
    (inSew1H === SewOH.e8)  -> elementsE8(3),
    (inSew1H === SewOH.e16) -> elementsE16(3),
    (inSew1H === SewOH.e32) -> 0.U,
    (inSew1H === SewOH.e64) -> 0.U,
  ))

  Seq(
    cvt64.io.in -> cvt64Input,
    cvt32.io.in -> cvt32Input,
    cvt16Slot0.io.in -> cvt16Slot0Input,
    cvt16Slot1.io.in -> cvt16Slot1Input,
  ).foreach { case (cvtIn, input) =>
    cvtIn.fire := fire
    cvtIn.data.src := input
    cvtIn.ctrl.opType := opType
    cvtIn.ctrl.rm := rm
    cvtIn.ctrl.inSew1H := inSew1H
    cvtIn.ctrl.outSew1H := outSew1H
    cvtIn.ctrl.isScalarFpInst := false.B
  }

  private val cvt64ResEx2 = cvt64.io.out.ex2.res
  private val cvt32ResEx2 = cvt32.io.out.ex2.res
  private val cvt16Slot0ResEx2 = cvt16Slot0.io.out.ex2.res
  private val cvt16Slot1ResEx2 = cvt16Slot1.io.out.ex2.res
  private val cvt64FflagsEx2 = cvt64.io.out.ex2.fflags
  private val cvt32FflagsEx2 = cvt32.io.out.ex2.fflags
  private val cvt16Slot0FflagsEx2 = cvt16Slot0.io.out.ex2.fflags
  private val cvt16Slot1FflagsEx2 = cvt16Slot1.io.out.ex2.fflags

  private val cvt64ResEx1 = cvt64.io.out.ex1.res
  private val cvt32ResEx1 = cvt32.io.out.ex1.res
  private val cvt16Slot0ResEx1 = cvt16Slot0.io.out.ex1.res
  private val cvt16Slot1ResEx1 = cvt16Slot1.io.out.ex1.res
  private val cvt64FflagsEx1 = cvt64.io.out.ex1.fflags
  private val cvt32FflagsEx1 = cvt32.io.out.ex1.fflags
  private val cvt16Slot0FflagsEx1 = cvt16Slot0.io.out.ex1.fflags
  private val cvt16Slot1FflagsEx1 = cvt16Slot1.io.out.ex1.fflags

  private val ex1ResE8  = packLowElems(elemBitsE8,  cvt64ResEx1, cvt32ResEx1, cvt16Slot0ResEx1, cvt16Slot1ResEx1)
  private val ex1ResE16 = packLowElems(elemBitsE16, cvt64ResEx1, cvt32ResEx1, cvt16Slot0ResEx1, cvt16Slot1ResEx1)
  private val ex1ResE32 = packLowElems(elemBitsE32, cvt64ResEx1, cvt32ResEx1)
  private val ex2ResE8  = packLowElems(elemBitsE8,  cvt64ResEx2, cvt32ResEx2, cvt16Slot0ResEx2, cvt16Slot1ResEx2)
  private val ex2ResE16 = packLowElems(elemBitsE16, cvt64ResEx2, cvt32ResEx2, cvt16Slot0ResEx2, cvt16Slot1ResEx2)
  private val ex2ResE32 = packLowElems(elemBitsE32, cvt64ResEx2, cvt32ResEx2)

  out.ex1.fflagsE8 := genFflagsE8(outSew1HDelay1, cvt64FflagsEx1, cvt32FflagsEx1, cvt16Slot0FflagsEx1, cvt16Slot1FflagsEx1, zeroFflags)
  out.ex2.fflagsE8 := genFflagsE8(outSew1HDelay2, cvt64FflagsEx2, cvt32FflagsEx2, cvt16Slot0FflagsEx2, cvt16Slot1FflagsEx2, cvt64FflagsEx2)

  out.ex1.res := Mux1H(Seq(
    (outSew1HDelay1 === SewOH.e8)  -> ex1ResE8,
    (outSew1HDelay1 === SewOH.e16) -> ex1ResE16,
    (outSew1HDelay1 === SewOH.e32) -> ex1ResE32,
    (outSew1HDelay1 === SewOH.e64) -> 0.U(xlen.W),
  ))
  out.ex2.res := Mux1H(Seq(
    (outSew1HDelay2 === SewOH.e8)  -> ex2ResE8,
    (outSew1HDelay2 === SewOH.e16) -> ex2ResE16,
    (outSew1HDelay2 === SewOH.e32) -> ex2ResE32,
    (outSew1HDelay2 === SewOH.e64) -> cvt64ResEx2,
  ))

  dontTouch(inSew1H)
  dontTouch(outSew1H)
}