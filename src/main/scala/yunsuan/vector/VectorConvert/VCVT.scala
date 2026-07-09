package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import yunsuan.VfcvtType
import yunsuan.util._

class CVTIO(width: Int) extends Bundle {
  val fire = Input(Bool())
  val src = Input(UInt(width.W))
  val opType = Input(UInt(8.W))
  val sew = Input(UInt(2.W))
  val rm = Input(UInt(3.W))
  val altfmt = Input(Bool())
  val input1H = Input(UInt(4.W))
  val output1H = Input(UInt(4.W))
  val isFpToVecInst = Input(Bool())
  val isFround = Input(UInt(2.W))
  val isFcvtmod = Input(Bool())
  val result = Output(UInt(width.W))
  val fflags = Output(UInt(5.W))
}

abstract class CVT(width: Int) extends Module{
  val io = IO(new CVTIO(width))
}

class VCVT(width: Int) extends Module{
  val io = IO(new CVTIO(width))
  val isVectorBf16Cvt = io.opType === VfcvtType.vfncvtbf16_ffw || io.opType === VfcvtType.vfwcvtbf16_ffv
  val isScalarBf16Cvt = io.opType === VfcvtType.fcvt_bf16_s || io.opType === VfcvtType.fcvt_s_bf16
  val isLowFpCvt =
    ((io.opType === VfcvtType.vfncvtbf16_ffw || io.opType === VfcvtType.vfwcvtbf16_ffv) && io.sew === 0.U) ||
      io.opType === VfcvtType.vfncvt_ffq ||
      io.opType === VfcvtType.vfncvt_sat_ffq ||
      (io.opType === VfcvtType.vfncvtbf16_sat_ffw && io.sew === 0.U) ||
      io.opType === VfcvtType.vfext_vf2
  val isBf16Cvt = (isVectorBf16Cvt && io.sew =/= 0.U) || isScalarBf16Cvt
  val isBf16CvtOut = RegEnable(RegEnable(isBf16Cvt, false.B, io.fire), false.B, GatedValidRegNext(io.fire))
  val isLowFpCvtOut = RegEnable(RegEnable(isLowFpCvt, false.B, io.fire), false.B, GatedValidRegNext(io.fire))
  val vcvtImpl = width match {
    case 16 => Module(new CVT16(16))
    case 32 => Module(new CVT32(32))
    case 64 => Module(new CVT64(64, isVectorCvt=true))
  }
  io <> vcvtImpl.io
  val lowFpCvtImpl = Module(new CVT_fp8_fp4(width))
  lowFpCvtImpl.io.fire := io.fire
  lowFpCvtImpl.io.src := io.src
  lowFpCvtImpl.io.opType := io.opType
  lowFpCvtImpl.io.sew := io.sew
  lowFpCvtImpl.io.rm := io.rm
  lowFpCvtImpl.io.altfmt := io.altfmt
  lowFpCvtImpl.io.input1H := io.input1H
  lowFpCvtImpl.io.output1H := io.output1H
  lowFpCvtImpl.io.isFpToVecInst := io.isFpToVecInst
  lowFpCvtImpl.io.isFround := io.isFround
  lowFpCvtImpl.io.isFcvtmod := io.isFcvtmod
  if (width >= 32) {
    val bf16CvtImpl = Module(new CVT_bf16(width))
    bf16CvtImpl.io.fire := io.fire
    bf16CvtImpl.io.src := io.src
    bf16CvtImpl.io.opType := io.opType
    bf16CvtImpl.io.sew := io.sew
    bf16CvtImpl.io.rm := io.rm
    bf16CvtImpl.io.altfmt := io.altfmt
    bf16CvtImpl.io.input1H := io.input1H
    bf16CvtImpl.io.output1H := io.output1H
    bf16CvtImpl.io.isFpToVecInst := io.isFpToVecInst
    bf16CvtImpl.io.isFround := io.isFround
    bf16CvtImpl.io.isFcvtmod := io.isFcvtmod
    io.result := Mux(isLowFpCvtOut, lowFpCvtImpl.io.result, Mux(isBf16CvtOut, bf16CvtImpl.io.result, vcvtImpl.io.result))
    io.fflags := Mux(isLowFpCvtOut, lowFpCvtImpl.io.fflags, Mux(isBf16CvtOut, bf16CvtImpl.io.fflags, vcvtImpl.io.fflags))
  } else {
    io.result := Mux(isLowFpCvtOut, lowFpCvtImpl.io.result, Mux(isBf16CvtOut, 0.U(width.W), vcvtImpl.io.result))
    io.fflags := Mux(isLowFpCvtOut, lowFpCvtImpl.io.fflags, Mux(isBf16CvtOut, 0.U(5.W), vcvtImpl.io.fflags))
  }
}
object VCVT {
  def apply(
             width: Int
           )(fire:    Bool,
             input:   UInt,
             opType:  UInt,
             sew:     UInt,
             rm:      UInt,
             altfmt:  Bool,
             input1H:      UInt,
             output1H:      UInt,
             isFpToVecInst: Bool,
             isFround: UInt,
             isFcvtmod: Bool
           ): (UInt, UInt) = {
    val vcvtWraper = Module(new VCVT(width))
    vcvtWraper.io.fire := fire
    vcvtWraper.io.src := input
    vcvtWraper.io.opType := opType
    vcvtWraper.io.sew := sew
    vcvtWraper.io.rm := rm
    vcvtWraper.io.altfmt := altfmt
    vcvtWraper.io.input1H := input1H
    vcvtWraper.io.output1H := output1H
    vcvtWraper.io.isFpToVecInst := isFpToVecInst
    vcvtWraper.io.isFround := isFround
    vcvtWraper.io.isFcvtmod := isFcvtmod
    (vcvtWraper.io.result, vcvtWraper.io.fflags)
  }
}
