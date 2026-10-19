package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import yunsuan.VfcvtType
import yunsuan.util._

class VectorCvtIO(width: Int) extends Bundle {
  val fire = Input(Bool())
  val src = Input(UInt(width.W))
  val opType = Input(UInt(8.W))
  val sew = Input(UInt(2.W))
  val rm = Input(UInt(3.W))
  val isFpToVecInst = Input(Bool())
  val isFround = Input(UInt(2.W))
  val isFcvtmod = Input(Bool())
  val isMXFP = Input(Bool())
  val factor = Input(UInt(8.W))

  val result = Output(UInt(width.W))
  val fflags = Output(UInt(20.W))
}

class VectorCvt(xlen :Int) extends Module{

  val io = IO(new VectorCvtIO(xlen))
  val (fire, src, opType, sew, rm, isFpToVecInst, isFround, isFcvtmod) = (io.fire, io.src, io.opType, io.sew, io.rm, io.isFpToVecInst, io.isFround, io.isFcvtmod)
  val isMXFP = io.isMXFP && (sew === "b00".U || sew === "b01".U)
  val isMXFPOut = RegEnable(RegEnable(isMXFP, false.B, fire), false.B, GatedValidRegNext(fire))
  val widen = opType(4, 3) // 0->single 1->widen 2->norrow => width of result
  val isVfnCvtBf16 = opType === VfcvtType.vfncvtbf16_ffw
  val isVfwCvtBf16 = opType === VfcvtType.vfwcvtbf16_ffv
  val isXX8 = opType === VfcvtType.vfncvtxx8_int8 || opType === VfcvtType.vfncvtxx8_e4m3 || opType === VfcvtType.vfncvtxx8_e5m2
  val isXX8Out = RegEnable(RegEnable(isXX8, false.B, fire), false.B, GatedValidRegNext(fire))

  // input width 8， 16， 32， 64
  val input1H = Wire(UInt(4.W))
  val commonInput1H = chisel3.util.experimental.decode.decoder(
    widen ## sew,
    TruthTable(
      Seq(
        BitPat("b00_01") -> BitPat("b0010"), // 16
        BitPat("b00_10") -> BitPat("b0100"), // 32
        BitPat("b00_11") -> BitPat("b1000"), // 64

        BitPat("b01_00") -> BitPat("b0001"), // 8
        BitPat("b01_01") -> BitPat("b0010"), // 16
        BitPat("b01_10") -> BitPat("b0100"), // 32

        BitPat("b10_00") -> BitPat("b0010"), // 16
        BitPat("b10_01") -> BitPat("b0100"), // 32
        BitPat("b10_10") -> BitPat("b1000"), // 64
      ),
      BitPat("b0000")
    )
  )
  input1H := Mux(isXX8 || isVfnCvtBf16, "b0100".U, Mux(isVfwCvtBf16, "b0010".U, commonInput1H))

  // output width 8， 16， 32， 64
  val output1H = Wire(UInt(4.W))
  val commonOutput1H = chisel3.util.experimental.decode.decoder(
    widen ## sew,
    TruthTable(
      Seq(
        BitPat("b00_01") -> BitPat("b0010"), // 16
        BitPat("b00_10") -> BitPat("b0100"), // 32
        BitPat("b00_11") -> BitPat("b1000"), // 64

        BitPat("b01_00") -> BitPat("b0010"), // 16
        BitPat("b01_01") -> BitPat("b0100"), // 32
        BitPat("b01_10") -> BitPat("b1000"), // 64

        BitPat("b10_00") -> BitPat("b0001"), // 8
        BitPat("b10_01") -> BitPat("b0010"), // 16
        BitPat("b10_10") -> BitPat("b0100"), // 32
      ),
      BitPat("b0000")
    )
  )
  output1H := Mux(isXX8, "b0001".U, Mux(isVfnCvtBf16, "b0010".U, Mux(isVfwCvtBf16, "b0100".U, commonOutput1H)))
  dontTouch(input1H)
  dontTouch(output1H)

  val inputWidth1H = input1H
  val outputWidth1H = RegEnable(RegEnable(output1H, fire), GatedValidRegNext(fire))


  val element8 = Wire(Vec(8,UInt(8.W)))
  val element16 = Wire(Vec(4,UInt(16.W)))
  val element32 = Wire(Vec(2,UInt(32.W)))
  val element64 = Wire(Vec(1,UInt(64.W)))

  element8 := src.asTypeOf(element8)
  element16 := src.asTypeOf(element16)
  element32 := src.asTypeOf(element32)
  element64 := src.asTypeOf(element64)

  val in0 = element64(0)
  val in1 = Mux1H(inputWidth1H, Seq(element8(1), element16(1), element32(1), 0.U))// input 0=> result 0 while norrow eg. 64b->32b
  val in2 = Mux1H(inputWidth1H, Seq(element8(2), element16(2), 0.U, 0.U))
  val in3 = Mux1H(inputWidth1H, Seq(element8(3), element16(3), 0.U, 0.U))


  val (result0, fflags0) = VCVT(64)(fire, in0, opType, sew, rm, input1H, output1H, isFpToVecInst, isFround, isFcvtmod)
  val (result1, fflags1) = VCVT(32)(fire, in1, opType, sew, rm, input1H, output1H, isFpToVecInst, isFround, isFcvtmod)
  val (result2, fflags2) = VCVT(16)(fire, in2, opType, sew, rm, input1H, output1H, isFpToVecInst, isFround, isFcvtmod)
  val (result3, fflags3) = VCVT(16)(fire, in3, opType, sew, rm, input1H, output1H, isFpToVecInst, isFround, isFcvtmod)

  val mxfp0 = Module(new CVT_mxfp)
  val mxfp1 = Module(new CVT_mxfp)
  for ((converter, input) <- Seq((mxfp0, element32(0)), (mxfp1, element32(1)))) {
    converter.io.fire := fire
    converter.io.src := input
    converter.io.factor := io.factor
    converter.io.rm := rm
    converter.io.isFp4 := sew === "b01".U
  }
  val mxfpResult = Mux(
    RegEnable(RegEnable(sew === "b01".U, false.B, fire), false.B, GatedValidRegNext(fire)),
    Cat(0.U(56.W), mxfp1.io.result(3, 0), mxfp0.io.result(3, 0)),
    Cat(0.U(48.W), mxfp1.io.result, mxfp0.io.result)
  )
  val mxfpFflags = Cat(0.U(10.W), mxfp1.io.fflags, mxfp0.io.fflags)

  val commonResult = Mux1H(outputWidth1H, Seq(
    result3(7,0) ## result2(7,0) ## result1(7,0) ## result0(7,0),
    result3(15,0) ## result2(15,0) ## result1(15,0) ## result0(15,0),
    result1(31,0) ## result0(31,0),
    result0
  ))

  val commonFflags = Mux1H(outputWidth1H, Seq(
    fflags3 ## fflags2 ## fflags1 ## fflags0,
    fflags3 ## fflags2 ## fflags1 ## fflags0,
    fflags1 ## fflags0,
    fflags0
  ))

  val xx8Converters = element32.map { element =>
    val converter = Module(new CVT_xx8)
    converter.io.fire := fire
    converter.io.src := element
    converter.io.opType := opType
    converter.io.rm := rm
    converter
  }
  val xx8Result = Cat(0.U(48.W), xx8Converters(1).io.result, xx8Converters(0).io.result)
  val xx8Fflags = Cat(0.U(10.W), xx8Converters(1).io.fflags, xx8Converters(0).io.fflags)

  io.result := Mux(isXX8Out, xx8Result, Mux(isMXFPOut, mxfpResult, commonResult))
  io.fflags := Mux(isXX8Out, xx8Fflags, Mux(isMXFPOut, mxfpFflags, commonFflags))
}
