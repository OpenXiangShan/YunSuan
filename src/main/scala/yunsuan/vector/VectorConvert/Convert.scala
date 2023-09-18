package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import yunsuan.encoding.Opcode.VfCvtOpcode._
import yunsuan.vector.VectorConvert.RoundingModle._

class VectorCvtIO(width: Int) extends Bundle {
  val src = Input(UInt(width.W))
  val opType = Input(UInt(8.W))
  val sew = Input(UInt(2.W))
  val rm = Input(UInt(3.W))

  val result = Output(UInt(width.W))
  val fflags = Output(UInt(20.W))
}

class VectorCvt(xlen :Int) extends Module{

  val io = IO(new VectorCvtIO(xlen))
  val (src, opType, sew, rm) = (io.src, io.opType, io.sew, io.rm)


  val widen = opType(4, 3) // 0->single 1->widen 2->norrow => width of result
  val inIsFp = opType.head(1).asBool
  val outIsFp = opType.tail(1).head(1).asBool

  val input1H = Wire(UInt(8.W))
  input1H := chisel3.util.experimental.decode.decoder(
    widen ## sew ## inIsFp,
    TruthTable(
      Seq(
        BitPat("b00_01_0") -> BitPat("b0000_0100"), // i16
        BitPat("b00_01_1") -> BitPat("b0000_1000"), // f16
        BitPat("b00_10_0") -> BitPat("b0001_0000"), // i32
        BitPat("b00_10_1") -> BitPat("b0010_0000"), // f32
        BitPat("b00_11_0") -> BitPat("b0100_0000"), // i64
        BitPat("b00_11_1") -> BitPat("b1000_0000"), // f64

        BitPat("b01_00_0") -> BitPat("b0000_0001"), // i8
        BitPat("b01_01_0") -> BitPat("b0000_0100"), // i16
        BitPat("b01_01_1") -> BitPat("b0000_1000"), // f16
        BitPat("b01_10_0") -> BitPat("b0001_0000"), // i32
        BitPat("b01_10_1") -> BitPat("b0010_0000"), // f32

        BitPat("b10_00_1") -> BitPat("b0000_1000"), // f16
        BitPat("b10_01_0") -> BitPat("b0001_0000"), // i32
        BitPat("b10_01_1") -> BitPat("b0010_0000"), // f32
        BitPat("b10_10_0") -> BitPat("b0100_0000"), // i64
        BitPat("b10_10_1") -> BitPat("b1000_0000"), // f64
      ),
      BitPat("b0000_0010") //f8, don't exist
    )
  )

  // output format i8, f8(don't exist), i16, f16, i32, f32, i64, f64
  val output1H = Wire(UInt(8.W))
  output1H := chisel3.util.experimental.decode.decoder(
    widen ## sew ## outIsFp,
    TruthTable(
      Seq(
        BitPat("b00_01_0") -> BitPat("b0000_0100"), // i16
        BitPat("b00_01_1") -> BitPat("b0000_1000"), // f16
        BitPat("b00_10_0") -> BitPat("b0001_0000"), // i32
        BitPat("b00_10_1") -> BitPat("b0010_0000"), // f32
        BitPat("b00_11_0") -> BitPat("b0100_0000"), // i64
        BitPat("b00_11_1") -> BitPat("b1000_0000"), // f64

        BitPat("b01_00_1") -> BitPat("b0000_1000"), // f16
        BitPat("b01_01_0") -> BitPat("b0001_0000"), // i32
        BitPat("b01_01_1") -> BitPat("b0010_0000"), // f32
        BitPat("b01_10_0") -> BitPat("b0100_0000"), // i64
        BitPat("b01_10_1") -> BitPat("b1000_0000"), // f64

        BitPat("b10_00_0") -> BitPat("b0000_0001"), // i8
        BitPat("b10_01_0") -> BitPat("b0000_0100"), // i16
        BitPat("b10_01_1") -> BitPat("b0000_1000"), // f16
        BitPat("b10_10_0") -> BitPat("b0001_0000"), // i32
        BitPat("b10_10_1") -> BitPat("b0010_0000"), // f32
      ),
      BitPat("b0000_0010") //f8, don't exist
    )
  )
  dontTouch(input1H)
  dontTouch(output1H)

  val inputWidth1H = VecInit((0 to 3).map(i =>input1H(2 * i) | input1H(2 * i + 1))).asUInt
  val outputWidth1H = VecInit((0 to 3).map(i =>output1H(2 * i) | output1H(2 * i + 1))).asUInt


  val element8 = Wire(Vec(8,UInt(8.W)))
  val element16 = Wire(Vec(4,UInt(16.W)))
  val element32 = Wire(Vec(2,UInt(32.W)))
  val element64 = Wire(Vec(1,UInt(64.W)))

  element8 := src.asTypeOf(element8)
  element16 := src.asTypeOf(element16)
  element32 := src.asTypeOf(element32)
  element64 := src.asTypeOf(element64)

  // todo: i8-> f16 src should get from vreg‘s low64， not vreg(31, 0) and vreg(95, 64)
  val in0 = Mux1H(inputWidth1H, Seq(element8(0), element16(0), element32(0), element64(0)))
  val in1 = Mux1H(inputWidth1H, Seq(element8(1), element16(1), element32(1), 0.U))// input 0=> result 0 while norrow eg. 64b->32b
  val in2 = Mux1H(inputWidth1H, Seq(element8(2), element16(2), 0.U, 0.U))
  val in3 = Mux1H(inputWidth1H, Seq(element8(3), element16(3), 0.U, 0.U))

  dontTouch(in0)

  val (result0, fflags0) = VCVT(64)(in0, opType, sew, rm, input1H, output1H)
  val (result1, fflags1) = VCVT(32)(in1, opType, sew, rm, input1H, output1H) // todo: replace 32
  val (result2, fflags2) = VCVT(16)(in2, opType, sew, rm, input1H, output1H) // todo: replace 16
  val (result3, fflags3) = VCVT(16)(in3, opType, sew, rm, input1H, output1H) // todo: replace 16

  io.result := Mux1H(outputWidth1H, Seq(
    result3(7,0) ## result2(7,0) ## result1(7,0) ## result0(7,0),
    result3(15,0) ## result2(15,0) ## result1(15,0) ## result0(15,0),
    result1(31,0) ## result0(31,0),
    result0
  ))

  io.fflags := Mux1H(outputWidth1H, Seq(
    fflags3 ## fflags2 ## fflags1 ## fflags0,
    fflags3 ## fflags2 ## fflags1 ## fflags0,
    fflags1 ## fflags0,
    fflags0
  ))
}