package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import yunsuan.util._
import yunsuan.encoding.Opcode.Opcodes.FCvtOpcode
import yunsuan.vector.Common._

class VectorCvtIO(width: Int) extends Bundle {
  val fire     = Input(Bool())
  val src      = Input(UInt(width.W))
  val opType   = Input(FCvtOpcode())
  val rm       = Input(Frm())
  val inSew1H  = Input(Sew())
  val outSew1H = Input(Sew())

  val result = Output(UInt(width.W))
  val fflags = Output(Vec(4, Fflags()))
}

class VectorCvt(xlen :Int) extends Module{

  val io = IO(new VectorCvtIO(xlen))
  val (fire, src, opType, rm, inSew1H, outSew1H) = (io.fire, io.src, io.opType, io.rm, io.inSew1H, io.outSew1H)

  dontTouch(inSew1H)
  dontTouch(outSew1H)

  val outSew1HDelay2 = RegEnable(RegEnable(outSew1H, fire), GatedValidRegNext(fire))

  val element8 = Wire(Vec(8,UInt(8.W)))
  val element16 = Wire(Vec(4,UInt(16.W)))
  val element32 = Wire(Vec(2,UInt(32.W)))
  val element64 = Wire(Vec(1,UInt(64.W)))

  element8 := src.asTypeOf(element8)
  element16 := src.asTypeOf(element16)
  element32 := src.asTypeOf(element32)
  element64 := src.asTypeOf(element64)

  val in0 = element64(0)
  val in1 = Mux1H(inSew1H, Seq(element8(1), element16(1), element32(1), 0.U))// input 0=> result 0 while norrow eg. 64b->32b
  val in2 = Mux1H(inSew1H, Seq(element8(2), element16(2), 0.U, 0.U))
  val in3 = Mux1H(inSew1H, Seq(element8(3), element16(3), 0.U, 0.U))


  val (result0, fflags0) = VCVT(64)(fire, in0, opType, rm, inSew1H, outSew1H, false.B)
  val (result1, fflags1) = VCVT(32)(fire, in1, opType, rm, inSew1H, outSew1H, false.B)
  val (result2, fflags2) = VCVT(16)(fire, in2, opType, rm, inSew1H, outSew1H, false.B)
  val (result3, fflags3) = VCVT(16)(fire, in3, opType, rm, inSew1H, outSew1H, false.B)

  io.result := Mux1H(outSew1HDelay2, Seq(
    result3(7,0) ## result2(7,0) ## result1(7,0) ## result0(7,0),
    result3(15,0) ## result2(15,0) ## result1(15,0) ## result0(15,0),
    result1(31,0) ## result0(31,0),
    result0
  ))

  val fflags = Mux1H(outSew1HDelay2, Seq(
    fflags3 ## fflags2 ## fflags1 ## fflags0,
    fflags3 ## fflags2 ## fflags1 ## fflags0,
    fflags1 ## fflags0,
    fflags0
  ))

  for (i <- 0 until 4) {
    io.fflags(i) := fflags(Fflags.width * (i + 1) - 1, Fflags.width * i)
  }
}
