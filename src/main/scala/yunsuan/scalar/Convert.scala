package yunsuan.scalar

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import yunsuan.util._
import yunsuan.vector.VectorConvert.CVT64

// Scalar Int to Float Convert.
class I2fCvtIO extends Bundle{
  val src = Input(UInt(64.W))
  val opType = Input(UInt(5.W))
  val rm = Input(UInt(3.W))
  val wflags = Input(Bool())
  val rmInst = Input(UInt(3.W))

  val result = Output(UInt(64.W))
  val fflags = Output(UInt(5.W))
}
class INT2FP(latency: Int, XLEN: Int) extends Module {
  val io = IO(new I2fCvtIO)
  val rm = Mux(io.rmInst === "b111".U, io.rm, io.rmInst)
  val regEnables = IO(Input(Vec(latency, Bool())))
  dontTouch(regEnables)
  // stage1
  val in = io.src
  val wflags = io.wflags
  val typeIn = io.opType(3)
  val typeOut = io.opType(2,1)
  val signIn = io.opType(0)
  val intValue = RegEnable(Mux(wflags,
    Mux(typeIn,
      Mux(!signIn, ZeroExt(in, XLEN), SignExt(in, XLEN)),
      Mux(!signIn, ZeroExt(in(31, 0), XLEN), SignExt(in(31, 0), XLEN))
    ),
    in
  ), regEnables(0))
  val typeInReg = RegEnable(typeIn, regEnables(0))
  val typeOutReg = RegEnable(typeOut, regEnables(0))
  val signInReg = RegEnable(signIn, regEnables(0))
  val wflagsReg = RegEnable(wflags, regEnables(0))
  val rmReg = RegEnable(rm, regEnables(0))

  // stage2
  val s2_typeInReg = typeInReg
  val s2_signInReg = signInReg
  val s2_typeOutReg = typeOutReg
  val s2_wflags = wflagsReg
  val s2_rmReg = rmReg

  val mux = Wire(new Bundle() {
    val data = UInt(XLEN.W)
    val exc = UInt(5.W)
  })

  mux.data := intValue
  mux.exc := 0.U

  when(s2_wflags){
    val i2fResults = for(t <- FPU.ftypes) yield {
      val i2f = Module(new IntToFP(t.expWidth, t.precision))
      i2f.io.sign := s2_signInReg
      i2f.io.long := s2_typeInReg
      i2f.io.int := intValue
      i2f.io.rm := s2_rmReg
      (i2f.io.result, i2f.io.fflags)
    }
    val (data, exc) = i2fResults.unzip
    mux.data := VecInit(data)(s2_typeOutReg)
    mux.exc := VecInit(exc)(s2_typeOutReg)
  }

  // stage 3
  val s3_out = RegEnable(mux, regEnables(1))
  val s3_tag = RegEnable(s2_typeOutReg, regEnables(1))

  io.fflags := s3_out.exc
  io.result := FPU.box(s3_out.data, s3_tag)
}
// Scalar Float to Int or Float Convert.
class FpCvtIO(width: Int) extends Bundle {
  val fire = Input(Bool())
  val src = Input(UInt(width.W))
  val opType = Input(UInt(8.W))
  val sew = Input(UInt(2.W))
  val rm = Input(UInt(3.W))
  val isFpToVecInst = Input(Bool())
  val isFround = Input(UInt(2.W))
  val isFcvtmod = Input(Bool())

  val result = Output(UInt(width.W))
  val fflags = Output(UInt(5.W))
}
class FPCVT(xlen :Int) extends Module{
  val io = IO(new FpCvtIO(xlen))
  val (opType, sew) = (io.opType, io.sew)
  val widen = opType(4, 3) // 0->single 1->widen 2->norrow => width of result
  // input width 8， 16， 32， 64
  val input1H = Wire(UInt(4.W))
  input1H := chisel3.util.experimental.decode.decoder(
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

        BitPat("b11_01") -> BitPat("b0010"), // f16->f64/i64/ui64
        BitPat("b11_11") -> BitPat("b1000"), // f64->f16
      ),
      BitPat("b0000")
    )
  )
  // output width 8， 16， 32， 64
  val output1H = Wire(UInt(4.W))
  output1H := chisel3.util.experimental.decode.decoder(
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

        BitPat("b11_11") -> BitPat("b0010"), // f64->f16
        BitPat("b11_01") -> BitPat("b1000"), // f16->f64/i64/ui64
      ),
      BitPat("b0000")
    )
  )
  dontTouch(input1H)
  dontTouch(output1H)
  val fcvt = Module(new CVT64(xlen, isVectorCvt=false))
  fcvt.io.sew := io.sew
  fcvt.io.fire := io.fire
  fcvt.io.src := io.src
  fcvt.io.rm := io.rm
  fcvt.io.opType := io.opType
  fcvt.io.rm := io.rm
  fcvt.io.isFpToVecInst := io.isFpToVecInst
  fcvt.io.isFround := io.isFround
  fcvt.io.isFcvtmod := io.isFcvtmod
  fcvt.io.input1H := input1H
  fcvt.io.output1H := output1H

  io.fflags := fcvt.io.fflags
  io.result := fcvt.io.result

}
