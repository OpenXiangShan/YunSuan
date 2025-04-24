package race.vpu.yunsuan

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._

import race.vpu._
import race.vpu.yunsuan._
import race.vpu.yunsuan.VectorConvert._
import race.vpu.yunsuan.util._
import race.vpu.VParams._

class VectorCvtIO(width: Int) extends Bundle {
  val fire = Input(Bool())
  val src = Input(UInt(width.W))
  val opType = Input(UInt(8.W))
  val sew = Input(UInt(2.W))
  val rm = Input(UInt(3.W))
  val isFpToVecInst = Input(Bool())
  val isFround = Input(UInt(2.W))
  val isFcvtmod = Input(Bool())
  val result = Output(UInt(width.W))
  val fflags = Output(UInt(20.W))
}

class VectorExuCvt extends  Module {
  val XLEN = 32
  val io = IO(new Bundle() {
      val fire              = Input(Bool())
      val vs2               = Input(UInt(VLEN.W))
      val op_code           = Input(UInt(8.W))
      val sew               = Input(UInt(2.W))
      val rm                = Input(UInt(3.W))
      val isFpToVecInst     = Input(Bool())
      val isFround          = Input(UInt(2.W))
      val isFcvtmod         = Input(Bool())
      val in_uop            = Input(new VUop)

      val result            = Output(UInt(VLEN.W))
      val fflags            = Output(UInt(5.W))
      val out_uop           = ValidIO(new VUop)
  })
  
  val result = Wire(Vec(VLEN/XLEN, UInt(XLEN.W)))
  val fflags = Wire(Vec(VLEN/XLEN, Vec(XLEN/16, UInt(5.W))))

  for (i <- 0 until (VLEN/XLEN)) {
    val vcvt = Module(new VectorCvt(64))
    // connect vcvt's io
    val src =  io.vs2(XLEN-1+i*XLEN, 0+i*XLEN) ## 0.U(32.W) 
    vcvt.io.fire          := io.fire
    vcvt.io.sew           := io.sew
    vcvt.io.opType        := io.op_code
    vcvt.io.rm            := io.rm
    vcvt.io.src           := src // 128 bit->vcvt
    vcvt.io.isFpToVecInst := false.B
    vcvt.io.isFround      := 0.U
    vcvt.io.isFcvtmod     := false.B
    result(i) := vcvt.io.result(63, 32)
    for (j <- 0 until XLEN / 16) {
      fflags(i)(j) := vcvt.io.fflags(5 * (j + 1) - 1, 5 * j)  
    }
  }

  io.result := Cat(result.reverse)  
  io.fflags := fflags.flatMap(_.toSeq).reduce(_ | _)
  
    
  // latency = 2
  val reg_valid_0 = RegNext(io.fire)
  val reg_valid_1 = RegNext(reg_valid_0)
  val reg_uop_0     = RegEnable(io.in_uop, io.fire)
  val reg_uop_1     = RegEnable(reg_uop_0, reg_valid_0)

  io.out_uop.valid := reg_valid_1
  io.out_uop.bits := reg_uop_1
}


class VectorCvt(xlen :Int) extends Module{

  val io = IO(new VectorCvtIO(xlen))
  val (fire, src, opType, sew, rm, isFpToVecInst, isFround, isFcvtmod) = (io.fire, io.src, io.opType, io.sew, io.rm, io.isFpToVecInst, io.isFround, io.isFcvtmod)
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
      ),
      BitPat("b0000")
    )
  )
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