package race.vpu.yunsuan

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._

import race.vpu._
import race.vpu.yunsuan._
import race.vpu.yunsuan.VectorConvert._
import race.vpu.yunsuan.util._
import race.vpu.VParams._
import _root_.yunsuan.encoding.Opcode.VialuOpcode.vctz
import _root_.yunsuan.util.LookupTreeDefault

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
  val io = IO(new Bundle() {
      val fire              = Input(Bool())
      val vs2               = Input(UInt(LaneWidth.W))
      val op_code           = Input(UInt(8.W))
      val sew               = Input(UInt(2.W))
      val rm                = Input(UInt(3.W))
      val uop_idx           = Input(Bool())
      val isFpToVecInst     = Input(Bool())
      val isFround          = Input(UInt(2.W))
      val isFcvtmod         = Input(Bool())
      val in_uop            = Input(new VUop)

      val result            = Output(UInt(LaneWidth.W))
      val fflags            = Output(Vec(LaneWidth/16, UInt(5.W)))
      val out_uop           = ValidIO(new VUop)
  })

  val widen  = io.op_code(4, 3)
  val is_widen = widen === 1.U

  val vcvt   = Module(new VectorCvt(LaneWidth))

  // val fp_format = LookupTreeDefault(widen, io.sew, List(
  //   1.U -> io.sew,
  //   2.U -> io.sew,
  // ))
  val fp_format = io.sew

  // connect vcvt's io
  // input width 8， 16， 32， 64
  val element64 = Wire(Vec((LaneWidth/64), UInt(64.W)))
  val element32 = Wire(Vec((LaneWidth/32), UInt(32.W)))
  val element16 = Wire(Vec((LaneWidth/16), UInt(16.W)))
  val element8  = Wire(Vec((LaneWidth/8), UInt(8.W)))
  element64 := io.vs2.asTypeOf(element64)
  element32 := io.vs2.asTypeOf(element32)
  element16 := io.vs2.asTypeOf(element16)
  element8  := io.vs2.asTypeOf(element8)

  // if widen
  val src =  Mux(io.uop_idx && is_widen, Cat(element32(0), element32(1)), io.vs2) //TODO: widen or narrow
  vcvt.io.fire          := io.fire
  vcvt.io.sew           := fp_format
  vcvt.io.opType        := io.op_code
  vcvt.io.rm            := io.rm
  vcvt.io.src           := src // 128 bit->vcvt
  vcvt.io.isFpToVecInst := false.B
  vcvt.io.isFround      := 0.U
  vcvt.io.isFcvtmod     := false.B
  
  // latency = 2
  val reg_valid_0 = RegNext(io.fire)
  val reg_valid_1 = RegNext(reg_valid_0)

  val reg_uop_0   = RegEnable(io.in_uop, io.fire)
  val reg_uop_1   = RegEnable(reg_uop_0, reg_valid_0)

  val reg_widen_0 = RegEnable(widen, io.fire)
  val reg_widen_1 = RegEnable(reg_widen_0, reg_valid_0)

  // // if narrow need one more latency

  val widen_or_single = reg_valid_1 && (reg_widen_1 === 1.U || reg_widen_1 === 0.U)
  val narrow          = reg_valid_1 && (reg_widen_1 === 2.U)
  // val vd_narrow       = Cat(reg_narrow_vd, vcvt.io.result((LaneWidth/2), 0)).asTypeOf(UInt(LaneWidth.W))
  // val fflags_narrow   = reg_narrow_fflag | vcvt.io.fflags

  val narrow_result_half = RegEnable(vcvt.io.result((LaneWidth/2)-1, 0), reg_valid_1 && (reg_uop_1.uopIdx === 0.U))
  val narrow_result      = Cat(vcvt.io.result((LaneWidth/2)-1, 0), narrow_result_half)
  val fflags_narrow_half = RegEnable(vcvt.io.fflags, io.fire)
  val narrow_fflags = fflags_narrow_half | vcvt.io.fflags

  io.out_uop.valid  := Mux(widen_or_single, reg_valid_1,
                        Mux(narrow && (reg_uop_1.uopIdx === 1.U), true.B, false.B))

  io.out_uop.bits   := reg_uop_1

  io.result         := Mux(widen_or_single, vcvt.io.result, 
                        Mux(narrow, narrow_result, 0.U))
  io.fflags         := Mux(widen_or_single, vcvt.io.fflags.asTypeOf(io.fflags), 
                        Mux(narrow, narrow_fflags.asTypeOf(io.fflags), 0.U.asTypeOf(io.fflags)))
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


  val element8  = Wire(Vec(8,UInt(8.W)))
  val element16 = Wire(Vec(4,UInt(16.W)))
  val element32 = Wire(Vec(2,UInt(32.W)))
  val element64 = Wire(Vec(1,UInt(64.W)))

  element8  := src.asTypeOf(element8)
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