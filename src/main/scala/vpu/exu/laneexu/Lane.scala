package race.vpu.exu.laneexu

import chisel3._
import chisel3.util._
import race.vpu._
import race.vpu.yunsuan._
import VParams._
import race.vpu.exu.laneexu.alu._
import race.vpu.exu.laneexu.fp._

class LaneInput extends Bundle {
  val uop = new VUop
  val vs1 = UInt(LaneWidth.W)
  val vs2 = UInt(LaneWidth.W)
  val vs3 = UInt(LaneWidth.W)
  val rs1 = UInt(xLen.W)
//   val prestart = UInt(NByteLane.W)
//   val mask = UInt(NByteLane.W)
//   val tail = UInt(NByteLane.W)
}

class LaneOutput extends Bundle {
  val uop = new VUop
  val vd = UInt(LaneWidth.W)
  val fflags = Vec(LaneWidth/16, UInt(5.W)) // For eew=32, fflags valid pattern is 0101
//   val vxsat = Bool() // Fixed-point accrued saturation flag
}

class Lane extends Module {
  val io = IO(new Bundle {
    val in = Input(ValidIO(new LaneInput))
    val out = ValidIO(new LaneOutput)
  })
  
  val alu = Module(new LaneALU)
  val vfadd = Module(new LaneFloatAdder)
  val vfma = Module(new LaneFMA)
  val vfcvt = Module(new LaneFloatCvt)
  
  val in = io.in.bits
  val sewFpIn = SewFpOH(in.uop.csr.vsew)
  val alu_out, vfadd_out, vfma_out, vfcvt_out = Wire(new LaneOutput)
   
  //---- ALU ----
  alu.io.in.valid := io.in.valid && io.in.bits.uop.ctrl.alu
  alu.io.in.bits := io.in.bits
  alu_out := alu.io.out.bits

  //---- FADD ----
  val vfadd_op = Wire(new Vfadd_setop)
  vfadd_op.funct := Cat(in.uop.ctrl.funct6, in.uop.ctrl.funct3)
  vfadd_op.vm := in.uop.ctrl.vm
  vfadd_op.vs1 := in.uop.ctrl.lsrc(0)
  vfadd_op.vs2 := in.uop.ctrl.lsrc(1)
  vfadd_op.op := vfadd_op.op_gen

  vfadd.io.fire := io.in.valid && in.uop.ctrl.vfa
  vfadd.io.in_uop := in.uop
  vfadd.io.vs1  := in.vs1
  vfadd.io.vs2  := in.vs2
  vfadd.io.frs1 := in.rs1
  vfadd.io.is_frs1 := in.uop.ctrl.vx
  vfadd.io.mask := 0.U // TODO: check it
  vfadd.io.uop_idx := in.uop.uopIdx 
  vfadd.io.round_mode := in.uop.csr.frm 
  vfadd.io.fp_format := in.uop.csr.vsew(1, 0)
  vfadd.io.opb_widening := in.uop.ctrl.widen2
  vfadd.io.res_widening := in.uop.ctrl.widen || in.uop.ctrl.widen2
  vfadd.io.op_code := vfadd_op.op
  vfadd.io.is_vm := in.uop.ctrl.vm

  vfadd_out.vd := vfadd.io.result
  vfadd_out.fflags := vfadd.io.fflags
  vfadd_out.uop := vfadd.io.out_uop.bits

  //---- FMA ----
  vfma.io.in.valid := io.in.valid && io.in.bits.uop.ctrl.vfma
  vfma.io.in.bits := io.in.bits
  vfma.io.sewIn := sewFpIn
  vfma_out := vfma.io.out.bits

  //---- FCVT ----
  val vfcvt_setop = Wire(new Vfcvt_setop)
  vfcvt_setop.funct := Cat(in.uop.ctrl.funct6, in.uop.ctrl.funct3)
  vfcvt_setop.vm := in.uop.ctrl.vm
  vfcvt_setop.vs1 := in.uop.ctrl.lsrc(0)
  vfcvt_setop.vs2 := in.uop.ctrl.lsrc(1)
  vfcvt_setop.op := vfcvt_setop.op_gen

  vfcvt.io.fire := io.in.valid && in.uop.ctrl.vfcvt 
  vfcvt.io.in_uop := in.uop
  vfcvt.io.vs2  := in.vs2
  vfcvt.io.uop_idx := in.uop.uopIdx 
  vfcvt.io.op_code := vfcvt_setop.op
  vfcvt.io.sew := in.uop.csr.vsew(1, 0)
  vfcvt.io.rm := in.uop.csr.frm
  vfcvt.io.isFpToVecInst := false.B
  vfcvt.io.isFround := 0.U
  vfcvt.io.isFcvtmod := false.B
  
  vfcvt_out.vd     := vfcvt.io.result
  vfcvt_out.fflags := vfcvt.io.fflags
  vfcvt_out.uop    := vfcvt.io.out_uop.bits


  io.out.valid := alu.io.out.valid || vfadd.io.out_uop.valid || vfma.io.out.valid || vfcvt.io.out_uop.valid
  io.out.bits := Mux1H(
    Seq(
      alu.io.out.valid          -> alu_out,
      vfadd.io.out_uop.valid    -> vfadd_out,
      vfma.io.out.valid         -> vfma_out,
      vfcvt.io.out_uop.valid    -> vfcvt_out
    )
  )
}