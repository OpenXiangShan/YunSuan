package race.vpu.exu.laneexu

import chisel3._
import chisel3.util._
import race.vpu._
import race.vpu.yunsuan._
import VParams._
import race.vpu.exu.laneexu.alu._

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
  val in = io.in.bits

  val vfadd_out = Wire(new LaneOutput)
  val vfma_out = Wire(new LaneOutput)
  val vcvt_out = Wire(new LaneOutput)
  val alu_out = Wire(new LaneOutput)

  val alu = Module(new LaneALU)
  alu.io.in.valid := io.in.valid && io.in.bits.uop.ctrl.alu
  alu.io.in.bits := io.in.bits

  alu_out := alu.io.out.bits

  val vfadd = Module(new VectorExuFloatAdder)
  val vfadd_op = Wire(new Vfa_setuop)

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
  vfadd.io.opb_widening := in.uop.ctrl.widen2 // TODO: 
  vfadd.io.res_widening := in.uop.ctrl.widen // TODO: 
  vfadd.io.op_code := vfadd_op.op
  vfadd.io.is_vec := true.B
  vfadd.io.fp_aIsFpCanonicalNAN := false.B
  vfadd.io.fp_bIsFpCanonicalNAN := false.B
  vfadd.io.maskForReduction := 0.U
  vfadd.io.is_vfwredosum := false.B
  vfadd.io.is_fold := 0.U
  vfadd.io.is_vm := in.uop.ctrl.vm
  vfadd_out.vd := vfadd.io.result
  vfadd_out.fflags := vfadd.io.fflags
  vfadd_out.uop := vfadd.io.out_uop.bits


  val vfma = Module(new VectorExuFloatFMA)
  val vfma_op = Wire(new Vfma_setuop)

  vfma_op.funct  := Cat(in.uop.ctrl.funct6, in.uop.ctrl.funct3)
  vfma_op.vm     := in.uop.ctrl.vm
  vfma_op.vs1    := in.uop.ctrl.lsrc(0)
  vfma_op.vs2    := in.uop.ctrl.lsrc(1)
  vfma_op.op     := vfma_op.op_gen

  vfma.io.fire := io.in.valid && in.uop.ctrl.vfma 
  vfma.io.in_uop := in.uop
  vfma.io.vs1  := in.vs1
  vfma.io.vs2  := in.vs2
  vfma.io.vs3  := in.vs3 
  vfma.io.uop_idx := in.uop.uopIdx 
  vfma.io.round_mode := in.uop.csr.frm
  vfma.io.fp_format := in.uop.csr.vsew(1, 0)
  vfma.io.op_code := vfma_op.op

  vfma.io.frs1 := in.rs1
  vfma.io.is_vec := true.B
  vfma.io.is_frs1 := in.uop.ctrl.vx
  vfma.io.res_widening := in.uop.ctrl.widen  
  
  vfma_out.vd := vfma.io.result
  vfma_out.fflags := vfma.io.fflags
  vfma_out.uop := vfma.io.out_uop.bits

  val vcvt = Module(new VectorExuCvt)
  val vcvt_setuop = Wire(new Vcvt_setuop)

  vcvt_setuop.funct := Cat(in.uop.ctrl.funct6, in.uop.ctrl.funct3)
  vcvt_setuop.vm := in.uop.ctrl.vm
  vcvt_setuop.vs1 := in.uop.ctrl.lsrc(0)
  vcvt_setuop.vs2 := in.uop.ctrl.lsrc(1)
  vcvt_setuop.op := vcvt_setuop.op_gen

  vcvt.io.fire := io.in.valid && in.uop.ctrl.vfcvt 
  vcvt.io.in_uop := in.uop
  vcvt.io.vs2  := in.vs2
  vcvt.io.uop_idx := in.uop.uopIdx 
  vcvt.io.op_code := vcvt_setuop.op
  vcvt.io.sew := in.uop.csr.vsew(1, 0)
  vcvt.io.rm := in.uop.csr.frm
  vcvt.io.isFpToVecInst := false.B
  vcvt.io.isFround := 0.U
  vcvt.io.isFcvtmod := false.B
  
  vcvt_out.vd     := vcvt.io.result
  vcvt_out.fflags := vcvt.io.fflags
  vcvt_out.uop    := vcvt.io.out_uop.bits

  io.out.valid := vfadd.io.out_uop.valid | vfma.io.out_uop.valid | vcvt.io.out_uop.valid | alu.io.out.valid  
            
  io.out.bits := Mux1H(
    Seq(
      vfadd.io.out_uop.valid      -> vfadd_out,
      vfma.io.out_uop.valid      -> vfma_out,
      vcvt.io.out_uop.valid     -> vcvt_out,
      alu.io.out.valid          -> alu_out
    )
  )

}