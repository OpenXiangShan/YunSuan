package race.vpu.Vexecution

import chisel3._
import chisel3.util._
import race.vpu._
import race.vpu.yunsuan._
import VParams._

class VExecution extends Module {
  val io = IO(new Bundle {
    val in = Input(new VExuInput)
    val out = Output(new VExuOutput)
  })

  val in = io.in
  // output types
  val vfa_out = Wire(new VExuOutput)
  val vdiv_out = Wire(new VExuOutput)
  
  // vector float adder
  val vfa = Module(new VectorEXUFloatAdder)
  val vfa_uop = Wire(new Vfa_setuop)
  vfa_uop.funct := Cat(in.vuop.ctrl.funct6, in.vuop.ctrl.funct3)
  vfa_uop.vm := in.vuop.ctrl.vm
  vfa_uop.vs1 := in.vuop.ctrl.lsrc(0)
  vfa_uop.vs2 := in.vuop.ctrl.lsrc(1)

  vfa.io.fire := 0.U // need to change
  vfa.io.vs1  := in.vSrc(0)
  vfa.io.vs2  := in.vSrc(1)
  vfa.io.frs1 := in.rs1
  vfa.io.is_frs1 := in.vuop.ctrl.vx
  vfa.io.mask := in.vSrc(3)
  vfa.io.uop_idx := in.vuop.uopIdx // TODO: check it
  vfa.io.round_mode := in.vuop.csr.frm //TODO: check it
  vfa.io.fp_format := in.vuop.csr.vsew(1, 0)
  vfa.io.opb_widening := in.vuop.ctrl.widen //
  vfa.io.res_widening := in.vuop.ctrl.widen2 //
  vfa.io.op_code := vfa_uop.op
  vfa.io.is_vec := in.vuop.ctrl.vv
  vfa.io.fp_aIsFpCanonicalNAN := false.B
  vfa.io.fp_bIsFpCanonicalNAN := false.B
  vfa.io.maskForReduction := 0.U
  vfa.io.is_vfwredosum := false.B
  vfa.io.is_fold := 0.U
  vfa.io.vs2_fold := 0.U

  vfa_out.vRes := vfa.io.result
  vfa_out.vfflags := vfa.io.fflags

  val futype = Cat(in.vuop.ctrl.vfadd, in.vuop.ctrl.vfma, in.vuop.ctrl.vfdiv, in.vuop.ctrl.vfcvt)

  io.out := Mux1H(futype, 
  Seq(
    vfa_out,
    vdiv_out
  ))
}