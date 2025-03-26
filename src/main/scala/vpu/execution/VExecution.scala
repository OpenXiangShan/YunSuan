package race.vpu.Vexecution

import chisel3._
import chisel3.util._
import race.vpu.yunsuan._
import race.vpu._
import VParams._

class VExecution extends Module {
  val io = IO(new Bundle {
    val in = Input(ValidIO(new VExuInput))
    val out = ValidIO(new VExuOutput)
  })

  val in = io.in.bits
  // output types
  val vfa_out = Wire(new VExuOutput)
  val vfd_out = Wire(new VExuOutput)
  val vff_out = Wire(new VExuOutput)
  val vcvt_out = Wire(new VExuOutput)
  val vrg_out = Wire(new VExuOutput)
  val vfred_out = Wire(new VExuOutput)

  // vector float adder
  val vfa = Module(new VectorExuFloatAdder)
  val vfa_op = Wire(new Vfa_setuop)

  vfa_op.funct := Cat(in.uop.ctrl.funct6, in.uop.ctrl.funct3)
  vfa_op.vm := in.uop.ctrl.vm
  vfa_op.vs1 := in.uop.ctrl.lsrc(0)
  vfa_op.vs2 := in.uop.ctrl.lsrc(1)

  vfa.io.fire := io.in.valid & in.uop.ctrl.vfa
  vfa.io.in_uop := in.uop
  vfa.io.vs1  := in.vSrc(0)
  vfa.io.vs2  := in.vSrc(1)
  vfa.io.frs1 := in.rs1
  vfa.io.is_frs1 := in.uop.ctrl.vx
  vfa.io.mask := in.vSrc(3)
  vfa.io.uop_idx := in.uop.uopIdx // TODO: check it
  vfa.io.round_mode := in.uop.csr.frm //TODO: check it
  vfa.io.fp_format := in.uop.csr.vsew(1, 0)
  vfa.io.opb_widening := in.uop.ctrl.widen //
  vfa.io.res_widening := in.uop.ctrl.widen2 //
  vfa.io.op_code := vfa_op.op
  vfa.io.is_vec := in.uop.ctrl.vv
  vfa.io.fp_aIsFpCanonicalNAN := false.B
  vfa.io.fp_bIsFpCanonicalNAN := false.B
  vfa.io.maskForReduction := 0.U
  vfa.io.is_vfwredosum := false.B
  vfa.io.is_fold := 0.U
  vfa.io.vs2_fold := 0.U

  vfa_out.vd := vfa.io.result
  vfa_out.fflags := vfa.io.fflags
  vfa_out.uop := vfa.io.out_uop.bits

  val vff = Module(new VectorExuFloatFMA)
  val vff_op = Wire(new Vff_setuop)

  vff_op.funct  := Cat(in.uop.ctrl.funct6, in.uop.ctrl.funct3)
  vff_op.vm     := in.uop.ctrl.vm
  vff_op.vs1    := in.uop.ctrl.lsrc(0)
  vff_op.vs2    := in.uop.ctrl.lsrc(1)

  vff.io.fire := io.in.valid & in.uop.ctrl.vfma 
  vff.io.in_uop := in.uop
  vff.io.vs1  := in.vSrc(0)
  vff.io.vs2  := in.vSrc(1)
  vff.io.vs3  := in.vSrc(2)
  vff.io.uop_idx := in.uop.uopIdx // TODO: check it
  vff.io.round_mode := in.uop.csr.frm //TODO: check it
  vff.io.fp_format := in.uop.csr.vsew(1, 0)
  vff.io.op_code := vff_op.op

  vff.io.frs1 := in.rs1
  vff.io.is_vec := in.uop.ctrl.vv
  vff.io.is_frs1 := in.uop.ctrl.vx
  vff.io.res_widening := in.uop.ctrl.widen2  // TODO: check it
  
  vff.io.fp_aIsFpCanonicalNAN := false.B
  vff.io.fp_bIsFpCanonicalNAN := false.B
  vff.io.fp_cIsFpCanonicalNAN := false.B
  
  vff_out.vd := vff.io.result
  vff_out.fflags := vff.io.fflags
  vff_out.uop := vff.io.out_uop.bits

  // val vfd = Module(new VectorExuFloatDivider)
  // val vfd_setuop = Wire(new Vfd_setuop)

  // vfd_setuop.funct := Cat(in.uop.ctrl.funct6, in.uop.ctrl.funct3)
  // vfd_setuop.vm := in.uop.ctrl.vm
  // vfd_setuop.vs1 := in.uop.ctrl.lsrc(0)
  // vfd_setuop.vs2 := in.uop.ctrl.lsrc(1)

  // vfd.io.fire := io.in.valid & in.uop.ctrl.vfdiv
  // vfd.io.vs1  := in.vSrc(0)
  // vfd.io.vs2  := in.vSrc(1)
  // vfd.io.fp_format := in.uop.csr.vsew(1, 0)
  // vfd.io.is_frs1 := vfd_setuop.is_frs1
  // vfd.io.is_frs2 := vfd_setuop.is_frs2
  // vfd.io.is_sqrt := false.B
  // vfd.io.round_mode := in.uop.csr.frm //TODO: check it
  // vfd.io.op_code := vfd_setuop.op
  // vfd.io.is_vec := in.uop.ctrl.vv
  // vfd.io.fp_aIsFpCanonicalNAN := false.B
  // vfd.io.fp_bIsFpCanonicalNAN := false.B

  // vfd.io.finish_ready_i     := false.B
  // vfd.io.finish_valid_o
  // vfd_out.vd := vfd.io.result
  // vfd_out.fflags := vfd.io.fflags

  val vcvt = Module(new VectorExuCvt)
  val vcvt_setuop = Wire(new Vcvt_setuop)

  vcvt_setuop.funct := Cat(in.uop.ctrl.funct6, in.uop.ctrl.funct3)
  vcvt_setuop.vm := in.uop.ctrl.vm
  vcvt_setuop.vs1 := in.uop.ctrl.lsrc(0)
  vcvt_setuop.vs2 := in.uop.ctrl.lsrc(1)

  vcvt.io.fire := io.in.valid & in.uop.ctrl.vfcvt // need to change
  vcvt.io.in_uop := in.uop
  vcvt.io.vs1  := in.vSrc(0)
  vcvt.io.op_code := vcvt_setuop.op
  vcvt.io.sew := in.uop.csr.vsew(1, 0)
  vcvt.io.rm := in.uop.csr.frm
  vcvt.io.isFpToVecInst := false.B
  vcvt.io.isFround := 0.U
  vcvt.io.isFcvtmod := false.B
  
  vcvt_out.vd := vcvt.io.result
  vcvt_out.fflags := vcvt.io.fflags
  vcvt_out.uop := vcvt.io.out_uop.bits

  // val vrg = Module(new VectorExuReggather)
  // val vrg_setuop = Wire(new Vrg_setuop)

  // vrg_setuop.funct := Cat(in.uop.ctrl.funct6, in.uop.ctrl.funct3)
  // vrg_setuop.vm := in.uop.ctrl.vm
  // vrg_setuop.vs1 := in.uop.ctrl.lsrc(0)
  // vrg_setuop.vs2 := in.uop.ctrl.lsrc(1)

  // vrg.io.fire := 0.U // need to change
  // vrg.io.vs1  := in.vSrc(0)
  // vrg.io.vs2  := in.vSrc(1)
  // vrg.io.old_vd := in.vSrc(2)

  // vrg.io.op_code  := vrg_setuop.op
  // vrg.io.sew      := in.uop.csr.vsew(1, 0)
  // vrg.io.mask     := in.vSrc(3)
  // vrg.io.vm       := in.uop.ctrl.vm
  // vrg_out.vd      := vrg.io.res_vd

  // vrg_out.fflags := 0.U
  
  val vfred = Module(new Vfreduction)
  val vfred_setuop = Wire(new Vfred_setuop)

  vfred_setuop.funct := Cat(in.uop.ctrl.funct6, in.uop.ctrl.funct3)
  vfred_setuop.vm := in.uop.ctrl.vm
  vfred_setuop.vs1 := in.uop.ctrl.lsrc(0)
  vfred_setuop.vs2 := in.uop.ctrl.lsrc(1)

  vfred.io.fire := io.in.valid & in.uop.ctrl.vfred
  vfred.io.in.vs1  := in.vSrc(0)
  vfred.io.in.vs2  := in.vSrc(1)(XLEN-1, 0)

  vfred.io.in.op_code       := vfred_setuop.op
  vfred.io.in.mask          := in.vSrc(3)
  vfred.io.in.uop           := in.uop
  vfred_out.vd              := Cat(Fill((VLEN-XLEN), 0.U), vfred.io.out.bits.result)
  vfred_out.fflags          := vfred.io.out.bits.fflags
  vfred_out.uop             := vfred.io.out.bits.uop

  io.out.valid := vfa.io.out_uop.valid | vff.io.out_uop.valid | vcvt.io.out_uop.valid | vfred.io.out.valid
  
  io.out.bits := Mux1H(
    Seq(
      vfa.io.out_uop.valid      -> vfa_out,
      vff.io.out_uop.valid      -> vff_out,
      vcvt.io.out_uop.valid     -> vcvt_out,
      vfred.io.out.valid        -> vfred_out
    )
  )

}