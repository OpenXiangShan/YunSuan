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
  // vector float adder
  val vfa_vsrc = Vec(3, Vec(VLEN/XLEN, UInt(XLEN.W)))
  for(j <- 0 until VLEN/XLEN){
    vfa_vsrc(0)(j) := io.in.vSrc(0)(XLEN*(j+1)-1, XLEN*j)
  }
  for(j <- 0 until VLEN/XLEN){
    vfa_vsrc(1)(j) := io.in.vSrc(1)(XLEN*(j+1)-1, XLEN*j)
  }
  for(j <- 0 until VLEN/XLEN){
    vfa_vsrc(2)(j) := io.in.vSrc(3)(XLEN*(j+1)-1, XLEN*j)
  }

  val sew = "b01".U
  for (i <- 0 until (VLEN / XLEN)) {
    val (src1, src2, src3) = (vfa_vsrc(0)(i), vfa_vsrc(1)(i), vfa_vsrc(2)(i))
    val vfa = Module(new VectorFloatAdder) // result at next cycle}
    require(vfa.io.fp_a.getWidth == XLEN)
    vfa.io.fire := busy
    vfa.io.fp_a := src1
    vfa.io.fp_b := src2
    vfa.io.widen_a := Cat(vfa_vsrc(0)(1)(31+i*32,0+i*32),vfa_vsrc(0)(0)(31+i*32,0+i*32))
    vfa.io.widen_b := Cat(vfa_vsrc(1)(1)(31+i*32,0+i*32),vfa_vsrc(1)(0)(31+i*32,0+i*32))
    vfa.io.frs1  := vfa_vsrc(1)(0) // VS1(63,0)
    vfa.io.fp_b := src2
    // TODO: change mask
    val maskTemp = Cat(src3(48),src3(32),src3(16),src3(0))
    vfa.io.mask := Mux1H(
      Seq(
        (sew === 1.U) -> maskTemp,
        (sew === 2.U) -> Cat(maskTemp(2),maskTemp(0)),
        (sew === 3.U) -> maskTemp(0)
      )
    )
    vfa.io.uop_idx := uop_idx(0)
    // TODO: which module to handle dest's original value
    vfa.io.round_mode   := rm
    vfa.io.fp_format    := sew
    vfa.io.opb_widening := src_widen
    vfa.io.res_widening := widen
    vfa.io.is_frs1      := is_frs1
    vfa.io.op_code      := opcode
    vfa.io.is_vec       := true.B // TODO: check it
    vfa.io.fp_aIsFpCanonicalNAN := false.B
    vfa.io.fp_bIsFpCanonicalNAN := false.B
    vfa.io.maskForReduction := 0.U
    vfa.io.is_vfwredosum := false.B
    vfa.io.is_fold := 0.U
    vfa.io.vs2_fold := Cat(in.src(0)(1), in.src(0)(0))
    vfa_result.result(i) := vfa.io.fp_result
    vfa_result.fflags(i) := vfa.io.fflags
    vfa_result.vxsat := 0.U // DontCare
    
  }

}