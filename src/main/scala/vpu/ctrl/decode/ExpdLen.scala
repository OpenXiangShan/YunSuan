package race.vpu.ctrl

import chisel3._
import chisel3.util._
import race.vpu.VParams._
import race.vpu._

class ExpdInfo extends Bundle {
  val expdLen = UInt(4.W)
  val ldstCtrl = new LdstCtrl
  val veew_minus_vsew = UInt(3.W)
  val mask_onlyOneReg = Bool()
  val gather16 = Bool()
  val expdLen_indexVd = UInt(4.W)
}

class ExpdLen extends Module {
  val io = IO(new Bundle {
    val in = Input(new VMacroOp)
    val out = Output(new ExpdInfo)
  })

  val ctrl = io.in.ctrl
  val info = io.in.csr

  /**
    * Calculate expanded length
    */
  val expdLen = Wire(UInt(4.W))
  val veewVd = io.in.veewVd
  val emulVd = io.in.emulVd
  val emulVs2 = io.in.emulVs2
  val lmul = Wire(UInt(4.W))
  val veew_minus_vsew = Wire(UInt(3.W))
  val ldstCtrl = Wire(new LdstCtrl)
  val expdLen_indexVd = Wire(UInt(4.W))
  
  lmul := Vlmul_to_lmul(info.vlmul)
  veew_minus_vsew := veewVd - info.vsew
  ldstCtrl := LdstDecoder(ctrl.funct6, ctrl.lsrc(1))  
  val nf_ldst = Wire(UInt(4.W))
  nf_ldst := ctrl.funct6(5,3) +& 1.U
  // For ld/st indexed and segment-indexed instrns, the final expdLen is the larger of data and indices 
  val expdLen_ldst = Mux(ldstCtrl.wholeReg, emulVd,
                      Mux(ldstCtrl.indexed, Mux(veew_minus_vsew(2), lmul, emulVd),
                          emulVd))
  val expdLen_segVd = Wire(UInt(4.W))
  expdLen_segVd := lmul * nf_ldst
  val expdLen_segVs2 = emulVs2
  val expdLen_seg = Mux(expdLen_segVd > expdLen_segVs2, expdLen_segVd, expdLen_segVs2)
  // For ld/st indexed and segment-indexed instrns, the pdestVal should stop at expd_len of vd
  expdLen_indexVd := Mux(ldstCtrl.segment, expdLen_segVd, emulVd)
  val perm_vmv_vfmv = ctrl.alu && !ctrl.opi && ctrl.funct6 === "b010000".U
                                              // mask   excludes viota/vid
  val mask_onlyOneReg = ctrl.mask && !(ctrl.funct6(3, 2) === "b01".U && ctrl.lsrc(0)(4))
  val gather16 = ctrl.funct6 === "b001110".U && ctrl.funct3 === 0.U
  //---- expdLen ----
  when (ctrl.isLdst && !ldstCtrl.mask) {
      expdLen := Mux(ldstCtrl.segment, expdLen_seg, expdLen_ldst) 
  }.elsewhen ((ldstCtrl.mask && ctrl.isLdst) || perm_vmv_vfmv || mask_onlyOneReg) {
      expdLen := 1.U
  }.elsewhen (ctrl.widen && !ctrl.redu || ctrl.widen2 || ctrl.narrow || gather16 && info.vsew === 0.U) {
      expdLen := Mux(info.vlmul(2), 1.U, lmul << 1)  // If lmul < 1, expdLen = 1 for widen/narrow
  }.elsewhen (ctrl.funct6 === "b100111".U && ctrl.funct3 === "b011".U) {//Whole register move
      expdLen := ctrl.lsrc(0)(2, 0) +& 1.U
  }.otherwise {
      expdLen := lmul
  }

  io.out.expdLen := expdLen
  io.out.ldstCtrl := ldstCtrl
  io.out.veew_minus_vsew := veew_minus_vsew
  io.out.mask_onlyOneReg := mask_onlyOneReg
  io.out.gather16 := gather16
  io.out.expdLen_indexVd := expdLen_indexVd
}