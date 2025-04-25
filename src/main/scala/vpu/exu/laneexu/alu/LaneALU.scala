/**
  * Note: 
  *   1) so far, the narrow-to-1 (e.g., compare) and narrow 
  *      instructions are not supported
  *   2) mask and old_vd are not supported 
  */
//TODO: (old_vd is used for compare, fma, ...)
package race.vpu.exu.laneexu.alu

import chisel3._
import chisel3.util._
import race.vpu._
import VParams._
import race.vpu.exu.laneexu._
import chisel3.util.experimental.decode._

class VIntFixpDecode extends Bundle {
  val sub = Bool()
  val misc = Bool()
  val fixp = Bool()
}

class LaneALU extends Module {
  val io = IO(new Bundle {
    val in = Input(ValidIO(new LaneInput))
    val out = ValidIO(new LaneOutput)
  })

  val uop = io.in.bits.uop
  val sew = SewOH(uop.csr.vsew)  // 0:8, 1:16, 2:32, 3:64
  val eewVd = SewOH(uop.veewVd)
  val fire = io.in.valid
  // broadcast rs1 or imm to all elements, assume xLen = 64
  val imm = uop.ctrl.lsrc(0)
  //                            |sign-extend imm to 64 bits|
  val rs1_imm = Mux(uop.ctrl.vi, imm.asSInt.pad(XLEN).asUInt, io.in.bits.rs1)
  val rs1_imm_repeat = Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => Fill(XLEN/n, rs1_imm(n-1, 0))))

  val vs1_rs1_imm = Mux(uop.ctrl.vv, io.in.bits.vs1, rs1_imm_repeat)
  
  // Ctrl signals from alu (funct6) decoder
  val opi = uop.ctrl.funct3(0) === uop.ctrl.funct3(1)
  val vAluDecode = Wire(new VIntFixpDecode)
  val truthTable = TruthTable(VAluTable.table, VAluTable.default)
  val decoderOut = decoder(QMCMinimizer, Cat(uop.ctrl.funct6, opi), truthTable)
  vAluDecode := decoderOut.asTypeOf(new VIntFixpDecode)

  val vIntAdder = Module(new VIntAdder64b)
  vIntAdder.io.funct6 := uop.ctrl.funct6
  vIntAdder.io.vm := uop.ctrl.vm
  vIntAdder.io.ma := uop.csr.ma
  vIntAdder.io.sew := sew
  vIntAdder.io.eewVd := eewVd
  vIntAdder.io.uopIdx := uop.uopIdx
  vIntAdder.io.vs1 := vs1_rs1_imm
  vIntAdder.io.vs2 := io.in.bits.vs2
  vIntAdder.io.oldVd := 0.U  // Not supported
  vIntAdder.io.vmask := 0.U  // So far, mask not supported
  vIntAdder.io.isSub := vAluDecode.sub
  vIntAdder.io.widen := uop.ctrl.widen
  vIntAdder.io.widen2 := uop.ctrl.widen2
  vIntAdder.io.narrow_to_1 := uop.ctrl.narrow_to_1

  val vIntMisc = Module(new VIntMisc64b)
  vIntMisc.io.funct6 := uop.ctrl.funct6
  vIntMisc.io.funct3 := uop.ctrl.funct3
  vIntMisc.io.vi := uop.ctrl.vi
  vIntMisc.io.vm := uop.ctrl.vm
  vIntMisc.io.vs1_imm := uop.ctrl.lsrc(0)
  vIntMisc.io.narrow := uop.ctrl.narrow
  vIntMisc.io.sew := sew
  vIntMisc.io.uopIdx := uop.uopIdx
  vIntMisc.io.vs1 := vs1_rs1_imm
  vIntMisc.io.vs2 := io.in.bits.vs2
  vIntMisc.io.vmask := 0.U  // So far, mask not supported

  val isAluAdder = !vAluDecode.misc && !uop.ctrl.narrow_to_1
  val isAluMisc = vAluDecode.misc && !uop.ctrl.narrow
  val isAluNarrow = uop.ctrl.narrow

  val aluOut = Mux1H(Seq(
    isAluAdder -> vIntAdder.io.vd,
    isAluMisc -> vIntMisc.io.vd
  ))

  io.out.bits.vd := RegEnable(aluOut, fire)
  io.out.bits.uop := RegEnable(uop, fire)
  io.out.bits.fflags := DontCare
  io.out.valid := RegNext(fire)


}