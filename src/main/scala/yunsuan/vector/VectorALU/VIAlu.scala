package yunsuan.vector.alu

import chisel3._
import chisel3.util._
import yunsuan.vector._
import yunsuan.vector.alu.VAluOpcode._

class VIAlu extends Module {
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(new VIFuInput))
    val out = ValidIO(new VIFuOutput)
  })

  // Latency of ALU is 1 cycles plus
  io.out.valid := RegNext(io.in.valid)

  val vReduAlu = Module(new Reduction)
  vReduAlu.io.in := io.in

  val in = Mux(vReduAlu.io.alu_in.valid, vReduAlu.io.alu_in, io.in)
  val srcTypeVs1 = in.bits.srcType(1)
  val srcTypeVs2 = in.bits.srcType(0)
  val vdType = in.bits.vdType
  val eewVs1 = SewOH(srcTypeVs1(1, 0))
  val eewVd = SewOH(vdType(1, 0))
  val mask = in.bits.mask
  val uopIdx = in.bits.info.uopIdx
  val narrow = srcTypeVs2(1, 0) === 3.U && vdType(1, 0) === 2.U ||
    srcTypeVs2(1, 0) === 2.U && vdType(1, 0) === 1.U ||
    srcTypeVs2(1, 0) === 1.U && vdType(1, 0) === 0.U
  val vstart_gte_vl = in.bits.info.vstart >= in.bits.info.vl

  val vIntFixpAlu = Module(new VIntFixpAlu)
  vIntFixpAlu.io.in.opcode := in.bits.opcode
  vIntFixpAlu.io.in.info := in.bits.info
  vIntFixpAlu.io.in.srcType := in.bits.srcType
  vIntFixpAlu.io.in.vdType := in.bits.vdType
  vIntFixpAlu.io.in.vs1 := in.bits.vs1
  vIntFixpAlu.io.in.vs2 := in.bits.vs2
  vIntFixpAlu.io.in.old_vd := in.bits.old_vd
  val eewVm = Mux(vdType === 15.U, eewVs1, eewVd)
  val maskIdx = Mux(narrow, uopIdx >> 1, uopIdx)
  vIntFixpAlu.io.in.mask16b := MaskExtract(mask, maskIdx, eewVm)
  vIntFixpAlu.io.ctrl.narrow := narrow
  vIntFixpAlu.io.ctrl.vstart_gte_vl := vstart_gte_vl

  val vMaskAlu = Module(new VMask)
  vMaskAlu.io.in := io.in

  /**
    * Output stage
    */
  val opcodeS1 = RegNext(in.bits.opcode)
  val vdFinal =
    Mux(opcodeS1.isIntFixp, vIntFixpAlu.io.out.vd, Mux(opcodeS1.isReduction, vReduAlu.io.out.vd, vMaskAlu.io.out.vd))

  io.out.bits.vd := vdFinal
  io.out.bits.vxsat := vIntFixpAlu.io.out.vxsat
}

object VerilogAlu extends App {
  println("Generating the VALU hardware")
  emitVerilog(new VIAlu(), Array("--target-dir", "build/vifu"))
}
