
package yunsuan.vector.alu

import chisel3._
import chisel3.util._
import yunsuan.vector._
import yunsuan.vector.alu.VAluOpcode._
import yunsuan.vector.alu.VSew._

class VIAlu extends Module {
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(new VIFuInput))
    val out = ValidIO(new VIFuOutput)
  })

  // Latency of ALU is 1 cycles plus
  io.out.valid := RegNext(io.in.valid)

  val srcTypeVs1 = io.in.bits.srcType(1)
  val srcTypeVs2 = io.in.bits.srcType(0)
  val vdType = io.in.bits.vdType
  val eewVs1 = SewOH(srcTypeVs1(1, 0))
  val eewVd = SewOH(vdType(1, 0))
  val mask = io.in.bits.mask
  val uopIdx = io.in.bits.info.uopIdx
  val narrow = srcTypeVs2(1, 0) === 3.U && vdType(1, 0) === 2.U ||
               srcTypeVs2(1, 0) === 2.U && vdType(1, 0) === 1.U ||
               srcTypeVs2(1, 0) === 1.U && vdType(1, 0) === 0.U
  val vstart_gte_vl = io.in.bits.info.vstart >= io.in.bits.info.vl

  val vIntFixpAlu = Module(new VIntFixpAlu)
  vIntFixpAlu.io.in.opcode := io.in.bits.opcode
  vIntFixpAlu.io.in.info := io.in.bits.info
  vIntFixpAlu.io.in.srcType := io.in.bits.srcType
  vIntFixpAlu.io.in.vdType := io.in.bits.vdType
  vIntFixpAlu.io.in.vs1 := io.in.bits.vs1
  vIntFixpAlu.io.in.vs2 := io.in.bits.vs2
  vIntFixpAlu.io.in.old_vd := io.in.bits.old_vd
  val eewVm = Mux(vdType === 15.U, eewVs1, eewVd)
  val maskIdx = Mux(narrow, uopIdx >> 1, uopIdx)
  vIntFixpAlu.io.in.mask16b := MaskExtract(mask, maskIdx, eewVm)
  vIntFixpAlu.io.ctrl.narrow := narrow
  vIntFixpAlu.io.ctrl.vstart_gte_vl := vstart_gte_vl
  val vsew = vdType(1, 0)
  val vs2 = io.in.bits.vs2

  val vReduAlu = Module(new Reduction)
  vReduAlu.io.in := io.in

  val vMaskAlu = Module(new VMask)
  vMaskAlu.io.in := io.in

  val vs2Ext = RegEnable(Mux1H(Seq(
    (vsew === e8)  -> BitsExtend(vs2(7,0), 64, true.B),
    (vsew === e16) -> BitsExtend(vs2(15,0), 64, true.B),
    (vsew === e32) -> BitsExtend(vs2(31,0), 64, true.B),
    (vsew === e64) -> vs2(63,0)
  )) , io.in.valid)

  /**
   * Output stage
   */
  val opcodeS1 = RegEnable(io.in.bits.opcode, io.in.valid)
  val vdFinal = Mux(opcodeS1.isVmvxs, vs2Ext,
                    Mux(opcodeS1.isIntFixp, vIntFixpAlu.io.out.vd,
                        Mux(opcodeS1.isReduction, vReduAlu.io.out.vd, vMaskAlu.io.out.vd)))

  io.out.bits.vd := vdFinal
  io.out.bits.vxsat := vIntFixpAlu.io.out.vxsat
}

object VerilogAlu extends App {
  println("Generating the VALU hardware")
  emitVerilog(new VIAlu(), Array("--target-dir", "build/vifu"))
}
