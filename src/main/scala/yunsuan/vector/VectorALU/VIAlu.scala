
package yunsuan.vector.alu

import chisel3._
import chisel3.util._
import yunsuan.vector._
import yunsuan.vector.alu.VAluOpcode._
import yunsuan.vector.alu.VSew._
import yunsuan.util._

class VIAlu extends Module {
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(new VIFuInput))
    val out = ValidIO(new VIFuOutput)
  })

  // Latency of ALU is 1 cycles plus


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
  val validS1 = RegNext(io.in.valid)
  val opcodeS2 = RegEnable(opcodeS1, validS1)
  val vs2ExtS2 = RegEnable(vs2Ext,validS1)
  val vdFinal = Mux(opcodeS2.isVmvxs, vs2ExtS2,
    Mux(opcodeS2.isReduction, vReduAlu.io.out.vd, vMaskAlu.io.out.vd))
  io.out.bits.vd := vdFinal
  io.out.bits.vxsat := false.B
  io.out.valid := GatedValidRegNext(validS1)
}

object VerilogAlu extends App {
  println("Generating the VALU hardware")
  emitVerilog(new VIAlu(), Array("--target-dir", "build/vifu"))
}
