
package yunsuan.vector.VectorALU

import chisel3._
import chisel3.util._
import yunsuan.vector.v2.Crypto.VClmul

class VCrypto extends Module {
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(new VCrypto.In))
    val out = ValidIO(new VCrypto.Out)
  })

  val isHigh = io.in.bits.opcode.isVclmulh

  val hi_hi = Wire(UInt(64.W))
  val hi_lo = Wire(UInt(64.W))
  val lo_hi = Wire(UInt(64.W))
  val lo_lo = Wire(UInt(64.W))

  // Zvbc vclmul/vclmulh

  val hi = Module(new VClmul)
  hi.in.a := io.in.bits.vs1(127, 64)
  hi.in.b := io.in.bits.vs2(127, 64)
  hi_hi := hi.out.ch
  hi_lo := hi.out.cl

  val lo = Module(new VClmul)
  lo.in.a := io.in.bits.vs1(63, 0)
  lo.in.b := io.in.bits.vs2(63, 0)
  lo_hi := lo.out.ch
  lo_lo := lo.out.cl

  val result = Cat(Mux(isHigh, hi_hi, hi_lo), Mux(isHigh, lo_hi, lo_lo))

  val stage1Result = RegEnable(result, io.in.valid)
  val stage1Valid = RegNext(io.in.valid, false.B)

  io.out.bits.vd    := RegEnable(stage1Result, stage1Valid)
  io.out.bits.vxsat := false.B
  io.out.valid      := RegNext(stage1Valid, false.B)
}

object VCrypto {
  object Opcode {
    val vclmul  = 0.U(1.W)
    val vclmulh = 1.U(1.W)
  }

  class Opcode extends Bundle {
    val op = UInt(1.W)

    def isVclmulh: Bool = op === Opcode.vclmulh
  }

  class In extends Bundle {
    val opcode = new Opcode
    val vs1 = UInt(128.W)
    val vs2 = UInt(128.W)
    val old_vd = UInt(128.W)
  }

  class Out extends Bundle {
    val vd = UInt(128.W)
    val vxsat = Bool()
  }
}