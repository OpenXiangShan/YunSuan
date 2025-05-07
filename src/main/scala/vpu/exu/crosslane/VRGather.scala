package race.vpu.exu.crosslane

import chisel3._
import chisel3.util._
import race.vpu._
import VParams._

class VRGather extends Module {
  val io = IO(new Bundle {
    val in = Input(ValidIO(new Bundle {
        val uop = new VUop
        val vs1 = UInt(VLEN.W)
        val vs2 = UInt(VLEN.W)
        val rs1 = UInt(XLEN.W)
    }))
    val out = ValidIO(new Bundle {
        val uop = new VUop
        val vd = UInt(VLEN.W)
    })
  })

  /**
    * So far, only vrgather.vi vd, vs2, 0 is supported, which performs
    *    "broadcast vs2[0] to vd"
    */
  // Check if it is vrgather.vi vd, vs2, 0
  val vs2 = io.in.bits.vs2
  val uop = io.in.bits.uop
  val sewOH = SewOH(uop.csr.vsew)
  val vrgather_vi = uop.ctrl.funct6 === "b001100".U && uop.ctrl.funct3(0)
  val uimmIs0 = uop.ctrl.vs1_imm === 0.U
  assert((io.in.valid && vrgather_vi && uimmIs0) || !io.in.valid, "For vrgather, only vrgather.vi vd, vs2, 0 is supported")

  // ---- Only for vrgather.vi vd, vs2, 0  !!!! ----
  // vs2_0_0 is the lowest 64-bit value of vs2[0]
  val vs2_0_0_reg = Reg(UInt(64.W))
  val keep_vs2_0_0 = RegInit(false.B)
  when (io.in.valid) {
    when (uop.uopEnd) {
      keep_vs2_0_0 := false.B
    }.otherwise {
      keep_vs2_0_0 := true.B
    }
  }
  when (!keep_vs2_0_0) { vs2_0_0_reg := vs2(63, 0) }
  val vs2_0_0 = Mux(keep_vs2_0_0, vs2_0_0_reg, vs2(63, 0))

  val vd_64b = Mux1H(sewOH.oneHot, Seq(8, 16, 32, 64).map(n => Fill(XLEN/n, vs2_0_0(n-1, 0))))
  val vd_broadcast = Fill(VLEN/64, vd_64b)
  io.out.bits.vd := vd_broadcast

  io.out.valid := io.in.valid
  io.out.bits.uop := io.in.bits.uop
}