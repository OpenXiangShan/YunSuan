package race.vpu.ctrl

import chisel3._
import chisel3.util._
import race.vpu.VParams._
import race.vpu._

class TailGen extends Module {
  val io = IO(new Bundle {
    val in = Input(new Bundle {
      val uop = new VUop
    })
    // Masks of VRF write.  bit 1: write RF, bit 0: undisturbed
    val maskWb = Output(UInt(VLEN.W))
  })

  val uop = io.in.uop
  val eewVd = SewOH(uop.veewVd)

  /**
    * Reduction
    */
  val maskRedu = Mux1H(Seq(
    eewVd.is8 -> Cat(0.U((VLEN-8).W), ~0.U(8.W)),
    eewVd.is16 -> Cat(0.U((VLEN-16).W), ~0.U(16.W)),
    eewVd.is32 -> Cat(0.U((VLEN-32).W), ~0.U(32.W)),
    eewVd.is64 -> Cat(0.U((VLEN-64).W), ~0.U(64.W)),
  ))

  io.maskWb := Mux(uop.ctrl.redu, maskRedu,
                   ~0.U(VLEN.W))
}
