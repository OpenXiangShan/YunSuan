package race.vpu.ctrl

import chisel3._
import chisel3.util._
import race.vpu.VParams._
import race.vpu._

class WritebackExu extends Module {
  val io = IO(new Bundle {
    val in = Input(new Bundle {
      val uop = new VUop
    })
    // Masks of VRF write.  bit 1: write RF, bit 0: undisturbed
    val maskWb = Output(UInt(VLEN.W))
  })

  val uop = io.in.uop
  val eewVd = SewOH(uop.veewVd)
  val (funct6, funct3) = (uop.ctrl.funct6, uop.ctrl.funct3)

  val perm_vmv_sx_sf = funct6 === "b010000".U && funct3(2) && (funct3(1) =/= funct3(0))
  val dest_vl_is_1 = perm_vmv_sx_sf || uop.ctrl.redu
  val vl_final = Mux(dest_vl_is_1, 1.U, uop.csr.vl)
  val uopIdx_final = Mux(dest_vl_is_1, 0.U, uop.uopIdx)

  // Type of tail: Wire(UInt(vlenb.W))   // Note: vlenb = VLEN/8
  // For eew = 16, only vlenb/2 bits are valid. For eew = 32, only vlenb/4, ...
  val tail = TailGen(vl_final, uopIdx_final, eewVd, uop.ctrl.narrow)
  val tailByByte = Mux1H(Seq(
    eewVd.is8 -> tail,
    eewVd.is16 -> VecInit(tail.asBools.take(vlenb/2).map(Fill(2, _))).asUInt,
    eewVd.is32 -> VecInit(tail.asBools.take(vlenb/4).map(Fill(4, _))).asUInt,
    eewVd.is64 -> VecInit(tail.asBools.take(vlenb/8).map(Fill(8, _))).asUInt
  ))

  val maskWb = VecInit(tailByByte.asBools.map(x => Fill(8, !x))).asUInt

  io.maskWb := maskWb

}
