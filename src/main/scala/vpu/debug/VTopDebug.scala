package race.vpu.debug

import chisel3._
import chisel3.util._
import race.vpu.VParams._
import race.vpu._
import race.vpu.debug.DebugRobParmas._

class VTopDebug extends Module {
  val io = IO(new Bundle {
    val dispatch_s2v = Flipped(DecoupledIO(new Dispatch_S2V))
  })

  val vTop = Module(new VTop)
  vTop.io.dispatch_s2v <> io.dispatch_s2v

  val dpicL2 = Module(new DpicLoadL2)
  vTop.io.l2.loadReq.ready := true.B
  dpicL2.io.enable := vTop.io.l2.loadReq.valid
  dpicL2.io.paddr := vTop.io.l2.loadReq.bits.addr(0)
  vTop.io.l2.loadRsp.valid := dpicL2.io.load_data.valid
  vTop.io.l2.loadRsp.bits.data := dpicL2.io.load_data.bits.data
  
  val dpicStoreL2 = Module(new DpicStoreL2)
  vTop.io.l2.storeReq.ready := true.B
  vTop.io.l2.storeAck.valid := false.B
  vTop.io.l2.storeAck.bits.dummy := false.B
  dpicStoreL2.io.store_req.bits := vTop.io.l2.storeReq.bits
  dpicStoreL2.io.store_req.valid := vTop.io.l2.storeReq.valid

  val debugRob = Module(new DebugRob)
  if (debugMode) {
    debugRob.io.fromCtrl := vTop.io.debugRob.get
  }
  val dpicGetVReg = Module(new DpicGetVReg)
  dpicGetVReg.io.enable := debugRob.io.commit.valid
  dpicGetVReg.io.isStore := debugRob.io.commit.bits.isStore
  dpicGetVReg.io.wrRf := debugRob.io.commit.bits.wrRf
  dpicGetVReg.io.rfAddr := debugRob.io.commit.bits.rfAddr
  dpicGetVReg.io.emulVd := debugRob.io.commit.bits.emulVd
  for (i <- 0 until 8) {
    if (i < robEntry_dataDepth) {
      when (debugRob.io.commit.valid && debugRob.io.commit.bits.wrRf) {
        dpicGetVReg.io.data8Regs(i) := debugRob.io.commit.bits.data(i)
      }.otherwise {
        dpicGetVReg.io.data8Regs(i) := 0.U
      }
    } else {
      dpicGetVReg.io.data8Regs(i) := 0.U
    }
  }

}

object VerilogVTopDebug extends App {
  println("Generating the VPU Top Debug hardware")
  emitVerilog(new VTopDebug(), Array("--target-dir", "build/verilog_vpu_debug"))
}
