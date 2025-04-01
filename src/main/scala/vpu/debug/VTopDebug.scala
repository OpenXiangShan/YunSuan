package race.vpu.debug

import chisel3._
import chisel3.util._
import race.vpu.VParams._
import race.vpu._

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
  
  vTop.io.l2.storeReq.ready := true.B
  vTop.io.l2.storeAck.valid := false.B
  vTop.io.l2.storeAck.bits.dummy := false.B
}

object VerilogVTopDebug extends App {
  println("Generating the VPU Top Debug hardware")
  emitVerilog(new VTopDebug(), Array("--target-dir", "build/verilog_vpu_debug"))
}
