package race.vpu

import chisel3._
import chisel3.util._
import VParams._

class VTop extends Module {
  val io = IO(new Bundle {
    val dispatch_s2v = Flipped(DecoupledIO(new Dispatch_S2V))
    val l2 = new Bundle {
      val loadReq = DecoupledIO(new VL2LoadReq)
      val loadRsp = Input(ValidIO(new VL2LoadRsp))
      val storeReq = DecoupledIO(new VL2StoreReq)
      val storeAck = Input(ValidIO(new VL2StoreAck))
    }
  })

  val vCtrlBlock = Module(new VCtrlBlock)
  val vExuBlock = Module(new VExuBlock)
  val vLsuBlock = Module(new VLsuBlock)

  vCtrlBlock.io.dispatch_s2v <> io.dispatch_s2v

  vExuBlock.io.in := vCtrlBlock.io.toExu
  vCtrlBlock.io.fromExu(0) := vExuBlock.io.out

  vCtrlBlock.io.lsu <> vLsuBlock.io.ctrl
  
  vLsuBlock.io.l2 <> io.l2
}
