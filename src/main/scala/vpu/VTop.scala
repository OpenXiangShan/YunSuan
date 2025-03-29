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
  // vCtrlBlock.io.fromExu(0).valid := true.B
  // vCtrlBlock.io.fromExu(0).bits := 0.U.asTypeOf(new VExuOutput)

  vCtrlBlock.io.lsu <> vLsuBlock.io.ctrl
  
  vLsuBlock.io.l2 <> io.l2
}

object VerilogVTop extends App {
  println("Generating the VPU Top hardware")
  emitVerilog(new VTop(), Array("--target-dir", "build/verilog_vpu"))
}

