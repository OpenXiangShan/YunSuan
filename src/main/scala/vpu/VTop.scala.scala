package race.vpu

import chisel3._
import chisel3.util._
import VParams._

class VTop extends Module {
  val io = IO(new Bundle {
    val dispatch_s2v = Flipped(DecoupledIO(new Dispatch_S2V))
    val temp_complt = ValidIO(new Bundle {
      val robIdx = new RobPtr
    })
  })

  val vCtrlBlock = Module(new VCtrlBlock)
  val vExuBlock = Module(new VExuBlock)
  val vLsuBlock = Module(new VLsuBlock)

  vCtrlBlock.io.dispatch_s2v <> io.dispatch_s2v

  vExuBlock.io.in := vCtrlBlock.io.toExu
  vCtrlBlock.io.fromExu(0) := vExuBlock.io.out
  
  io.temp_complt.valid := vExuBlock.io.out.valid
  io.temp_complt.bits.robIdx := vExuBlock.io.out.bits.uop.robIdx
}
