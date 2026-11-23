package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import yunsuan.util.GatedValidRegNext
import yunsuan.vector.cvt.BF16Convert

/** Legacy scalar/VectorCvt interface: align the one-cycle core with native CVT.
  * New independent hosts use BF16Convert directly; this delay is compatibility.
  */
class CVT_bf16(width: Int = 64) extends CVT(width) {
  val core = Module(new BF16Convert(width))
  core.io <> io
  val aligned = GatedValidRegNext(io.fire)
  io.result := RegEnable(core.io.result, 0.U(width.W), aligned)
  io.fflags := RegEnable(core.io.fflags, 0.U(5.W), aligned)
}
