package yunsuan.vector.cvt

import chisel3._
import chisel3.util._
import yunsuan.VfcvtType
import yunsuan.vector.VectorConvert.CVT32

/** Standalone, proven native kernels in the same library as the packed families.
  * These wrappers specialize controls; BF16 uses its one-cycle numeric entry.
  * Other IEEE widths remain available through the legacy VectorCvt interface.
  * Hosts define their own provider selection and adaptation for those widths.
  */
class NativeFP32Convert extends Module {
  val io = IO(new Bundle {
    val fire = Input(Bool())
    val src = Input(UInt(32.W))
    val toFloat = Input(Bool())
    val signed = Input(Bool())
    val rm = Input(UInt(3.W))
    val result = Output(UInt(32.W))
    val fflags = Output(UInt(5.W))
  })
  val core = Module(new CVT32(32))
  core.io.fire := io.fire
  core.io.src := io.src
  core.io.opType := Mux(io.toFloat,
    Mux(io.signed, VfcvtType.vfcvt_fxv, VfcvtType.vfcvt_fxuv),
    Mux(io.signed, VfcvtType.vfcvt_xfv, VfcvtType.vfcvt_xufv))
  core.io.sew := 2.U
  core.io.rm := io.rm
  core.io.input1H := "b0100".U
  core.io.output1H := "b0100".U
  core.io.isFpToVecInst := false.B
  core.io.isFround := 0.U
  core.io.isFcvtmod := false.B
  when(io.fire) { assert(io.rm <= 4.U) }
  io.result := core.io.result
  io.fflags := core.io.fflags
}

class NativeBF16Convert extends Module {
  val io = IO(new Bundle {
    val fire = Input(Bool())
    val src = Input(UInt(32.W))
    val narrow = Input(Bool())
    val rm = Input(UInt(3.W))
    val result = Output(UInt(32.W))
    val fflags = Output(UInt(5.W))
  })
  val core = Module(new BF16Convert(32))
  core.io.fire := io.fire
  core.io.src := io.src
  core.io.opType := Mux(io.narrow, VfcvtType.vfncvtbf16_ffw, VfcvtType.vfwcvtbf16_ffv)
  core.io.sew := 1.U
  core.io.rm := io.rm
  core.io.input1H := Mux(io.narrow, "b0100".U, "b0010".U)
  core.io.output1H := Mux(io.narrow, "b0010".U, "b0100".U)
  core.io.isFpToVecInst := false.B
  core.io.isFround := 0.U
  core.io.isFcvtmod := false.B
  when(io.fire) { assert(io.rm <= 4.U) }
  io.result := core.io.result
  io.fflags := core.io.fflags
}

/** One capability vocabulary can describe independent hardware providers.
  * Latency is numerical core cycles; all listed fixed pipelines accept II=1.
  * Host layout/flags and legal ISA subsets are defined by their own adapters.
  */
object NativeConvertCapabilities {
  case class Family(name: String, formats: Set[String], provider: String,
    coreLatency: Int, initiationInterval: Int, standalone: Boolean)
  val fp32Integer = Family("fp32-integer", Set("FP32", "I32", "U32"),
    "NativeFP32Convert / CVT32", 2, 1, true)
  val brainFloat = Family("brain-float", Set("BF16", "FP32"),
    "NativeBF16Convert / BF16Convert", 1, 1, true)
  val all = Seq(fp32Integer, brainFloat)
}
