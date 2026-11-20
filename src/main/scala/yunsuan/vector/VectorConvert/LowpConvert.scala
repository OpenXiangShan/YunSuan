package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import yunsuan.util.GatedValidRegNext
import yunsuan.vector.cvt.{ConvertFormat, ConvertParams, FloatToFP32, GenericConvert, GenericConvertIO, IntegerToFP32}

/** Source-compatible numeric IDs; new users can use cvt.ConvertFormat. */
object LowpFormat {
  val I8 = ConvertFormat.I8.id
  val U8 = ConvertFormat.U8.id
  val E4M3 = ConvertFormat.E4M3.id
  val E5M2 = ConvertFormat.E5M2.id
  val E2M1 = ConvertFormat.E2M1.id
}

/** Compatibility facades for existing direct primitive users. */
class Int8ToFP32 extends IntegerToFP32(8)
class LowpFloatToFP32(
  expWidth: Int,
  fracWidth: Int,
  bias: Int,
  hasNaN: Boolean,
  hasInfinity: Boolean = false
) extends FloatToFP32(expWidth, fracWidth, bias, hasNaN, hasInfinity)

/** The default profile preserves all five formats, two-cycle latency and II=1.
  * Hosts may supply a smaller capability set at elaboration time.
  */
class LowpConvert(params: ConvertParams = ConvertParams()) extends Module {
  val io = IO(new GenericConvertIO)
  val core = Module(new GenericConvert(params))
  core.io <> io
  if (core.latency == 1) {
    // Existing direct consumers expect two cycles even for a raw decode profile.
    val aligned = GatedValidRegNext(io.fire)
    io.result := RegEnable(core.io.result, 0.U(32.W), aligned)
    io.fflags := RegEnable(core.io.fflags, 0.U(5.W), aligned)
  }
}
