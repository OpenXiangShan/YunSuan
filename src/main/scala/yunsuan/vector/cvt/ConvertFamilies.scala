package yunsuan.vector.cvt

import chisel3._
import chisel3.util._
import yunsuan.vector.VectorConvert.{FP32ToFloat8Format, FP32ToInt8, FP32ToMXFPFormat}

/** Family interface with an optional preparation register.
  * GenericConvert supplies request checking and the final result register.
  */
class ConvertFamilyIO(pipeline: Boolean = false) extends Bundle {
  val fire = if (pipeline) Some(Input(Bool())) else None
  val src = Input(UInt(32.W))
  val format = Input(UInt(3.W))
  val scaled = Input(Bool())
  val factor = Input(UInt(8.W))
  val rm = Input(UInt(3.W))
  val result = Output(UInt(32.W))
  val fflags = Output(UInt(5.W))
}

/** F32 to enabled low formats. The proven format rounders remain the numeric
  * implementation; parameter selection happens before hardware elaboration.
  */
class EncodeFamily(val params: ConvertParams, pipeline: Boolean = false) extends Module {
  require(params.encode, "EncodeFamily requires Encode capability")
  val io = IO(new ConvertFamilyIO(pipeline))
  private val factor = if (!params.scaled) 127.U(8.W)
    else if (!params.raw) io.factor else Mux(io.scaled, io.factor, 127.U(8.W))

  private val selectFormat = if (pipeline) RegEnable(io.format, io.fire.get) else io.format
  private val outputs = params.orderedFormats.map { format =>
    val (result, flags) = if (ConvertFormat.integers.contains(format)) {
      val core = Module(new FP32ToInt8(signed = format == ConvertFormat.I8, pipeline = pipeline))
      core.io.fire.foreach(_ := io.fire.get)
      core.suggestName(s"encode_$format")
      core.io.src := io.src
      core.io.rm := io.rm
      (core.io.result, core.io.fflags)
    } else {
      val spec = ConvertFormat.floatSpec(format)
      if (!params.scaled && format != ConvertFormat.E2M1) {
        val maxExp = (1 << spec.exp) - (if (spec.hasInfinity) 2 else 1)
        val nan = if (spec.hasInfinity) 0x7e else 0x7f
        val core = Module(new FP32ToFloat8Format(spec.exp, spec.frac, spec.bias,
          maxExp, spec.maxFiniteFrac, spec.hasInfinity, nan, pipeline = pipeline))
        core.io.fire.foreach(_ := io.fire.get)
        core.suggestName(s"encode_$format")
        core.io.src := io.src
        core.io.rm := io.rm
        (core.io.result, core.io.fflags)
      } else {
        val core = Module(new FP32ToMXFPFormat(spec.exp, spec.frac, spec.bias,
          spec.maxFiniteFrac, spec.hasNaN, spec.hasInfinity, pipeline = pipeline))
        core.io.fire.foreach(_ := io.fire.get)
        core.suggestName(s"encode_$format")
        core.io.src := io.src
        core.io.factor := factor
        core.io.rm := io.rm
        (core.io.result, core.io.fflags)
      }
    }
    (format.id.U(3.W), result, flags)
  }
  io.result := MuxLookup(selectFormat, 0.U(32.W))(outputs.map { case (id, result, _) => id -> result })
  io.fflags := MuxLookup(selectFormat, 0.U(5.W))(outputs.map { case (id, _, flags) => id -> flags })
}

/** Enabled low formats to F32, sharing one exact integer normalization path. */
class DecodeFamily(val params: ConvertParams, pipeline: Boolean = false) extends Module {
  require(params.decode, "DecodeFamily requires Decode capability")
  val io = IO(new ConvertFamilyIO(pipeline))
  private val factor = if (!params.scaled) 127.U(8.W)
    else if (!params.raw) io.factor else Mux(io.scaled, io.factor, 127.U(8.W))
  private val selectFormat = if (pipeline) RegEnable(io.format, io.fire.get) else io.format
  private val integers = params.orderedFormats.filter(ConvertFormat.integers.contains)
  private val integerOutputs = if (integers.nonEmpty) {
    val core = Module(new IntegerToFP32(8, pipeline = pipeline))
    core.io.fire.foreach(_ := io.fire.get)
    core.suggestName("decode_integer")
    core.io.src := io.src(7, 0)
    core.io.signed := (if (integers.size == 1) (integers.head == ConvertFormat.I8).B
      else io.format === ConvertFormat.I8.id.U)
    integers.map(format => (format.id.U(3.W), core.io.result, 0.U(5.W)))
  } else Seq.empty
  private val floatOutputs = params.orderedFormats.filter(ConvertFormat.floats.contains).map { format =>
    val spec = ConvertFormat.floatSpec(format)
    val core = Module(new FloatToFP32(spec.exp, spec.frac, spec.bias,
      spec.hasNaN, spec.hasInfinity, allowScale = params.scaled, pipeline = pipeline))
    core.io.fire.foreach(_ := io.fire.get)
    core.suggestName(s"decode_$format")
    core.io.src := io.src(format.width - 1, 0)
    core.io.factor := factor
    core.io.rm := io.rm
    (format.id.U(3.W), core.io.result, core.io.fflags)
  }
  private val outputs = integerOutputs ++ floatOutputs
  io.result := MuxLookup(selectFormat, 0.U(32.W))(outputs.map { case (id, result, _) => id -> result })
  io.fflags := MuxLookup(selectFormat, 0.U(5.W))(outputs.map { case (id, _, flags) => id -> flags })
}
