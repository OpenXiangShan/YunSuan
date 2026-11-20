package yunsuan.vector.cvt

import chisel3._
import chisel3.util._
import yunsuan.util.GatedValidRegNext

class GenericConvertIO extends Bundle {
  val fire = Input(Bool())
  val src = Input(UInt(32.W))
  val format = Input(UInt(3.W))
  val decode = Input(Bool())
  val scaled = Input(Bool())
  val factor = Input(UInt(8.W))
  val rm = Input(UInt(3.W))
  val result = Output(UInt(32.W))
  val fflags = Output(UInt(5.W))
}

/** Reusable, parameter-prunable low-format conversion lane.
  * An accepted request samples data and controls together. Results appear
  * after latency clock edges, II=1; outputs hold after draining bubbles. Low payloads
  * occupy the least significant bits. There is no host packing or ISA decode.
  */
object GenericConvert {
  // Encoding and scaled reconstruction have long normalization/rounding paths.
  // Raw decode-only profiles use one output register with no alignment bubble.
  def hasPreparationStage(params: ConvertParams, pipeline: Boolean = true): Boolean =
    pipeline && (params.encode || params.scaled)
  def latency(params: ConvertParams = ConvertParams(), pipeline: Boolean = true): Int =
    if (hasPreparationStage(params, pipeline)) 2 else 1
}

class GenericConvert(val params: ConvertParams = ConvertParams(), pipeline: Boolean = true) extends Module {
  val io = IO(new GenericConvertIO)
  val prepared = GenericConvert.hasPreparationStage(params, pipeline)
  val latency = GenericConvert.latency(params, pipeline)

  private val formatLegal = params.orderedFormats.map(f => io.format === f.id.U).reduce(_ || _)
  private val directionLegal = (params.encode.B && !io.decode) || (params.decode.B && io.decode)
  private val isFloat = ConvertFormat.floats.toSeq.map(f => io.format === f.id.U).reduce(_ || _)
  private val scaleLegal = (!io.scaled && params.raw.B) || (io.scaled && params.scaled.B && isFloat)
  when(io.fire) {
    assert(formatLegal, "GenericConvert: disabled or unknown format")
    assert(directionLegal, "GenericConvert: disabled direction")
    assert(scaleLegal, "GenericConvert: disabled or invalid scale mode")
    assert(io.rm <= 4.U, "GenericConvert: unsupported rounding mode")
  }

  private def connectFamily(family: ConvertFamilyIO): Unit = {
    family.fire.foreach(_ := io.fire)
    family.src := io.src
    family.format := io.format
    family.scaled := io.scaled
    family.factor := io.factor
    family.rm := io.rm
  }
  private val encoded = if (params.encode) {
    val encodeFamily = Module(new EncodeFamily(params, prepared))
    connectFamily(encodeFamily.io)
    Some((encodeFamily.io.result, encodeFamily.io.fflags))
  } else None
  private val decoded = if (params.decode) {
    val decodeFamily = Module(new DecodeFamily(params, prepared))
    connectFamily(decodeFamily.io)
    Some((decodeFamily.io.result, decodeFamily.io.fflags))
  } else None
  private val selectDecode = if (prepared) RegEnable(io.decode, io.fire) else io.decode
  private val (resultNext, flagsNext) = (encoded, decoded) match {
    case (Some((enc, encFlags)), Some((dec, decFlags))) =>
      (Mux(selectDecode, dec, enc), Mux(selectDecode, decFlags, encFlags))
    case (Some(output), None) => output
    case (None, Some(output)) => output
    case _ => throw new IllegalArgumentException("empty conversion direction set")
  }
  private val outputFire = if (prepared) GatedValidRegNext(io.fire) else io.fire
  io.result := RegEnable(resultNext, 0.U(32.W), outputFire)
  io.fflags := RegEnable(flagsNext, 0.U(5.W), outputFire)
}
