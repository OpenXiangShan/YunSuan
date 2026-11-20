package yunsuan.vector.cvt

import chisel3._
import chisel3.util._
import yunsuan.VfmaOpCode
import yunsuan.vector.VectorFloatFMA
import yunsuan.vector.VectorConvert.FP32ToMXFPFormat

/** Elaboration-time direction selection; no hardware is instantiated until used. */
case class NvPayloadProfile(encode: Boolean = true, decode: Boolean = true) {
  require(encode || decode, "NV payload profile must enable at least one direction")
}

object NvPayloadProfile {
  val Both = NvPayloadProfile()
  val EncodeOnly = NvPayloadProfile(decode = false)
  val DecodeOnly = NvPayloadProfile(encode = false)
}

object NvPayloadPipeline {
  val latency = 6 // mixed/decode; encode-only needs four cycles
  def latency(profile: NvPayloadProfile): Int = if (profile.decode) 6 else 4
  val initiationInterval = 1
}

/** Raw FP32, RNE, three registered stages. Each existing YunSuan FMA instance
  * supplies two independent FP32 multiply lanes. Unused odd lanes are zeroed.
  */
private[cvt] class NvFp32Multiply(val lanes: Int) extends Module {
  require(lanes > 0)
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val a = Input(Vec(lanes, UInt(32.W)))
    val b = Input(Vec(lanes, UInt(32.W)))
    val result = Output(Vec(lanes, UInt(32.W)))
    val flags = Output(Vec(lanes, UInt(5.W)))
  })
  for (slice <- 0 until (lanes + 1) / 2) {
    val mul = Module(new VectorFloatFMA)
    def pair(xs: Vec[UInt]): UInt = Cat(
      if (2 * slice + 1 < lanes) xs(2 * slice + 1) else 0.U(32.W), xs(2 * slice))
    mul.io.fire := io.valid
    mul.io.fp_a := pair(io.a)
    mul.io.fp_b := pair(io.b)
    mul.io.fp_c := 0.U
    mul.io.widen_a := 0.U
    mul.io.widen_b := 0.U
    mul.io.uop_idx := false.B
    mul.io.round_mode := 0.U
    mul.io.fp_format := 2.U
    mul.io.op_code := VfmaOpCode.vfmul
    mul.io.frs1 := 0.U
    mul.io.is_vec := true.B
    mul.io.is_frs1 := false.B
    mul.io.res_widening := false.B
    mul.io.fp_aIsFpCanonicalNAN := false.B
    mul.io.fp_bIsFpCanonicalNAN := false.B
    mul.io.fp_cIsFpCanonicalNAN := false.B
    for (offset <- 0 until 2 if 2 * slice + offset < lanes) {
      io.result(2 * slice + offset) := mul.io.fp_result(32 * offset + 31, 32 * offset)
      io.flags(2 * slice + offset) := mul.io.fflags(5 * offset + 4, 5 * offset)
    }
  }
}

/** Optional fixed-scale NVFP4 payload stream for NV_G16_STAGED_RNE.
  *
  * Each valid group samples all context; output follows six rising edges later (four for encode-only).
  * II=1 group/cycle, no backpressure; the consumer must accept every valid output.
  * Reset discards in-flight groups. Data and flags are meaningful only with valid.
  * Every lane is active; packing, masks, G16 boundaries and tails are caller-owned.
  *
  * Encode: src is raw FP32. Caller supplies
  *   inverse = RN32(1 / RN32(alpha * max(decodeE4(scale), 2^-9))).
  * Hardware computes E2M1_RNE(RN32(src * inverse)); zeroBlock replaces the rounded
  * product with +0 before E2M1 conversion, retaining the multiplication flags.
  * Encode result occupies bits 3:0, with upper bits zero. Preparation flags belong
  * to the caller and must be combined separately for whole-block flags.
  *
  * Decode: src[3:0] is E2M1 and scale is the stored E4M3 code. Hardware computes
  * RN32(RN32(decodeE2(src) * decodeE4(scale)) * alpha), including two distinct FP32
  * rounding points. Decode never clamps scale or applies the encode zero override.
  * The finite-input, positive-alpha profile is supplied by the caller; IEEE FP32
  * special values follow the reused primitives, without hidden scale preparation.
  */
class NvPayloadPipeline(
    val lanes: Int = 2,
    val tagWidth: Int = 8,
    val profile: NvPayloadProfile = NvPayloadProfile.Both) extends Module {
  require(lanes > 0 && tagWidth > 0)
  val latency = NvPayloadPipeline.latency(profile)
  val io = IO(new Bundle {
    val in = Flipped(Valid(new Bundle {
      val src = Vec(lanes, UInt(32.W))
      val decode = Bool()
      val inverse = UInt(32.W)
      val scale = UInt(8.W)
      val alpha = UInt(32.W)
      val zeroBlock = Bool()
      val tag = UInt(tagWidth.W)
    }))
    val out = Valid(new Bundle {
      val result = Vec(lanes, UInt(32.W))
      val laneFlags = Vec(lanes, UInt(5.W))
      val flags = UInt(5.W)
      val decode = Bool()
      val tag = UInt(tagWidth.W)
    })
  })

  if (!profile.decode) when(io.in.valid) { assert(!io.in.bits.decode) }
  if (!profile.encode) when(io.in.valid) { assert(io.in.bits.decode) }
  val decoding = if (profile.encode && profile.decode) io.in.bits.decode else profile.decode.B
  val valid = Seq.iterate(io.in.valid, latency + 1)(v => RegNext(v, false.B))
  def delay[T <: Data](data: T, cycles: Int, enable: Bool): T = {
    var value = data
    var fire = enable
    for (_ <- 0 until cycles) {
      value = RegEnable(value, fire)
      fire = RegNext(fire, false.B)
    }
    value
  }
  val decode3 = delay(decoding, 3, io.in.valid)
  val alpha3 = delay(io.in.bits.alpha, 3, io.in.valid)
  val zero3 = delay(io.in.bits.zeroBlock, 3, io.in.valid)
  val first = Module(new NvFp32Multiply(lanes))
  first.io.valid := io.in.valid

  val decodedScale = if (profile.decode) {
    val decoder = Module(new FloatToFP32(4, 3, 7, hasNaN = true, allowScale = false))
    decoder.io.src := io.in.bits.scale
    decoder.io.factor := 127.U
    decoder.io.rm := 0.U
    decoder.io.result
  } else 0.U(32.W)
  for (lane <- 0 until lanes) {
    val decodedPayload = if (profile.decode) {
      val decoder = Module(new FloatToFP32(2, 1, 1, hasNaN = false, allowScale = false))
      decoder.io.src := io.in.bits.src(lane)(3, 0)
      decoder.io.factor := 127.U
      decoder.io.rm := 0.U
      decoder.io.result
    } else 0.U(32.W)
    first.io.a(lane) := Mux(decoding, decodedPayload, io.in.bits.src(lane))
    first.io.b(lane) := Mux(decoding, decodedScale, io.in.bits.inverse)
  }

  val encoded = Wire(Vec(lanes, UInt(32.W)))
  val encodeFlags = Wire(Vec(lanes, UInt(5.W)))
  for (lane <- 0 until lanes) {
    if (profile.encode) {
      val encoder = Module(new FP32ToMXFPFormat(2, 1, 1, 1, hasNaN = false))
      encoder.io.src := Mux(zero3, 0.U, first.io.result(lane))
      encoder.io.factor := 127.U
      encoder.io.rm := 0.U
      encoded(lane) := delay(Cat(0.U(28.W), encoder.io.result), latency - 3, valid(3))
      encodeFlags(lane) := delay(first.io.flags(lane) | encoder.io.fflags, latency - 3, valid(3))
    } else {
      encoded(lane) := 0.U
      encodeFlags(lane) := 0.U
    }
  }
  val decoded = Wire(Vec(lanes, UInt(32.W)))
  val decodeFlags = Wire(Vec(lanes, UInt(5.W)))
  if (profile.decode) {
    val second = Module(new NvFp32Multiply(lanes))
    second.io.valid := valid(3) && decode3
    second.io.a := first.io.result
    second.io.b := VecInit(Seq.fill(lanes)(alpha3))
    for (lane <- 0 until lanes) {
      decoded(lane) := second.io.result(lane)
      decodeFlags(lane) := second.io.flags(lane) | delay(first.io.flags(lane), latency - 3, valid(3))
    }
  } else {
    decoded := VecInit(Seq.fill(lanes)(0.U(32.W)))
    decodeFlags := VecInit(Seq.fill(lanes)(0.U(5.W)))
  }
  io.out.valid := valid(latency)
  io.out.bits.decode := delay(decoding, latency, io.in.valid)
  io.out.bits.tag := delay(io.in.bits.tag, latency, io.in.valid)
  for (lane <- 0 until lanes) {
    io.out.bits.result(lane) := Mux(io.out.bits.decode, decoded(lane), encoded(lane))
    io.out.bits.laneFlags(lane) := Mux(io.out.bits.decode, decodeFlags(lane), encodeFlags(lane))
  }
  io.out.bits.flags := io.out.bits.laneFlags.reduce(_ | _)
}
