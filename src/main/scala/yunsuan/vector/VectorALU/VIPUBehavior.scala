package yunsuan.vector.VectorALU

import chisel3._
import chisel3.util._
import yunsuan.vector.{VIFuInput, VIFuOutput}
import yunsuan.vector.alu.VAluOpcode

/**
  * Stateless carrier for the mask/scalar subset of VIPU.
  *
  * VIPU also contains reductions whose architectural value is accumulated
  * over a sequence of vector uops.  Those operations remain on the original
  * VIAlu path for now.  The operations below are single-uop computations
  * when vlmul=0 and uopIdx=0; keeping the guard in the wrapper makes this
  * carrier a safe, explicit partial model rather than an approximation of a
  * multi-uop state machine.
  */
class VIPUComputeCarrier(val modelName: String = "vec-int-pu-lane") extends RawModule {
  override def desiredName: String =
    "WolvrixBehaviorComputeGroup_v1_" + modelName.replace('-', '_')

  private val inputWidth = new VIFuInput().getWidth
  private val outputWidth = new VIFuOutput().getWidth
  val io = IO(new Bundle {
    val in = Input(UInt(inputWidth.W))
    val out = Output(UInt(outputWidth.W))
  })

  private val req = io.in.asTypeOf(new VIFuInput)
  private val op = req.opcode.op
  private val activeBits = Wire(Vec(128, Bool()))
  private val active = activeBits.asUInt
  for (i <- 0 until 128) {
    activeBits(i) := req.vs2(i) && (req.info.vm || req.mask(i)) && (i.U < req.info.vl)
  }

  private def signExtend(value: UInt, width: Int): UInt = {
    val sign = value(width - 1)
    Cat(Fill(64 - width, sign), value)
  }

  private val vmvxs = Wire(UInt(64.W))
  vmvxs := signExtend(req.vs2(7, 0), 8)
  when (req.vdType(1, 0) === 1.U) {
    vmvxs := signExtend(req.vs2(15, 0), 16)
  }.elsewhen (req.vdType(1, 0) === 2.U) {
    vmvxs := signExtend(req.vs2(31, 0), 32)
  }.elsewhen (req.vdType(1, 0) === 3.U) {
    vmvxs := req.vs2(63, 0)
  }

  private val first = Wire(UInt(64.W))
  first := Fill(64, 1.U(1.W))
  var firstSeen: Bool = false.B
  for (i <- 0 until 128) {
    when (!firstSeen && active(i)) {
      first := i.U
    }
    firstSeen = firstSeen || active(i)
  }

  private val count = PopCount(active)
  private val vcpop = Cat(0.U(56.W), count)

  private def prefixMask(): UInt = {
    val out = Wire(UInt(128.W))
    var seen: Bool = false.B
    val bits = Seq.tabulate(128) { i =>
      val bit = !seen
      seen = seen || active(i)
      bit
    }
    out := VecInit(bits.reverse).asUInt
    out
  }

  private val vmsbf = prefixMask()
  private val vmsif = Cat(vmsbf(126, 0), 1.U)
  private val vmsof = ~vmsbf & vmsif

  private def iotaFor(width: Int): UInt = {
    val elems = 128 / width
    val out = Wire(Vec(elems, UInt(width.W)))
    var countBefore: UInt = 0.U(width.W)
    for (i <- 0 until elems) {
      out(i) := countBefore
      for (j <- 0 until width) {
        countBefore = countBefore + active(i * width + j)
      }
    }
    Cat(out.reverse)
  }

  private val viota = Wire(UInt(128.W))
  viota := iotaFor(8)
  when (req.vdType(1, 0) === 1.U) {
    viota := iotaFor(16)
  }.elsewhen (req.vdType(1, 0) === 2.U) {
    viota := iotaFor(32)
  }.elsewhen (req.vdType(1, 0) === 3.U) {
    viota := iotaFor(64)
  }

  private def vidFor(width: Int): UInt = {
    val elems = 128 / width
    val out = Wire(Vec(elems, UInt(width.W)))
    val shift = log2Ceil(elems)
    for (i <- 0 until elems) {
      out(i) := (i.U(width.W) + (req.info.uopIdx << shift))(width - 1, 0)
    }
    Cat(out.reverse)
  }
  private val vid = MuxLookup(req.vdType(1, 0), vidFor(8))(Seq(
    1.U -> vidFor(16),
    2.U -> vidFor(32),
    3.U -> vidFor(64)
  ))

  private val result = Wire(new VIFuOutput)
  result.vd := 0.U
  result.vxsat := false.B
  when (op === VAluOpcode.vmvxs) {
    result.vd := Cat(0.U(64.W), vmvxs)
  }.elsewhen (op === VAluOpcode.vfirst) {
    result.vd := Cat(0.U(64.W), first)
  }.elsewhen (op === VAluOpcode.vcpop) {
    result.vd := Cat(0.U(64.W), vcpop)
  }.elsewhen (op === VAluOpcode.viota) {
    result.vd := viota
  }.elsewhen (op === VAluOpcode.vid) {
    result.vd := vid
  }.elsewhen (op === VAluOpcode.vmsbf) {
    result.vd := vmsbf
  }.elsewhen (op === VAluOpcode.vmsif) {
    result.vd := vmsif
  }.elsewhen (op === VAluOpcode.vmsof) {
    result.vd := vmsof
  }

  io.out := result.asUInt
}
