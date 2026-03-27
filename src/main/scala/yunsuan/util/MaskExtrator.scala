package yunsuan.util

import chisel3._
import chisel3.util._

class MaskExtractorIO(vlen: Int) extends Bundle {
  private val numBytes = vlen / 8

  val in = Input(new Bundle {
    val mask = UInt(numBytes.W)
    val vsew = UInt(2.W)
  })
  val out = Output(new Bundle {
    val mask = UInt(numBytes.W)
  })
}

class MaskExtractor(vlen: Int) extends Module {
  private val numBytes = vlen / 8

  val io = IO(new MaskExtractorIO(vlen))

  private val mask = io.in.mask
  private val vsew = io.in.vsew
  private val extractedMask = Wire(UInt(vlen.W))

  extractedMask := Mux1H(Seq(
    (vsew === 0.U)  -> mask,
    (vsew === 1.U) -> VecInit(mask.asBools.flatMap(Seq.fill(2)(_))).asUInt,
    (vsew === 2.U) -> VecInit(mask.asBools.flatMap(Seq.fill(4)(_))).asUInt,
    (vsew === 3.U) -> VecInit(mask.asBools.flatMap(Seq.fill(8)(_))).asUInt,
  ))

  io.out.mask := extractedMask
}

object MaskExtractor {
  def apply(vlen: Int)(mask: UInt, vsew: UInt): UInt = {
    val maskExtractor = Module(new MaskExtractor(vlen))
    maskExtractor.io.in.mask := mask
    maskExtractor.io.in.vsew := vsew
    maskExtractor.io.out.mask
  }
}
