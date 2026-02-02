package yunsuan.vector.v2.Shuffle

import _root_.circt.stage._
import chisel3._
import chisel3.util._
import yunsuan.util.ModuleWrapper.{ModuleMux1H, ModuleVec}
import yunsuan.vector.Common._

class GatherIndexGen(val vlen: Int, val dlen: Int, MinDataWidth: Int = 8) extends Module with GatherConfig {
  require(isPow2(dlen) && dlen >= 64)

  override def desiredName: String = super.desiredName + s"_${dlen}b"

  val E64Vlmax = dlen * 8 / 64
  val E32Vlmax = dlen * 8 / 32
  val E16Vlmax = dlen * 8 / 16
  val E8Vlmax  = dlen * 8 / 8
  val E64ValidBits = log2Ceil(E64Vlmax)
  val E32ValidBits = log2Ceil(E32Vlmax)
  val E16ValidBits = log2Ceil(E16Vlmax)
  val E8ValidBits  = log2Ceil(E8Vlmax)

  val in = IO(Input(ValidIO(new Bundle {
    val isGatherEI16 = Bool()
    val uopIdx = UInt(log2Ceil(vlen / dlen * 8).W)
    val lmul = new GatherLmulBundle
    val sew = new GatherSewBundle
    val ei = new GatherSewBundle
    // Todo: if vrgatherei16, vs1 should be selected before the register of EXU
    val vs1 = UInt(dlen.W)
    val mask = UInt((dlen / MinDataWidth).W) // 1bit per MinWidth Data
  })))
  val out = IO(Output(new Bundle {
    val gather = ValidIO(new GatherIndexBundle(NumElem, IndexWidth))
    val gatherei16 = ValidIO(new Bundle {})
  }))

  val sew = in.bits.sew
  val ei = in.bits.ei
  val lmul = in.bits.lmul

  val e64Vs1 = in.bits.vs1.to64bitVec
  val e32Vs1 = in.bits.vs1.to32bitVec
  val e16Vs1 = in.bits.vs1.to16bitVec
  val e8Vs1  = in.bits.vs1.to8bitVec

  val uopIdx = in.bits.uopIdx

  val maskVf1 = in.bits.mask
  val maskVf2 = in.bits.mask.toVf2Vec
  val maskVf4 = in.bits.mask.toVf4Vec

  def Index: UInt = UInt(IndexWidth.W)

  val e64ei64MinSewIdx = Wire(Vec(dlen / 64, Vec(64 / MinDataWidth, Index)))
  val e32ei32MinSewIdx = Wire(Vec(dlen / 32, Vec(32 / MinDataWidth, Index)))
  val e16ei16MinSewIdx = Wire(Vec(dlen / 16, Vec(16 / MinDataWidth, Index)))
  val e8ei8MinSewIdx   = Wire(Vec(dlen /  8, Vec( 8 / MinDataWidth, Index)))
  // `dlen / 2` means that only half of vs1 is used.
  // `16 / MinDataWidth * 2` means that each index will produce 2x MinDataWidth index.
  val e64ei16MinSewIdx = Wire(Vec(dlen / 4 / 16, Vec(64 / MinDataWidth, Index)))
  val e32ei16MinSewIdx = Wire(Vec(dlen / 2 / 16, Vec(32 / MinDataWidth, Index)))
  val e8ei16MinSewIdx  = Wire(Vec(dlen / 16, Vec( 8 / MinDataWidth, Index)))

  /**
   * For example, if sew = 64,
   * {{{
   *   for (i <- 0 until vlen / 64) {
   *     for (j <- 0 until 64 / MinSEW) {
   *       e64MinSewIdx(i)(j) := Cat(e64Vs1(i).take(E64ValidBits), j.U((log2Ceil(64) - log2Ceil(MinSEW)).W))
   *     }
   *   }
   * }}}
   */
  def calculateMinSewIdx(eXXMinSewIdx: Vec[Vec[UInt]], eXXVs1: Vec[UInt], EXXValidBits: Int, sew: Int) = {
    for (i <- 0 until dlen / sew) {
      for (j <- 0 until sew / MinDataWidth) {
        eXXMinSewIdx(i)(j) := Cat(eXXVs1(i).takeOrPad(EXXValidBits), j.U((log2Ceil(sew) - log2Ceil(MinDataWidth)).W))
      }
    }
  }

  calculateMinSewIdx(e64ei64MinSewIdx, e64Vs1, E64ValidBits, 64)
  calculateMinSewIdx(e32ei32MinSewIdx, e32Vs1, E32ValidBits, 32)
  calculateMinSewIdx(e16ei16MinSewIdx, e16Vs1, E16ValidBits, 16)
  calculateMinSewIdx( e8ei8MinSewIdx,  e8Vs1,  E8ValidBits,  8)

  for (i <- e32ei16MinSewIdx.indices) {
    for (j <- e32ei16MinSewIdx.head.indices) {
      e32ei16MinSewIdx(i)(j) := Cat(e16Vs1(i).takeOrPad(E16ValidBits - 1), j.U((log2Ceil(16) - log2Ceil(MinDataWidth) + 1).W))
    }
  }

  for (i <- e64ei16MinSewIdx.indices) {
    for (j <- e64ei16MinSewIdx.head.indices) {
      e64ei16MinSewIdx(i)(j) := Cat(e16Vs1(i).takeOrPad(E16ValidBits - 2), j.U((log2Ceil(16) - log2Ceil(MinDataWidth) + 2).W))
    }
  }

  for (i <- e8ei16MinSewIdx.indices) {
    for (j <- e8ei16MinSewIdx.head.indices) {
      e8ei16MinSewIdx(i)(j) := Cat(e16Vs1(i).takeOrPad(E16ValidBits), j.U((log2Ceil(16) - log2Ceil(MinDataWidth - 1)).W))
    }
  }

  val e64ei64Idx = e64ei64MinSewIdx.flatten
  val e32ei32Idx = e32ei32MinSewIdx.flatten
  val e16ei16Idx = e16ei16MinSewIdx.flatten
  val e8ei8Idx   = e8ei8MinSewIdx.flatten
  val e64ei16Idx = e64ei16MinSewIdx.flatten
  val e32ei16Idx = e32ei16MinSewIdx.flatten
  // when sew is less than index width, pad to dlen / MinDataWidth
  val e8ei16Idx  = e8ei16MinSewIdx.flatten.padTo(dlen / MinDataWidth, 0.U)

  val e64ei64 = dontTouch(sew.e64 && ei.e64)
  val e32ei32 = dontTouch(sew.e32 && ei.e32)
  val e16ei16 = dontTouch(sew.e16 && ei.e16)
  val e8ei8   = dontTouch(sew.e8  && ei.e8)

  val e64ei16 = dontTouch(sew.e64 && ei.e16)
  val e32ei16 = dontTouch(sew.e32 && ei.e16)
  val e8ei16  = dontTouch(sew.e8  && ei.e16)

  val geVlmax = Wire(Vec(NumElem, Bool()))
  for (i <- 0 until NumElem) {
    // index >= vlmax
    geVlmax(i) := Mux1H(Seq(
      lmul.mf8 -> (out.gather.bits.index(i).head(6) =/= 0.U),
      lmul.mf4 -> (out.gather.bits.index(i).head(5) =/= 0.U),
      lmul.mf2 -> (out.gather.bits.index(i).head(4) =/= 0.U),
      lmul.m1  -> (out.gather.bits.index(i).head(3) =/= 0.U),
      lmul.m2  -> (out.gather.bits.index(i).head(2) =/= 0.U),
      lmul.m4  -> (out.gather.bits.index(i).head(1) =/= 0.U),
      // in m8 configuration, index will never greater than or equal to vlmax
    ))
  }

  out.gather.valid := in.valid && !in.bits.isGatherEI16
  out.gatherei16.valid := in.valid && in.bits.isGatherEI16

  for (i <- out.gather.bits.indexValid.indices) {
    out.gather.bits.indexValid(i) := !geVlmax(i)
    out.gather.bits.fillZeros(i) := geVlmax(i)
    out.gather.bits.fillScala(i) := false.B
  }

  for (i <- out.gather.bits.index.indices) {
    out.gather.bits.index(i) := Mux1H(Seq(
      e64ei64 -> e64ei64Idx(i),
      e32ei32 -> e32ei32Idx(i),
      e16ei16 -> e16ei16Idx(i),
      e8ei8   -> e8ei8Idx(i),

      e64ei16 -> e64ei16Idx(i),
      e32ei16 -> e32ei16Idx(i),
      e8ei16  -> e8ei16Idx(i),
    ))
  }
}

object GatherIndexGenMain extends App {
  println("Generating the GatherIndexGen hardware")

  val firtoolOpts = Array(
    "--target", "systemverilog",
    "-O=release",
    "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

  (new ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(() => new GatherIndexGen(dlen = 128, vlen = 128, MinDataWidth = 8)) +: firtoolAnno
  )

  println("done")
}
