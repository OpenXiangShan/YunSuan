package yunsuan.vector.v2.Shuffle

import _root_.circt.stage._
import chisel3._
import chisel3.util._
import yunsuan.util.ModuleWrapper.ModuleMux1H
import yunsuan.util.ModuleWrapper.ModuleVec
import yunsuan.vector.Common._
import yunsuan.vector.v2.Shuffle.GatherCommon._


class GatherIndexGen(implicit val param: GatherParam) extends GatherModule {

  override def desiredName: String = super.desiredName + s"_${DLEN}b"

  val in = IO(Input(ValidIO(new GatherIndexGen.In)))
  val out = IO(Output(new GatherIndexGen.Out))

  val E64Vlmax = DLEN * 8 / 64

  val E32Vlmax = DLEN * 8 / 32

  val E16Vlmax = DLEN * 8 / 16

  val E8Vlmax = DLEN * 8 / 8

  val E64ValidBits = log2Ceil(E64Vlmax)

  val E32ValidBits = log2Ceil(E32Vlmax)

  val E16ValidBits = log2Ceil(E16Vlmax)

  val E8ValidBits = log2Ceil(E8Vlmax)

  val sew = in.bits.sew

  val ei = in.bits.ei

  val lmul = in.bits.lmul
  // LMUL in units of m1 (=8): mf8=1, mf4=2, mf2=4, m1=8, m2=16, m4=32, m8=64
  private val lmulFrac = Mux1H(Seq(
    lmul.mf8 -> 1.U(7.W),
    lmul.mf4 -> 2.U(7.W),
    lmul.mf2 -> 4.U(7.W),
    lmul.m1  -> 8.U(7.W),
    lmul.m2  -> 16.U(7.W),
    lmul.m4  -> 32.U(7.W),
    lmul.m8  -> 64.U(7.W),
  ))

  val e64Vs1 = in.bits.vs1.to64bitVec

  val e32Vs1 = in.bits.vs1.to32bitVec

  val e16Vs1 = in.bits.vs1.to16bitVec

  val e8Vs1 = in.bits.vs1.to8bitVec

  val uopIdx = in.bits.uopIdx

  val e64ei64MinSewIdx = Wire(Vec(DLEN / 64, Vec(64 / MinDataWidth, Index)))

  val e32ei32MinSewIdx = Wire(Vec(DLEN / 32, Vec(32 / MinDataWidth, Index)))

  val e16ei16MinSewIdx = Wire(Vec(DLEN / 16, Vec(16 / MinDataWidth, Index)))

  val e8ei8MinSewIdx = Wire(Vec(DLEN / 8, Vec(8 / MinDataWidth, Index)))

  // `DLEN / 2` means that only half of vs1 is used.
  // `16 / MinDataWidth * 2` means that each index will produce 2x MinDataWidth index.
  val e64ei16MinSewIdx = Wire(Vec(DLEN / 4 / 16, Vec(64 / MinDataWidth, Index)))

  val e32ei16MinSewIdx = Wire(Vec(DLEN / 2 / 16, Vec(32 / MinDataWidth, Index)))

  // SEW=8 EI16 (new scheme): each gather uop reads 2 vs1 regs (2nd via vs3 port).
  // 16 slots = slots 0-7 from vs1 (src1), slots 8-15 from vs3 (src3); index = raw e16 value.
  val e8Vs1High = in.bits.vs3.to16bitVec
  val e8ei16Idx = Wire(Vec(NumElem, Index()))

  /**
   * For example, if sew = 64,
   * {{{
   *   for (i <- 0 until VLEN / 64) {
   *     for (j <- 0 until 64 / MinSEW) {
   *       e64MinSewIdx(i)(j) := Cat(e64Vs1(i).take(E64ValidBits), j.U((log2Ceil(64) - log2Ceil(MinSEW)).W))
   *     }
   *   }
   * }}}
   */
  def calculateMinSewIdx(eXXMinSewIdx: Vec[Vec[UInt]], eXXVs1: Vec[UInt], EXXValidBits: Int, sew: Int) =
    for (i <- 0 until DLEN / sew) {
      for (j <- 0 until sew / MinDataWidth) {
        eXXMinSewIdx(i)(j) := Cat(eXXVs1(i).takeOrPad(EXXValidBits), j.U((log2Ceil(sew) - log2Ceil(MinDataWidth)).W))
      }
    }

  calculateMinSewIdx(
    e64ei64MinSewIdx,
    e64Vs1,
    E64ValidBits,
    64
  )
  calculateMinSewIdx(
    e32ei32MinSewIdx,
    e32Vs1,
    E32ValidBits,
    32
  )
  calculateMinSewIdx(
    e16ei16MinSewIdx,
    e16Vs1,
    E16ValidBits,
    16
  )
  calculateMinSewIdx(e8ei8MinSewIdx, e8Vs1, E8ValidBits, 8)

  // SEW=32/EI=16: the e16 index is half the dest width, so 2 uops share one
  // vs1 register (Src1Select INCF2). splitToVecByWidth(64) gives 2 groups of
  // 4 e16; uopIdx(0) directly selects the group (synthesizes to a 2-to-1 mux).
  val e32ei16Grp = in.bits.vs1.splitToVecByWidth(64)
  val e32ei16Elems = e32ei16Grp(uopIdx(0, 0)).splitToVecByWidth(16)
  for (i <- e32ei16MinSewIdx.indices) {
    for (j <- e32ei16MinSewIdx.head.indices) {
      e32ei16MinSewIdx(i)(j) :=
        Cat(e32ei16Elems(i).takeOrPad(E16ValidBits - 1), j.U((log2Ceil(16) - log2Ceil(MinDataWidth) + 1).W))
    }
  }

  // SEW=64/EI=16: 4 uops share one vs1 register (Src1Select INCF4).
  // splitToVecByWidth(32) gives 4 groups of 2 e16; uopIdx(1,0) directly selects
  // the group (synthesizes to a 4-to-1 mux).
  val e64ei16Grp = in.bits.vs1.splitToVecByWidth(32)
  val e64ei16Elems = e64ei16Grp(uopIdx(1, 0)).splitToVecByWidth(16)
  for (i <- e64ei16MinSewIdx.indices) {
    for (j <- e64ei16MinSewIdx.head.indices) {
      e64ei16MinSewIdx(i)(j) :=
        Cat(e64ei16Elems(i).takeOrPad(E16ValidBits - 2), j.U((log2Ceil(16) - log2Ceil(MinDataWidth) + 2).W))
    }
  }

  for (i <- 0 until DLEN / 16) {
    e8ei16Idx(i) := e16Vs1(i).takeOrPad(IndexWidth)
    e8ei16Idx(i + DLEN / 16) := e8Vs1High(i).takeOrPad(IndexWidth)
  }

  val e64ei64Idx = e64ei64MinSewIdx.flatten

  val e32ei32Idx = e32ei32MinSewIdx.flatten

  val e16ei16Idx = e16ei16MinSewIdx.flatten

  val e8ei8Idx = e8ei8MinSewIdx.flatten

  val e64ei16Idx = e64ei16MinSewIdx.flatten

  val e32ei16Idx = e32ei16MinSewIdx.flatten

  val e64ei64 = dontTouch(sew.e64 && ei.e64)

  val e32ei32 = dontTouch(sew.e32 && ei.e32)

  val e16ei16 = dontTouch(sew.e16 && ei.e16)

  val e8ei8 = dontTouch(sew.e8 && ei.e8)

  val e64ei16 = dontTouch(sew.e64 && ei.e16)

  val e32ei16 = dontTouch(sew.e32 && ei.e16)

  val e8ei16 = dontTouch(sew.e8 && ei.e16)

  val geVlmax = Wire(Vec(NumElem, Bool()))

  // Element-level out-of-range: upper bits of SEW/EI-width element are non-zero.
  // These are missed by the byte-level geVlmax check because the index
  // construction truncates the element to EXXValidBits bits.
  def expandElemOor(vs1Vec: Vec[UInt], sew: Int, validBits: Int): Vec[Bool] = {
    val result = Wire(Vec(NumElem, Bool()))
    val nElems = DLEN / sew
    val subPerElem = sew / MinDataWidth
    for (i <- 0 until nElems) {
      val oor = if (validBits < sew) vs1Vec(i)(sew - 1, validBits).orR else false.B
      for (j <- 0 until subPerElem) {
        result(i * subPerElem + j) := oor
      }
    }
    for (k <- nElems * subPerElem until NumElem) {
      result(k) := false.B
    }
    result
  }

  val e64ei64ElemOor = expandElemOor(e64Vs1, 64, E64ValidBits)
  val e32ei32ElemOor = expandElemOor(e32Vs1, 32, E32ValidBits)
  val e16ei16ElemOor = expandElemOor(e16Vs1, 16, E16ValidBits)
  val e8ei8ElemOor   = expandElemOor(e8Vs1,   8,  E8ValidBits)
  // EI=16, SEW>16 (e32ei16/e64ei16): the byte-index path slices vs1 by uopIdx
  // and spreads each e16 index over SEW/8 byte slots, truncating the e16 to
  // (E16ValidBits - log2(sew/16)) bits. An out-of-range e16 (>= vlmax in SEW
  // elements) can thus wrap back into the truncated byte-index space and slip past
  // the byte-level geVlmax check (which is skipped for m8). So the element-level
  // OOR must use the SAME sliced source and slot layout as the byte-index path and
  // flag e16 >= vlmax, turning every out-of-range index into fillZeros.
  val e32ei16ElemOor = Wire(Vec(NumElem, Bool()))
  val e64ei16ElemOor = Wire(Vec(NumElem, Bool()))
  // vlmax (in SEW elements) = LMUL*VLEN/SEW = lmulFrac*VLEN/(8*SEW). For SEW>16
  // the VLEN/(8*SEW) factor is < 1, i.e. a right-shift of lmulFrac by
  // log2(8*SEW/VLEN); clamp to >= 1 (fractional VLMAX counts as 1 element).
  val e32ei16Vlmax = (lmulFrac >> log2Ceil(8 * 32 / VLEN)).max(1.U)
  val e64ei16Vlmax = (lmulFrac >> log2Ceil(8 * 64 / VLEN)).max(1.U)
  for (i <- 0 until e32ei16MinSewIdx.length) {
    val oor = e32ei16Elems(i) >= e32ei16Vlmax
    for (j <- 0 until e32ei16MinSewIdx.head.length) {
      e32ei16ElemOor(i * 4 + j) := oor
    }
  }
  for (i <- 0 until e64ei16MinSewIdx.length) {
    val oor = e64ei16Elems(i) >= e64ei16Vlmax
    for (j <- 0 until e64ei16MinSewIdx.head.length) {
      e64ei16ElemOor(i * 8 + j) := oor
    }
  }
  // SEW=8: index = raw e16 value (IndexWidth bits); element OOR = e16 bits above index width
  val e8ei16ElemOor = Wire(Vec(NumElem, Bool()))
  for (i <- 0 until DLEN / 16) {
    e8ei16ElemOor(i) := e16Vs1(i)(15, IndexWidth).orR
    e8ei16ElemOor(i + DLEN / 16) := e8Vs1High(i)(15, IndexWidth).orR
  }

  for (i <- 0 until NumElem) {
    val elemOor = Mux1H(Seq(
      e64ei64 -> e64ei64ElemOor(i),
      e32ei32 -> e32ei32ElemOor(i),
      e16ei16 -> e16ei16ElemOor(i),
      e8ei8   -> e8ei8ElemOor(i),
      e64ei16 -> e64ei16ElemOor(i),
      e32ei16 -> e32ei16ElemOor(i),
      e8ei16  -> e8ei16ElemOor(i),
    ))
    // index >= vlmax (byte-level) OR element upper bits set (element-level)
    geVlmax(i) := elemOor ||
      Mux1H(
        Seq(
          lmul.mf8 -> (out.gather.bits.index(i).head(6) =/= 0.U),
          lmul.mf4 -> (out.gather.bits.index(i).head(5) =/= 0.U),
          lmul.mf2 -> (out.gather.bits.index(i).head(4) =/= 0.U),
          lmul.m1  -> (out.gather.bits.index(i).head(3) =/= 0.U),
          lmul.m2  -> (out.gather.bits.index(i).head(2) =/= 0.U),
          lmul.m4  -> (out.gather.bits.index(i).head(1) =/= 0.U)
          // in m8 configuration, index will never greater than or equal to vlmax
        )
      )
  }

  out.gather.valid     := in.valid && !in.bits.isGatherEI16
  out.gatherei16.valid := in.valid && in.bits.isGatherEI16

  for (i <- out.gather.bits.indexValid.indices) {
    out.gather.bits.indexValid(i) := !geVlmax(i)
    out.gather.bits.fillZeros(i)  := geVlmax(i)
    out.gather.bits.fillScala(i)  := false.B
  }

  for (i <- out.gather.bits.index.indices) {
    out.gather.bits.index(i) :=
      Mux1H(
        Seq(
          e64ei64 -> e64ei64Idx(i),
          e32ei32 -> e32ei32Idx(i),
          e16ei16 -> e16ei16Idx(i),
          e8ei8   -> e8ei8Idx(i),
          e64ei16 -> e64ei16Idx(i),
          e32ei16 -> e32ei16Idx(i),
          e8ei16  -> e8ei16Idx(i)
        )
      )
  }
}

object GatherIndexGen {
  class In(implicit val param: GatherParam) extends GatherBundle {
    val isGatherEI16 = Bool()
    val uopIdx       = UInt(log2Ceil(VLEN / DLEN * 8).W)
    val lmul         = new GatherLmulBundle
    val sew          = new GatherSewBundle
    val ei           = new GatherSewBundle
    // Todo: if vrgatherei16, vs1 should be selected before the register of EXU
    val vs1  = UInt(DLEN.W)
    // SEW=8 vrgatherei16: the 2nd vs1 register (vs1+2i+1) arrives via the vs3 port
    val vs3  = UInt(DLEN.W)
  }

  class Out(implicit val param: GatherParam) extends GatherBundle {
    val gather     = ValidIO(new GatherIndexBundle(NumElem, IndexWidth))
    val gatherei16 = ValidIO(new Bundle {})
  }

  def main(args: Array[String]): Unit = {
    import _root_.circt.stage._

    val targetDir = "build/vector"

    println(s"Generating the $this hardware in $targetDir")

    val firtoolOpts = Array(
      "-O=release",
      "--disable-annotation-unknown",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
    )
    val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

    (new ChiselStage).execute(
      Array(
        "--target",
        "systemverilog",
        "--target-dir",
        targetDir,
      ) ++ args,
      chisel3.stage.ChiselGeneratorAnnotation(
        () => new GatherIndexGen()(GatherParam.default)
      ) +: firtoolAnno
    )

    println("done")
  }
}

