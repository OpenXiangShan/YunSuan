package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.ExpectationValueFormat
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.util.SpecBase
import yunsuan.vector.v2.Shuffle.GatherCommon.GatherParam

/** Meta with robIdx and pdest for round-trip testing (mirrors xiangshan PermMeta). */
class FakePermMeta extends Bundle {
  val robIdx = UInt(8.W)
  val pdest  = UInt(8.W)
}
object FakePermMeta {
  def apply() = new FakePermMeta
}

class PermUnitSpec extends SpecBase with GatherSpecUtils {

  val NumPipes = 4
  val NumUnits = GatherParam.default.numUnits
  val issue2exu = GatherParam.default.issue2exu

  // ---- Golden objects --------------------------------------------------

  def foldBytes(bytes: Seq[Int]): BigInt =
    bytes.foldRight(BigInt(0))((b, a) => (a << 8) | BigInt(b))

  object Table {
    def inc(numTable: Int = 1): Seq[Int] = Seq.tabulate(numTable * 16)(identity)
  }

  object Index {
    def allInTable0(numTable: Int = 1) = Seq.tabulate(numTable * 16)(x => 15 - x % 16)
    def allInTable1(numTable: Int = 1) = Seq.tabulate(numTable * 16)(x => 16 + 15 - x % 16)
    def unzip2(numTable: Int = 1) = Seq.tabulate(numTable * 16)(x => x / 2 + (x % 2) * 16)
    def unzip4(numTable: Int = 1) = Seq.tabulate(numTable * 16)(x => x / 4 + x % 4 * 16)
  }

  object Prefill {
    def default(numTable: Int = 1): Seq[Int] = Seq.tabulate(numTable * 16)(0xff - _)
  }

  // ---- Per-op index flag generators (formulas borrowed from per-op specs) ----

  // Per-byte (index, indexValid, fillZeros, fillScala).
  def goldenGather(vs1: BigInt, sew: Int, ei: Int, lmulFrac: Int, uopIdx: Int): Seq[(Int, Boolean, Boolean, Boolean)] = {
    val sewbVal = sewb(sew)
    val nIdxElems = dlen / ei
    val vlmax = lmulFrac * vlen / 64
    val res = Array.fill(numElem)((0, false, false, false))
    for (idxElem <- 0 until nIdxElems; j <- 0 until sewbVal) {
      val rawIdx = ((vs1 >> (idxElem * ei)) & ((BigInt(1) << ei) - 1)).toInt
      val eightBIdx = rawIdx * sewbVal + j
      val outPos = idxElem * sewbVal + j
      if (outPos < numElem) {
        val geVlmax = eightBIdx >= vlmax
        res(outPos) = (eightBIdx, !geVlmax, geVlmax, false)
      }
    }
    res.toSeq
  }

  def goldenSlideUp(tableIdx: Int, offset: Int, sew: Int, vl: Int): Seq[(Int, Boolean, Boolean, Boolean)] = {
    val shift = sewShift(sew)
    val e8Off = offset << shift
    val e8Vl = vl << shift
    val offsetOverflow = e8Off >= e8Vl // offset >= vl: no element is shifted
    val tableBase = tableIdx * numElem // absolute byte base of this uop's segment
    (0 until numElem).map { i =>
      val absPos = tableBase + i
      // stay region is global (whole LMUL group): absPos < min(e8Vl, e8Off)
      val isStay = offsetOverflow || (absPos < e8Vl && absPos < e8Off)
      val idx = if (isStay) absPos else absPos - e8Off
      // stay elements are "unchanged" (old vd) -> PREFILL (iv=false), not
      // a LOOKUP of vs2[i] (emu vslideup_vi-0 block3).
      (idx.max(0), !isStay, false, false)
    }
  }

  def goldenSlide1Up(sew: Int, tableIdx: Int = 0): Seq[(Int, Boolean, Boolean, Boolean)] = {
    val sb = sewb(sew)
    val tableBase = tableIdx * numElem
    (0 until numElem).map { i =>
      val elemIdx = tableIdx * numElem / sb + i / sb
      val isFirst = tableIdx == 0 && elemIdx == 0
      ((tableBase + i - sb).max(0), !isFirst, false, isFirst)
    }
  }

  def goldenSlideDown(tableIdx: Int, offset: Int, sew: Int, lmulFrac: Int): Seq[(Int, Boolean, Boolean, Boolean)] = {
    val e8Off = offset << sewShift(sew)
    val vlmax8b = lmulFrac * vlen / 64
    val tableBase = tableIdx * dlen / 8
    (0 until numElem).map { i =>
      val iPlusOffset = tableBase + i + e8Off
      val outOfRange = iPlusOffset >= vlmax8b
      (iPlusOffset, !outOfRange, outOfRange, false)
    }
  }

  def goldenSlide1Down(tableIdx: Int, sew: Int, vl: Int): Seq[(Int, Boolean, Boolean, Boolean)] = {
    val sb = sewb(sew)
    val tableBase = tableIdx * dlen / 8
    (0 until numElem).map { i =>
      val elemIdx = tableBase / sb + i / sb
      val isLast = elemIdx == vl - 1
      val iPlusOff = tableBase + i + sb
      if (isLast) (iPlusOff, false, false, true) else (iPlusOff, true, false, false)
    }
  }

  /** Full PermUnit golden: per-byte result for one uop, replicating IndexGen
    * elemFlags/elemData (tail/mask -> PREFILL, fz -> ZERO, fs -> SCALAR, iv -> LOOKUP).
    */
  def goldenPerm(
    flags:       Seq[(Int, Boolean, Boolean, Boolean)],
    table:       Seq[Int],
    tableRange:  Seq[Int],
    prefill:     Seq[Int],
    scalarBytes: Seq[Int],
    sew:         Int,
    uopIdx:      Int,
    vl:          Int,
    vta:         Boolean,
    vma:         Boolean,
    mask:        BigInt,
    isCompress:  Boolean = false
  ): BigInt = {
    val e8vl = vl << sewShift(sew)
    val sb = sewb(sew)
    def scalarByte(i: Int): Int = scalarBytes(i % sb)
    flags.indices.map { i =>
      val (idx, iv, fz, fs) = flags(i)
      val isTail = (uopIdx * numElem + i) >= e8vl
      val maskInactive = !isCompress && (((mask >> i) & 1) == 0)
      val agnostic = if (isCompress || isTail) vta else vma
      val prefillByte = if (agnostic) 0xff else prefill(i)
      if (isTail || maskInactive) prefillByte
      else if (fz) 0
      else if (fs) scalarByte(i)
      else if (iv) { if (tableRange.contains(idx / 16)) table(idx) else idx }
      else prefillByte
    }.foldRight(BigInt(0))((b, a) => (a << 8) | BigInt(b))
  }

  def gatherGolden(vs1: BigInt, sew: Int, ei: Int, lmulFrac: Int, uopIdx: Int,
    table: Seq[Int], tableRange: Seq[Int], prefill: Seq[Int],
    vl: Int, vta: Boolean = false, vma: Boolean = false, mask: BigInt = -1
  ): BigInt = {
    val flags = goldenGather(vs1, sew, ei, lmulFrac, uopIdx)
    val m = if (mask < 0) (BigInt(1) << numElem) - 1 else mask
    goldenPerm(flags, table, tableRange, prefill, Seq.fill(8)(0), sew, uopIdx, vl, vta, vma, m)
  }

  // ---- Poke helpers ----------------------------------------------------

  def pMeta(dut: PermUnit[FakePermMeta], pipe: Int, robIdx: Int, pdest: Int): Unit = {
    val m = dut.in.pipes(pipe).bits.meta
    m.robIdx.poke(robIdx)
    m.pdest.poke(pdest)
  }

  def pDefaults(dut: PermUnit[FakePermMeta], pipe: Int): Unit = {
    val p = dut.in.pipes(pipe)
    p.valid.poke(false)
    p.bits.placeIdx.poke(0); p.bits.blockBase.poke(0)
    val o = p.bits.op
    o.isGather.poke(false); o.isGatherEI16.poke(false)
    o.isSlideUp.poke(false); o.isSlide1Up.poke(false)
    o.isSlideDown.poke(false); o.isSlide1Down.poke(false)
    o.isZip.poke(false); o.isUnzip.poke(false); o.isCompress.poke(false)
    p.bits.uopIdx.poke(0)
    pSew(p.bits.sew, 8); pSew(p.bits.ei, 8); pLmul(p.bits.lmul, 8)
    p.bits.blockFieldNum.poke(1)
    p.bits.vl.poke(16)
    p.bits.vta.poke(false); p.bits.vma.poke(false)
    p.bits.vs1.poke(0); p.bits.vs2.poke(0); p.bits.vs3.poke(0); p.bits.rs1.poke(0)
    p.bits.mask.poke((BigInt(1) << numElem) - 1); p.bits.zipNum.poke(0)
    pMeta(dut, pipe, 0, 0)
  }

  def pGather(dut: PermUnit[FakePermMeta], pipe: Int,
    sew: Int, ei: Int, vs1: BigInt, vs2: Seq[Int], vs3: Seq[Int],
    lmulFrac: Int = 8, uopIdx: Int = 0, blockBase: Int = 0, placeIdx: Int = 0,
    vl: Int = -1, vta: Boolean = false, vma: Boolean = false,
    mask: BigInt = -1, robIdx: Int = 0, pdest: Int = 0
  ): Unit = {
    val p = dut.in.pipes(pipe)
    p.valid.poke(true)
    p.bits.op.isGather.poke(true)
    p.bits.placeIdx.poke(placeIdx); p.bits.blockBase.poke(blockBase)
    p.bits.uopIdx.poke(uopIdx)
    pSew(p.bits.sew, sew); pSew(p.bits.ei, ei); pLmul(p.bits.lmul, lmulFrac)
    p.bits.blockFieldNum.poke(((lmulFrac / 8).max(1)).U)
    p.bits.vl.poke(if (vl < 0) (lmulFrac / 8) * vlen / sew else vl)
    p.bits.vta.poke(vta); p.bits.vma.poke(vma)
    p.bits.vs1.poke(vs1)
    p.bits.vs2.poke(foldBytes(vs2))
    p.bits.vs3.poke(foldBytes(vs3))
    p.bits.mask.poke(if (mask < 0) (BigInt(1) << numElem) - 1 else mask)
    pMeta(dut, pipe, robIdx, pdest)
  }

  def pPipeOff(dut: PermUnit[FakePermMeta], pipe: Int): Unit = {
    dut.in.pipes(pipe).valid.poke(false)
  }

  // ---- Op pokes (vslideup / vslide1up / vslidedown / vslide1down) ----------

  def pSlideUp(dut: PermUnit[FakePermMeta], pipe: Int,
    offset: Int, vs2: Seq[Int], vs3: Seq[Int],
    sew: Int = 8, lmulFrac: Int = 8, uopIdx: Int = 0, blockBase: Int = 0, placeIdx: Int = 0,
    vl: Int = -1, vta: Boolean = false, vma: Boolean = false,
    mask: BigInt = -1, robIdx: Int = 0, pdest: Int = 0
  ): Unit = {
    val p = dut.in.pipes(pipe)
    p.valid.poke(true)
    p.bits.op.isSlideUp.poke(true)
    p.bits.placeIdx.poke(placeIdx); p.bits.blockBase.poke(blockBase)
    p.bits.uopIdx.poke(uopIdx)
    pSew(p.bits.sew, sew); pLmul(p.bits.lmul, lmulFrac)
    p.bits.blockFieldNum.poke(((lmulFrac / 8).max(1)).U)
    p.bits.vl.poke(if (vl < 0) (lmulFrac / 8) * vlen / sew else vl)
    p.bits.vta.poke(vta); p.bits.vma.poke(vma)
    p.bits.rs1.poke(offset)
    p.bits.vs2.poke(foldBytes(vs2)); p.bits.vs3.poke(foldBytes(vs3))
    p.bits.mask.poke(if (mask < 0) (BigInt(1) << numElem) - 1 else mask)
    pMeta(dut, pipe, robIdx, pdest)
  }

  def pSlide1Up(dut: PermUnit[FakePermMeta], pipe: Int,
    scalar: BigInt, vs2: Seq[Int], vs3: Seq[Int],
    sew: Int = 8, lmulFrac: Int = 8, uopIdx: Int = 0, blockBase: Int = 0, placeIdx: Int = 0,
    vl: Int = -1, vta: Boolean = false, vma: Boolean = false,
    mask: BigInt = -1, robIdx: Int = 0, pdest: Int = 0
  ): Unit = {
    val p = dut.in.pipes(pipe)
    p.valid.poke(true)
    p.bits.op.isSlide1Up.poke(true)
    p.bits.placeIdx.poke(placeIdx); p.bits.blockBase.poke(blockBase)
    p.bits.uopIdx.poke(uopIdx)
    pSew(p.bits.sew, sew); pLmul(p.bits.lmul, lmulFrac)
    p.bits.blockFieldNum.poke(((lmulFrac / 8).max(1)).U)
    p.bits.vl.poke(if (vl < 0) (lmulFrac / 8) * vlen / sew else vl)
    p.bits.vta.poke(vta); p.bits.vma.poke(vma)
    p.bits.rs1.poke(scalar)
    p.bits.vs2.poke(foldBytes(vs2)); p.bits.vs3.poke(foldBytes(vs3))
    p.bits.mask.poke(if (mask < 0) (BigInt(1) << numElem) - 1 else mask)
    pMeta(dut, pipe, robIdx, pdest)
  }

  def pSlideDown(dut: PermUnit[FakePermMeta], pipe: Int,
    offset: Int, vs2: Seq[Int], vs3: Seq[Int],
    sew: Int = 8, lmulFrac: Int = 8, uopIdx: Int = 0, blockBase: Int = 0, placeIdx: Int = 0,
    vl: Int = -1, vta: Boolean = false, vma: Boolean = false,
    mask: BigInt = -1, robIdx: Int = 0, pdest: Int = 0
  ): Unit = {
    val p = dut.in.pipes(pipe)
    p.valid.poke(true)
    p.bits.op.isSlideDown.poke(true)
    p.bits.placeIdx.poke(placeIdx); p.bits.blockBase.poke(blockBase)
    p.bits.uopIdx.poke(uopIdx)
    pSew(p.bits.sew, sew); pLmul(p.bits.lmul, lmulFrac)
    p.bits.blockFieldNum.poke(((lmulFrac / 8).max(1)).U)
    p.bits.vl.poke(if (vl < 0) (lmulFrac / 8) * vlen / sew else vl)
    p.bits.vta.poke(vta); p.bits.vma.poke(vma)
    p.bits.rs1.poke(offset)
    p.bits.vs2.poke(foldBytes(vs2)); p.bits.vs3.poke(foldBytes(vs3))
    p.bits.mask.poke(if (mask < 0) (BigInt(1) << numElem) - 1 else mask)
    pMeta(dut, pipe, robIdx, pdest)
  }

  def pSlide1Down(dut: PermUnit[FakePermMeta], pipe: Int,
    scalar: BigInt, vs2: Seq[Int], vs3: Seq[Int],
    sew: Int = 8, lmulFrac: Int = 8, uopIdx: Int = 0, blockBase: Int = 0, placeIdx: Int = 0,
    vl: Int = -1, vta: Boolean = false, vma: Boolean = false,
    mask: BigInt = -1, robIdx: Int = 0, pdest: Int = 0
  ): Unit = {
    val p = dut.in.pipes(pipe)
    p.valid.poke(true)
    p.bits.op.isSlide1Down.poke(true)
    p.bits.placeIdx.poke(placeIdx); p.bits.blockBase.poke(blockBase)
    p.bits.uopIdx.poke(uopIdx)
    pSew(p.bits.sew, sew); pLmul(p.bits.lmul, lmulFrac)
    p.bits.blockFieldNum.poke(((lmulFrac / 8).max(1)).U)
    p.bits.vl.poke(if (vl < 0) (lmulFrac / 8) * vlen / sew else vl)
    p.bits.vta.poke(vta); p.bits.vma.poke(vma)
    p.bits.rs1.poke(scalar)
    p.bits.vs2.poke(foldBytes(vs2)); p.bits.vs3.poke(foldBytes(vs3))
    p.bits.mask.poke(if (mask < 0) (BigInt(1) << numElem) - 1 else mask)
    pMeta(dut, pipe, robIdx, pdest)
  }

  // ---- Golden wrappers (slide ops) ----------------------------------------
  // slideup / slide1up: indices are relative to the uop's own table segment
  // (vs2.slice(uopIdx*16, ...)); slidedown / slide1down: indices are absolute
  // (include tableIdx*16), so they index the full vs2.
  def slideUpGolden(offset: Int, vs2: Seq[Int], vs3: Seq[Int], sew: Int, lmulFrac: Int,
    uopIdx: Int, vl: Int, vta: Boolean = false, vma: Boolean = false, mask: BigInt = -1): BigInt = {
    val flags = goldenSlideUp(uopIdx, offset, sew, vl)
    // table = full group (absolute index); prefill = uop's own vs3 slice
    goldenPerm(flags, vs2, 0 until Math.max(1, lmulFrac / 8), vs3,
      Seq.fill(8)(0), sew, uopIdx, vl, vta, vma,
      if (mask < 0) (BigInt(1) << numElem) - 1 else mask)
  }

  def slide1UpGolden(scalarBytes: Seq[Int], vs2: Seq[Int], vs3: Seq[Int], sew: Int, lmulFrac: Int,
    uopIdx: Int, vl: Int, vta: Boolean = false, vma: Boolean = false, mask: BigInt = -1): BigInt = {
    val flags = goldenSlide1Up(sew, uopIdx)
    // table = full group (absolute index); prefill = uop's own vs3 slice
    goldenPerm(flags, vs2, 0 until Math.max(1, lmulFrac / 8), vs3,
      scalarBytes, sew, uopIdx, vl, vta, vma,
      if (mask < 0) (BigInt(1) << numElem) - 1 else mask)
  }

  def slideDownGolden(offset: Int, vs2: Seq[Int], vs3: Seq[Int], sew: Int, lmulFrac: Int,
    uopIdx: Int, vl: Int, vta: Boolean = false, vma: Boolean = false, mask: BigInt = -1): BigInt = {
    val flags = goldenSlideDown(uopIdx, offset, sew, lmulFrac)
    goldenPerm(flags, vs2, 0 until (lmulFrac / 8), vs3,
      Seq.fill(8)(0), sew, uopIdx, vl, vta, vma,
      if (mask < 0) (BigInt(1) << numElem) - 1 else mask)
  }

  def slide1DownGolden(scalarBytes: Seq[Int], vs2: Seq[Int], vs3: Seq[Int], sew: Int, lmulFrac: Int,
    uopIdx: Int, vl: Int, vta: Boolean = false, vma: Boolean = false, mask: BigInt = -1): BigInt = {
    val flags = goldenSlide1Down(uopIdx, sew, vl)
    goldenPerm(flags, vs2, 0 until (lmulFrac / 8), vs3,
      scalarBytes, sew, uopIdx, vl, vta, vma,
      if (mask < 0) (BigInt(1) << numElem) - 1 else mask)
  }

  // ---- Misc helper ---------------------------------------------------------
  def scalarBytes(v: BigInt): Seq[Int] = (0 until 8).map(i => ((v >> (8 * i)) & 0xff).toInt)

  // ---- vcompress ----
  def pCompress(dut: PermUnit[FakePermMeta], pipe: Int,
    mask: BigInt, vs2: Seq[Int], vs3: Seq[Int],
    lmulFrac: Int = 8, uopIdx: Int = 0, blockBase: Int = 0, placeIdx: Int = 0,
    vl: Int = -1, vta: Boolean = false, robIdx: Int = 0, pdest: Int = 0,
    sew: Int = 8
  ): Unit = {
    val p = dut.in.pipes(pipe)
    p.valid.poke(true)
    p.bits.op.isCompress.poke(true)
    p.bits.placeIdx.poke(placeIdx); p.bits.blockBase.poke(blockBase)
    p.bits.uopIdx.poke(uopIdx)
    pSew(p.bits.sew, sew); pLmul(p.bits.lmul, lmulFrac)
    p.bits.blockFieldNum.poke(((lmulFrac / 8).max(1)).U)
    p.bits.vl.poke(if (vl < 0) (lmulFrac / 8) * vlen / sew else vl)
    p.bits.vta.poke(vta); p.bits.vma.poke(true)  // compress uses byte mask (vs1), merge mask irrelevant
    p.bits.vs1.poke(mask)   // compress element mask (SEW-expanded inside IndexGen)
    p.bits.vs2.poke(foldBytes(vs2)); p.bits.vs3.poke(foldBytes(vs3))
    p.bits.mask.poke((BigInt(1) << numElem) - 1)
    pMeta(dut, pipe, robIdx, pdest)
  }

  /** vcompress golden (byte-granular, global packing across chunks):
    * packed[p] = vs2Full[globalPos of (p+1)-th set bit]; a uop's output byte j
    * = packed[uopIdx*16 + j] when that global output position < popcount(mask),
    * else prefill (vta-agnostic or vs3 local).
    * Only mask bits within vl select source elements (RISC-V V spec: vcompress
    * packs "the first vl elements of vs2"); bits beyond vl are ignored.
    */
  def compressGolden(mask: BigInt, vs2Full: Seq[Int], vs3: Seq[Int], uopIdx: Int,
    vl: Int, vta: Boolean = false): BigInt = {
    val activeMask = mask & ((BigInt(1) << vl) - 1)
    val total = activeMask.bitCount
    val packed = Array.ofDim[Int](total)
    var p = 0
    for (g <- vs2Full.indices) {
      if (((activeMask >> g) & 1) == 1) { packed(p) = vs2Full(g); p += 1 }
    }
    (0 until numElem).map { j =>
      val outPos = uopIdx * numElem + j
      if (outPos < vl && outPos < total) packed(outPos)
      else if (vta) 0xff else vs3(j % numElem)
    }.foldRight(BigInt(0))((b, a) => (a << 8) | BigInt(b))
  }

  def initDut(dut: PermUnit[FakePermMeta]): Unit = {
    for (p <- 0 until NumPipes) pDefaults(dut, p)
    dut.in.wbReady.foreach(_.poke(true))
    dut.in.unitFlush.foreach(_.poke(false))
    dut.in.wakeAck.foreach(_.poke(true))
    dut.clock.step()
  }

  def eWb(dut: PermUnit[FakePermMeta], pipe: Int, dest: BigInt): Unit = {
    dut.out.pipes(pipe).valid.expect(true.B, s"pipe $pipe wb.valid")
    dut.out.pipes(pipe).bits.data.expect(dest)
  }

  def eWbAfter(dut: PermUnit[FakePermMeta], pipe: Int, dest: BigInt, steps: Int): Unit = {
    var waited = 0
    while (!dut.out.pipes(pipe).valid.peekBoolean() && waited < steps + issue2exu + NumUnits) {
      dut.clock.step()
      waited += 1
    }
    eWb(dut, pipe, dest)
  }

  /** Step maxSteps cycles, latching each pipe's first writeback; used when
    * wbReady=1 fires the writeback for a single cycle (timing varies).
    */
  def collectWb(dut: PermUnit[FakePermMeta], maxSteps: Int): Array[Option[BigInt]] = {
    val wbData = Array.fill(NumPipes)(Option.empty[BigInt])
    for (_ <- 0 until maxSteps) {
      for (p <- 0 until NumPipes) {
        if (dut.out.pipes(p).valid.peekBoolean() && wbData(p).isEmpty) {
          wbData(p) = Some(dut.out.pipes(p).bits.data.peekValue().asBigInt)
        }
      }
      dut.clock.step()
    }
    wbData
  }

  // ---- Tests ---------------------------------------------------------------

  describe("Scaling") {

    describe("LMUL=1") {
      it("identity unit 0") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1)
          val golden = gatherGolden(0, 8, 8, 8, 0, table, 0 until 1, Prefill.default(1), 16)
          pGather(dut, 0, 8, 8, 0, table, Prefill.default(1))
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("identity unit 4") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1)
          val golden = gatherGolden(0, 8, 8, 8, 0, table, 0 until 1, Prefill.default(1), 16)
          pGather(dut, 0, 8, 8, 0, table, Prefill.default(1), blockBase = 4)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("two parallel") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1)
          val golden = gatherGolden(0, 8, 8, 8, 0, table, 0 until 1, Prefill.default(1), 16)
          pGather(dut, 0, 8, 8, 0, table, Prefill.default(1), blockBase = 0)
          pGather(dut, 2, 8, 8, 0, table, Prefill.default(1), blockBase = 2)
          dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 2)
          eWbAfter(dut, 0, golden, 2)
          eWbAfter(dut, 2, golden, 0)
        }
      }
    }

    describe("LMUL=2") {
      it("identity") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(2)
          val vs1 = foldBytes(Index.allInTable0(1))
          val golden = gatherGolden(vs1, 8, 8, 16, 0, table, 0 until 2, Prefill.default(2), 32)
          pGather(dut, 0, 8, 8, vs1, table.slice(0, 16), Prefill.default(2).slice(0, 16), lmulFrac = 16, uopIdx = 0, placeIdx = 0)
          pGather(dut, 1, 8, 8, vs1, table.slice(16, 32), Prefill.default(2).slice(16, 32), lmulFrac = 16, uopIdx = 1, placeIdx = 1)
          dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
          eWbAfter(dut, 0, golden, 4)
        }
      }
      it("all in seg1") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(2)
          val vs1 = foldBytes(Index.allInTable1(1))
          val golden = gatherGolden(vs1, 8, 8, 16, 0, table, 0 until 2, Prefill.default(2), 32)
          pGather(dut, 0, 8, 8, vs1, table.slice(0, 16), Prefill.default(2).slice(0, 16), lmulFrac = 16, uopIdx = 0, placeIdx = 0)
          pGather(dut, 1, 8, 8, vs1, table.slice(16, 32), Prefill.default(2).slice(16, 32), lmulFrac = 16, uopIdx = 1, placeIdx = 1)
          dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
          eWbAfter(dut, 0, golden, 4)
        }
      }
      it("zip interleave") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(2)
          val vs1 = foldBytes(Index.unzip2(1))
          val golden = gatherGolden(vs1, 8, 8, 16, 0, table, 0 until 2, Prefill.default(2), 32)
          pGather(dut, 0, 8, 8, vs1, table.slice(0, 16), Prefill.default(2).slice(0, 16), lmulFrac = 16, uopIdx = 0, placeIdx = 0)
          pGather(dut, 1, 8, 8, vs1, table.slice(16, 32), Prefill.default(2).slice(16, 32), lmulFrac = 16, uopIdx = 1, placeIdx = 1)
          dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
          eWbAfter(dut, 0, golden, 4)
        }
      }
      it("blockBase=2") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(2)
          val vs1 = foldBytes(Index.allInTable0(1))
          val golden = gatherGolden(vs1, 8, 8, 16, 0, table, 0 until 2, Prefill.default(2), 32)
          pGather(dut, 0, 8, 8, vs1, table.slice(0, 16), Prefill.default(2).slice(0, 16), lmulFrac = 16, uopIdx = 0, blockBase = 2, placeIdx = 0)
          pGather(dut, 1, 8, 8, vs1, table.slice(16, 32), Prefill.default(2).slice(16, 32), lmulFrac = 16, uopIdx = 1, blockBase = 2, placeIdx = 1)
          dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
          eWbAfter(dut, 2, golden, 4)
        }
      }
    }

    describe("LMUL=4") {
      it("identity") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(4)
          val vs1 = foldBytes(Index.unzip4(1))
          val golden = gatherGolden(vs1, 8, 8, 32, 0, table, 0 until 4, Prefill.default(4), 64)
          for (p <- 0 until 4) {
            pGather(dut, p, 8, 8, vs1, table.slice(p * 16, (p + 1) * 16),
              Prefill.default(4).slice(p * 16, (p + 1) * 16), lmulFrac = 32, uopIdx = p, placeIdx = p)
          }
          dut.clock.step(); for (p <- 0 until 4) pPipeOff(dut, p)
          eWbAfter(dut, 0, golden, 12)
        }
      }
      it("blockBase=4") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(4)
          val vs1 = foldBytes(Index.unzip4(1))
          val golden = gatherGolden(vs1, 8, 8, 32, 0, table, 0 until 4, Prefill.default(4), 64)
          for (p <- 0 until 4) {
            pGather(dut, p, 8, 8, vs1, table.slice(p * 16, (p + 1) * 16),
              Prefill.default(4).slice(p * 16, (p + 1) * 16), lmulFrac = 32, uopIdx = p, blockBase = 4, placeIdx = p)
          }
          dut.clock.step(); for (p <- 0 until 4) pPipeOff(dut, p)
          eWbAfter(dut, 0, golden, 14)
        }
      }
    }

    describe("LMUL=8") {
      it("identity") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(8)
          val vs1 = foldBytes(Index.allInTable0(1))
          val golden0 = gatherGolden(vs1, 8, 8, 64, 0, table, 0 until 8, Prefill.default(8), 128)
          for (p <- 0 until 4) {
            pGather(dut, p, 8, 8, vs1, table.slice(p * 16, (p + 1) * 16),
              Prefill.default(8).slice(p * 16, (p + 1) * 16), lmulFrac = 64, uopIdx = p, placeIdx = p)
          }
          dut.clock.step(); for (p <- 0 until 4) pPipeOff(dut, p)
          for (p <- 0 until 4) {
            pGather(dut, p, 8, 8, vs1, table.slice((p + 4) * 16, (p + 5) * 16),
              Prefill.default(8).slice((p + 4) * 16, (p + 5) * 16), lmulFrac = 64, uopIdx = p + 4, placeIdx = p + 4)
          }
          dut.clock.step(); for (p <- 0 until 4) pPipeOff(dut, p)
          eWbAfter(dut, 0, golden0, 11)
        }
      }

      describe("upper ring blockBase=8 smoke") {
        it("m1 uses unit 8") {
          simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
            initDut(dut)
            val table = Table.inc(1)
            val golden = gatherGolden(0, 8, 8, 8, 0, table, 0 until 1, Prefill.default(1), 16)
            pGather(dut, 0, 8, 8, 0, table, Prefill.default(1), blockBase = 8)
            dut.clock.step(); pPipeOff(dut, 0)
            eWbAfter(dut, 0, golden, 2)
          }
        }

        it("m2 uses units 8-9") {
          simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
            initDut(dut)
            val table = Table.inc(2)
            val vs1 = foldBytes(Index.allInTable0(1))
            val golden = gatherGolden(vs1, 8, 8, 16, 0, table, 0 until 2, Prefill.default(2), 32)
            pGather(dut, 0, 8, 8, vs1, table.slice(0, 16), Prefill.default(2).slice(0, 16),
              lmulFrac = 16, uopIdx = 0, blockBase = 8, placeIdx = 0)
            pGather(dut, 1, 8, 8, vs1, table.slice(16, 32), Prefill.default(2).slice(16, 32),
              lmulFrac = 16, uopIdx = 1, blockBase = 8, placeIdx = 1)
            dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
            eWbAfter(dut, 0, golden, 4)
          }
        }

        it("m4 uses units 8-11") {
          simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
            initDut(dut)
            val table = Table.inc(4)
            val vs1 = foldBytes(Index.unzip4(1))
            val golden = gatherGolden(vs1, 8, 8, 32, 0, table, 0 until 4, Prefill.default(4), 64)
            for (p <- 0 until 4) {
              pGather(dut, p, 8, 8, vs1, table.slice(p * 16, (p + 1) * 16),
                Prefill.default(4).slice(p * 16, (p + 1) * 16),
                lmulFrac = 32, uopIdx = p, blockBase = 8, placeIdx = p)
            }
            dut.clock.step(); for (p <- 0 until 4) pPipeOff(dut, p)
            eWbAfter(dut, 0, golden, 12)
          }
        }

        it("m8 uses units 8-15") {
          simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
            initDut(dut)
            val table = Table.inc(8)
            val vs1 = foldBytes(Index.allInTable0(1))
            val golden = gatherGolden(vs1, 8, 8, 64, 0, table, 0 until 8, Prefill.default(8), 128)
            for (p <- 0 until 4) {
              pGather(dut, p, 8, 8, vs1, table.slice(p * 16, (p + 1) * 16),
                Prefill.default(8).slice(p * 16, (p + 1) * 16),
                lmulFrac = 64, uopIdx = p, blockBase = 8, placeIdx = p)
            }
            dut.clock.step(); for (p <- 0 until 4) pPipeOff(dut, p)
            for (p <- 0 until 4) {
              pGather(dut, p, 8, 8, vs1, table.slice((p + 4) * 16, (p + 5) * 16),
                Prefill.default(8).slice((p + 4) * 16, (p + 5) * 16),
                lmulFrac = 64, uopIdx = p + 4, blockBase = 8, placeIdx = p + 4)
            }
            dut.clock.step(); for (p <- 0 until 4) pPipeOff(dut, p)
            eWbAfter(dut, 0, golden, 11)
          }
        }
      }
    }
  }

  describe("MultiUop") {

    // Per-cycle injection: uops of one instruction arrive in consecutive cycles
    // (back-to-back dispatch). Placement is dynamic, so a later uop may land in
    // a different block slot than the previous one.
    describe("per-cycle") {

      // Per-cycle in-order: uop1's place must not prematurely clear uop0's
      // table. Guarded by clearTable's indexValid.andR (see questions.md Q7).
      it("M1: LMUL=2 uopIdx 0->1 same pipe") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(2)
          val vs1 = foldBytes(Index.allInTable0(1))
          val golden = gatherGolden(vs1, 8, 8, 16, 0, table, 0 until 2, Prefill.default(2), 32)
          // uop0 arrives cycle 0, uop1 one cycle later, both on pipe0
          // placeIdx is the same-cycle index among same-robIdx uops: single uop
          // per cycle => placeIdx is always 0.
          pGather(dut, 0, 8, 8, vs1, table.slice(0, 16), Prefill.default(2).slice(0, 16), lmulFrac = 16, uopIdx = 0, placeIdx = 0)
          dut.clock.step(); pPipeOff(dut, 0)
          pGather(dut, 0, 8, 8, vs1, table.slice(16, 32), Prefill.default(2).slice(16, 32), lmulFrac = 16, uopIdx = 1, placeIdx = 0)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 6)
        }
      }

      it("M4: LMUL=2 out-of-order uopIdx 1->0") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(2)
          // each uop's index points to its own table segment (reversed), so the
          // two results are distinguishable.
          def idxOwn(u: Int) = foldBytes(Seq.tabulate(16)(x => u * 16 + 15 - x))
          val res0 = gatherGolden(idxOwn(0), 8, 8, 16, 0, table, 0 until 2, Prefill.default(2), 32)
          val res1 = gatherGolden(idxOwn(1), 8, 8, 16, 1, table, 0 until 2, Prefill.default(2), 32)
          // uopIdx 1 arrives first -> first free unit0 (wbPort 0); uopIdx 0 next
          // -> unit1 (wbPort 1). Check each port's own result.
          pGather(dut, 0, 8, 8, idxOwn(1), table.slice(16, 32), Prefill.default(2).slice(16, 32), lmulFrac = 16, uopIdx = 1, placeIdx = 0)
          dut.clock.step(); pPipeOff(dut, 0)
          pGather(dut, 0, 8, 8, idxOwn(0), table.slice(0, 16), Prefill.default(2).slice(0, 16), lmulFrac = 16, uopIdx = 0, placeIdx = 0)
          dut.clock.step(); pPipeOff(dut, 0)
          val wb = Array.fill(2)(Option.empty[BigInt])
          for (_ <- 0 until 24 if wb.exists(_.isEmpty)) {
            for (p <- 0 until 2 if dut.out.pipes(p).valid.peekBoolean()) {
              wb(p) = Some(dut.out.pipes(p).bits.data.peekValue().asBigInt)
            }
            dut.clock.step()
          }
          wb(1).getOrElse(fail("uop0 never wrote back")) mustBe res0
          wb(0).getOrElse(fail("uop1 never wrote back")) mustBe res1
        }
      }

      it("M2: LMUL=4 per-cycle uopIdx 0..3 same pipe") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(4)
          val vs1 = foldBytes(Index.allInTable0(1))
          val golden = gatherGolden(vs1, 8, 8, 32, 0, table, 0 until 4, Prefill.default(4), 64)
          // 4 uops, one per cycle, all on pipe0 (placeIdx=0: single uop per cycle)
          for (u <- 0 until 4) {
            pGather(dut, 0, 8, 8, vs1, table.slice(u * 16, (u + 1) * 16),
              Prefill.default(4).slice(u * 16, (u + 1) * 16), lmulFrac = 32, uopIdx = u, placeIdx = 0)
            dut.clock.step(); pPipeOff(dut, 0)
          }
          eWbAfter(dut, 0, golden, 12)
        }
      }

      it("M3: LMUL=8 per-cycle uopIdx 0..7 same pipe") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(8)
          val vs1 = foldBytes(Index.allInTable0(1))
          val golden = gatherGolden(vs1, 8, 8, 64, 0, table, 0 until 8, Prefill.default(8), 128)
          // 8 uops, one per cycle, all on pipe0 (deeper back-to-back than the
          // 2x4 scaling case); placeIdx=0 (single uop per cycle)
          for (u <- 0 until 8) {
            pGather(dut, 0, 8, 8, vs1, table.slice(u * 16, (u + 1) * 16),
              Prefill.default(8).slice(u * 16, (u + 1) * 16), lmulFrac = 64, uopIdx = u, placeIdx = 0)
            dut.clock.step(); pPipeOff(dut, 0)
          }
          eWbAfter(dut, 0, golden, 16)
        }
      }

      it("M5: LMUL=4 out-of-order uopIdx 2,0,3,1") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(4)
          def idxOwn(u: Int) = foldBytes(Seq.tabulate(16)(x => u * 16 + 15 - x))
          val res = (0 until 4).map(u => gatherGolden(idxOwn(u), 8, 8, 32, u, table, 0 until 4, Prefill.default(4), 64))
          val order = Seq(2, 0, 3, 1)
          for (u <- order) {
            pGather(dut, 0, 8, 8, idxOwn(u), table.slice(u * 16, (u + 1) * 16),
              Prefill.default(4).slice(u * 16, (u + 1) * 16), lmulFrac = 32, uopIdx = u, placeIdx = 0)
            dut.clock.step(); pPipeOff(dut, 0)
          }
          // arrival order -> first-free units 0..3 -> wbPort = unitIdx % 4
          eWbAfter(dut, 0, res(2), 12)   // uop2 -> unit0 -> pipe0
          eWbAfter(dut, 1, res(0), 0)    // uop0 -> unit1 -> pipe1
          eWbAfter(dut, 2, res(3), 0)    // uop3 -> unit2 -> pipe2
          eWbAfter(dut, 3, res(1), 0)    // uop1 -> unit3 -> pipe3
        }
      }
    }

    // Same-cycle injection of independent instructions into distinct blocks.
    describe("same-cycle distinct blocks") {

      it("M6: 4x LMUL=1 at U0/U2/U4/U6") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1)
          val golden = gatherGolden(0, 8, 8, 8, 0, table, 0 until 1, Prefill.default(1), 16)
          // 4 independent LMUL=1 instrs, one per pipe, blocks 0/2/4/6
          for (p <- 0 until 4) pGather(dut, p, 8, 8, 0, table, Prefill.default(1), blockBase = 2 * p)
          dut.clock.step(); for (p <- 0 until 4) pPipeOff(dut, p)
          // pipe0 serves unit0+unit4, pipe2 serves unit2+unit6 (priority: lowest unit first)
          eWbAfter(dut, 0, golden, 3)  // unit0
          eWbAfter(dut, 2, golden, 0)  // unit2
          dut.in.wbReady(0).poke(true); dut.in.wbReady(2).poke(true)
          dut.clock.step()
          eWb(dut, 0, golden)          // unit4
          eWb(dut, 2, golden)          // unit6
        }
      }
    }

    // Emu-derived (logs/vrgather_vv-20260813-141215.log): vrgather.vv v20, v28, v16,
    // e8/m4/tu/mu, vl=31, masked with the log's v0. All uops share robIdx.
    // Dispatch: uop0-2 land in one block at cy0 on pipes 1-3 (placeIdx 0,1,2);
    // uop3 arrives cy1 on pipe0 (placeIdx 0).
    describe("mixed 3+1 same block") {
      it("M8: emu vrgather m4 vl=31 tu/mu, cy0 pipes1-3 cy1 pipe0") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          // wbReady constant 1 from the start, as in the real pipeline / IT
          // (the emu pipeline always accepts writeback).
          for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
          // vs1 (index) per-uop slices: v16..v19
          val vs1 = Seq(
            Seq(0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f),
            Seq(0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01),
            Seq(0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f),
            Seq(0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8)
          )
          // vs2 (table) per-uop slices: v28..v31
          val table = Seq(
            Seq(0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00),
            Seq(0x07,0x00,0x01,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02),
            Seq(0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01),
            Seq(0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00)
          )
          // vs3 (undisturbed source, REF vd): v20..v23
          val vs3 = Seq(
            Seq(0x00,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00),
            Seq(0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x0b),
            Seq(0x2b,0x7f,0x5f,0xf0,0x11,0x5d,0xfd,0x9e,0x9a,0x0a,0x60,0x39,0x3b,0x0b,0x50,0x15),
            Seq(0xf5,0x89,0x1b,0x42,0x6a,0x56,0xb9,0x90,0x3a,0x19,0x69,0x9a,0xf8,0xac,0x6e,0x0d)
          )
          // unmasked (nomask emu test): mask is all-ones -> gather only, no
          // masked-off disturbance; vs3 still applies to the tail (uop2/uop3)
          val fullTable = table.flatten
          val golden = (0 until 4).map(u =>
            gatherGolden(foldBytes(vs1(u)), 8, 8, 32, u, fullTable, 0 until 4, vs3(u), 31,
              vta = false, vma = false))
          // cy0: uop0-2 into pipes 1-3 (same robIdx, placeIdx 0,1,2)
          for (p <- 1 to 3) {
            val u = p - 1
            pGather(dut, p, 8, 8, foldBytes(vs1(u)), table(u), vs3(u),
              lmulFrac = 32, uopIdx = u, placeIdx = p - 1, vl = 31,
              vta = false, vma = false)
          }
          dut.clock.step()
          for (p <- 1 to 3) pPipeOff(dut, p)
          // cy1: uop3 into pipe0
          pGather(dut, 0, 8, 8, foldBytes(vs1(3)), table(3), vs3(3),
            lmulFrac = 32, uopIdx = 3, placeIdx = 0, vl = 31,
            vta = false, vma = false)
          dut.clock.step()
          pPipeOff(dut, 0)
          // first-free landing -> uop0 unit0, uop1 unit1, uop2 unit2, uop3 unit3.
          // With wbReady=1 the writeback is consumed immediately (valid for one
          // cycle), so collect each pipe's data as it appears, then check that
          // the whole block drains (hasIndex -> 0). Repro: IT shows hasTable
          // stays set after writeback, so also assert hasTable -> 0.
          val wbData = Array.fill(NumPipes)(Option.empty[BigInt])
          var drained = false
          for (cy <- 0 until 40; if !drained) {
            for (p <- 0 until NumPipes) {
              if (dut.out.pipes(p).valid.peekBoolean() && wbData(p).isEmpty) {
                wbData(p) = Some(dut.out.pipes(p).bits.data.peekValue().asBigInt)
              }
            }
            drained = (0 until NumUnits).forall(u => !dut.out.unitStatus(u).hasIndex.peekBoolean())
            dut.clock.step()
          }
          for (p <- 0 until NumPipes) {
            wbData(p).getOrElse(fail(s"pipe $p never wrote back")) mustBe golden(p)
          }
          for (u <- 0 until NumUnits) {
            dut.out.unitStatus(u).hasIndex.expect(false.B, s"unit $u hasIndex drained")
            dut.out.unitStatus(u).hasTable.expect(false.B, s"unit $u hasTable drained")
          }
        }
      }

      // Emu-derived (logs/vrgather_vv-20260813-200244.log): vrgather.vv v20, v28, v16,
      // e8/m4/tu/mu, vl=64 (full m4). The emu core DEADLOCKED (no commit for 15000
      // cycles). Dispatch: uop0-2 at cy0 on pipes 1-3 (placeIdx 0,1,2), uop3 at
      // cy3 on pipe0 (placeIdx 0) - a 3-cycle gap before the last uop.
      it("M9: emu vrgather m4 vl=64 deadlock, cy0 pipes1-3 cy3 pipe0") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
          // vs1 (index) per-uop slices: v16..v19
          val vs1 = Seq(
            Seq(0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f),
            Seq(0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01),
            Seq(0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f),
            Seq(0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8)
          )
          // vs2 (table) per-uop slices: v28..v31
          val table = Seq(
            Seq(0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00),
            Seq(0x07,0x00,0x01,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02),
            Seq(0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01),
            Seq(0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00)
          )
          // vs3 (REF vd): v20..v23
          val vs3 = Seq(
            Seq(0x00,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00),
            Seq(0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02),
            Seq(0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x00,0x00,0x00,0x00,0x00),
            Seq(0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x00)
          )
          val fullTable = table.flatten
          val golden = (0 until 4).map(u =>
            gatherGolden(foldBytes(vs1(u)), 8, 8, 32, u, fullTable, 0 until 4, vs3(u), 64,
              vta = false, vma = false))
          // cy0: uop0-2 into pipes 1-3 (same robIdx, placeIdx 0,1,2)
          for (p <- 1 to 3) {
            val u = p - 1
            pGather(dut, p, 8, 8, foldBytes(vs1(u)), table(u), vs3(u),
              lmulFrac = 32, uopIdx = u, placeIdx = p - 1, vl = 64,
              vta = false, vma = false)
          }
          dut.clock.step()
          for (p <- 1 to 3) pPipeOff(dut, p)
          // cy1, cy2 idle
          dut.clock.step(); dut.clock.step()
          // cy3: uop3 into pipe0
          pGather(dut, 0, 8, 8, foldBytes(vs1(3)), table(3), vs3(3),
            lmulFrac = 32, uopIdx = 3, placeIdx = 0, vl = 64,
            vta = false, vma = false)
          dut.clock.step()
          pPipeOff(dut, 0)
          // collect writeback + drain (like M8); wide window to distinguish a
          // true deadlock (never completes) from a very slow flow
          val wbData = Array.fill(NumPipes)(Option.empty[BigInt])
          var drained = false
          for (cy <- 0 until 80; if !drained) {
            for (p <- 0 until NumPipes) {
              if (dut.out.pipes(p).valid.peekBoolean() && wbData(p).isEmpty) {
                wbData(p) = Some(dut.out.pipes(p).bits.data.peekValue().asBigInt)
              }
            }
            drained = (0 until NumUnits).forall(u => !dut.out.unitStatus(u).hasIndex.peekBoolean())
            dut.clock.step()
          }
          for (p <- 0 until NumPipes) {
            wbData(p).getOrElse(fail(s"pipe $p never wrote back")) mustBe golden(p)
          }
          for (u <- 0 until NumUnits) {
            dut.out.unitStatus(u).hasIndex.expect(false.B, s"unit $u hasIndex drained")
            dut.out.unitStatus(u).hasTable.expect(false.B, s"unit $u hasTable drained")
          }
        }
      }

      // Emu-derived (logs/vrgather_vv-20260814-105840.log): vrgather.vv v20, v28,
      // v16, e16/m4/tu/mu, vl=32 (full m4). The emu ABORTED (v22/v23 high wrong,
      // index-like values instead of the gathered result). Dispatch: uop0-2 at
      // cy0 on pipes 1-3 (placeIdx 0,1,2), uop3 at cy2 on pipe0 (placeIdx 0) -
      // a 2-cycle gap before the last uop (between M8's cy1 and M9's cy3).
      it("M10: emu vrgather e16 m4 vl=32 abort, cy0 pipes1-3 cy2 pipe0") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
          // e16 index elements per-uop: v16..v19
          val vs1 = Seq(
            Seq(0x0000,0x0001,0x0003,0xfff8,0xff80,0x7fff,0x7fff,0x7fff),
            Seq(0xffff,0xffff,0x0000,0x0001,0x0003,0xfff8,0xff80,0x7fff),
            Seq(0x7fff,0x7fff,0xffff,0xffff,0x0000,0x0001,0x0003,0xfff8),
            Seq(0xff80,0x7fff,0x7fff,0x7fff,0xffff,0xffff,0x0000,0x0001)
          )
          // e16 table bytes per-uop: v28..v31
          val table = Seq(
            Seq(0x00,0x00,0x02,0x00,0x0f,0x00,0x00,0x00,0xf8,0xff,0x00,0x00,0xff,0x07,0x01,0x00),
            Seq(0x00,0x00,0xff,0xff,0x00,0x00,0x02,0x00,0x0f,0x00,0x00,0x00,0xf8,0xff,0x00,0x00),
            Seq(0xff,0x07,0x01,0x00,0x00,0x00,0xff,0xff,0x00,0x00,0x02,0x00,0x0f,0x00,0x00,0x00),
            Seq(0xf8,0xff,0x00,0x00,0xff,0x07,0x01,0x00,0x00,0x00,0xff,0xff,0x00,0x00,0x02,0x00)
          )
          // vs3 (REF vd): v20..v23 (vl=32 is full m4, no tail/mask -> unused)
          val vs3 = Seq(
            Seq(0x00,0x00,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00),
            Seq(0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00),
            Seq(0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x00,0x00,0x00,0x00),
            Seq(0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00)
          )
          def e16v(elems: Seq[Int]): BigInt =
            elems.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (e, k)) => acc | (BigInt(e) << (16 * k)) }
          val fullTable = table.flatten
          val golden = (0 until 4).map(u =>
            gatherGolden(e16v(vs1(u)), 16, 16, 32, u, fullTable, 0 until 4, vs3(u), 32,
              vta = false, vma = false))
          // cy0: uop0-2 into pipes 1-3 (placeIdx 0,1,2)
          for (p <- 1 to 3) {
            val u = p - 1
            pGather(dut, p, 16, 16, e16v(vs1(u)), table(u), vs3(u),
              lmulFrac = 32, uopIdx = u, placeIdx = p - 1, vl = 32,
              vta = false, vma = false)
          }
          dut.clock.step()
          for (p <- 1 to 3) pPipeOff(dut, p)
          // cy1 idle, then uop3 into pipe0 at cy2 (2-cycle gap)
          dut.clock.step()
          pGather(dut, 0, 16, 16, e16v(vs1(3)), table(3), vs3(3),
            lmulFrac = 32, uopIdx = 3, placeIdx = 0, vl = 32,
            vta = false, vma = false)
          dut.clock.step()
          pPipeOff(dut, 0)
          // collect writeback + drain
          val wbData = Array.fill(NumPipes)(Option.empty[BigInt])
          var drained = false
          for (cy <- 0 until 80; if !drained) {
            for (p <- 0 until NumPipes) {
              if (dut.out.pipes(p).valid.peekBoolean() && wbData(p).isEmpty) {
                wbData(p) = Some(dut.out.pipes(p).bits.data.peekValue().asBigInt)
              }
            }
            drained = (0 until NumUnits).forall(u => !dut.out.unitStatus(u).hasIndex.peekBoolean())
            dut.clock.step()
          }
          for (p <- 0 until NumPipes) {
            wbData(p).getOrElse(fail(s"pipe $p never wrote back")) mustBe golden(p)
          }
          for (u <- 0 until NumUnits) {
            dut.out.unitStatus(u).hasIndex.expect(false.B, s"unit $u hasIndex drained")
            dut.out.unitStatus(u).hasTable.expect(false.B, s"unit $u hasTable drained")
          }
        }
      }
    }

    // Emu-derived (logs/vrgather_vv-20260814-140042.log): vrgather.vv v16, v8,
    // v24, e8/m8/tu/mu, vl=63. The core stalled after this instruction (no
    // commit for 15000 cycles). 8 uops of one LMUL=8 block dispatched
    // out-of-order with a 4-cycle gap:
    //   cy0 uop0,1   cy1 uop2   cy2 uop7   (idle cy3-5)   cy6 uop3,4   cy7 uop5,6
    // Bug: uop3-6 (the last 4 to dispatch) never assert wake.valid, so the
    // dependent instruction in the core never issues (no commit).
    describe("mixed out-of-order LMUL=8 (emu 140042)") {
      it("M11: all 8 uops wake despite a 4-cycle dispatch gap") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
          val table = Table.inc(8)
          val vs1 = foldBytes(Index.allInTable0(1))
          // Tag each uop with a distinct pdest so its wake is identifiable.
          def inj(u: Int, pipe: Int, placeIdx: Int): Unit =
            pGather(dut, pipe, 8, 8, vs1, table.slice(u * 16, (u + 1) * 16),
              Prefill.default(8).slice(u * 16, (u + 1) * 16),
              lmulFrac = 64, uopIdx = u, placeIdx = placeIdx, vl = 128,
              robIdx = 1, pdest = u)
          def off(ps: Int*): Unit = ps.foreach(pPipeOff(dut, _))
          // dispatch schedule
          inj(0, 1, 0); inj(1, 2, 1)
          dut.clock.step(); off(1, 2)
          inj(2, 3, 0)
          dut.clock.step(); off(3)
          inj(7, 0, 0)
          dut.clock.step(); off(0)
          for (_ <- 0 until 3) dut.clock.step() // cy3-5 idle
          inj(3, 0, 0); inj(4, 1, 1)
          dut.clock.step(); off(0, 1)
          inj(5, 2, 0); inj(6, 3, 1)
          dut.clock.step(); off(2, 3)
          // Collect wake events; ack each so the next unit on a pipe is revealed
          // (wake output is priority-arbitrated per pipe).
          val seen = scala.collection.mutable.Set[Int]()
          var cy = 0
          while (cy < 40 && seen.size < 8) {
            for (p <- 0 until NumPipes) {
              if (dut.out.wake(p).valid.peekBoolean()) {
                val pdest = dut.out.wake(p).bits.meta.pdest.peekValue().asBigInt.toInt
                seen += pdest
                dut.in.wakeAck(p).poke(true)
              }
            }
            dut.clock.step()
            dut.in.wakeAck.foreach(_.poke(false))
            cy += 1
          }
          seen mustBe (0 until 8).toSet
        }
      }
    }

    // Emu-derived (logs/vrgather_vv-20260814-204042.log): vrgather.vv e8/m4
    // vl=64. Store mismatch: DUT wrote 0x02 where REF wrote 0x00. 4 uops of one
    // LMUL=4 block dispatched out-of-order:
    //   cy0 uop0 (first entrant)   cy1 uop3, uop1, uop2
    // Control-signal check (wake timing) only; the writeback data for this
    // schedule is a separate gather/flow bug and is not asserted here.
    describe("mixed 1+3 out-of-order LMUL=4 (emu 204042)") {
      it("M12: first entrant must not wake before the block is assembled") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
          // vs1 (e8 indices) per uop, from the emu log
          val vs1 = Seq(
            Seq(0x7f,0xf8,0xf8,0x03,0x01,0x00,0xff,0xff,0x7f,0x7f,0x7f,0xf8,0xf8,0x03,0x01,0x00),
            Seq(0x01,0x00,0xff,0xff,0x7f,0x7f,0x7f,0xf8,0xf8,0x03,0x01,0x00,0xff,0xff,0x7f,0x7f),
            Seq(0x7f,0x7f,0x7f,0xf8,0xf8,0x03,0x01,0x00,0xff,0xff,0x7f,0x7f,0x7f,0xff,0x8f,0x80),
            Seq(0xf8,0x03,0x01,0x00,0xff,0xff,0x7f,0x7f,0x7f,0xf8,0xf8,0x03,0x01,0x00,0xff,0xff)
          )
          // vs2 (e8 table) per uop
          val table = Seq(
            Seq(0x00,0x0f,0x02,0x00,0xff,0x00,0x01,0x07,0x00,0x08,0x00,0x0f,0x02,0x00,0xff,0x00),
            Seq(0x00,0x08,0x00,0x0f,0x02,0x00,0xff,0x00,0x01,0x07,0x00,0x08,0x00,0x0f,0x02,0x00),
            Seq(0x02,0x00,0xff,0x00,0x01,0x07,0x00,0x08,0x00,0x0f,0x02,0x00,0xff,0x00,0x01,0x07),
            Seq(0x01,0x07,0x00,0x08,0x00,0x0f,0x02,0x00,0xff,0x00,0x01,0x07,0x00,0x08,0x00,0x0f)
          )
          val vs3 = Seq.fill(4)(Seq.fill(16)(0))
          def inj(u: Int, pipe: Int, placeIdx: Int): Unit =
            pGather(dut, pipe, 8, 8, foldBytes(vs1(u)), table(u), vs3(u),
              lmulFrac = 32, uopIdx = u, placeIdx = placeIdx, vl = 64,
              robIdx = 1, pdest = u)
          // cy0: uop0 (first entrant)
          inj(0, 0, 0)
          dut.clock.step(); pPipeOff(dut, 0)
          // cy1: uop0 alone -> block incomplete, no wake may fire
          for (p <- 0 until NumPipes)
            dut.out.wake(p).valid.expect(false.B, s"no wake before assembly (pipe $p)")
          // cy1: uop3,1,2 (placeIdx 0,1,2 in pipe order)
          inj(3, 1, 0); inj(1, 2, 1); inj(2, 3, 2)
          dut.clock.step(); pPipeOff(dut, 1); pPipeOff(dut, 2); pPipeOff(dut, 3)
          // Collect wake events (control only); ack each so the next unit on a
          // pipe is revealed. Every uop of the assembled block must wake.
          val seen = scala.collection.mutable.Set[Int]()
          var cy = 0
          while (cy < 20 && seen.size < 4) {
            for (p <- 0 until NumPipes) {
              if (dut.out.wake(p).valid.peekBoolean()) {
                val pdest = dut.out.wake(p).bits.meta.pdest.peekValue().asBigInt.toInt
                seen += pdest
                dut.in.wakeAck(p).poke(true)
              }
            }
            dut.clock.step()
            dut.in.wakeAck.foreach(_.poke(false))
            cy += 1
          }
          seen mustBe (0 until 4).toSet
        }
      }
    }

    // Emu-derived (logs/vrgather_vv-20260815-145113.log): vrgather.vv v16, v8,
    // v24, e8/m8/tu/mu, vl=128 (full m8). emu diff: v21 (uop5)/v22 (uop6) wrong
    // (elements keep the index value 0x7f instead of the gathered table value).
    // Dispatch (out-of-order, 4-cycle gap before the last uop):
    //   t0 uop0   t1 uop1   t2 uop3,4,2   t3 uop5,6   (idle t4-6)   t7 uop7
    describe("mixed out-of-order LMUL=8 gap (emu 20260815-145113)") {
      it("M13: emu vrgather m8 vl=128 data wrong uop5/6") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
          // vs1 (index) per-uop slices: v24..v31
          val vs1 = Seq(
            Seq(0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f),
            Seq(0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01),
            Seq(0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f),
            Seq(0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8),
            Seq(0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff),
            Seq(0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f),
            Seq(0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01),
            Seq(0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f)
          )
          // vs2 (table) per-uop slices: v8..v15
          val table = Seq(
            Seq(0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00),
            Seq(0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02),
            Seq(0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01),
            Seq(0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00),
            Seq(0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff),
            Seq(0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00),
            Seq(0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02),
            Seq(0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01)
          )
          // vs3 (undisturbed source): vl=128 is full m8 + nomask -> unused
          val vs3 = Seq.fill(8)(Seq.fill(16)(0))
          val fullTable = table.flatten
          val golden = (0 until 8).map(u =>
            gatherGolden(foldBytes(vs1(u)), 8, 8, 64, u, fullTable, 0 until 8, vs3(u), 128,
              vta = false, vma = false))
          def inj(u: Int, pipe: Int, placeIdx: Int): Unit =
            pGather(dut, pipe, 8, 8, foldBytes(vs1(u)), table(u), vs3(u),
              lmulFrac = 64, uopIdx = u, placeIdx = placeIdx, vl = 128,
              robIdx = 1, pdest = u)
          def off(ps: Int*): Unit = ps.foreach(pPipeOff(dut, _))
          // t0 uop0, t1 uop1
          inj(0, 0, 0); dut.clock.step(); off(0)
          inj(1, 0, 0); dut.clock.step(); off(0)
          // t2 uop3,4,2 (placeIdx 0,1,2)
          inj(3, 0, 0); inj(4, 1, 1); inj(2, 2, 2)
          dut.clock.step(); off(0, 1, 2)
          // t3 uop5,6 (placeIdx 0,1)
          inj(5, 0, 0); inj(6, 1, 1)
          dut.clock.step(); off(0, 1)
          // t4-6 idle
          for (_ <- 0 until 3) dut.clock.step()
          // t7 uop7
          inj(7, 0, 0); dut.clock.step(); off(0)
          // collect writeback tagged by pdest (robust to unit landing), then drain
          val wbData = Array.fill(8)(Option.empty[BigInt])
          var drained = false
          for (cy <- 0 until 80; if !drained) {
            for (p <- 0 until NumPipes) {
              if (dut.out.pipes(p).valid.peekBoolean()) {
                val pd = dut.out.pipes(p).bits.meta.pdest.peekValue().asBigInt.toInt
                if (wbData(pd).isEmpty) {
                  wbData(pd) = Some(dut.out.pipes(p).bits.data.peekValue().asBigInt)
                }
              }
            }
            drained = (0 until NumUnits).forall(u => !dut.out.unitStatus(u).hasIndex.peekBoolean())
            dut.clock.step()
          }
          for (u <- 0 until 8) {
            wbData(u).getOrElse(fail(s"uop $u never wrote back")) mustBe golden(u)
          }
          for (u <- 0 until NumUnits) {
            dut.out.unitStatus(u).hasIndex.expect(false.B, s"unit $u hasIndex drained")
            dut.out.unitStatus(u).hasTable.expect(false.B, s"unit $u hasTable drained")
          }
        }
      }
    }
  }

  describe("Ops") {

    // ---- vrgather.vv (ei = sew) ----
    describe("vrgather") {
      it("G1: SEW=8 e8 identity") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1)
          val vs1 = foldBytes(Seq.tabulate(16)(identity))
          val golden = gatherGolden(vs1, 8, 8, 8, 0, table, 0 until 1, Prefill.default(1), 16)
          pGather(dut, 0, 8, 8, vs1, table, Prefill.default(1))
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("G2: SEW=16 e16") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1)
          val vs1 = (0 until 8).foldLeft(BigInt(0))((acc, k) => acc | (BigInt(k) << (16 * k)))
          val golden = gatherGolden(vs1, 16, 16, 8, 0, table, 0 until 1, Prefill.default(1), 8)
          pGather(dut, 0, 16, 16, vs1, table, Prefill.default(1))
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("G3: SEW=32 e32") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1)
          val vs1 = (0 until 4).foldLeft(BigInt(0))((acc, k) => acc | (BigInt(k) << (32 * k)))
          val golden = gatherGolden(vs1, 32, 32, 8, 0, table, 0 until 1, Prefill.default(1), 4)
          pGather(dut, 0, 32, 32, vs1, table, Prefill.default(1))
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("G4: SEW=64 e64") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1)
          val vs1 = (0 until 2).foldLeft(BigInt(0))((acc, k) => acc | (BigInt(k) << (64 * k)))
          val golden = gatherGolden(vs1, 64, 64, 8, 0, table, 0 until 1, Prefill.default(1), 2)
          pGather(dut, 0, 64, 64, vs1, table, Prefill.default(1))
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("G5: out-of-range -> fillZeros") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1)
          // byte 5 indexes 20 >= vlmax(16) -> fillZeros
          val vs1 = foldBytes(Seq.tabulate(16)(i => if (i == 5) 20 else i))
          val golden = gatherGolden(vs1, 8, 8, 8, 0, table, 0 until 1, Prefill.default(1), 16)
          pGather(dut, 0, 8, 8, vs1, table, Prefill.default(1))
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("G6: masked-off -> prefill (vma)") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1)
          val vs1 = foldBytes(Seq.tabulate(16)(identity))
          // mask bit3=0 -> element 3 uses prefill (vma=0 -> vs3, not agnostic)
          val mask = (BigInt(1) << numElem) - 1 - (BigInt(1) << 3)
          val golden = gatherGolden(vs1, 8, 8, 8, 0, table, 0 until 1, Prefill.default(1), 16, mask = mask)
          pGather(dut, 0, 8, 8, vs1, table, Prefill.default(1), mask = mask)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }

      // Q12: masked-off elements must preserve vs3 (vma=0) even when their index
      // is out of range (fz). The RTL's isPrefill = !fz && (...) wrongly lets fz
      // override the mask (writes 0 instead of vs3). Contrast G6 which uses
      // in-range (iv) indices for the masked-off elements.
      it("G7: masked-off out-of-range preserves vs3") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1)
          // even elements: masked-off (mask bit 0) with out-of-range index 0xff;
          // odd elements: active (mask bit 1) with in-range index 0 -> vs2[0].
          val vs1 = foldBytes(Seq.tabulate(16)(i => if (i % 2 == 0) 0xff else 0x00))
          val vs3 = Seq.fill(16)(0xaa)
          val mask = 0xaaaa
          val golden = gatherGolden(vs1, 8, 8, 8, 0, table, 0 until 1, vs3, 16,
            vta = false, vma = false, mask = mask)
          pGather(dut, 0, 8, 8, vs1, table, vs3, mask = mask)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
    }

    // ---- vslideup ----
    describe("vslideup") {
      it("SU1: offset=0 (identity from vs2)") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = Seq.tabulate(16)(i => 0x10 + i)
          val vs3 = Prefill.default(1)
          val golden = slideUpGolden(0, vs2, vs3, 8, 8, 0, 16)
          pSlideUp(dut, 0, 0, vs2, vs3)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("SU2: offset=3 (bottom stays vs2, top shifted)") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = Seq.tabulate(16)(i => 0x10 + i)
          val vs3 = Prefill.default(1)
          val golden = slideUpGolden(3, vs2, vs3, 8, 8, 0, 16)
          pSlideUp(dut, 0, 3, vs2, vs3)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("SU3: offset overflow (>= vlmax) -> all vs2 preserved") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = Seq.tabulate(16)(i => 0x10 + i)
          val vs3 = Prefill.default(1)
          val golden = slideUpGolden(20, vs2, vs3, 8, 8, 0, 16)
          pSlideUp(dut, 0, 20, vs2, vs3)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("SU4: partial vl -> tail prefill") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = Seq.tabulate(16)(i => 0x10 + i)
          val vs3 = Prefill.default(1)
          val golden = slideUpGolden(1, vs2, vs3, 8, 8, 0, 10)
          pSlideUp(dut, 0, 1, vs2, vs3, vl = 10)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("SU5: mf8 vl=2 normalized VI offset one (emu vslideup_vi-0)") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = Seq(0x00, 0x02, 0x0f, 0x00, 0xff, 0x7f, 0xff, 0x00,
                        0x02, 0x0f, 0x00, 0xff, 0x7f, 0xff, 0x00, 0x02)
          val vs3 = Seq(0x00, 0x02, 0x80, 0x32, 0xdc, 0x75, 0xac, 0x30,
                        0x95, 0x12, 0x76, 0x5b, 0xaa, 0xc0, 0x44, 0x83)
          val golden = slideUpGolden(1, vs2, vs3, 8, 1, 0, 2)
          val p = dut.in.pipes(0)
          p.valid.poke(true)
          p.bits.op.isSlideUp.poke(true)
          p.bits.placeIdx.poke(0); p.bits.blockBase.poke(0); p.bits.uopIdx.poke(0)
          pSew(p.bits.sew, 8); pLmul(p.bits.lmul, 1)
          p.bits.blockFieldNum.poke(1.U)
          p.bits.vl.poke(2)
          p.bits.vta.poke(false); p.bits.vma.poke(false)
          // The caller converts its replicated VI source into a scalar offset.
          p.bits.rs1.poke(1)
          p.bits.vs2.poke(foldBytes(vs2)); p.bits.vs3.poke(foldBytes(vs3))
          p.bits.mask.poke((BigInt(1) << numElem) - 1)
          pMeta(dut, 0, 0, 0)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("SU6: mf8 vl=2 normalized offset>=vl all unchanged (emu block3)") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          // offset=3 >= vl=2 -> ISA: every element unchanged (old vd = vs3);
          // current RTL does identity LOOKUP (vs2[i]) -> byte1=0x02 != 0x00.
          val vs2 = Seq(0x00, 0x02, 0x0f, 0x00, 0xff, 0x7f, 0xff, 0x00,
                        0x02, 0x0f, 0x00, 0xff, 0x7f, 0xff, 0x00, 0x02)
          val vs3 = Seq(0x00, 0x00, 0x80, 0x32, 0xdc, 0x75, 0xac, 0x30,
                        0x95, 0x12, 0x76, 0x5b, 0xaa, 0xc0, 0x44, 0x83)
          val golden = foldBytes(vs3) // all unchanged (old v1 after block2)
          val p = dut.in.pipes(0)
          p.valid.poke(true)
          p.bits.op.isSlideUp.poke(true)
          p.bits.placeIdx.poke(0); p.bits.blockBase.poke(0); p.bits.uopIdx.poke(0)
          pSew(p.bits.sew, 8); pLmul(p.bits.lmul, 1)
          p.bits.blockFieldNum.poke(1.U)
          p.bits.vl.poke(2)
          p.bits.vta.poke(false); p.bits.vma.poke(false)
          p.bits.rs1.poke(3)
          p.bits.vs2.poke(foldBytes(vs2)); p.bits.vs3.poke(foldBytes(vs3))
          p.bits.mask.poke((BigInt(1) << numElem) - 1)
          pMeta(dut, 0, 0, 0)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("SU7: mf8 vl=2 offset>=vl vma=1 hole keeps vs3 (not 0xFF)") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          // ma: the stay (hole) elements are still "unchanged" (vs3), only
          // mask-inactive/tail follow agnostic policy (Q26).
          val vs2 = Seq(0x00, 0x02, 0x0f, 0x00, 0xff, 0x7f, 0xff, 0x00,
                        0x02, 0x0f, 0x00, 0xff, 0x7f, 0xff, 0x00, 0x02)
          val vs3 = Seq.fill(16)(0x01) // distinctive: hole must keep 0x01, not 0xFF
          val golden = foldBytes(vs3)
          val p = dut.in.pipes(0)
          p.valid.poke(true)
          p.bits.op.isSlideUp.poke(true)
          p.bits.placeIdx.poke(0); p.bits.blockBase.poke(0); p.bits.uopIdx.poke(0)
          pSew(p.bits.sew, 8); pLmul(p.bits.lmul, 1)
          p.bits.blockFieldNum.poke(1.U)
          p.bits.vl.poke(2)
          p.bits.vta.poke(false); p.bits.vma.poke(true)
          p.bits.rs1.poke(3)
          p.bits.vs2.poke(foldBytes(vs2)); p.bits.vs3.poke(foldBytes(vs3))
          p.bits.mask.poke((BigInt(1) << numElem) - 1)
          pMeta(dut, 0, 0, 0)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("SU8: LMUL=2 e8 vl=32 offset=0 uop1 must read vs5 not vs4 (emu vslideup_vi-1 pc=0x800008d2)") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          // emu vslideup_vi-1: vslideup.vi v2, v4, 0 @ pc 0x800008d2 (e8/m2/vl=32).
          // DUT wrote uop1 (=v3) with v4's bytes instead of v5's. Feed each uop
          // its own source register; offset=0 -> identity per uop.
          val vs4 = Seq(0x00, 0x02, 0x0f, 0x00, 0xff, 0x7f, 0xff, 0x00,
                        0x02, 0x00, 0x0f, 0x00, 0xff, 0x7f, 0xff, 0x02)
          val vs5 = Seq(0x0f, 0x00, 0xff, 0x7f, 0xff, 0x00, 0x02, 0x0f,
                        0x00, 0xff, 0x7f, 0xff, 0x00, 0x02, 0x0f, 0x00)
          val vs45 = vs4 ++ vs5 // full 32-byte source group
          val vs3 = Seq.fill(32)(0) // no stay elements at offset=0
          val g0 = slideUpGolden(0, vs45, vs3.take(16), 8, 16, 0, 32)
          val g1 = slideUpGolden(0, vs45, vs3.drop(16), 8, 16, 1, 32)
          pSlideUp(dut, 0, 0, vs4, vs3.take(16), sew = 8, lmulFrac = 16, uopIdx = 0, placeIdx = 0, vl = 32)
          pSlideUp(dut, 1, 0, vs5, vs3.drop(16), sew = 8, lmulFrac = 16, uopIdx = 1, placeIdx = 1, vl = 32)
          dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
          // Latch each pipe's first wb; compare in Hex (readable vs decimal).
          val seen = Array(false, false)
          for (_ <- 0 until 12) {
            if (!seen(0) && dut.out.pipes(0).valid.peekBoolean()) {
              dut.out.pipes(0).bits.data.expect(g0, ExpectationValueFormat.Hex); seen(0) = true
            }
            if (!seen(1) && dut.out.pipes(1).valid.peekBoolean()) {
              dut.out.pipes(1).bits.data.expect(g1, ExpectationValueFormat.Hex); seen(1) = true
            }
            dut.clock.step()
          }
          seen(0) mustBe true
          seen(1) mustBe true
        }
      }
    }

    // ---- vslide1up ----
    describe("vslide1up") {
      it("S1U1: SEW=8 scalar head") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = Seq.tabulate(16)(i => 0x10 + i)
          val vs3 = Prefill.default(1)
          val scalar = BigInt(0x5A)
          val golden = slide1UpGolden(scalarBytes(scalar), vs2, vs3, 8, 8, 0, 16)
          pSlide1Up(dut, 0, scalar, vs2, vs3)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("S1U2: SEW=64 scalar replicated") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = Seq.tabulate(16)(i => 0x10 + i)
          val vs3 = Prefill.default(1)
          val scalar = BigInt(0x5A)
          val golden = slide1UpGolden(scalarBytes(scalar), vs2, vs3, 64, 8, 0, 2)
          pSlide1Up(dut, 0, scalar, vs2, vs3, sew = 64)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("S1U3: partial vl -> tail prefill") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = Seq.tabulate(16)(i => 0x10 + i)
          val vs3 = Prefill.default(1)
          val scalar = BigInt(0x5A)
          val golden = slide1UpGolden(scalarBytes(scalar), vs2, vs3, 8, 8, 0, 10)
          pSlide1Up(dut, 0, scalar, vs2, vs3, vl = 10)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
    }

    // ---- vslidedown ----
    describe("vslidedown") {
      it("SD1: offset=2 (top fillZeros)") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = Seq.tabulate(16)(i => 0x10 + i)
          val vs3 = Prefill.default(1)
          val golden = slideDownGolden(2, vs2, vs3, 8, 8, 0, 16)
          pSlideDown(dut, 0, 2, vs2, vs3)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("SD2: offset overflow -> all fillZeros") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = Seq.tabulate(16)(i => 0x10 + i)
          val vs3 = Prefill.default(1)
          val golden = slideDownGolden(20, vs2, vs3, 8, 8, 0, 16)
          pSlideDown(dut, 0, 20, vs2, vs3)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("SD3: partial vl -> tail prefill") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = Seq.tabulate(16)(i => 0x10 + i)
          val vs3 = Prefill.default(1)
          val golden = slideDownGolden(1, vs2, vs3, 8, 8, 0, 10)
          pSlideDown(dut, 0, 1, vs2, vs3, vl = 10)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("SD4: m4 vl=31 offset=31 cross-chunk source (emu vslidedown_vi)") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
          val vs2 = Seq.tabulate(64)(i => 0x10 + i)
          val vs3 = Prefill.default(4)
          // golden uses full vs2 (cross-chunk reads) but chunk-local vs3 (tail)
          val goldens = (0 until 4).map(c =>
            slideDownGolden(31, vs2, vs3.slice(c * 16, (c + 1) * 16), 8, 32, c, 31))
          def inj(c: Int, pipe: Int, placeIdx: Int): Unit =
            pSlideDown(dut, pipe, 31, vs2.slice(c * 16, (c + 1) * 16), vs3.slice(c * 16, (c + 1) * 16),
              lmulFrac = 32, uopIdx = c, placeIdx = placeIdx, vl = 31, robIdx = 1, pdest = c)
          def off(ps: Int*): Unit = ps.foreach(pPipeOff(dut, _))
          // emu dispatch: t0 uop0+uop1, t1 uop3+uop2 (pc 800026a6).
          // placeIdx = per-cycle uop index (0-based each cycle), not the uop number.
          inj(0, 0, 0); inj(1, 1, 1)
          dut.clock.step(); off(0, 1)
          inj(3, 0, 0); inj(2, 1, 1)
          dut.clock.step(); off(0, 1)
          // collect writeback tagged by pdest, then drain
          val wbData = Array.fill(4)(Option.empty[BigInt])
          var drained = false
          for (cy <- 0 until 80; if !drained) {
            for (p <- 0 until NumPipes) {
              if (dut.out.pipes(p).valid.peekBoolean()) {
                val pd = dut.out.pipes(p).bits.meta.pdest.peekValue().asBigInt.toInt
                if (wbData(pd).isEmpty) {
                  wbData(pd) = Some(dut.out.pipes(p).bits.data.peekValue().asBigInt)
                }
              }
            }
            drained = (0 until NumUnits).forall(u => !dut.out.unitStatus(u).hasIndex.peekBoolean())
            dut.clock.step()
          }
          for (c <- 0 until 4) {
            wbData(c).getOrElse(fail(s"uop $c never wrote back")) mustBe goldens(c)
          }
        }
      }
      it("SD5: m4 vl=31 offset=1 (emu vslidedown_vi-1 pc 26de)") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
          val vs2 = Seq.tabulate(64)(i => 0x10 + i)
          val vs3 = Prefill.default(4)
          val goldens = (0 until 4).map(c =>
            slideDownGolden(1, vs2, vs3.slice(c * 16, (c + 1) * 16), 8, 32, c, 31))
          def inj(c: Int, pipe: Int, placeIdx: Int): Unit =
            pSlideDown(dut, pipe, 1, vs2.slice(c * 16, (c + 1) * 16), vs3.slice(c * 16, (c + 1) * 16),
              lmulFrac = 32, uopIdx = c, placeIdx = placeIdx, vl = 31, robIdx = 1, pdest = c)
          def off(ps: Int*): Unit = ps.foreach(pPipeOff(dut, _))
          // emu dispatch: t0 uop0+uop1, t1 uop3+uop2 (placeIdx = per-cycle uop index)
          inj(0, 0, 0); inj(1, 1, 1)
          dut.clock.step(); off(0, 1)
          inj(3, 0, 0); inj(2, 1, 1)
          dut.clock.step(); off(0, 1)
          val wbData = Array.fill(4)(Option.empty[BigInt])
          var drained = false
          for (cy <- 0 until 80; if !drained) {
            for (p <- 0 until NumPipes) {
              if (dut.out.pipes(p).valid.peekBoolean()) {
                val pd = dut.out.pipes(p).bits.meta.pdest.peekValue().asBigInt.toInt
                if (wbData(pd).isEmpty) {
                  wbData(pd) = Some(dut.out.pipes(p).bits.data.peekValue().asBigInt)
                }
              }
            }
            drained = (0 until NumUnits).forall(u => !dut.out.unitStatus(u).hasIndex.peekBoolean())
            dut.clock.step()
          }
          for (c <- 0 until 4) {
            wbData(c).getOrElse(fail(s"uop $c never wrote back")) mustBe goldens(c)
          }
        }
      }
      it("SD6: m4 vl=31 rs1=0xff..ff sign-extended imm31 (emu vslidedown_vi)") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
          val vs2 = Seq.tabulate(64)(i => 0x10 + i)
          val vs3 = Prefill.default(4)
          // regression: with the decoder fixed (VSLIDEDOWN_VI -> OPIVIU), the
          // zero-extended imm31 reaches the PermUnit as rs1=0x1f (offset=31).
          val goldens = (0 until 4).map(c =>
            slideDownGolden(31, vs2, vs3.slice(c * 16, (c + 1) * 16), 8, 32, c, 31))
          def inj(c: Int, pipe: Int, placeIdx: Int): Unit = {
            val p = dut.in.pipes(pipe)
            p.valid.poke(true)
            p.bits.op.isSlideDown.poke(true)
            p.bits.placeIdx.poke(placeIdx); p.bits.blockBase.poke(0); p.bits.uopIdx.poke(c)
            pSew(p.bits.sew, 8); pLmul(p.bits.lmul, 32)
            p.bits.blockFieldNum.poke(4.U)
            p.bits.vl.poke(31)
            p.bits.vta.poke(false); p.bits.vma.poke(false)
            p.bits.rs1.poke(0x1f)
            p.bits.vs2.poke(foldBytes(vs2.slice(c * 16, (c + 1) * 16)))
            p.bits.vs3.poke(foldBytes(vs3.slice(c * 16, (c + 1) * 16)))
            p.bits.mask.poke((BigInt(1) << numElem) - 1)
            pMeta(dut, pipe, 1, c)
          }
          def off(ps: Int*): Unit = ps.foreach(pPipeOff(dut, _))
          // emu dispatch: t0 uop0+uop1, t1 uop3+uop2 (placeIdx = per-cycle uop index)
          inj(0, 0, 0); inj(1, 1, 1)
          dut.clock.step(); off(0, 1)
          inj(3, 0, 0); inj(2, 1, 1)
          dut.clock.step(); off(0, 1)
          val wbData = Array.fill(4)(Option.empty[BigInt])
          var drained = false
          for (cy <- 0 until 80; if !drained) {
            for (p <- 0 until NumPipes) {
              if (dut.out.pipes(p).valid.peekBoolean()) {
                val pd = dut.out.pipes(p).bits.meta.pdest.peekValue().asBigInt.toInt
                if (wbData(pd).isEmpty) {
                  wbData(pd) = Some(dut.out.pipes(p).bits.data.peekValue().asBigInt)
                }
              }
            }
            drained = (0 until NumUnits).forall(u => !dut.out.unitStatus(u).hasIndex.peekBoolean())
            dut.clock.step()
          }
          for (c <- 0 until 4) {
            wbData(c).getOrElse(fail(s"uop $c never wrote back")) mustBe goldens(c)
          }
        }
      }
      it("SD7: mf4 e16 vl=2 rs1=0xff80 overflowed offset (emu vslidedown_vx-0 pc 0x8000200a)") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
          // v2 loaded e16/m1 from testdata+992: elems {0000,0200,000f,0000,...}
          val vs2 = Seq(0x00, 0x00, 0x00, 0x02, 0x0f, 0x00, 0x00, 0x00,
                        0xf8, 0xff, 0x00, 0x00, 0xff, 0x07, 0x01, 0x00)
          val vs3 = Seq.fill(16)(0xaa) // old v1, unchanged tail (tu)
          // ISA: offset=0xff80 >= VLMAX(2) -> active elems fillZeros (0).
          // Bug: SlideDownIndexGen offset field is 7 bits -> 0xff80 truncates
          // to 0 -> identity LOOKUP (vs2[i]); offsetOverflow unused there.
          val golden = slideDownGolden(0xff80, vs2, vs3, 16, 2, 0, 2)
          pSlideDown(dut, 0, 0xff80, vs2, vs3, sew = 16, lmulFrac = 2,
            uopIdx = 0, blockBase = 0, placeIdx = 0, vl = 2)
          dut.clock.step(); pPipeOff(dut, 0)
          val wbs = collectWb(dut, 30)
          wbs(0).getOrElse(fail("pipe 0 never wrote back")) mustBe golden
        }
      }
    }

    // ---- vslide1down ----
    describe("vslide1down") {
      it("S1D1: SEW=8 scalar tail (vl-1)") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = Seq.tabulate(16)(i => 0x10 + i)
          val vs3 = Prefill.default(1)
          val scalar = BigInt(0x5A)
          val golden = slide1DownGolden(scalarBytes(scalar), vs2, vs3, 8, 8, 0, 16)
          pSlide1Down(dut, 0, scalar, vs2, vs3)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("S1D2: SEW=64 scalar replicated") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = Seq.tabulate(16)(i => 0x10 + i)
          val vs3 = Prefill.default(1)
          val scalar = BigInt(0x5A)
          val golden = slide1DownGolden(scalarBytes(scalar), vs2, vs3, 64, 8, 0, 2)
          pSlide1Down(dut, 0, scalar, vs2, vs3, sew = 64)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
      it("S1D3: partial vl -> tail prefill") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = Seq.tabulate(16)(i => 0x10 + i)
          val vs3 = Prefill.default(1)
          val scalar = BigInt(0x5A)
          val golden = slide1DownGolden(scalarBytes(scalar), vs2, vs3, 8, 8, 0, 10)
          pSlide1Down(dut, 0, scalar, vs2, vs3, vl = 10)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 2)
        }
      }
    }

    // ---- vcompress ----
    describe("vcompress") {
      it("C1: mask all-1 -> identity") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = Seq.tabulate(16)(i => 0x10 + i)
          val vs3 = Prefill.default(1)
          val mask = (BigInt(1) << numElem) - 1
          val golden = compressGolden(mask, vs2, vs3, 0, 16)
          pCompress(dut, 0, mask, vs2, vs3)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 3)
        }
      }
      it("C2: mask all-0 -> all prefill") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = Seq.tabulate(16)(i => 0x10 + i)
          val vs3 = Prefill.default(1)
          val golden = compressGolden(0, vs2, vs3, 0, 16)
          pCompress(dut, 0, 0, vs2, vs3)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 3)
        }
      }
      it("C3: mask alternating (0x5555)") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = Seq.tabulate(16)(i => 0x10 + i)
          val vs3 = Prefill.default(1)
          val mask = BigInt("5555", 16)
          val golden = compressGolden(mask, vs2, vs3, 0, 16)
          pCompress(dut, 0, mask, vs2, vs3)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 3)
        }
      }
      it("C4: mask single-bit -> one element compressed") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = Seq.tabulate(16)(i => 0x10 + i)
          val vs3 = Prefill.default(1)
          val mask = BigInt(1) << 7
          val golden = compressGolden(mask, vs2, vs3, 0, 16)
          pCompress(dut, 0, mask, vs2, vs3)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 3)
        }
      }
      it("C5: uopIdx=1 chunk offset") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = Seq.tabulate(32)(i => 0x10 + i)
          val vs3 = Prefill.default(2)
          // chunk0 active at byte 3, chunk1 all active -> total popcount 17
          val mask = (BigInt(1) << 3) | ((BigInt(1) << numElem) - 1) << numElem
          // uop1 output byte0 = 17th active byte (vs2[31]); rest prefill
          val golden = compressGolden(mask, vs2, vs3.slice(16, 32), 1, 32)
          pCompress(dut, 0, mask, vs2.slice(16, 32), vs3.slice(16, 32), lmulFrac = 16, uopIdx = 1, placeIdx = 1)
          pCompress(dut, 1, mask, vs2.slice(0, 16), vs3.slice(0, 16), lmulFrac = 16, uopIdx = 0, placeIdx = 0)
          dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
          eWbAfter(dut, 1, golden, 5)
        }
      }
      it("C6: mf8 vl=2 mask bit beyond vl -> all tail (emu vcompress_vm-0)") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          // vs1=0x0100 (bit 8): element 8 >= vl=2 -> ignored per spec (only the
          // first vl mask bits select). 0 elements compressed -> all tail (tu).
          val vs2 = Seq(0x08, 0x00, 0x0F, 0x00, 0x00, 0x02, 0x00, 0xFF,
                        0x07, 0x01, 0x08, 0x00, 0x0F, 0x00, 0x00, 0x02)
          val vs3 = Prefill.default(1)
          val mask = 0x100
          val golden = compressGolden(mask, vs2, vs3, 0, 2)
          pCompress(dut, 0, mask, vs2, vs3, lmulFrac = 1, vl = 2)
          dut.clock.step(); pPipeOff(dut, 0)
          eWbAfter(dut, 0, golden, 3)
        }
      }
      it("C7: e16 m2 vl=7 mask SEW-expanded (emu vcompress_vm-0)") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          // e16: mask bit i selects the 2-byte element i; the byteMask consumed
          // by CompressIndexGen must be SEW-expanded (bits 2i & 2i+1), not the
          // element-granular vs1 (emu vcompress_vm-0, e16/m2).
          val vs2 = Seq.tabulate(32)(i => 0x10 + i) // m2 = 2 chunks
          val vs3 = Prefill.default(2)
          // element mask for chunk 0: elements 0,1,4 selected (vl=7)
          val elemMask = BigInt(1) | (BigInt(1) << 1) | (BigInt(1) << 4)
          val byteMask = (0 until 16).foldLeft(BigInt(0))((acc, b) =>
            if (((elemMask >> (b / 2)) & 1) == 1) acc | (BigInt(1) << b) else acc)
          val golden = compressGolden(byteMask, vs2.slice(0, 16), vs3.slice(0, 16), 0, 14)
          pCompress(dut, 1, elemMask, vs2.slice(0, 16), vs3.slice(0, 16),
            lmulFrac = 16, uopIdx = 0, placeIdx = 0, vl = 7, sew = 16)
          pCompress(dut, 0, 0, vs2.slice(16, 32), vs3.slice(16, 32),
            lmulFrac = 16, uopIdx = 1, placeIdx = 1, vl = 7, sew = 16)
          dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
          eWbAfter(dut, 0, golden, 5) // placeIdx=0 -> output pipe 0
        }
      }

      it("C8: replay e16 m8 interleaved blocks from rvv-bench wave 212526-212529") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          dut.in.wbReady.foreach(_.poke(true))
          dut.in.flush.s1.foreach(_.poke(false))

          val allOnes = (BigInt(1) << 128) - 1
          val vs2 = Seq.fill(16)(0xff)
          val vs3 = Seq.fill(16)(0)

          // The earlier e8/m8 transaction leaves a table in unit 15 at 212525.
          // Each row is one cycle, in pipe order; None means the pipe is idle.
          val prelude = Seq(
            Seq(Some((0, 0, 0, 55)), Some((3, 0, 1, 55)), Some((1, 0, 2, 55)), Some((2, 0, 3, 55))),
            Seq(Some((4, 0, 0, 55)), Some((2, 8, 0, 56)), Some((5, 0, 1, 55)), Some((6, 0, 2, 55))),
            Seq(Some((7, 0, 0, 55)), Some((6, 8, 0, 56)), Some((0, 8, 1, 56)), Some((1, 8, 2, 56))),
            Seq(Some((3, 8, 0, 56)), None, Some((4, 8, 1, 56)), Some((5, 8, 2, 56))),
            Seq(Some((7, 8, 0, 56)), None, None, None),
          )
          // (uopIdx, blockBase, placeIdx, robIdx), in pipe order for each cycle.
          val wave = Seq(
            Seq((3, 0, 0, 57), (0, 0, 1, 57), (1, 0, 2, 57), (2, 0, 3, 57)),
            Seq((2, 8, 0, 58), (4, 0, 0, 57), (5, 0, 1, 57), (6, 0, 2, 57)),
            Seq((6, 8, 0, 58), (7, 0, 0, 57), (0, 8, 1, 58), (1, 8, 2, 58)),
            Seq((7, 8, 0, 58), (3, 8, 1, 58), (4, 8, 2, 58), (5, 8, 3, 58)),
          )

          def drive(cycle: Seq[Option[(Int, Int, Int, Int)]], sew: Int): Unit = {
            cycle.zipWithIndex.foreach { case (input, pipe) =>
              pDefaults(dut, pipe)
              input.foreach { case (uopIdx, blockBase, placeIdx, robIdx) =>
                pCompress(dut, pipe, allOnes, vs2, vs3,
                  lmulFrac = 64, uopIdx = uopIdx, blockBase = blockBase,
                  placeIdx = placeIdx, vl = 64, vta = true, robIdx = robIdx,
                  pdest = blockBase + uopIdx + (if (sew == 8) 32 else 0), sew = sew)
              }
            }
            dut.clock.step()
          }

          prelude.foreach(drive(_, sew = 8))
          (0 until NumPipes).foreach(pPipeOff(dut, _))
          // At 212474 the redirect flushes s1 pipe 0 and units 0..14,
          // leaving the table in unit 15 without a matching index.
          dut.in.flush.s1(0).poke(true)
          (0 until 15).foreach(dut.in.unitFlush(_).poke(true))
          dut.clock.step()
          dut.out.unitStatus(15).hasTable.expect(false.B,
            "flush of unit 14 must not forward its table into unflushed unit 15")
          dut.in.flush.s1(0).poke(false)
          (0 until 15).foreach(dut.in.unitFlush(_).poke(false))
          for (_ <- 0 until 51) dut.clock.step()

          wave.foreach(cycle => drive(cycle.map(Some(_)), sew = 16))
          (0 until NumPipes).foreach(pPipeOff(dut, _))

          val completed = Array.fill(NumUnits)(false)
          for (_ <- 0 until 30) {
            for (pipe <- 0 until NumPipes if dut.out.pipes(pipe).valid.peekBoolean()) {
              val uop = dut.out.pipes(pipe).bits.meta.pdest.peekValue().asBigInt.toInt
              completed(uop) = true
              dut.out.pipes(pipe).bits.data.expect(allOnes)
            }
            dut.clock.step()
          }
          assert(completed.forall(identity), s"missing output uops: ${completed.indices.filterNot(completed(_)).mkString(",")}")
        }
      }
    }
  }

  // ── Fill: tail / mask strategies ─────────────────────
  describe("Fill") {
    it("F1: tail undisturbed mf8 vl=2") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val vs1 = foldBytes(Seq.tabulate(16)(identity))
        val vs3 = Prefill.default(1)
        // mf8 (lmulFrac=1): vlmax=2 bytes; elements 2..15 are tail (vta=0 ->
        // vs3 undisturbed).
        val golden = gatherGolden(vs1, 8, 8, 1, 0, table, 0 until 1, vs3, 2)
        pGather(dut, 0, 8, 8, vs1, table, vs3, lmulFrac = 1, vl = 2)
        dut.clock.step(); pPipeOff(dut, 0)
        eWbAfter(dut, 0, golden, 2)
      }
    }

    it("F2: tail undisturbed mf2 vl=4") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val vs1 = foldBytes(Seq.tabulate(16)(identity))
        val vs3 = Prefill.default(1)
        val golden = gatherGolden(vs1, 8, 8, 2, 0, table, 0 until 1, vs3, 4)
        pGather(dut, 0, 8, 8, vs1, table, vs3, lmulFrac = 2, vl = 4)
        dut.clock.step(); pPipeOff(dut, 0)
        eWbAfter(dut, 0, golden, 2)
      }
    }

    it("F3: tail undisturbed mf4 vl=1") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val vs1 = foldBytes(Seq.tabulate(16)(identity))
        val vs3 = Prefill.default(1)
        val golden = gatherGolden(vs1, 8, 8, 4, 0, table, 0 until 1, vs3, 1)
        pGather(dut, 0, 8, 8, vs1, table, vs3, lmulFrac = 4, vl = 1)
        dut.clock.step(); pPipeOff(dut, 0)
        eWbAfter(dut, 0, golden, 2)
      }
    }

    it("F4: tail agnostic vta=1 -> 0xff") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val vs1 = foldBytes(Seq.tabulate(16)(identity))
        val vs3 = Prefill.default(1)
        // vl=12 -> elements 12..15 tail; vta=1 -> agnostic (expect 0xff).
        val golden = gatherGolden(vs1, 8, 8, 8, 0, table, 0 until 1, vs3, 12, vta = true)
        pGather(dut, 0, 8, 8, vs1, table, vs3, vl = 12, vta = true)
        dut.clock.step(); pPipeOff(dut, 0)
        eWbAfter(dut, 0, golden, 2)
      }
    }

    it("F5: masked-off vma=0 mask=0 -> all vs3") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val vs1 = foldBytes(Seq.tabulate(16)(identity))
        val vs3 = Prefill.default(1)
        val golden = gatherGolden(vs1, 8, 8, 8, 0, table, 0 until 1, vs3, 16, mask = 0)
        pGather(dut, 0, 8, 8, vs1, table, vs3, mask = 0)
        dut.clock.step(); pPipeOff(dut, 0)
        eWbAfter(dut, 0, golden, 2)
      }
    }

    it("F6: mixed mask + tail") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val vs1 = foldBytes(Seq.tabulate(16)(identity))
        val vs3 = Prefill.default(1)
        // vl=12 -> elements 12..15 tail (vs3); mask bit 3 clear -> elem 3 vs3.
        val mask = (BigInt(1) << numElem) - 1 - (BigInt(1) << 3)
        val golden = gatherGolden(vs1, 8, 8, 8, 0, table, 0 until 1, vs3, 12, mask = mask)
        pGather(dut, 0, 8, 8, vs1, table, vs3, vl = 12, mask = mask)
        dut.clock.step(); pPipeOff(dut, 0)
        eWbAfter(dut, 0, golden, 2)
      }
    }

    it("F7: m1 vl=16 all active") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val vs1 = foldBytes(Seq.tabulate(16)(identity))
        val vs3 = Prefill.default(1)
        val golden = gatherGolden(vs1, 8, 8, 8, 0, table, 0 until 1, vs3, 16)
        pGather(dut, 0, 8, 8, vs1, table, vs3)
        dut.clock.step(); pPipeOff(dut, 0)
        eWbAfter(dut, 0, golden, 2)
      }
    }
  }

  // ── Writeback: wbReady / arbitration / status ────────
  describe("Writeback") {
    it("W1: multi-unit wake ack reserves writeback independently of wbReady") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        dut.in.wakeAck.foreach(_.poke(false))
        dut.in.wbReady(0).poke(false)
        val table = Table.inc(2)
        val old = Prefill.default(2)
        val golden = gatherGolden(0, 8, 8, 16, 0, table, 0 until 2, old.take(16), 32)
        pGather(dut, 0, 8, 8, 0, table.take(16), old.take(16), lmulFrac = 16)
        pGather(dut, 1, 8, 8, 0, table.drop(16), old.drop(16),
          lmulFrac = 16, uopIdx = 1, placeIdx = 1)
        dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
        var cy = 0
        while (cy < 10 && !dut.out.wake(0).valid.peekBoolean()) { dut.clock.step(); cy += 1 }
        dut.out.wake(0).valid.expect(true.B)
        dut.in.wakeAck(0).poke(true)
        dut.out.release.expect(1.U, "wake ack, not wbReady, releases this entry")
        dut.clock.step()
        dut.in.wakeAck(0).poke(false)
        dut.out.wake(0).valid.expect(false.B)
        for (_ <- 0 until issue2exu - 1) {
          dut.out.pipes(0).valid.expect(false.B)
          dut.out.release.expect(0.U)
          dut.clock.step()
        }
        for (_ <- 0 until 3) {
          dut.out.pipes(0).valid.expect(true.B, "reserved writeback remains valid while ready is low")
          dut.out.pipes(0).bits.data.expect(golden)
          dut.out.unitStatus(0).hasIndex.expect(true.B)
          dut.out.release.expect(0.U)
          dut.clock.step()
        }
        dut.in.wbReady(0).poke(true)
        dut.clock.step()
        dut.out.pipes(0).valid.expect(false.B)
        dut.out.unitStatus(0).hasIndex.expect(false.B)
      }
    }

    it("W2: early wbReady -> wb fires as soon as done") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
        val table = Table.inc(1)
        val golden = gatherGolden(0, 8, 8, 8, 0, table, 0 until 1, Prefill.default(1), 16)
        pGather(dut, 0, 8, 8, 0, table, Prefill.default(1))
        dut.clock.step(); pPipeOff(dut, 0)
        val wb = collectWb(dut, 8)
        wb(0).getOrElse(fail("pipe 0 never wrote back")) mustBe golden
      }
    }

    it("W3: two results same cycle, different pipes") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
        val table = Table.inc(1)
        val golden = gatherGolden(0, 8, 8, 8, 0, table, 0 until 1, Prefill.default(1), 16)
        pGather(dut, 0, 8, 8, 0, table, Prefill.default(1), blockBase = 0)
        pGather(dut, 2, 8, 8, 0, table, Prefill.default(1), blockBase = 2)
        dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 2)
        val wb = collectWb(dut, 8)
        wb(0).getOrElse(fail("pipe 0 never wrote back")) mustBe golden
        wb(2).getOrElse(fail("pipe 2 never wrote back")) mustBe golden
      }
    }

    it("W4: same-pipe arbitration U0+U4 -> lower index wins") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        dut.in.wakeAck.foreach(_.poke(false))
        dut.in.wbReady(0).poke(false)
        val table = Table.inc(1)
        val golden = gatherGolden(0, 8, 8, 8, 0, table, 0 until 1, Prefill.default(1), 16)
        // U0 and U4 share pipe0; hold both completed results before granting WB.
        pGather(dut, 0, 8, 8, 0, table, Prefill.default(1), blockBase = 0)
        dut.clock.step(); pPipeOff(dut, 0)
        pGather(dut, 0, 8, 8, 0, table, Prefill.default(1), blockBase = 4)
        dut.clock.step(); pPipeOff(dut, 0)
        var cy = 0
        while (cy < 12 && !(dut.out.unitStatus(0).done.peekBoolean() &&
                            dut.out.unitStatus(4).done.peekBoolean())) { dut.clock.step(); cy += 1 }
        dut.out.unitStatus(0).done.expect(true.B)
        dut.out.unitStatus(4).done.expect(true.B)
        dut.out.pipes(0).valid.expect(true.B)
        dut.out.wake.foreach(_.valid.expect(false.B))
        dut.out.release.expect(0.U)
        // U0 wins the first WB, then U4 writes back in the following cycle.
        dut.in.wbReady(0).poke(true)
        dut.out.pipes(0).bits.data.expect(golden)
        dut.out.release.expect(1.U)
        dut.clock.step()
        dut.out.pipes(0).valid.expect(true.B)
        dut.out.pipes(0).bits.data.expect(golden)
        dut.out.release.expect(16.U)
        dut.clock.step()
        dut.out.pipes(0).valid.expect(false.B)
        dut.out.release.expect(0.U)
        dut.out.unitStatus(0).hasIndex.expect(false.B)
        dut.out.unitStatus(4).hasIndex.expect(false.B)
      }
    }

    it("W5: interleaved mapping LMUL=2 -> pipes 0/1") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
        val table = Table.inc(2)
        val vs1 = foldBytes(Index.allInTable0(1))
        val g0 = gatherGolden(vs1, 8, 8, 16, 0, table, 0 until 2, Prefill.default(2), 32)
        val g1 = gatherGolden(vs1, 8, 8, 16, 1, table, 0 until 2, Prefill.default(2), 32)
        pGather(dut, 0, 8, 8, vs1, table.slice(0, 16), Prefill.default(2).slice(0, 16),
          lmulFrac = 16, uopIdx = 0, placeIdx = 0, vl = 32)
        pGather(dut, 1, 8, 8, vs1, table.slice(16, 32), Prefill.default(2).slice(16, 32),
          lmulFrac = 16, uopIdx = 1, placeIdx = 1, vl = 32)
        dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
        val wb = collectWb(dut, 8)
        wb(0).getOrElse(fail("pipe 0 never wrote back")) mustBe g0
        wb(1).getOrElse(fail("pipe 1 never wrote back")) mustBe g1
      }
    }

    it("W6: unitStatus cleared after drain") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
        val table = Table.inc(1)
        val golden = gatherGolden(0, 8, 8, 8, 0, table, 0 until 1, Prefill.default(1), 16)
        pGather(dut, 0, 8, 8, 0, table, Prefill.default(1))
        dut.clock.step(); pPipeOff(dut, 0)
        val wb = collectWb(dut, 8)
        wb(0).getOrElse(fail("pipe 0 never wrote back")) mustBe golden
        // after drain: index/counter/done cleared; m1 (blockSize=1) has no ring
        // flow/clearTable (not covered by any block controller), so the table
        // stays registered until flush or a new placement.
        val s = dut.out.unitStatus(0)
        s.hasIndex.expect(false.B, "hasIndex cleared after drain")
        s.counter.expect(0.U, "counter back to 0")
        s.done.expect(false.B, "done false after drain")
        s.hasTable.expect(false.B, "m1 table stays until flush/reuse")
      }
    }
  }

  // ── Wakeup: vperm wake (LMUL>1) ──────────────────────
  describe("Wakeup") {
    it("K1: LMUL=2 completed wake reserves delay zero") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(2)
        val vs1 = foldBytes(Index.allInTable0(1))
        pGather(dut, 0, 8, 8, vs1, table.slice(0, 16), Prefill.default(2).slice(0, 16),
          lmulFrac = 16, uopIdx = 0, placeIdx = 0, vl = 32)
        pGather(dut, 1, 8, 8, vs1, table.slice(16, 32), Prefill.default(2).slice(16, 32),
          lmulFrac = 16, uopIdx = 1, placeIdx = 1, vl = 32)
        dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
        var cy = 0
        var seen = false
        while (cy < 12 && !seen) {
          if (dut.out.wake(0).valid.peekBoolean()) {
            seen = true
            val delay = dut.out.wake(0).bits.delay.peekValue().asBigInt.toInt
            val counter = dut.out.unitStatus(0).counter.peekValue().asBigInt.toInt
            delay mustBe 0
            counter mustBe 0
          }
          dut.clock.step(); cy += 1
        }
        seen mustBe true
      }
    }

    it("K2: LMUL=4 wake valid + delay") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(4)
        val vs1 = foldBytes(Index.allInTable0(1))
        for (p <- 0 until 4) {
          pGather(dut, p, 8, 8, vs1, table.slice(p * 16, (p + 1) * 16),
            Prefill.default(4).slice(p * 16, (p + 1) * 16), lmulFrac = 32, uopIdx = p, placeIdx = p, vl = 64)
        }
        dut.clock.step()
        for (p <- 0 until 4) pPipeOff(dut, p)
        var cy = 0
        var seen = false
        while (cy < 12 && !seen) {
          if (dut.out.wake(0).valid.peekBoolean()) {
            seen = true
            val delay = dut.out.wake(0).bits.delay.peekValue().asBigInt.toInt
            val counter = dut.out.unitStatus(0).counter.peekValue().asBigInt.toInt
            delay mustBe 0
            counter mustBe 0
          }
          dut.clock.step(); cy += 1
        }
        seen mustBe true
      }
    }

    it("K3: LMUL=8 wake valid + delay") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(8)
        val vs1 = foldBytes(Index.allInTable0(1))
        for (u <- 0 until 8) {
          pGather(dut, 0, 8, 8, vs1, table.slice(u * 16, (u + 1) * 16),
            Prefill.default(8).slice(u * 16, (u + 1) * 16), lmulFrac = 64, uopIdx = u, placeIdx = 0, vl = 128)
          dut.clock.step(); pPipeOff(dut, 0)
        }
        var cy = 0
        var seen = false
        while (cy < 20 && !seen) {
          if (dut.out.wake(0).valid.peekBoolean()) {
            seen = true
            val delay = dut.out.wake(0).bits.delay.peekValue().asBigInt.toInt
            val counter = dut.out.unitStatus(0).counter.peekValue().asBigInt.toInt
            delay mustBe 0
            counter mustBe 0
          }
          dut.clock.step(); cy += 1
        }
        seen mustBe true
      }
    }

    it("K4: wake holds while unacked") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        dut.in.wakeAck.foreach(_.poke(false))
        // A completed request remains visible until its pipe accepts it.
        val table = Table.inc(8)
        val vs1 = foldBytes(Index.allInTable0(1))
        for (u <- 0 until 8) {
          pGather(dut, 0, 8, 8, vs1, table.slice(u * 16, (u + 1) * 16),
            Prefill.default(8).slice(u * 16, (u + 1) * 16), lmulFrac = 64, uopIdx = u, placeIdx = 0, vl = 128)
          dut.clock.step(); pPipeOff(dut, 0)
        }
        // Without ack the wake is level-based (not a one-shot pulse): it stays
        // asserted even after the result is complete.
        var cy = 0
        var held = false
        while (cy < 20 && !held) {
          if (dut.out.wake(0).valid.peekBoolean()) {
            dut.clock.step()
            if (dut.out.wake(0).valid.peekBoolean()) { held = true }
          } else { dut.clock.step() }
          cy += 1
        }
        held mustBe true
      }
    }

    it("K5: wake cleared by ack") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        dut.in.wakeAck.foreach(_.poke(false))
        val table = Table.inc(2)
        val vs1 = foldBytes(Index.allInTable0(1))
        pGather(dut, 0, 8, 8, vs1, table.slice(0, 16), Prefill.default(2).slice(0, 16),
          lmulFrac = 16, uopIdx = 0, placeIdx = 0, vl = 32)
        pGather(dut, 1, 8, 8, vs1, table.slice(16, 32), Prefill.default(2).slice(16, 32),
          lmulFrac = 16, uopIdx = 1, placeIdx = 1, vl = 32)
        dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
        var cy = 0
        var seen = false
        while (cy < 12 && !seen) {
          if (dut.out.wake(0).valid.peekBoolean()) {
            seen = true
            dut.out.release.expect(0.U, "release waits for wake ack")
            dut.in.wakeAck(0).poke(true)
            dut.out.release.expect(1.U, "accepted wake releases only unit 0")
            dut.clock.step()
            dut.in.wakeAck(0).poke(false)
            dut.out.wake(0).valid.expect(false.B, "wake drops after ack")
            dut.out.release.expect(0.U)
          } else { dut.clock.step(); cy += 1 }
        }
        seen mustBe true
      }
    }

    it("K5a: simultaneous fixed writebacks release two physical units without wakes") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        dut.in.wakeAck.foreach(_.poke(false))
        val table = Table.inc(1)
        val old = Prefill.default(1)
        pGather(dut, 0, 8, 8, 0, table, old, blockBase = 0)
        pGather(dut, 2, 8, 8, 0, table, old, blockBase = 2)
        dut.clock.step()
        pPipeOff(dut, 0)
        pPipeOff(dut, 2)

        dut.clock.step()
        dut.out.pipes(0).valid.expect(true.B)
        dut.out.pipes(2).valid.expect(true.B)
        dut.out.wake.foreach(_.valid.expect(false.B))
        dut.out.release.expect(5.U, "release units 0 and 2 on fixed WB fire")
        dut.clock.step()
        dut.out.release.expect(0.U)
      }
    }

    it("K7: wake meta == pdest") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(2)
        val vs1 = foldBytes(Index.allInTable0(1))
        pGather(dut, 0, 8, 8, vs1, table.slice(0, 16), Prefill.default(2).slice(0, 16),
          lmulFrac = 16, uopIdx = 0, placeIdx = 0, vl = 32, robIdx = 2, pdest = 0x55)
        pGather(dut, 1, 8, 8, vs1, table.slice(16, 32), Prefill.default(2).slice(16, 32),
          lmulFrac = 16, uopIdx = 1, placeIdx = 1, vl = 32, robIdx = 2, pdest = 0x55)
        dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
        var cy = 0
        var seen = false
        while (cy < 12 && !seen) {
          if (dut.out.wake(0).valid.peekBoolean()) {
            seen = true
            dut.out.wake(0).bits.meta.pdest.expect(0x55.U, "wake meta.pdest")
            dut.out.wake(0).bits.meta.robIdx.expect(2.U, "wake meta.robIdx")
          } else { dut.clock.step(); cy += 1 }
        }
        seen mustBe true
      }
    }

    it("K6: no wake until all block tables placed") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(2)
        val vs1 = foldBytes(Index.allInTable0(1))
        // uop0 only (uop1 not yet dispatched): the m2 block is genuinely
        // incomplete (U1's table not even in s1) -> wake must stay deasserted.
        pGather(dut, 0, 8, 8, vs1, table.slice(0, 16), Prefill.default(2).slice(0, 16),
          lmulFrac = 16, uopIdx = 0, placeIdx = 0, vl = 32)
        dut.clock.step(); pPipeOff(dut, 0)
        for (_ <- 0 until 4) {
          dut.out.wake(0).valid.expect(false.B, "no wake while block incomplete")
          dut.clock.step()
        }
        // dispatch uop1 (per-cycle placeIdx=0) -> block complete -> wake fires
        pGather(dut, 0, 8, 8, vs1, table.slice(16, 32), Prefill.default(2).slice(16, 32),
          lmulFrac = 16, uopIdx = 1, placeIdx = 0, vl = 32)
        dut.clock.step(); pPipeOff(dut, 0)
        var cy = 0
        var sawWake = false
        while (cy < 10 && !sawWake) {
          if (dut.out.wake(0).valid.peekBoolean()) sawWake = true
          dut.clock.step(); cy += 1
        }
        sawWake mustBe true
      }
    }

    it("K9: flush clears wakedUp so a new uop can wake") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        dut.in.wakeAck.foreach(_.poke(false))
        val table = Table.inc(2)
        val vs1 = foldBytes(Index.allInTable0(1))
        def dispatch(pdest: Int): Unit = {
          pGather(dut, 0, 8, 8, vs1, table.slice(0, 16), Prefill.default(2).slice(0, 16),
            lmulFrac = 16, uopIdx = 0, placeIdx = 0, vl = 32, pdest = pdest)
          pGather(dut, 1, 8, 8, vs1, table.slice(16, 32), Prefill.default(2).slice(16, 32),
            lmulFrac = 16, uopIdx = 1, placeIdx = 1, vl = 32, pdest = pdest)
          dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
        }
        // first block: wake + ack -> wakedUp set, wake off
        dispatch(0x11)
        var cy = 0
        var acked = false
        while (cy < 12 && !acked) {
          if (dut.out.wake(0).valid.peekBoolean()) {
            dut.in.wakeAck(0).poke(true)
            dut.clock.step()
            dut.in.wakeAck(0).poke(false)
            acked = true
          } else { dut.clock.step(); cy += 1 }
        }
        acked mustBe true
        // flush clears wakedUp (and the finished block's state)
        for (i <- 0 until NumUnits) dut.in.unitFlush(i).poke(true)
        dut.clock.step()
        for (i <- 0 until NumUnits) dut.in.unitFlush(i).poke(false)
        // second block on the same units must be able to wake again
        dispatch(0x22)
        var cy2 = 0
        var woke = false
        while (cy2 < 12 && !woke) {
          if (dut.out.wake(0).valid.peekBoolean()) {
            dut.out.wake(0).bits.meta.pdest.expect(0x22.U, "second block wakes")
            woke = true
          } else { dut.clock.step(); cy2 += 1 }
        }
        woke mustBe true
      }
    }

    it("K10: LMUL=8 unacked wake holds delay zero at completion") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        dut.in.wakeAck.foreach(_.poke(false))
        val table = Table.inc(8)
        val vs1 = foldBytes(Index.allInTable0(1))
        for (u <- 0 until 8) {
          pGather(dut, 0, 8, 8, vs1, table.slice(u * 16, (u + 1) * 16),
            Prefill.default(8).slice(u * 16, (u + 1) * 16), lmulFrac = 64, uopIdx = u, placeIdx = 0, vl = 128)
          dut.clock.step(); pPipeOff(dut, 0)
        }
        // Pipe0 holds U0's completed request while its ack is withheld.
        var samples = Seq.empty[(Int, Int)]
        var cy = 0
        while (cy < 20 && samples.length < 4) {
          if (dut.out.wake(0).valid.peekBoolean()) {
            val d = dut.out.wake(0).bits.delay.peekValue().asBigInt.toInt
            val c = dut.out.unitStatus(0).counter.peekValue().asBigInt.toInt
            samples :+= (d, c)
          }
          dut.clock.step(); cy += 1
        }
        samples.length must be >= 2
        samples.foreach { case (d, c) =>
          d mustBe 0
          c mustBe 0
        }
      }
    }

    it("K8: same-pipe wake arbitration U0+U4 -> lower index wins") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        dut.in.wakeAck.foreach(_.poke(false))
        val tableA = Table.inc(2)
        val tableB = Table.inc(2)
        val vs1 = foldBytes(Index.allInTable0(1))
        // Block A (U0-1, pdest 0xAA) and Block B (U4-5, pdest 0xBB), both on
        // pipe0/1. Dispatched one cycle apart so their wake windows overlap.
        pGather(dut, 0, 8, 8, vs1, tableA.slice(0, 16), Prefill.default(2).slice(0, 16),
          lmulFrac = 16, uopIdx = 0, placeIdx = 0, vl = 32, robIdx = 1, pdest = 0xAA)
        pGather(dut, 1, 8, 8, vs1, tableA.slice(16, 32), Prefill.default(2).slice(16, 32),
          lmulFrac = 16, uopIdx = 1, placeIdx = 1, vl = 32, robIdx = 1, pdest = 0xAA)
        dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
        pGather(dut, 0, 8, 8, vs1, tableB.slice(0, 16), Prefill.default(2).slice(0, 16),
          lmulFrac = 16, uopIdx = 0, placeIdx = 0, vl = 32, robIdx = 2, pdest = 0xBB, blockBase = 4)
        pGather(dut, 1, 8, 8, vs1, tableB.slice(16, 32), Prefill.default(2).slice(16, 32),
          lmulFrac = 16, uopIdx = 1, placeIdx = 1, vl = 32, robIdx = 2, pdest = 0xBB, blockBase = 4)
        dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
        // Pipe0 arbitrates U0/U4. Ack U0 before expecting the held U4 request.
        var pdests = Seq.empty[Int]
        var cy = 0
        while (cy < 16 && pdests.size < 2) {
          if (dut.out.wake(0).valid.peekBoolean()) {
            pdests :+= dut.out.wake(0).bits.meta.pdest.peekValue().asBigInt.toInt
            dut.in.wakeAck(0).poke(true)
          }
          dut.clock.step(); cy += 1
          dut.in.wakeAck(0).poke(false)
        }
        pdests mustBe Seq(0xAA, 0xBB)
      }
    }

    it("K11: flush before all units wake resets allTableHasFilled (no early wake on reuse)") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        dut.in.wakeAck.foreach(_.poke(false))
        val table = Table.inc(8)
        val vs1 = foldBytes(Index.allInTable0(1))
        // First m8 block: let uop0 wake (ack), then flush before the late uops
        // (uop3-6) wake. allTableHasFilled must be reset by the block-empty
        // condition (flush clears the units), not linger at 1.
        for (u <- 0 until 8) {
          pGather(dut, 0, 8, 8, vs1, table.slice(u * 16, (u + 1) * 16),
            Prefill.default(8).slice(u * 16, (u + 1) * 16), lmulFrac = 64, uopIdx = u, placeIdx = 0, vl = 128)
          dut.clock.step(); pPipeOff(dut, 0)
        }
        var acked = false
        var cy = 0
        while (cy < 12 && !acked) {
          if (dut.out.wake(0).valid.peekBoolean()) {
            dut.in.wakeAck(0).poke(true); dut.clock.step(); dut.in.wakeAck(0).poke(false)
            acked = true
          } else { dut.clock.step(); cy += 1 }
        }
        acked mustBe true
        for (i <- 0 until NumUnits) dut.in.unitFlush(i).poke(true)
        dut.clock.step()
        for (i <- 0 until NumUnits) dut.in.unitFlush(i).poke(false)
        // Reuse the same block with a PARTIAL m8 (uop0-3 only, block incomplete).
        // With allTableHasFilled correctly reset to 0, no unit may wake until the
        // block is assembled again.
        for (u <- 0 until 4) {
          pGather(dut, 0, 8, 8, vs1, table.slice(u * 16, (u + 1) * 16),
            Prefill.default(8).slice(u * 16, (u + 1) * 16), lmulFrac = 64, uopIdx = u, placeIdx = 0, vl = 128, robIdx = 9, pdest = 0x90 + u)
          dut.clock.step(); pPipeOff(dut, 0)
        }
        for (_ <- 0 until 8) {
          dut.out.wake(0).valid.expect(false.B, "no premature wake on incomplete reused block")
          dut.clock.step()
        }
      }
    }
  }

  // ── Flush ────────────────────────────────────────────
  describe("Flush") {
    it("H1: flush before dispatch does not break a following gather") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        for (i <- 0 until NumUnits) dut.in.unitFlush(i).poke(true)
        dut.clock.step()
        for (i <- 0 until NumUnits) dut.in.unitFlush(i).poke(false)
        val table = Table.inc(1)
        val golden = gatherGolden(0, 8, 8, 8, 0, table, 0 until 1, Prefill.default(1), 16)
        pGather(dut, 0, 8, 8, 0, table, Prefill.default(1))
        dut.clock.step(); pPipeOff(dut, 0)
        eWbAfter(dut, 0, golden, 2)
      }
    }

    it("H2: unit flush does not cancel an in-flight (s1) uop") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val golden = gatherGolden(0, 8, 8, 8, 0, table, 0 until 1, Prefill.default(1), 16)
        pGather(dut, 0, 8, 8, 0, table, Prefill.default(1))
        // Flush right after dispatch (uop still in s0/s1). The unit-level flush
        // clears the ring units, but the in-flight uop is not cancelled
        // (IndexGen is not flushed) and still places/completes. Pins this
        // behavior; if a pipeline-level flush is later added, this changes.
        for (i <- 0 until NumUnits) dut.in.unitFlush(i).poke(true)
        dut.clock.step()
        for (i <- 0 until NumUnits) dut.in.unitFlush(i).poke(false)
        pPipeOff(dut, 0)
        val wb = collectWb(dut, 8)
        wb(0).getOrElse(fail("pipe 0 never wrote back")) mustBe golden
      }
    }

    it("H3: flush during m2 flow resets units (no wb)") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(2)
        val vs1 = foldBytes(Index.allInTable0(1))
        pGather(dut, 0, 8, 8, vs1, table.slice(0, 16), Prefill.default(2).slice(0, 16),
          lmulFrac = 16, uopIdx = 0, placeIdx = 0, vl = 32)
        pGather(dut, 1, 8, 8, vs1, table.slice(16, 32), Prefill.default(2).slice(16, 32),
          lmulFrac = 16, uopIdx = 1, placeIdx = 1, vl = 32)
        dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
        dut.clock.step() // let tables place
        for (i <- 0 until NumUnits) dut.in.unitFlush(i).poke(true)
        dut.clock.step()
        for (i <- 0 until NumUnits) dut.in.unitFlush(i).poke(false)
        var cy = 0
        var wbSeen = false
        while (cy < 8) {
          if (dut.out.pipes(0).valid.peekBoolean() || dut.out.pipes(1).valid.peekBoolean()) wbSeen = true
          dut.clock.step(); cy += 1
        }
        wbSeen mustBe false
        for (u <- 0 until 4) {
          dut.out.unitStatus(u).hasIndex.expect(false.B)
          dut.out.unitStatus(u).hasTable.expect(false.B)
        }
      }
    }

    it("H4: flush after wb.valid clears without leakage") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val golden = gatherGolden(0, 8, 8, 8, 0, table, 0 until 1, Prefill.default(1), 16)
        pGather(dut, 0, 8, 8, 0, table, Prefill.default(1))
        dut.clock.step(); pPipeOff(dut, 0)
        var cy = 0
        while (cy < 10 && !dut.out.pipes(0).valid.peekBoolean()) { dut.clock.step(); cy += 1 }
        dut.out.pipes(0).valid.expect(true.B, "wb.valid holds (wbReady=0)")
        for (i <- 0 until NumUnits) dut.in.unitFlush(i).poke(true)
        dut.clock.step()
        for (i <- 0 until NumUnits) dut.in.unitFlush(i).poke(false)
        dut.out.pipes(0).valid.expect(false.B, "wb cleared by flush")
        for (i <- 0 until NumUnits) {
          dut.out.unitStatus(i).hasIndex.expect(false.B)
          dut.out.unitStatus(i).hasTable.expect(false.B)
        }
      }
    }

    it("H5: back-to-back reuse after flush") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val golden = gatherGolden(0, 8, 8, 8, 0, table, 0 until 1, Prefill.default(1), 16)
        pGather(dut, 0, 8, 8, 0, table, Prefill.default(1))
        dut.clock.step(); pPipeOff(dut, 0)
        eWbAfter(dut, 0, golden, 2)
        for (i <- 0 until NumUnits) dut.in.unitFlush(i).poke(true)
        dut.clock.step()
        for (i <- 0 until NumUnits) dut.in.unitFlush(i).poke(false)
        pGather(dut, 0, 8, 8, 0, table, Prefill.default(1))
        dut.clock.step(); pPipeOff(dut, 0)
        val wb = collectWb(dut, 8)
        wb(0).getOrElse(fail("pipe 0 never wrote back")) mustBe golden
      }
    }

    it("H6: flush clears unit status (hasTable/hasIndex/counter)") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(2)
        val vs1 = foldBytes(Index.allInTable0(1))
        pGather(dut, 0, 8, 8, vs1, table.slice(0, 16), Prefill.default(2).slice(0, 16),
          lmulFrac = 16, uopIdx = 0, placeIdx = 0, vl = 32)
        pGather(dut, 1, 8, 8, vs1, table.slice(16, 32), Prefill.default(2).slice(16, 32),
          lmulFrac = 16, uopIdx = 1, placeIdx = 1, vl = 32)
        dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
        for (i <- 0 until NumUnits) dut.in.unitFlush(i).poke(true)
        dut.clock.step()
        for (i <- 0 until NumUnits) dut.in.unitFlush(i).poke(false)
        for (u <- 0 until NumUnits) {
          val s = dut.out.unitStatus(u)
          s.hasTable.expect(false.B, s"unit $u hasTable cleared")
          s.hasIndex.expect(false.B, s"unit $u hasIndex cleared")
          s.counter.expect(0.U, s"unit $u counter cleared")
          s.done.expect(false.B, s"unit $u done false")
        }
      }
    }
  }

  // ── Meta: robIdx/pdest round-trip ────────────────────
  describe("Meta") {
    it("T1: meta round-trips to out.metas") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        pGather(dut, 0, 8, 8, 0, table, Prefill.default(1), robIdx = 3, pdest = 0x5a)
        dut.clock.step(); pPipeOff(dut, 0)
        var cy = 0
        while (cy < 6 && !dut.out.unitStatus(0).hasTable.peekBoolean()) { dut.clock.step(); cy += 1 }
        dut.out.metas.unit(0).valid.expect(true.B, "metas(0).valid")
        dut.out.metas.unit(0).bits.pdest.expect(0x5a.U, "metas(0).pdest")
        dut.out.metas.unit(0).bits.robIdx.expect(3.U, "metas(0).robIdx")
      }
    }

    it("T2: meta round-trips to writeback") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        pGather(dut, 0, 8, 8, 0, table, Prefill.default(1), robIdx = 3, pdest = 0x5a)
        dut.clock.step(); pPipeOff(dut, 0)
        var cy = 0
        var seen = false
        while (cy < 8 && !seen) {
          if (dut.out.pipes(0).valid.peekBoolean()) {
            seen = true
            dut.out.pipes(0).bits.meta.pdest.expect(0x5a.U, "wb meta.pdest")
            dut.out.pipes(0).bits.meta.robIdx.expect(3.U, "wb meta.robIdx")
          } else { dut.clock.step(); cy += 1 }
        }
        seen mustBe true
      }
    }

    it("T3: meta round-trips to wake (LMUL>1)") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(2)
        val vs1 = foldBytes(Index.allInTable0(1))
        pGather(dut, 0, 8, 8, vs1, table.slice(0, 16), Prefill.default(2).slice(0, 16),
          lmulFrac = 16, uopIdx = 0, placeIdx = 0, vl = 32, robIdx = 3, pdest = 0x5a)
        pGather(dut, 1, 8, 8, vs1, table.slice(16, 32), Prefill.default(2).slice(16, 32),
          lmulFrac = 16, uopIdx = 1, placeIdx = 1, vl = 32, robIdx = 3, pdest = 0x5a)
        dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
        var cy = 0
        var seen = false
        while (cy < 10 && !seen) {
          if (dut.out.wake(0).valid.peekBoolean()) {
            seen = true
            dut.out.wake(0).bits.meta.pdest.expect(0x5a.U, "wake meta.pdest")
            dut.out.wake(0).bits.meta.robIdx.expect(3.U, "wake meta.robIdx")
          } else { dut.clock.step(); cy += 1 }
        }
        seen mustBe true
      }
    }

    // ---- vrgatherei16 SEW=8: gather + rvs3 old-data pair ----
    describe("vrgatherei16") {

      // e16 index bytes (little-endian: lo byte first)
      def e16idx(vals: Seq[Int]): Seq[Int] = vals.flatMap(v => Seq(v & 0xff, v >> 8))

      // ei16e8 dual-vs1 index: slots 0-7 <- vs1 (vs1+2i), slots 8-15 <- vs3 (vs1+2i+1)
      def goldenEi16e8Idx(vs1Lo: BigInt, vs1Hi: BigInt, lmulFrac: Int): Seq[(Int, Boolean, Boolean, Boolean)] = {
        val vlmax = lmulFrac * vlen / 64
        def idx16(v: BigInt, ie: Int) = ((v >> (ie * 16)) & 0xffff).toInt
        (0 until 8).map { ie => val i = idx16(vs1Lo, ie); (i, i < vlmax, i >= vlmax, false) } ++
          (0 until 8).map { ie => val i = idx16(vs1Hi, ie); (i, i < vlmax, i >= vlmax, false) }
      }

      // Merged gather+rvs3 output for vd register effUopIdx: active -> gather's
      // table lookup / zero (rvs3 contributes ZERO(0)); tail/mask-inactive ->
      // rvs3 old-vd or agnostic. effUopIdx = uopIdx % blockSize (SEW=8).
      def ei16e8Golden(vs1Lo: BigInt, vs1Hi: BigInt, lmulFrac: Int, table: Seq[Int],
        oldVd: Seq[Int], effUopIdx: Int, vl: Int, vta: Boolean, vma: Boolean, mask: BigInt): BigInt = {
        val flags = goldenEi16e8Idx(vs1Lo, vs1Hi, lmulFrac)
        goldenPerm(flags, table, 0 until Math.max(1, lmulFrac / 8), oldVd, Seq.fill(8)(0), 8, effUopIdx, vl, vta, vma, mask)
      }

      def pGatherEi16(dut: PermUnit[FakePermMeta], pipe: Int,
        vs1Lo: Seq[Int], vs1Hi: Seq[Int], vs2: Seq[Int],
        lmulFrac: Int = 8, uopIdx: Int = 0, blockBase: Int = 0, placeIdx: Int = 0,
        vl: Int = -1, vta: Boolean = false, vma: Boolean = false,
        mask: BigInt = -1, robIdx: Int = 0, pdest: Int = 0
      ): Unit = {
        val p = dut.in.pipes(pipe)
        p.valid.poke(true)
        pOpReset(dut, pipe) // cross-cycle tests reuse a pipe with a different op
        p.bits.op.isGatherEI16.poke(true)
        p.bits.placeIdx.poke(placeIdx); p.bits.blockBase.poke(blockBase)
        p.bits.uopIdx.poke(uopIdx)
        pSew(p.bits.sew, 8); pSew(p.bits.ei, 16); pLmul(p.bits.lmul, lmulFrac)
        p.bits.blockFieldNum.poke(((lmulFrac / 8).max(1)).U)
        p.bits.vl.poke(if (vl < 0) (lmulFrac / 8) * vlen / 8 else vl)
        p.bits.vta.poke(vta); p.bits.vma.poke(vma)
        p.bits.vs1.poke(foldBytes(vs1Lo))
        p.bits.vs2.poke(foldBytes(vs2))
        p.bits.vs3.poke(foldBytes(vs1Hi))
        p.bits.mask.poke(if (mask < 0) (BigInt(1) << numElem) - 1 else mask)
        pMeta(dut, pipe, robIdx, pdest)
      }

      // vrgatherei16 with SEW>8 (single vs1 index register, no dual-vs1): poke a
      // gatherEI16 uop with arbitrary SEW, vs1 = e16 index register, vs3 = old-vd.
      def pGatherEi16Sew(dut: PermUnit[FakePermMeta], pipe: Int,
        vs1: Seq[Int], vs2: Seq[Int], vs3: Seq[Int], sew: Int,
        lmulFrac: Int = 8, uopIdx: Int = 0, blockBase: Int = 0, placeIdx: Int = 0,
        vl: Int = -1, vta: Boolean = false, vma: Boolean = false,
        mask: BigInt = -1, robIdx: Int = 0, pdest: Int = 0
      ): Unit = {
        val p = dut.in.pipes(pipe)
        p.valid.poke(true)
        pOpReset(dut, pipe)
        p.bits.op.isGatherEI16.poke(true)
        p.bits.placeIdx.poke(placeIdx); p.bits.blockBase.poke(blockBase)
        p.bits.uopIdx.poke(uopIdx)
        pSew(p.bits.sew, sew); pSew(p.bits.ei, 16); pLmul(p.bits.lmul, lmulFrac)
        p.bits.blockFieldNum.poke(((lmulFrac / 8).max(1)).U)
        p.bits.vl.poke(if (vl < 0) (lmulFrac / 8) * vlen / sew else vl)
        p.bits.vta.poke(vta); p.bits.vma.poke(vma)
        p.bits.vs1.poke(foldBytes(vs1))
        p.bits.vs2.poke(foldBytes(vs2))
        p.bits.vs3.poke(foldBytes(vs3))
        p.bits.mask.poke(if (mask < 0) (BigInt(1) << numElem) - 1 else mask)
        pMeta(dut, pipe, robIdx, pdest)
      }

      def pRvs3(dut: PermUnit[FakePermMeta], pipe: Int,
        oldVd: Seq[Int],
        lmulFrac: Int = 8, uopIdx: Int = 1, blockBase: Int = 0, placeIdx: Int = 0,
        vl: Int = -1, vta: Boolean = false, vma: Boolean = false,
        mask: BigInt = -1, robIdx: Int = 0, pdest: Int = 0
      ): Unit = {
        val p = dut.in.pipes(pipe)
        p.valid.poke(true)
        pOpReset(dut, pipe) // cross-cycle tests reuse a pipe with a different op
        p.bits.op.isGatherEI16Rvs3.poke(true)
        p.bits.placeIdx.poke(placeIdx); p.bits.blockBase.poke(blockBase)
        p.bits.uopIdx.poke(uopIdx)
        pSew(p.bits.sew, 8); pSew(p.bits.ei, 16); pLmul(p.bits.lmul, lmulFrac)
        p.bits.blockFieldNum.poke(((lmulFrac / 8).max(1)).U)
        p.bits.vl.poke(if (vl < 0) (lmulFrac / 8) * vlen / 8 else vl)
        p.bits.vta.poke(vta); p.bits.vma.poke(vma)
        p.bits.vs3.poke(foldBytes(oldVd))
        p.bits.mask.poke(if (mask < 0) (BigInt(1) << numElem) - 1 else mask)
        pMeta(dut, pipe, robIdx, pdest)
      }

      // Clear all op flags (cross-cycle tests reuse a pipe with a different op).
      def pOpReset(dut: PermUnit[FakePermMeta], pipe: Int): Unit = {
        val o = dut.in.pipes(pipe).bits.op
        o.isGather.poke(false); o.isGatherEI16.poke(false); o.isGatherEI16Rvs3.poke(false)
        o.isSlideUp.poke(false); o.isSlide1Up.poke(false)
        o.isSlideDown.poke(false); o.isSlide1Down.poke(false)
        o.isZip.poke(false); o.isUnzip.poke(false); o.isCompress.poke(false)
      }

      describe("NOMASK") {
        it("E1: m1 same-cycle pair (wb uses rvs3 pdest)") {
          simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
            initDut(dut)
            val table = Table.inc(1)
            val idxLo = e16idx((0 until 8).toList)
            val idxHi = e16idx((8 until 16).toList)
            val oldVd = (0 until 16).map(i => 0xa0 + i)
            val golden = ei16e8Golden(foldBytes(idxLo), foldBytes(idxHi), 8, table, oldVd, 0, 16, false, false, -1)
            pGatherEi16(dut, 0, idxLo, idxHi, table, lmulFrac = 8, uopIdx = 0, blockBase = 0, placeIdx = 0, vl = 16, robIdx = 1, pdest = 0x11)
            pRvs3(dut, 1, oldVd, lmulFrac = 8, uopIdx = 1, blockBase = 0, placeIdx = 0, vl = 16, robIdx = 1, pdest = 0x22)
            dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
            var cy = 0
            while (cy < 12 && !dut.out.pipes(0).valid.peekBoolean()) { dut.clock.step(); cy += 1 }
            dut.out.pipes(0).valid.expect(true.B, "m1 pair writes back")
            dut.out.pipes(0).bits.data.expect(golden, "merged gather+old-vd")
            dut.out.pipes(0).bits.meta.pdest.expect(0x22.U, "wb target = rvs3 pdest")
          }
        }
        it("E2: m2 same-cycle pairs") {
          simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
            initDut(dut)
            for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
            val table = Table.inc(2)
            val oldVd = Seq((0 until 16).map(i => 0xa0 + i), (0 until 16).map(i => 0xb0 + i))
            val idxLo = Seq(e16idx((0 until 8).toList), e16idx((16 until 24).toList))
            val idxHi = Seq(e16idx((8 until 16).toList), e16idx((24 until 32).toList))
            val g0 = ei16e8Golden(foldBytes(idxLo(0)), foldBytes(idxHi(0)), 16, table, oldVd(0), 0, 32, false, false, -1)
            val g1 = ei16e8Golden(foldBytes(idxLo(1)), foldBytes(idxHi(1)), 16, table, oldVd(1), 1, 32, false, false, -1)
            pGatherEi16(dut, 0, idxLo(0), idxHi(0), table.slice(0, 16), lmulFrac = 16, uopIdx = 0, blockBase = 0, placeIdx = 0, vl = 32, pdest = 0)
            pRvs3(dut, 1, oldVd(0), lmulFrac = 16, uopIdx = 2, blockBase = 0, placeIdx = 0, vl = 32, pdest = 0)
            pGatherEi16(dut, 2, idxLo(1), idxHi(1), table.slice(16, 32), lmulFrac = 16, uopIdx = 1, blockBase = 0, placeIdx = 1, vl = 32, pdest = 1)
            pRvs3(dut, 3, oldVd(1), lmulFrac = 16, uopIdx = 3, blockBase = 0, placeIdx = 1, vl = 32, pdest = 1)
            dut.clock.step(); for (p <- 0 until 4) pPipeOff(dut, p)
            val wb = collectWb(dut, 20)
            wb(0).getOrElse(fail("pair0 never wrote back")) mustBe g0
            wb(1).getOrElse(fail("pair1 never wrote back")) mustBe g1
          }
        }
        it("E3: m2 cross-cycle (gathers then rvs3s)") {
          simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
            initDut(dut)
            for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
            val table = Table.inc(2)
            val oldVd = Seq((0 until 16).map(i => 0xa0 + i), (0 until 16).map(i => 0xb0 + i))
            val idxLo = Seq(e16idx((0 until 8).toList), e16idx((16 until 24).toList))
            val idxHi = Seq(e16idx((8 until 16).toList), e16idx((24 until 32).toList))
            val g0 = ei16e8Golden(foldBytes(idxLo(0)), foldBytes(idxHi(0)), 16, table, oldVd(0), 0, 32, false, false, -1)
            val g1 = ei16e8Golden(foldBytes(idxLo(1)), foldBytes(idxHi(1)), 16, table, oldVd(1), 1, 32, false, false, -1)
            // cy0: gathers only; each unit's hasIndex stays 0 (waitRvs3) until rvs3
            pGatherEi16(dut, 0, idxLo(0), idxHi(0), table.slice(0, 16), lmulFrac = 16, uopIdx = 0, blockBase = 0, placeIdx = 0, vl = 32, pdest = 0)
            pGatherEi16(dut, 1, idxLo(1), idxHi(1), table.slice(16, 32), lmulFrac = 16, uopIdx = 1, blockBase = 0, placeIdx = 1, vl = 32, pdest = 1)
            dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
            // cy1: rvs3s merge the old-data and release writeback
            pRvs3(dut, 0, oldVd(0), lmulFrac = 16, uopIdx = 2, blockBase = 0, placeIdx = 0, vl = 32, pdest = 0)
            pRvs3(dut, 1, oldVd(1), lmulFrac = 16, uopIdx = 3, blockBase = 0, placeIdx = 1, vl = 32, pdest = 1)
            dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
            val wb = collectWb(dut, 20)
            wb(0).getOrElse(fail("pair0 never wrote back")) mustBe g0
            wb(1).getOrElse(fail("pair1 never wrote back")) mustBe g1
          }
        }
        it("E6: mf8 rvs3-before-gather (out-of-order pair)") {
          simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
            initDut(dut)
            for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
            val table = Table.inc(1)
            val idxLo = e16idx(Seq(0, 1))
            val idxHi = Seq.fill(16)(0)
            val oldVd = (0 until 16).map(i => 0xa0 + i)
            // mf8 (lmulFrac=1), vl=2: bytes 0,1 active, 2-15 tail (tu)
            val golden = ei16e8Golden(foldBytes(idxLo), foldBytes(idxHi), 1, table, oldVd, 0, 2, false, false, -1)
            // cy0: rvs3 (uop1) arrives before its gather (out-of-order dispatch)
            pRvs3(dut, 1, oldVd, lmulFrac = 1, uopIdx = 1, blockBase = 0, placeIdx = 0, vl = 2, robIdx = 1, pdest = 0x22)
            dut.clock.step(); pPipeOff(dut, 1)
            // cy1: gather (uop0)
            pGatherEi16(dut, 0, idxLo, idxHi, table, lmulFrac = 1, uopIdx = 0, blockBase = 0, placeIdx = 0, vl = 2, robIdx = 1, pdest = 0x11)
            dut.clock.step(); pPipeOff(dut, 0)
            var cy = 0
            while (cy < 12 && !dut.out.pipes(0).valid.peekBoolean()) { dut.clock.step(); cy += 1 }
            dut.out.pipes(0).valid.expect(true.B, "mf8 out-of-order pair writes back")
            dut.out.pipes(0).bits.data.expect(golden, "active gathered, tail = old vd")
            dut.out.pipes(0).bits.meta.pdest.expect(0x22.U, "wb target = rvs3 pdest")
          }
        }
        it("E7: emu replay mf8 rvs3-before-gather") {
          simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
            initDut(dut)
            for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
            val vs1Lo = Seq(0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f) // gather vs1 (slots 0-7)
            val vs1Hi = Seq(0x00,0x81,0x07,0x00,0x04,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00) // gather vs3 (2nd vs1, slots 8-15 tail)
            val table = Seq(0x00,0x81,0x07,0x00,0x04,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00) // gather vs2
            val oldVd = Seq(0x52,0x45,0x80,0x32,0xdc,0x75,0xac,0x30,0x95,0x12,0x76,0x5b,0xaa,0xc0,0x44,0x83) // rvs3 vs3
            val golden = ei16e8Golden(foldBytes(vs1Lo), foldBytes(vs1Hi), 1, table, oldVd, 0, 2, false, false, -1)
            // rvs3 (uop1) arrives before its gather
            pRvs3(dut, 0, oldVd, lmulFrac = 1, uopIdx = 1, blockBase = 0, placeIdx = 0, vl = 2, robIdx = 0x71, pdest = 0x53)
            dut.clock.step(); pPipeOff(dut, 0)
            // cy1: gather (uop0)
            pGatherEi16(dut, 0, vs1Lo, vs1Hi, table, lmulFrac = 1, uopIdx = 0, blockBase = 0, placeIdx = 0, vl = 2, robIdx = 0x71, pdest = 0x53)
            dut.clock.step(); pPipeOff(dut, 0)
            var cy = 0
            while (cy < 12 && !dut.out.pipes(0).valid.peekBoolean()) { dut.clock.step(); cy += 1 }
            dut.out.pipes(0).valid.expect(true.B, "emu mf8 replay writes back")
            dut.out.pipes(0).bits.data.expect(golden, "index out-of-range -> 0, tail -> old vd")
            dut.out.pipes(0).bits.meta.pdest.expect(0x53.U, "wb target = rvs3 pdest")
          }
        }
        // vrgatherei16.vv v2,v6,v4, e32/m2/tu/mu, vl=8. uop0->unit0(v2), uop1->unit1(v3).
        it("E8: emu replay e32 m2 vl=8 gatherEI16") {
          simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
            initDut(dut)
            for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
            val idx = Seq(0x00,0x00,0x00,0x00,0x01,0x00,0x00,0x00,0x03,0x00,0x00,0x00,0xf8,0xff,0xff,0xff) // v4 e16 [0,0,1,0,3,0,0xfff8,0xffff]
            val srcLo = Seq(0x00,0x00,0x00,0x00,0x02,0x00,0x00,0x00,0x0f,0x00,0x00,0x00,0x00,0x00,0x00,0x00) // v6 e32 [0,2,15,0]
            val srcHi = Seq(0xf8,0xff,0xff,0xff,0x00,0x00,0x00,0x00,0xff,0x07,0x00,0x00,0x01,0x00,0x00,0x00) // v7 e32 [0xfffffff8,0,0x7ff,1]
            val oldV2 = Seq(0xcc,0x8c,0x67,0xad,0x62,0xd4,0xb3,0xb1,0xee,0x30,0x02,0xa3,0x7a,0x51,0x03,0x5f)
            val oldV3 = Seq(0xac,0xef,0x65,0x23,0xed,0x27,0xcf,0x7d,0x27,0x35,0xba,0x00,0xf8,0x50,0x67,0x0a)
            // Correct result (nemu reference): uop0 (v2) -> [0,0,2,0],
            // uop1 (v3) should use e4-e7=[3,0,0xfff8,0xffff] -> [0,0,0,0]
            val gV2 = foldBytes(Seq(0,0,0,0, 0,0,0,0, 2,0,0,0, 0,0,0,0))
            val gV3 = BigInt(0)
            pGatherEi16Sew(dut, 0, idx, srcLo, oldV2, sew = 32, lmulFrac = 16, uopIdx = 0, blockBase = 0, placeIdx = 0, vl = 8, robIdx = 0x141, pdest = 0x67)
            pGatherEi16Sew(dut, 1, idx, srcHi, oldV3, sew = 32, lmulFrac = 16, uopIdx = 1, blockBase = 0, placeIdx = 1, vl = 8, robIdx = 0x141, pdest = 0x68)
            dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
            val wb = collectWb(dut, 20)
            println(s"[E8-diag] wb0(v2)=0x${wb(0).map(_.toString(16)).getOrElse("NA")} golden=0x${gV2.toString(16)}")
            println(s"[E8-diag] wb1(v3)=0x${wb(1).map(_.toString(16)).getOrElse("NA")} golden=0x${gV3.toString(16)}")
            wb(0).getOrElse(fail("v2 (pdest 0x67) never wrote back")) mustBe gV2
            wb(1).getOrElse(fail("v3 (pdest 0x68) never wrote back")) mustBe gV3
          }
        }
        // vrgatherei16.vv v2, v8, v12, e8/m4/tu/mu, vl=31 (robIdx=0xe2). The emu
        // core DEADLOCKED: every ring unit's counter decrements to 1 then stops,
        // so done (counter==0 && hasIndex) never fires and nothing writes back.
        // Dispatch (from FST): rvs3 uopIdx 4,5,6 at cy0 (pipes 1-3, placeIdx 0,1,2),
        // rvs3 uopIdx 7 at cy1 (pipe1, placeIdx 3), gather uopIdx 0,1,2 at cy2
        // (pipes 1-3, placeIdx 0,1,2), gather uopIdx 3 at cy3 (pipe0, placeIdx 3).
        // All uops share robIdx, blockBase 0, blockSize 4 (m4).
        it("E9: emu replay e8 m4 vl=31 vrgatherei16 deadlock") {
          simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
            initDut(dut)
            for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
            // Source register data captured at the 0xe2 entries (per-pipe input).
            val oldVd4 = Seq(0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01)
            val oldVd5 = Seq(0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8)
            val oldVd6 = Seq(0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02)
            val oldVd7 = oldVd4
            val gIdx0 = Seq(0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f)
            val gIdxHi0 = oldVd4
            val gTab0 = Seq(0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00)
            val gIdx1 = Seq(0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f)
            val gIdxHi1 = oldVd5
            val gTab1 = Seq(0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02)
            val gIdx2 = Seq(0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00)
            val gIdxHi2 = oldVd6
            val gTab2 = Seq(0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01)
            val gIdx3 = Seq(0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01)
            val gIdxHi3 = Seq(0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00)
            val gTab3 = gIdxHi3
            // cy0: rvs3 uopIdx 4,5,6 (pipes 1-3). placeIdx = nth non-rvs3 same-robIdx
            // uop of this cycle; rvs3 uops are not counted -> all 0 (their indexPos
            // is derived from uopIdx % blockSize, not placeIdx).
            pRvs3(dut, 1, oldVd4, lmulFrac = 32, uopIdx = 4, blockBase = 0, placeIdx = 0, vl = 31, robIdx = 0xe2, pdest = 0x52)
            pRvs3(dut, 2, oldVd5, lmulFrac = 32, uopIdx = 5, blockBase = 0, placeIdx = 0, vl = 31, robIdx = 0xe2, pdest = 0x53)
            pRvs3(dut, 3, oldVd6, lmulFrac = 32, uopIdx = 6, blockBase = 0, placeIdx = 0, vl = 31, robIdx = 0xe2, pdest = 0x54)
            dut.clock.step()
            for (p <- 0 until NumPipes) pPipeOff(dut, p)
            // cy1: rvs3 uopIdx 7 (pipe1 back-to-back)
            pRvs3(dut, 1, oldVd7, lmulFrac = 32, uopIdx = 7, blockBase = 0, placeIdx = 0, vl = 31, robIdx = 0xe2, pdest = 0x67)
            dut.clock.step()
            for (p <- 0 until NumPipes) pPipeOff(dut, p)
            // cy2: gather uopIdx 0,1,2 (pipes 1-3)
            pGatherEi16(dut, 1, gIdx0, gIdxHi0, gTab0, lmulFrac = 32, uopIdx = 0, blockBase = 0, placeIdx = 0, vl = 31, robIdx = 0xe2)
            pGatherEi16(dut, 2, gIdx1, gIdxHi1, gTab1, lmulFrac = 32, uopIdx = 1, blockBase = 0, placeIdx = 1, vl = 31, robIdx = 0xe2)
            pGatherEi16(dut, 3, gIdx2, gIdxHi2, gTab2, lmulFrac = 32, uopIdx = 2, blockBase = 0, placeIdx = 2, vl = 31, robIdx = 0xe2)
            dut.clock.step()
            for (p <- 0 until NumPipes) pPipeOff(dut, p)
            // cy3: gather uopIdx 3 (pipe0) - 0th non-rvs3 uop of this cycle
            pGatherEi16(dut, 0, gIdx3, gIdxHi3, gTab3, lmulFrac = 32, uopIdx = 3, blockBase = 0, placeIdx = 0, vl = 31, robIdx = 0xe2)
            dut.clock.step()
            for (p <- 0 until NumPipes) pPipeOff(dut, p)
            // collect writeback + drain (like M9); on the buggy RTL every unit's
            // counter sticks at 1 -> done (counter==0 && hasIndex) never fires
            // -> no unit drains (reproduces the emu deadlock, so this test fails
            // until the flow bug is fixed). rvs3 uops arrive before their gathers,
            // so hasIndex is only set once the gather lands; wait for the block to
            // become active before checking drain.
            val wbData = Array.fill(NumPipes)(Option.empty[BigInt])
            var sawActive = false
            var drained = false
            for (cy <- 0 until 40; if !drained) {
              for (p <- 0 until NumPipes) {
                if (dut.out.pipes(p).valid.peekBoolean() && wbData(p).isEmpty) {
                  wbData(p) = Some(dut.out.pipes(p).bits.data.peekValue().asBigInt)
                }
              }
              val anyActive = (0 until NumUnits).exists(u => dut.out.unitStatus(u).hasIndex.peekBoolean())
              if (anyActive) sawActive = true
              drained = sawActive && (0 until NumUnits).forall(u => !dut.out.unitStatus(u).hasIndex.peekBoolean())
              dut.clock.step()
            }
            for (u <- 0 until NumUnits) {
              dut.out.unitStatus(u).hasIndex.expect(false.B, s"unit $u hasIndex drained")
              dut.out.unitStatus(u).hasTable.expect(false.B, s"unit $u hasTable drained")
            }
            (0 until NumPipes).foreach { p =>
              wbData(p).getOrElse(fail(s"pipe $p never wrote back (deadlock)"))
            }
          }
        }
        // vrgatherei16.vv e8/m4/tu/mu, vl=32 (robIdx=0x105, pc 0x80004720). The emu
        // (emu_logs/20260826-173423) still deadlocked on this later loop iteration:
        // ABORT at pc 0x8000420e, v4/v5 mismatch. Dispatch (from FST): rvs3 uopIdx
        // 5,4,6 at cy0 (pipes 0,2,3), rvs3 uopIdx 7 at cy1 (pipe1), gather uopIdx
        // 0,2 at cy2 (pipes 1,3), gather uopIdx 1 at cy3 (pipe2), gather uopIdx 3
        // at cy4 (pipe1). placeIdx = is2PlaceIdx (nth same-robIdx uop of the cycle,
        // counting rvs3 too). Regression for the indexPos=uopIdx%blockSize fix.
        it("E10: emu replay e8 m4 vl=32 vrgatherei16 deadlock") {
          simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
            initDut(dut)
            for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
            // Source register data captured at the 0x105 entries (per-pipe input).
            val oldVd5 = Seq(0xf3,0x64,0x19,0x99,0xc9,0x38,0x9f,0xc5,0x06,0x2c,0x01,0xca,0xda,0xbb,0x9e,0x0b)
            val oldVd4 = Seq(0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8)
            val oldVd6 = Seq(0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02)
            val oldVd7 = Seq(0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00)
            val gIdx0 = oldVd7
            val gIdxHi0 = oldVd7
            val gTab0 = oldVd7
            val gIdx2 = Seq(0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00)
            val gIdxHi2 = oldVd6
            val gTab2 = Seq(0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01)
            val gIdx1 = Seq(0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f)
            val gIdxHi1 = oldVd4
            val gTab1 = Seq(0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02)
            val gIdx3 = oldVd7
            val gIdxHi3 = oldVd7
            val gTab3 = oldVd7
            // cy0: rvs3 uopIdx 5,4,6 (pipes 0,2,3; placeIdx = log is2PlaceIdx 0,1,2)
            pRvs3(dut, 0, oldVd5, lmulFrac = 32, uopIdx = 5, blockBase = 0, placeIdx = 0, vl = 32, robIdx = 0x105, pdest = 0x64)
            pRvs3(dut, 2, oldVd4, lmulFrac = 32, uopIdx = 4, blockBase = 0, placeIdx = 1, vl = 32, robIdx = 0x105, pdest = 0x63)
            pRvs3(dut, 3, oldVd6, lmulFrac = 32, uopIdx = 6, blockBase = 0, placeIdx = 2, vl = 32, robIdx = 0x105, pdest = 0x65)
            dut.clock.step()
            for (p <- 0 until NumPipes) pPipeOff(dut, p)
            // cy1: rvs3 uopIdx 7 (pipe1)
            pRvs3(dut, 1, oldVd7, lmulFrac = 32, uopIdx = 7, blockBase = 0, placeIdx = 0, vl = 32, robIdx = 0x105, pdest = 0x66)
            dut.clock.step()
            for (p <- 0 until NumPipes) pPipeOff(dut, p)
            // cy2: gather uopIdx 0,2 (pipes 1,3)
            pGatherEi16(dut, 1, gIdx0, gIdxHi0, gTab0, lmulFrac = 32, uopIdx = 0, blockBase = 0, placeIdx = 0, vl = 32, robIdx = 0x105)
            pGatherEi16(dut, 3, gIdx2, gIdxHi2, gTab2, lmulFrac = 32, uopIdx = 2, blockBase = 0, placeIdx = 1, vl = 32, robIdx = 0x105)
            dut.clock.step()
            for (p <- 0 until NumPipes) pPipeOff(dut, p)
            // cy3: gather uopIdx 1 (pipe2)
            pGatherEi16(dut, 2, gIdx1, gIdxHi1, gTab1, lmulFrac = 32, uopIdx = 1, blockBase = 0, placeIdx = 0, vl = 32, robIdx = 0x105)
            dut.clock.step()
            for (p <- 0 until NumPipes) pPipeOff(dut, p)
            // cy4: gather uopIdx 3 (pipe1)
            pGatherEi16(dut, 1, gIdx3, gIdxHi3, gTab3, lmulFrac = 32, uopIdx = 3, blockBase = 0, placeIdx = 0, vl = 32, robIdx = 0x105)
            dut.clock.step()
            for (p <- 0 until NumPipes) pPipeOff(dut, p)
            // collect writeback + drain (wait for the block to become active first)
            val wbData = Array.fill(NumPipes)(Option.empty[BigInt])
            var sawActive = false
            var drained = false
            for (cy <- 0 until 40; if !drained) {
              for (p <- 0 until NumPipes) {
                if (dut.out.pipes(p).valid.peekBoolean() && wbData(p).isEmpty) {
                  wbData(p) = Some(dut.out.pipes(p).bits.data.peekValue().asBigInt)
                }
              }
              val anyActive = (0 until NumUnits).exists(u => dut.out.unitStatus(u).hasIndex.peekBoolean())
              if (anyActive) sawActive = true
              drained = sawActive && (0 until NumUnits).forall(u => !dut.out.unitStatus(u).hasIndex.peekBoolean())
              dut.clock.step()
            }
            for (u <- 0 until NumUnits) {
              dut.out.unitStatus(u).hasIndex.expect(false.B, s"unit $u hasIndex drained")
              dut.out.unitStatus(u).hasTable.expect(false.B, s"unit $u hasTable drained")
            }
            (0 until NumPipes).foreach { p =>
              wbData(p).getOrElse(fail(s"pipe $p never wrote back (deadlock)"))
            }
          }
        }
        // vrgatherei16.vv e8/m4/tu/mu, vl=64 (robIdx=0x126=294, pc 0x8000428a).
        // The emu (emu_logs/20260827-161241) stuck (no commit for 15000
        // cycles) on this instruction after the placeIdx fix (rvs3 uops are not
        // counted for placeIdx). placeIdx = nth same-robIdx NON-rvs3 uop of the
        // cycle. Exact dispatch from FST, 3 batches 2 cycles apart:
        //   b0: rvs3 uopIdx 5,6 (pipes 2,3, placeIdx 0,0, pdest 0x75,0x76)
        //   b2: gather uopIdx 3 (pipe0, placeIdx 0), rvs3 uopIdx 4 (pipe1,
        //       placeIdx 0, pdest 0x6), gather uopIdx 1,2 (pipes 2,3, placeIdx 1,2)
        //   b4: rvs3 uopIdx 7 (pipe0, placeIdx 0, pdest 0x77), gather uopIdx 0
        //       (pipe1, placeIdx 0)
        // Regression: must NOT deadlock (all units drain, all pipes write back).
        it("E11: emu replay e8 m4 vl=64 vrgatherei16 deadlock (placeIdx non-rvs3)") {
          simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
            initDut(dut)
            for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
            // rvs3 uopIdx 5 (pipe2): old-vd
            val rOld5 = Seq(0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8)
            // rvs3 uopIdx 6 (pipe3): old-vd
            val rOld6 = Seq(0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02)
            // gather uopIdx 3 (pipe0): idx-lo=0, idx-hi=f5,89..., table=0
            val gIdx3lo = Seq.fill(16)(0x00)
            val gIdx3hi = Seq(0xf5,0x89,0x1b,0x42,0x6a,0x56,0xb9,0x90,0x3a,0x19,0x69,0x9a,0xf8,0xac,0x6e,0x0d)
            val gTab3 = Seq.fill(16)(0x00)
            // rvs3 uopIdx 4 (pipe1): old-vd
            val rOld4 = Seq(0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01)
            // gather uopIdx 1 (pipe2)
            val gIdx1lo = Seq(0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f)
            val gIdx1hi = Seq(0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8)
            val gTab1 = Seq(0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02)
            // gather uopIdx 2 (pipe3)
            val gIdx2lo = Seq(0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00)
            val gIdx2hi = Seq(0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02)
            val gTab2 = Seq(0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01)
            // rvs3 uopIdx 7 (pipe0): old-vd (== gather uopIdx3 idx-hi)
            val rOld7 = gIdx3hi
            // gather uopIdx 0 (pipe1)
            val gIdx0lo = Seq(0x00,0x01,0x03,0xf8,0xf8,0x7f,0x7f,0x7f,0xff,0xff,0x00,0x01,0x03,0xf8,0xf8,0x7f)
            val gIdx0hi = rOld4
            val gTab0 = Seq(0x00,0x02,0x0f,0x00,0x08,0x00,0x07,0x01,0x00,0xff,0x00,0x02,0x0f,0x00,0x08,0x00)
            // b0: rvs3 uopIdx 5,6 (pipes 2,3). placeIdx = nth same-robIdx
            // NON-rvs3 uop of the cycle; rvs3 uops are not counted -> 0,0.
            pRvs3(dut, 2, rOld5, lmulFrac = 32, uopIdx = 5, blockBase = 0, placeIdx = 0, vl = 64, robIdx = 0x126, pdest = 0x75)
            pRvs3(dut, 3, rOld6, lmulFrac = 32, uopIdx = 6, blockBase = 0, placeIdx = 0, vl = 64, robIdx = 0x126, pdest = 0x76)
            dut.clock.step()
            for (p <- 0 until NumPipes) pPipeOff(dut, p)
            // gap (emu: next batch 2 cycles later)
            dut.clock.step()
            for (p <- 0 until NumPipes) pPipeOff(dut, p)
            // b2: gather uopIdx 3 (pipe0), rvs3 uopIdx 4 (pipe1), gather uopIdx 1,2 (pipes 2,3)
            pGatherEi16(dut, 0, gIdx3lo, gIdx3hi, gTab3, lmulFrac = 32, uopIdx = 3, blockBase = 0, placeIdx = 0, vl = 64, robIdx = 0x126)
            pRvs3(dut, 1, rOld4, lmulFrac = 32, uopIdx = 4, blockBase = 0, placeIdx = 0, vl = 64, robIdx = 0x126, pdest = 0x06)
            pGatherEi16(dut, 2, gIdx1lo, gIdx1hi, gTab1, lmulFrac = 32, uopIdx = 1, blockBase = 0, placeIdx = 1, vl = 64, robIdx = 0x126)
            pGatherEi16(dut, 3, gIdx2lo, gIdx2hi, gTab2, lmulFrac = 32, uopIdx = 2, blockBase = 0, placeIdx = 2, vl = 64, robIdx = 0x126)
            dut.clock.step()
            for (p <- 0 until NumPipes) pPipeOff(dut, p)
            // gap
            dut.clock.step()
            for (p <- 0 until NumPipes) pPipeOff(dut, p)
            // b4: rvs3 uopIdx 7 (pipe0), gather uopIdx 0 (pipe1). placeIdx:
            // gather is the 0th non-rvs3 uop of this cycle -> 0.
            pRvs3(dut, 0, rOld7, lmulFrac = 32, uopIdx = 7, blockBase = 0, placeIdx = 0, vl = 64, robIdx = 0x126, pdest = 0x77)
            pGatherEi16(dut, 1, gIdx0lo, gIdx0hi, gTab0, lmulFrac = 32, uopIdx = 0, blockBase = 0, placeIdx = 0, vl = 64, robIdx = 0x126)
            dut.clock.step()
            for (p <- 0 until NumPipes) pPipeOff(dut, p)
            // drain: any writeback must not fire while indexDone is unresolved
            val wbData = Array.fill(NumPipes)(Option.empty[BigInt])
            var sawActive = false
            var drained = false
            for (cy <- 0 until 40; if !drained) {
              for (p <- 0 until NumPipes) {
                if (dut.out.pipes(p).valid.peekBoolean() && wbData(p).isEmpty) {
                  wbData(p) = Some(dut.out.pipes(p).bits.data.peekValue().asBigInt)
                }
              }
              val anyActive = (0 until NumUnits).exists(u => dut.out.unitStatus(u).hasIndex.peekBoolean())
              if (anyActive) sawActive = true
              drained = sawActive && (0 until NumUnits).forall(u => !dut.out.unitStatus(u).hasIndex.peekBoolean())
              dut.clock.step()
            }
            for (u <- 0 until NumUnits) {
              dut.out.unitStatus(u).hasIndex.expect(false.B, s"unit $u hasIndex drained")
              dut.out.unitStatus(u).hasTable.expect(false.B, s"unit $u hasTable drained")
            }
            (0 until NumPipes).foreach { p =>
              wbData(p).getOrElse(fail(s"pipe $p never wrote back (deadlock)"))
            }
          }
        }
        // vrgatherei16.vv v8, v24, v16, e32/m8/tu/mu, vl=15 (robIdx=0x116, pc
        // 0x80000404). The emu (emu_logs/20260828-174224 run-vrgatherei16_vv-1.log)
        // difftest-fails on this instruction: "v9_high different ... right =
        // 0x0000000000000000, wrong = 0x00000000fffffff8". uop1's element 6 uses
        // e16 index 0xfff8 (must be out-of-range -> 0), but the e32ei16 OOR flag
        // is misaligned so 0xfff8 escapes and reads table[24] = 0xfffffff8.
        // Dispatch (from FST build/2026-08-28-18-04-28.fst), 8 gather uops (no
        // rvs3), blockSize 8 (m8): cy0 uop0(p1,plc0) uop1(p2,plc1); cy1
        // uop2(p3,plc1) uop3(p1,plc0); cy2 uop4(p2,plc1) uop5(p0,plc0); gap;
        // cy6 uop7(p1,plc0); cy7 uop6(p3,plc0). vs1 = index slice (v16+uopIdx/2),
        // vs2 = table chunk (v24+uopIdx), vs3 = old vd. Golden = nemu REF regs:
        // v8=[0,0,2,0], v9/v10=0, v11=[0,0,0,0xf50471f1], v12-v15 = old vd (tail).
        it("E12: emu replay e32 m8 vl=15 vrgatherei16 OOR (v9_high=0xfffffff8)") {
          simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
            initDut(dut)
            for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
            // Index register slices (v16..v19), each 8 e16 indices = 16 bytes.
            val vs1_01 = Seq(0x00,0x00,0x00,0x00,0x01,0x00,0x00,0x00,0x03,0x00,0x00,0x00,0xf8,0xff,0xff,0xff) // v16
            val vs1_23 = Seq(0x80,0xff,0xff,0xff,0xff,0x7f,0x00,0x00,0xff,0x7f,0x00,0x00,0xff,0x7f,0x00,0x00) // v17
            val vs1_45 = Seq(0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x00,0x00,0x00,0x00,0x01,0x00,0x00,0x00) // v18
            val vs1_67 = Seq(0x03,0x00,0x00,0x00,0xf8,0xff,0xff,0xff,0x80,0xff,0xff,0xff,0xff,0x7f,0x00,0x00) // v19
            // Table chunks v24..v31 (4 e32 each).
            val tab0 = Seq(0x00,0x00,0x00,0x00,0x02,0x00,0x00,0x00,0x0f,0x00,0x00,0x00,0x00,0x00,0x00,0x00) // v24
            val tab1 = Seq(0xf8,0xff,0xff,0xff,0x00,0x00,0x00,0x00,0xff,0x07,0x00,0x00,0x01,0x00,0x00,0x00) // v25
            val tab2 = Seq(0x00,0x00,0x00,0x00,0xff,0xff,0xff,0xff,0x00,0x00,0x00,0x00,0x02,0x00,0x00,0x00) // v26
            val tab3 = Seq(0x0f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf8,0xff,0xff,0xff,0x00,0x00,0x00,0x00) // v27
            val tab4 = Seq(0xff,0x07,0x00,0x00,0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xff,0xff,0xff,0xff) // v28
            val tab5 = Seq(0x00,0x00,0x00,0x00,0x02,0x00,0x00,0x00,0x0f,0x00,0x00,0x00,0x00,0x00,0x00,0x00) // v29
            val tab6 = Seq(0xf8,0xff,0xff,0xff,0x00,0x00,0x00,0x00,0xff,0x07,0x00,0x00,0x01,0x00,0x00,0x00) // v30
            val tab7 = Seq(0x00,0x00,0x00,0x00,0xff,0xff,0xff,0xff,0x00,0x00,0x00,0x00,0x02,0x00,0x00,0x00) // v31
            // Old vd (vs3) captured at the 0x116 entries.
            val oldV8  = Seq(0xbc,0xbe,0xb8,0xdd,0xcb,0x4c,0xd6,0x3a,0x31,0x62,0xff,0xdd,0xfa,0xcb,0xdc,0xf3)
            val oldV9  = Seq(0x1d,0xdc,0xb8,0x4c,0x12,0xfa,0x40,0xa3,0xc6,0xc5,0x8f,0x22,0x1e,0xa5,0x33,0x81)
            val oldV10 = Seq(0x3d,0xa9,0x66,0xb8,0x50,0x51,0x11,0x7d,0x17,0x3b,0xcd,0x47,0x09,0xd4,0xc0,0xe3)
            val oldV11 = Seq(0x24,0x31,0x7d,0x28,0xaf,0xd9,0xce,0xca,0xd0,0x5e,0xe9,0xef,0xf1,0x71,0x04,0xf5)
            val oldV12 = Seq(0xf3,0x55,0x40,0xcc,0xe5,0x35,0x6c,0x76,0x2d,0x41,0xac,0x4d,0xfa,0xe5,0x6d,0xdc)
            val oldV13 = Seq(0x34,0xf5,0xce,0x9c,0x30,0x55,0x87,0xea,0x20,0x24,0xf6,0xd8,0x95,0x9e,0xfb,0xb7)
            val oldV14 = Seq(0x72,0x02,0xea,0x4a,0x00,0xd1,0xfb,0xb0,0x92,0x03,0x40,0xe4,0x57,0x5c,0x60,0xd9)
            val oldV15 = Seq(0x7c,0x08,0x8e,0x7f,0x2c,0xc1,0xda,0xe7,0x9c,0x61,0x32,0x03,0xc2,0x24,0xb4,0x2c)
            // Golden per pdest (nemu REF). v12-v15 are all-tail (tu) -> old vd.
            val golden = Map(
              0x1b -> foldBytes(Seq(0,0,0,0, 0,0,0,0, 2,0,0,0, 0,0,0,0)),           // v8  [0,0,2,0]
              0x1c -> BigInt(0),                                                     // v9
              0x1d -> BigInt(0),                                                     // v10
              0x1e -> foldBytes(Seq(0,0,0,0, 0,0,0,0, 0,0,0,0, 0xf1,0x71,0x04,0xf5)), // v11 tail elem3
              0x1f -> foldBytes(oldV12),                                             // v12 tail
              0x2f -> foldBytes(oldV13),                                             // v13 tail
              0x37 -> foldBytes(oldV14),                                             // v14 tail
              0x38 -> foldBytes(oldV15)                                              // v15 tail
            )
            // cy0: uop0 (pipe1), uop1 (pipe2)
            pGatherEi16Sew(dut, 1, vs1_01, tab0, oldV8,  sew = 32, lmulFrac = 64, uopIdx = 0, blockBase = 0, placeIdx = 0, vl = 15, robIdx = 0x116, pdest = 0x1b)
            pGatherEi16Sew(dut, 2, vs1_01, tab1, oldV9,  sew = 32, lmulFrac = 64, uopIdx = 1, blockBase = 0, placeIdx = 1, vl = 15, robIdx = 0x116, pdest = 0x1c)
            dut.clock.step()
            for (p <- 0 until NumPipes) pPipeOff(dut, p)
            // cy1: uop2 (pipe3), uop3 (pipe1)
            pGatherEi16Sew(dut, 3, vs1_23, tab2, oldV10, sew = 32, lmulFrac = 64, uopIdx = 2, blockBase = 0, placeIdx = 1, vl = 15, robIdx = 0x116, pdest = 0x1d)
            pGatherEi16Sew(dut, 1, vs1_23, tab3, oldV11, sew = 32, lmulFrac = 64, uopIdx = 3, blockBase = 0, placeIdx = 0, vl = 15, robIdx = 0x116, pdest = 0x1e)
            dut.clock.step()
            for (p <- 0 until NumPipes) pPipeOff(dut, p)
            // cy2: uop4 (pipe2), uop5 (pipe0)
            pGatherEi16Sew(dut, 2, vs1_45, tab4, oldV12, sew = 32, lmulFrac = 64, uopIdx = 4, blockBase = 0, placeIdx = 1, vl = 15, robIdx = 0x116, pdest = 0x1f)
            pGatherEi16Sew(dut, 0, vs1_45, tab5, oldV13, sew = 32, lmulFrac = 64, uopIdx = 5, blockBase = 0, placeIdx = 0, vl = 15, robIdx = 0x116, pdest = 0x2f)
            dut.clock.step()
            for (p <- 0 until NumPipes) pPipeOff(dut, p)
            // gap (emu: next uops 4 cycles later)
            for (_ <- 0 until 3) { dut.clock.step(); for (p <- 0 until NumPipes) pPipeOff(dut, p) }
            // cy6: uop7 (pipe1)
            pGatherEi16Sew(dut, 1, vs1_67, tab7, oldV15, sew = 32, lmulFrac = 64, uopIdx = 7, blockBase = 0, placeIdx = 0, vl = 15, robIdx = 0x116, pdest = 0x38)
            dut.clock.step()
            for (p <- 0 until NumPipes) pPipeOff(dut, p)
            // cy7: uop6 (pipe3)
            pGatherEi16Sew(dut, 3, vs1_67, tab6, oldV14, sew = 32, lmulFrac = 64, uopIdx = 6, blockBase = 0, placeIdx = 0, vl = 15, robIdx = 0x116, pdest = 0x37)
            dut.clock.step()
            for (p <- 0 until NumPipes) pPipeOff(dut, p)
            // collect writebacks keyed by pdest (uops share pipes across cycles)
            val wb = scala.collection.mutable.Map.empty[Int, BigInt]
            for (_ <- 0 until 40; if wb.size < golden.size) {
              for (p <- 0 until NumPipes) {
                if (dut.out.pipes(p).valid.peekBoolean()) {
                  val pd = dut.out.pipes(p).bits.meta.pdest.peekValue().asBigInt.toInt
                  if (!wb.contains(pd)) wb(pd) = dut.out.pipes(p).bits.data.peekValue().asBigInt
                }
              }
              dut.clock.step()
            }
            golden.foreach { case (pd, g) =>
              val got = wb.get(pd)
              println(f"[E12-diag] pdest 0x$pd%02x wb = 0x${got.getOrElse(BigInt(-1))}%032x  golden = 0x$g%032x  ${if (got.contains(g)) "OK" else "MISMATCH"}")
            }
            golden.foreach { case (pd, g) =>
              wb.get(pd).getOrElse(fail(f"pdest 0x$pd%02x never wrote back")) mustBe g
            }
          }
        }
      }

      describe("MASK") {
        it("E4: m1 masked-off keeps old vd") {
          simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
            initDut(dut)
            val table = Table.inc(1)
            val idxLo = e16idx((0 until 8).toList)
            val idxHi = e16idx((8 until 16).toList)
            val oldVd = (0 until 16).map(i => 0xa0 + i)
            val mask = 0x00ff // bytes 0-7 active, 8-15 masked-off
            val golden = ei16e8Golden(foldBytes(idxLo), foldBytes(idxHi), 8, table, oldVd, 0, 16, false, false, mask)
            pGatherEi16(dut, 0, idxLo, idxHi, table, lmulFrac = 8, uopIdx = 0, blockBase = 0, placeIdx = 0, vl = 16, mask = mask, pdest = 0x22)
            pRvs3(dut, 1, oldVd, lmulFrac = 8, uopIdx = 1, blockBase = 0, placeIdx = 0, vl = 16, mask = mask, pdest = 0x22)
            dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
            var cy = 0
            while (cy < 12 && !dut.out.pipes(0).valid.peekBoolean()) { dut.clock.step(); cy += 1 }
            dut.out.pipes(0).valid.expect(true.B)
            dut.out.pipes(0).bits.data.expect(golden, "active gathered, masked-off = old vd")
          }
        }
        it("E5: m2 tail agnostic (vta)") {
          simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
            initDut(dut)
            for (p <- 0 until NumPipes) dut.in.wbReady(p).poke(true)
            val table = Table.inc(2)
            val oldVd = Seq((0 until 16).map(i => 0xa0 + i), (0 until 16).map(i => 0xb0 + i))
            val idxLo = Seq(e16idx((0 until 8).toList), e16idx((16 until 24).toList))
            val idxHi = Seq(e16idx((8 until 16).toList), e16idx((24 until 32).toList))
            // vl=20: uop1 bytes 16-19 active, 20-31 tail -> vta agnostic (0xff)
            val g0 = ei16e8Golden(foldBytes(idxLo(0)), foldBytes(idxHi(0)), 16, table, oldVd(0), 0, 20, true, false, -1)
            val g1 = ei16e8Golden(foldBytes(idxLo(1)), foldBytes(idxHi(1)), 16, table, oldVd(1), 1, 20, true, false, -1)
            pGatherEi16(dut, 0, idxLo(0), idxHi(0), table.slice(0, 16), lmulFrac = 16, uopIdx = 0, blockBase = 0, placeIdx = 0, vl = 20, vta = true, pdest = 0)
            pRvs3(dut, 1, oldVd(0), lmulFrac = 16, uopIdx = 2, blockBase = 0, placeIdx = 0, vl = 20, vta = true, pdest = 0)
            pGatherEi16(dut, 2, idxLo(1), idxHi(1), table.slice(16, 32), lmulFrac = 16, uopIdx = 1, blockBase = 0, placeIdx = 1, vl = 20, vta = true, pdest = 1)
            pRvs3(dut, 3, oldVd(1), lmulFrac = 16, uopIdx = 3, blockBase = 0, placeIdx = 1, vl = 20, vta = true, pdest = 1)
            dut.clock.step(); for (p <- 0 until 4) pPipeOff(dut, p)
            val wb = collectWb(dut, 20)
            wb(0).getOrElse(fail("pair0 never wrote back")) mustBe g0
            wb(1).getOrElse(fail("pair1 never wrote back")) mustBe g1
          }
        }
      }
    }
  }

  describe("FST vrgather.vi flush regression") {
    // build/2026-10-05-22-43-05_2256389.fst: e8/m8, vl=1, vta=vma=1.
    // Each row is one cycle in pipe order: (uopIdx, blockBase, placeIdx, pdest, robIdx).
    val initialBatches = Seq(
      Seq((3, 0, 0, 0x10, 0x0c), (0, 0, 1, 0x41, 0x0c),
          (1, 0, 2, 0x1a, 0x0c), (2, 0, 3, 0x78, 0x0c)),
      Seq((7, 0, 0, 0x03, 0x0c), (4, 0, 1, 0x75, 0x0c),
          (5, 0, 2, 0x1e, 0x0c), (6, 0, 3, 0x4a, 0x0c)),
      Seq((3, 8, 0, 0x79, 0x0d), (2, 8, 1, 0x05, 0x0d),
          (0, 8, 2, 0x55, 0x0d), (1, 8, 3, 0x37, 0x0d)),
      Seq((7, 8, 0, 0x6c, 0x0d), (6, 8, 1, 0x56, 0x0d),
          (4, 8, 2, 0x4f, 0x0d), (5, 8, 3, 0x60, 0x0d)),
    )
    val retryBatches = Seq(
      initialBatches.head,
      Seq((2, 8, 0, 0x05, 0x0d), (4, 0, 0, 0x75, 0x0c),
          (5, 0, 1, 0x1e, 0x0c), (6, 0, 2, 0x4a, 0x0c)),
      Seq((6, 8, 0, 0x56, 0x0d), (7, 0, 0, 0x03, 0x0c),
          (0, 8, 1, 0x55, 0x0d), (1, 8, 2, 0x37, 0x0d)),
      Seq((7, 8, 0, 0x6c, 0x0d), (3, 8, 1, 0x79, 0x0d),
          (4, 8, 2, 0x4f, 0x0d), (5, 8, 3, 0x60, 0x0d)),
    )

    def driveBatch(dut: PermUnit[FakePermMeta], batch: Seq[(Int, Int, Int, Int, Int)]): Unit = {
      batch.zipWithIndex.foreach { case ((uopIdx, blockBase, placeIdx, pdest, robIdx), pipe) =>
        pDefaults(dut, pipe)
        pGather(dut, pipe, sew = 8, ei = 8, vs1 = BigInt("03" * 16, 16),
          vs2 = Seq.fill(numElem)(0xff), vs3 = Seq.fill(numElem)(3),
          lmulFrac = 64, uopIdx = uopIdx, blockBase = blockBase, placeIdx = placeIdx,
          vl = 1, vta = true, vma = true, robIdx = robIdx, pdest = pdest)
        dut.in.pipes(pipe).bits.rs1.poke(BigInt("03" * 8, 16).U)
      }
      dut.clock.step()
    }

    def replayThroughFlush(dut: PermUnit[FakePermMeta]): Unit = {
      initDut(dut)
      dut.in.flush.s1.foreach(_.poke(false))
      // 2276574-2276577ps: two m8 groups; the final beat is still in s1.
      initialBatches.foreach(driveBatch(dut, _))
      dut.in.pipes.foreach(_.valid.poke(false))
      for (unit <- 0 until NumUnits) {
        dut.out.unitStatus(unit).hasIndex.expect((unit < 12).B,
          s"2276578ps: index placement differs for unit $unit")
      }
      for (unit <- Seq(14, 15)) {
        dut.out.unitStatus(unit).hasTable.expect(true.B,
          s"2276578ps: unit $unit must hold a table before flush")
        dut.out.unitStatus(unit).counter.expect(0.U)
      }

      // 2276578ps: s1=0xf, unit=0x0fff. The parent derives unit flush
      // from index-owned meta, so table-only units 12-15 receive no flush.
      dut.in.flush.s1.foreach(_.poke(true))
      for (unit <- 0 until NumUnits) dut.in.unitFlush(unit).poke((unit < 12).B)
      dut.clock.step()
      dut.in.flush.s1.foreach(_.poke(false))
      dut.in.unitFlush.foreach(_.poke(false))
      dut.out.unitStatus.foreach(_.hasIndex.expect(false.B))
    }

    it("clears unit14 and unit15 tables after the 2276578ps flush") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        replayThroughFlush(dut)
        val afterFlush = Seq(14, 15).map(dut.out.unitStatus(_).hasTable.peekBoolean())
        // 2276579-2276606ps: no new inputs, but the FST keeps both tables.
        dut.clock.step(28)
        val afterIdle = Seq(14, 15).map(dut.out.unitStatus(_).hasTable.peekBoolean())
        withClue(s"unit14/unit15 hasTable immediately after flush=$afterFlush, after 28 idle cycles=$afterIdle: ") {
          (afterFlush, afterIdle) mustBe (Seq(false, false), Seq(false, false))
        }
      }
    }

    it("replays the 2276607ps retry after flush without table placement-flow conflicts") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        replayThroughFlush(dut)
        dut.clock.step(28)
        retryBatches.foreach(driveBatch(dut, _))
        dut.in.pipes.foreach(_.valid.poke(false))
        val expected = retryBatches.flatten.map(_._4).toSet
        val seenWake = scala.collection.mutable.Set.empty[Int]
        val seenWb = scala.collection.mutable.Set.empty[Int]
        // This includes the 2276613ps-equivalent placeTable+flowTable failure.
        for (_ <- 0 until 24) {
          for (pipe <- 0 until NumPipes) {
            val wake = dut.out.wake(pipe)
            if (wake.valid.peekBoolean()) {
              val pdest = wake.bits.meta.pdest.peekValue().asBigInt.toInt
              assert(expected(pdest) && seenWake.add(pdest), s"duplicate/unexpected wake pdest=$pdest")
              wake.bits.delay.expect(0.U)
            }
            val wb = dut.out.pipes(pipe)
            if (wb.valid.peekBoolean()) {
              val pdest = wb.bits.meta.pdest.peekValue().asBigInt.toInt
              assert(expected(pdest) && seenWb.add(pdest), s"duplicate/unexpected writeback pdest=$pdest")
              wb.bits.data.expect(((BigInt(1) << 128) - 1).U)
            }
          }
          dut.clock.step()
        }
        seenWake.toSet mustBe expected
        seenWb.toSet mustBe expected
      }
    }

    it("does not flush table-only residents of the adjacent live m8 block") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        dut.in.flush.s1.foreach(_.poke(false))
        driveBatch(dut, initialBatches.head)
        driveBatch(dut, initialBatches(2))
        dut.in.pipes.foreach(_.valid.poke(false))
        dut.clock.step()
        for (unit <- Seq(7, 15)) {
          dut.out.unitStatus(unit).hasTable.expect(true.B)
          dut.out.unitStatus(unit).hasIndex.expect(false.B)
        }

        for (unit <- 0 until 4) dut.in.unitFlush(unit).poke(true)
        dut.clock.step()
        dut.in.unitFlush.foreach(_.poke(false))
        for (unit <- 0 until 8) {
          dut.out.unitStatus(unit).hasTable.expect(false.B)
          dut.out.unitStatus(unit).hasIndex.expect(false.B)
        }
        dut.out.unitStatus(15).hasTable.expect(true.B, "neighboring block must retain its table")
        for (unit <- 8 until 12) dut.out.unitStatus(unit).hasIndex.expect(true.B)
      }
    }
  }

  describe("FST vslideup.vi wake regression") {
    it("replays 85293-85296ps and wakes all eight m4 uops") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        dut.in.wakeAck.foreach(_.poke(false))
        dut.in.wbReady.foreach(_.poke(true))

        // build/2026-10-03-07-58-19_78369.fst: two interleaved m4 blocks.
        // (pipe, uopIdx, blockBase, placeIdx, pdest, robIdx)
        val batches = Seq(
          Seq((0, 1, 12, 0, 0x4f, 0x46), (3, 0, 12, 1, 0x0e, 0x46)),
          Seq((0, 2, 12, 0, 0x74, 0x46), (1, 3, 0, 0, 0x71, 0x47),
              (2, 1, 0, 1, 0x19, 0x47), (3, 0, 0, 2, 0x4e, 0x47)),
          Seq((0, 3, 12, 0, 0x46, 0x46), (1, 2, 0, 0, 0x67, 0x47))
        )
        val expected = batches.flatten.map(_._5).toSet
        val allOnes = Seq.fill(numElem)(0xff)
        val repeatedThree = BigInt("03" * 8, 16)
        val seenWake = scala.collection.mutable.Set.empty[Int]
        val seenWb = scala.collection.mutable.Set.empty[Int]
        val wakeCycleAndDelay = scala.collection.mutable.Map.empty[Int, (Int, Int)]
        val wbCycle = scala.collection.mutable.Map.empty[Int, Int]
        var cycle = 0

        def stepAndObserve(): Unit = {
          for (pipe <- 0 until NumPipes) {
            val wake = dut.out.wake(pipe)
            if (wake.valid.peekBoolean()) {
              val pdest = wake.bits.meta.pdest.peekValue().asBigInt.toInt
              assert(expected(pdest) && seenWake.add(pdest), f"duplicate/unexpected wake pdest=0x$pdest%02x")
              wake.bits.delay.expect(0.U)
              wakeCycleAndDelay(pdest) = (cycle, 0)
              dut.in.wakeAck(pipe).poke(true)
            }
          }
          for (pipe <- 0 until NumPipes) {
            val wb = dut.out.pipes(pipe)
            if (wb.valid.peekBoolean()) {
              val pdest = wb.bits.meta.pdest.peekValue().asBigInt.toInt
              assert(expected(pdest) && seenWb.add(pdest), f"duplicate/unexpected wb pdest=0x$pdest%02x")
              wb.bits.data.expect(foldBytes(allOnes).U)
              wbCycle(pdest) = cycle
            }
          }
          dut.clock.step()
          dut.in.wakeAck.foreach(_.poke(false))
          cycle += 1
        }

        for (batch <- batches) {
          batch.foreach { case (pipe, uopIdx, blockBase, placeIdx, pdest, robIdx) =>
            pSlideUp(dut, pipe, offset = 3, vs2 = allOnes, vs3 = allOnes,
              sew = 8, lmulFrac = 32, uopIdx = uopIdx, blockBase = blockBase,
              placeIdx = placeIdx, vl = 64, vta = true, robIdx = robIdx, pdest = pdest)
            dut.in.pipes(pipe).bits.rs1.poke(repeatedThree.U)
            dut.in.pipes(pipe).bits.vs1.poke(foldBytes(Seq.fill(numElem)(3)).U)
            dut.in.pipes(pipe).bits.vma.poke(true)
          }
          stepAndObserve()
          dut.in.pipes.foreach(_.valid.poke(false))
        }

        // 85296ps has no valid inputs; 85298-85302ps contains the missing wake.
        for (_ <- 0 until 24) stepAndObserve()
        withClue(s"wakes missing ${expected -- seenWake}, observed $seenWake: ") {
          seenWake.toSet mustBe expected
        }
        seenWb.toSet mustBe expected
        expected.foreach { pdest =>
          val (wakeCycle, delay) = wakeCycleAndDelay(pdest)
          withClue(f"pdest 0x$pdest%02x wake delay=$delay: ") {
            wbCycle(pdest) - wakeCycle mustBe issue2exu - delay
          }
        }
      }
    }
  }

  describe("single-unit fixed writeback timing") {
    it("replays vcompress oldVD on the fixed boundary without wake ack") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        dut.in.wakeAck.foreach(_.poke(false))
        val oldVD = BigInt("8344c0aa5b76129530ac75dc32808aa4", 16)
        val oldBytes = (0 until numElem).map(i => ((oldVD >> (8 * i)) & 0xff).toInt)
        val table = Seq(0x00, 0x02, 0x0f, 0x00, 0x08, 0x00, 0x07, 0x01,
          0x00, 0xff, 0x00, 0x02, 0x0f, 0x00, 0x08, 0x00)
        for (lmulFrac <- Seq(1, 2, 4, 8)) {
          pCompress(dut, 1, 0x100, table, oldBytes, lmulFrac = lmulFrac,
            vl = 2, robIdx = 0x6e, pdest = 0x52)
          dut.clock.step()
          pPipeOff(dut, 1)
          dut.out.pipes.foreach(_.valid.expect(false.B))
          dut.out.wake.foreach(_.valid.expect(false.B))
          dut.clock.step()
          dut.out.pipes(0).valid.expect(true.B, "fixed promise requires data now, not three cycles later")
          dut.out.pipes(0).bits.data.expect(oldVD)
          dut.out.pipes(0).bits.meta.pdest.expect(0x52.U)
          dut.out.pipes(0).bits.meta.robIdx.expect(0x6e.U)
          dut.out.wake.foreach(_.valid.expect(false.B))
          dut.out.release.expect(1.U, "fixed WB fire releases the allocated unit")
          dut.clock.step()
          dut.out.pipes.foreach(_.valid.expect(false.B))
          dut.out.wake.foreach(_.valid.expect(false.B))
          dut.out.release.expect(0.U)
          dut.out.unitStatus(0).hasIndex.expect(false.B)
          dut.out.unitStatus(0).hasTable.expect(false.B)
        }
      }
    }

    it("holds fixed data under backpressure and releases once on WB fire") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        dut.in.wakeAck.foreach(_.poke(false))
        dut.in.wbReady(0).poke(false)
        val table = Table.inc()
        val oldVD = Prefill.default()
        val golden = foldBytes(oldVD)
        pCompress(dut, 1, 0, table, oldVD, lmulFrac = 1, vl = 2, pdest = 0x52)
        dut.clock.step()
        pPipeOff(dut, 1)
        dut.clock.step()
        for (_ <- 0 until issue2exu + 2) {
          dut.out.pipes(0).valid.expect(true.B)
          dut.out.pipes(0).bits.data.expect(golden)
          dut.out.pipes(0).bits.meta.pdest.expect(0x52.U)
          dut.out.wake.foreach(_.valid.expect(false.B))
          dut.out.release.expect(0.U)
          dut.out.unitStatus(0).hasIndex.expect(true.B)
          dut.clock.step()
        }
        dut.in.wbReady(0).poke(true)
        dut.out.release.expect(1.U)
        dut.clock.step()
        dut.out.pipes.foreach(_.valid.expect(false.B))
        dut.out.release.expect(0.U)
        dut.out.unitStatus(0).hasIndex.expect(false.B)
        dut.out.unitStatus(0).hasTable.expect(false.B)
      }
    }
  }

  describe("wake-ack writeback reservation") {
    it("does not write back without ack and writes exactly three cycles after ack") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        dut.in.wakeAck.foreach(_.poke(false))
        dut.in.wbReady.foreach(_.poke(true))
        val table = Table.inc(2)
        val old = Prefill.default(2)
        pGather(dut, 0, 8, 8, 0, table.take(16), old.take(16), lmulFrac = 16, pdest = 0x55)
        pGather(dut, 1, 8, 8, 0, table.drop(16), old.drop(16),
          lmulFrac = 16, uopIdx = 1, placeIdx = 1)
        dut.clock.step()
        pPipeOff(dut, 0)
        pPipeOff(dut, 1)

        for (_ <- 0 until 12) {
          dut.out.pipes.foreach(_.valid.expect(false.B))
          dut.out.release.expect(0.U)
          dut.clock.step()
        }
        dut.out.wake(0).valid.expect(true.B)
        dut.out.wake(0).bits.delay.expect(0.U)
        dut.in.wakeAck(0).poke(true)
        dut.out.release.expect(1.U, "accepted wake releases its entry")
        dut.out.pipes(0).valid.expect(false.B)
        dut.clock.step()
        dut.in.wakeAck(0).poke(false)
        for (_ <- 0 until issue2exu - 1) {
          dut.out.pipes(0).valid.expect(false.B)
          dut.out.release.expect(0.U)
          dut.clock.step()
        }
        dut.out.pipes(0).valid.expect(true.B)
        dut.out.pipes(0).bits.meta.pdest.expect(0x55.U)
        dut.out.release.expect(0.U)
        dut.clock.step()
        dut.out.pipes(0).valid.expect(false.B)
        dut.out.release.expect(0.U)
      }
    }

    it("cancels a reserved writeback when its unit flushes") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        dut.in.wakeAck.foreach(_.poke(false))
        dut.in.wbReady.foreach(_.poke(true))
        val table = Table.inc(2)
        val old = Prefill.default(2)
        pGather(dut, 0, 8, 8, 0, table.take(16), old.take(16), lmulFrac = 16)
        pGather(dut, 1, 8, 8, 0, table.drop(16), old.drop(16),
          lmulFrac = 16, uopIdx = 1, placeIdx = 1)
        dut.clock.step()
        pPipeOff(dut, 0)
        pPipeOff(dut, 1)
        for (_ <- 0 until 12 if !dut.out.wake(0).valid.peekBoolean()) dut.clock.step()
        dut.out.wake(0).valid.expect(true.B)
        dut.in.wakeAck(0).poke(true)
        dut.out.release.expect(1.U)
        dut.clock.step()
        dut.in.wakeAck(0).poke(false)
        dut.in.unitFlush(0).poke(true)
        dut.clock.step()
        dut.in.unitFlush(0).poke(false)
        for (_ <- 0 until issue2exu + 2) {
          dut.out.pipes.foreach(_.valid.expect(false.B))
          dut.out.release.expect(0.U)
          dut.clock.step()
        }
      }
    }
  }
}
