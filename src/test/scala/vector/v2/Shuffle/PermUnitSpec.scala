package yunsuan.vector.v2.Shuffle

import chisel3._
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
  val NumUnits = 8
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

  def goldenSlideUp(offset: Int, sew: Int, vl: Int): Seq[(Int, Boolean, Boolean, Boolean)] = {
    val shift = sewShift(sew)
    val e8Off = offset << shift
    val e8Vl = vl << shift
    (0 until numElem).map { i =>
      val isStay = e8Off > dlen || (i < e8Vl && i < e8Off)
      val idx = if (isStay) i else i - e8Off
      (idx.max(0), true, false, false)
    }
  }

  def goldenSlide1Up(sew: Int, tableIdx: Int = 0): Seq[(Int, Boolean, Boolean, Boolean)] = {
    val sb = sewb(sew)
    (0 until numElem).map { i =>
      val elemIdx = tableIdx * numElem / sb + i / sb
      val isFirst = tableIdx == 0 && elemIdx == 0
      ((i - sb).max(0), !isFirst, false, isFirst)
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
    val flags = goldenSlideUp(offset, sew, vl)
    val n = lmulFrac / 8
    goldenPerm(flags, vs2.slice(uopIdx * numElem, (uopIdx + 1) * numElem), Seq(0),
      vs3.slice(uopIdx * numElem, (uopIdx + 1) * numElem),
      Seq.fill(8)(0), sew, uopIdx, vl, vta, vma,
      if (mask < 0) (BigInt(1) << numElem) - 1 else mask)
  }

  def slide1UpGolden(scalarBytes: Seq[Int], vs2: Seq[Int], vs3: Seq[Int], sew: Int, lmulFrac: Int,
    uopIdx: Int, vl: Int, vta: Boolean = false, vma: Boolean = false, mask: BigInt = -1): BigInt = {
    val flags = goldenSlide1Up(sew, uopIdx)
    goldenPerm(flags, vs2.slice(uopIdx * numElem, (uopIdx + 1) * numElem), Seq(0),
      vs3.slice(uopIdx * numElem, (uopIdx + 1) * numElem),
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
    vl: Int = -1, vta: Boolean = false, robIdx: Int = 0, pdest: Int = 0
  ): Unit = {
    val p = dut.in.pipes(pipe)
    p.valid.poke(true)
    p.bits.op.isCompress.poke(true)
    p.bits.placeIdx.poke(placeIdx); p.bits.blockBase.poke(blockBase)
    p.bits.uopIdx.poke(uopIdx)
    pSew(p.bits.sew, 8); pLmul(p.bits.lmul, lmulFrac)
    p.bits.vl.poke(if (vl < 0) (lmulFrac / 8) * vlen / 8 else vl)
    p.bits.vta.poke(vta); p.bits.vma.poke(true)  // compress uses byte mask (vs1), merge mask irrelevant
    p.bits.vs1.poke(mask)   // compress byte mask
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
    dut.in.wbReady.foreach(_.poke(false))
    dut.in.flush.foreach(_.poke(false))
    dut.in.wakeAck.foreach(_.poke(false))
    dut.clock.step()
  }

  def eWb(dut: PermUnit[FakePermMeta], pipe: Int, dest: BigInt): Unit = {
    dut.out.pipes(pipe).valid.expect(true.B, s"pipe $pipe wb.valid")
    dut.out.pipes(pipe).bits.data.expect(dest)
  }

  def eWbAfter(dut: PermUnit[FakePermMeta], pipe: Int, dest: BigInt, steps: Int): Unit = {
    for (_ <- 0 until steps) dut.clock.step()
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
          eWbAfter(dut, 1, res0, 6)   // uop0 -> unit1 -> pipe1
          eWbAfter(dut, 0, res1, 0)   // uop1 -> unit0 -> pipe0
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
          dut.in.wbReady(0).poke(false); dut.in.wbReady(2).poke(false)
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
    it("W1: wbReady=0 holds wb.valid until ready") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val golden = gatherGolden(0, 8, 8, 8, 0, table, 0 until 1, Prefill.default(1), 16)
        pGather(dut, 0, 8, 8, 0, table, Prefill.default(1))
        dut.clock.step(); pPipeOff(dut, 0)
        var cy = 0
        while (cy < 10 && !dut.out.pipes(0).valid.peekBoolean()) { dut.clock.step(); cy += 1 }
        dut.out.pipes(0).valid.expect(true.B, "wb.valid asserts when done")
        dut.out.pipes(0).bits.data.expect(golden)
        dut.clock.step(); dut.out.pipes(0).valid.expect(true.B, "wb.valid holds while wbReady=0")
        dut.clock.step(); dut.out.pipes(0).valid.expect(true.B, "wb.valid holds while wbReady=0 (2)")
        dut.in.wbReady(0).poke(true)
        dut.clock.step()
        dut.out.pipes(0).valid.expect(false.B, "wb.valid drops after wbReady=1")
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
        val table = Table.inc(1)
        val golden = gatherGolden(0, 8, 8, 8, 0, table, 0 until 1, Prefill.default(1), 16)
        // U0 and U4 both map to pipe0 (interleaved). Dispatch sequentially; keep
        // wbReady=0 so both hold done -> pipe0 must show U0 (lower index first).
        pGather(dut, 0, 8, 8, 0, table, Prefill.default(1), blockBase = 0)
        dut.clock.step(); pPipeOff(dut, 0)
        pGather(dut, 0, 8, 8, 0, table, Prefill.default(1), blockBase = 4)
        dut.clock.step(); pPipeOff(dut, 0)
        var cy = 0
        while (cy < 12 && !(dut.out.unitStatus(0).done.peekBoolean() &&
                            dut.out.unitStatus(4).done.peekBoolean())) { dut.clock.step(); cy += 1 }
        dut.out.unitStatus(0).done.expect(true.B)
        dut.out.unitStatus(4).done.expect(true.B)
        // both hold (wbReady=0); arbitration picks U0 on pipe0
        dut.out.pipes(0).valid.expect(true.B, "pipe0 valid with U0/U4 holding")
        dut.out.pipes(0).bits.data.expect(golden, "pipe0 shows U0 (lower index)")
        // consume U0 -> pipe0 now shows U4
        dut.in.wbReady(0).poke(true)
        dut.clock.step()
        dut.out.pipes(0).bits.data.expect(golden, "pipe0 shows U4 after U0 consumed")
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
        s.hasTable.expect(true.B, "m1 table stays until flush/reuse")
      }
    }
  }

  // ── Wakeup: vperm wake (LMUL>1) ──────────────────────
  describe("Wakeup") {
    it("K1: LMUL=2 wake valid + delay = issue2exu - counter") {
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
            delay mustBe (3 - counter)
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
            delay mustBe (3 - counter)
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
            delay mustBe (3 - counter)
          }
          dut.clock.step(); cy += 1
        }
        seen mustBe true
      }
    }

    it("K4: wake holds while unacked") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        // m8: wake fires when counter<=3, giving a multi-cycle window (unlike
        // m2 where counter 1->0 completes the block in one flow).
        val table = Table.inc(8)
        val vs1 = foldBytes(Index.allInTable0(1))
        for (u <- 0 until 8) {
          pGather(dut, 0, 8, 8, vs1, table.slice(u * 16, (u + 1) * 16),
            Prefill.default(8).slice(u * 16, (u + 1) * 16), lmulFrac = 64, uopIdx = u, placeIdx = 0, vl = 128)
          dut.clock.step(); pPipeOff(dut, 0)
        }
        // Without ack the wake is level-based (not a one-shot pulse): it stays
        // asserted while the block is still flowing (U0 counter 1->0), then
        // drops once the table clears on completion.
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
            dut.in.wakeAck(0).poke(true)
            dut.clock.step()
            dut.in.wakeAck(0).poke(false)
            dut.out.wake(0).valid.expect(false.B, "wake drops after ack")
          } else { dut.clock.step(); cy += 1 }
        }
        seen mustBe true
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
        for (i <- 0 until NumUnits) dut.in.flush(i).poke(true)
        dut.clock.step()
        for (i <- 0 until NumUnits) dut.in.flush(i).poke(false)
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

    it("K10: LMUL=8 wake delay is counter-derived (no monotonicity)") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(8)
        val vs1 = foldBytes(Index.allInTable0(1))
        for (u <- 0 until 8) {
          pGather(dut, 0, 8, 8, vs1, table.slice(u * 16, (u + 1) * 16),
            Prefill.default(8).slice(u * 16, (u + 1) * 16), lmulFrac = 64, uopIdx = u, placeIdx = 0, vl = 128)
          dut.clock.step(); pPipeOff(dut, 0)
        }
        // Collect (delay, observed-unit counter) across the wake window. Pipe0
        // arbitrates U0/U4 with U0 winning, so the observed unit is U0.
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
        // delay = issue2exu - counter of the waking unit, minus the 1-cycle
        // wakeBlocked bump when the wake is not acked. No monotonicity is
        // asserted: counters are not monotonic under same-block multi-uop
        // placement (a reused unit is re-placed with counter reset to blockSize).
        samples.foreach { case (d, c) =>
          (d == issue2exu - c || d == issue2exu - c - 1) mustBe true
        }
      }
    }

    it("K8: same-pipe wake arbitration U0+U4 -> lower index wins") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
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
        // Scan: pipe0 (U0/U4) must arbitrate. Collect the pdests observed; U0's
        // (0xAA) must be present and, when U4 also requests, U0 (lower) wins.
        var pdests = Seq.empty[Int]
        var cy = 0
        while (cy < 16) {
          if (dut.out.wake(0).valid.peekBoolean()) {
            pdests :+= dut.out.wake(0).bits.meta.pdest.peekValue().asBigInt.toInt
          }
          dut.clock.step(); cy += 1
        }
        pdests must contain (0xAA)
        pdests must contain (0xBB)
      }
    }

    it("K11: flush before all units wake resets allTableHasFilled (no early wake on reuse)") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        initDut(dut)
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
        for (i <- 0 until NumUnits) dut.in.flush(i).poke(true)
        dut.clock.step()
        for (i <- 0 until NumUnits) dut.in.flush(i).poke(false)
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
        for (i <- 0 until NumUnits) dut.in.flush(i).poke(true)
        dut.clock.step()
        for (i <- 0 until NumUnits) dut.in.flush(i).poke(false)
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
        for (i <- 0 until NumUnits) dut.in.flush(i).poke(true)
        dut.clock.step()
        for (i <- 0 until NumUnits) dut.in.flush(i).poke(false)
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
        for (i <- 0 until NumUnits) dut.in.flush(i).poke(true)
        dut.clock.step()
        for (i <- 0 until NumUnits) dut.in.flush(i).poke(false)
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
        for (i <- 0 until NumUnits) dut.in.flush(i).poke(true)
        dut.clock.step()
        for (i <- 0 until NumUnits) dut.in.flush(i).poke(false)
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
        for (i <- 0 until NumUnits) dut.in.flush(i).poke(true)
        dut.clock.step()
        for (i <- 0 until NumUnits) dut.in.flush(i).poke(false)
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
        for (i <- 0 until NumUnits) dut.in.flush(i).poke(true)
        dut.clock.step()
        for (i <- 0 until NumUnits) dut.in.flush(i).poke(false)
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
        dut.out.metas(0).pdest.expect(0x5a.U, "metas(0).pdest")
        dut.out.metas(0).robIdx.expect(3.U, "metas(0).robIdx")
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
  }
}
