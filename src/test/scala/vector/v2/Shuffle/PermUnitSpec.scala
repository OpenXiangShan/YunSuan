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
    */
  def compressGolden(mask: BigInt, vs2Full: Seq[Int], vs3: Seq[Int], uopIdx: Int,
    vl: Int, vta: Boolean = false): BigInt = {
    val total = mask.bitCount
    val packed = Array.ofDim[Int](total)
    var p = 0
    for (g <- vs2Full.indices) {
      if (((mask >> g) & 1) == 1) { packed(p) = vs2Full(g); p += 1 }
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
    }
  }
}
