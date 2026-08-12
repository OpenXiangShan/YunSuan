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
}
