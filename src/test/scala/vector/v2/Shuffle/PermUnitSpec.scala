package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.vector.v2.Shuffle.GatherCommon.{FakeRobPtr, GatherParam}

class PermUnitSpec extends AnyFunSpec with ChiselSim with Matchers
    with ParallelTestExecution with GatherSpecBase with GatherSpecUtils {

  val NumPipes = 4

  // ── Helpers: poke PermUnit pipes ─────────────────────────────────────

  def pS0Defaults(dut: PermUnit[FakeRobPtr], pipe: Int): Unit = {
    val s0 = dut.in.s0pipes(pipe)
    s0.valid.poke(false)
    s0.bits.placeIdx.poke(0)
    s0.bits.blockBase.poke(0)
    s0.bits.blockSize.poke(1)
    s0.bits.opcode.poke(0)
    s0.bits.uopIdx.poke(0)
    pSew(s0.bits.sew, 8); pSew(s0.bits.ei, 8)
    pLmul(s0.bits.lmul, 8)
    s0.bits.vl.poke(16)
    s0.bits.isGatherEI16.poke(false)
    s0.bits.vs1.poke(0)
    s0.bits.mask.poke(0)
    s0.bits.tableIdx.poke(0)
    s0.bits.offset.poke(0)
    s0.bits.offsetOverflow.poke(false)
    s0.bits.isSlide1Up.poke(false)
    s0.bits.isSlide1Down.poke(false)
    s0.bits.zipNum.poke(0)
    s0.bits.byteMask.poke(0)
    for (i <- 0 until numElem) s0.bits.compressData(i).poke(0)
    s0.bits.vs3.poke(0)
  }

  def pS0Gather(
    dut: PermUnit[FakeRobPtr], pipe: Int,
    sew: Int, ei: Int, vs1: BigInt, uopIdx: Int = 0,
    blockBase: Int = 0, blockSize: Int = 1, placeIdx: Int = 0
  ): Unit = {
    val s0 = dut.in.s0pipes(pipe)
    s0.valid.poke(true)
    s0.bits.opcode.poke(IndexGenOpcode.gather)
    s0.bits.placeIdx.poke(placeIdx)
    s0.bits.blockBase.poke(blockBase)
    s0.bits.blockSize.poke(blockSize)
    s0.bits.uopIdx.poke(uopIdx)
    pSew(s0.bits.sew, sew); pSew(s0.bits.ei, ei)
    pLmul(s0.bits.lmul, 8)
    s0.bits.vs1.poke(vs1)
    s0.bits.mask.poke((BigInt(1) << numElem) - 1)
  }

  def pS0Off(dut: PermUnit[FakeRobPtr], pipe: Int): Unit = {
    dut.in.s0pipes(pipe).valid.poke(false)
  }

  /** Poke s1 pipe. fillZeros/fillScala come from IndexGen, not from s1. */
  def pS1(
    dut:        PermUnit[FakeRobPtr],
    pipe:       Int,
    table:      Seq[Int],
    prefill:    Seq[Int],
    scalarData: BigInt = BigInt(0),
    placeIdx:   Int = 0,
    blockBase:  Int = 0,
    blockSize:  Int = 1,
    tableIdx:   Int = 0
  ): Unit = {
    val s1 = dut.in.s1pipes(pipe)
    s1.valid.poke(true)
    s1.bits.placeIdx.poke(placeIdx)
    s1.bits.blockBase.poke(blockBase)
    s1.bits.blockSize.poke(blockSize)
    s1.bits.tableIdx.poke(tableIdx)
    s1.bits.scalarData.poke(scalarData)
    table.zipWithIndex.foreach { case (v, i) => s1.bits.table(i).poke(v) }
    prefill.zipWithIndex.foreach { case (v, i) => s1.bits.prefillData(i).poke(v) }
  }

  def pS1Off(dut: PermUnit[FakeRobPtr], pipe: Int): Unit = {
    dut.in.s1pipes(pipe).valid.poke(false)
  }

  def initDut(dut: PermUnit[FakeRobPtr]): Unit = {
    for (p <- 0 until NumPipes) {
      pS0Defaults(dut, p)
      dut.in.s1pipes(p).valid.poke(false)
      dut.in.wbReady(p).poke(false)
    }
    dut.in.flush.foreach(_.poke(false))
    dut.clock.step()
  }

  def eWb(dut: PermUnit[FakeRobPtr], pipe: Int, dest: BigInt): Unit = {
    dut.out.pipes(pipe).valid.expect(true.B, s"pipe $pipe wb.valid")
    dut.out.pipes(pipe).bits.data.expect(dest)
  }

  // ── Utilities ────────────────────────────────────────────────────────

  def foldBytes(bytes: Seq[Int]): BigInt =
    bytes.foldRight(BigInt(0))((b, a) => (a << 8) | BigInt(b))

  object Table { def inc(n: Int = 1) = Seq.tabulate(n * 16)(identity) }
  object Prefill { def default = Seq.tabulate(16)(0xff - _) }

  def goldenWb(
    table: Seq[Int], index: Seq[Int], indexValid: Seq[Boolean],
    fillZeros: Seq[Boolean], fillScala: Seq[Boolean],
    prefill: Seq[Int], tableRange: Seq[Int]
  ): BigInt = {
    index.indices.map { i =>
      if (indexValid(i)) {
        if (tableRange.contains(index(i) / 16)) table(index(i)) else index(i)
      } else if (fillZeros(i)) 0
      else if (fillScala(i)) 0
      else prefill(i)
    }.foldRight(BigInt(0))((b, a) => (a << 8) | BigInt(b))
  }

  def goldenGatherIdx(vs1: BigInt, sew: Int, ei: Int, lmulFrac: Int, uopIdx: Int): Seq[(Int, Boolean, Boolean, Boolean)] = {
    val s = sewb(sew)
    val nIdx = dlen / ei
    val vlmax = lmulFrac * vlen / 64
    val res = Array.fill(numElem)((0, false, false, false))
    for (ie <- 0 until nIdx; j <- 0 until s) {
      val raw = ((vs1 >> (ie * ei)) & ((BigInt(1) << ei) - 1)).toInt
      val pos = ie * s + j
      if (pos < numElem) {
        val idx = raw * s + j
        res(pos) = (idx, idx < vlmax, idx >= vlmax, false)
      }
    }
    res.toSeq
  }

  // ── Tests ────────────────────────────────────────────────────────────

  describe("PermUnit") {

    describe("LMUL=1") {
      it("identity gather") {
        simulate(new PermUnit[FakeRobPtr](FakeRobPtr())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1)
          val g = goldenGatherIdx(0, sew = 8, ei = 8, lmulFrac = 8, uopIdx = 0)
          val golden = goldenWb(table, g.map(_._1), g.map(_._2), g.map(_._3), g.map(_._4), Prefill.default, 0 until 1)
          pS0Gather(dut, 0, sew = 8, ei = 8, vs1 = 0)
          dut.clock.step(); pS0Off(dut, 0)
          pS1(dut, 0, table, Prefill.default)
          dut.clock.step(); pS1Off(dut, 0)
          dut.clock.step()
          eWb(dut, 0, golden)
        }
      }

      // Note: out-of-range index is handled by IndexGen fillZeros,
      // not by GatherRing's notfound path. The PermUnit pipeline
      // correctly propagates fillZeros to produce 0 result.

      it("blockBase=4") {
        simulate(new PermUnit[FakeRobPtr](FakeRobPtr())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1)
          val g = goldenGatherIdx(0, sew = 8, ei = 8, lmulFrac = 8, uopIdx = 0)
          val golden = goldenWb(table, g.map(_._1), g.map(_._2), g.map(_._3), g.map(_._4), Prefill.default, 0 until 1)
          pS0Gather(dut, 0, sew = 8, ei = 8, vs1 = 0, blockBase = 4)
          dut.clock.step(); pS0Off(dut, 0)
          pS1(dut, 0, table, Prefill.default, blockBase = 4)
          dut.clock.step(); pS1Off(dut, 0)
          dut.clock.step()
          eWb(dut, 0, golden)
        }
      }
    }

    describe("LMUL=2") {
      it("two pipes identity gather") {
        simulate(new PermUnit[FakeRobPtr](FakeRobPtr())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(2)
          val g = goldenGatherIdx(0, sew = 8, ei = 8, lmulFrac = 8, uopIdx = 0)
          val golden = goldenWb(table, g.map(_._1), g.map(_._2), g.map(_._3), g.map(_._4), Prefill.default, 0 until 2)
          pS0Gather(dut, 0, sew = 8, ei = 8, vs1 = 0, blockSize = 2, placeIdx = 0)
          pS0Gather(dut, 1, sew = 8, ei = 8, vs1 = 0, blockSize = 2, placeIdx = 1)
          dut.clock.step()
          pS0Off(dut, 0); pS0Off(dut, 1)
          pS1(dut, 0, table.slice(0, 16), Prefill.default, blockSize = 2, tableIdx = 0, placeIdx = 0)
          pS1(dut, 1, table.slice(16, 32), Prefill.default, blockSize = 2, tableIdx = 1, placeIdx = 1)
          dut.clock.step()
          pS1Off(dut, 0); pS1Off(dut, 1)
          for (_ <- 0 until 3) dut.clock.step()
          eWb(dut, 0, golden)
        }
      }
    }

    describe("Slide1Up") {
      it("SEW=64, scalar fills first SEW element") {
        simulate(new PermUnit[FakeRobPtr](FakeRobPtr())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1)
          val sew = 64
          // vslide1up: bytes 0..7 = scalar, bytes 8..15 = table[0..7]
          val scalarData = BigInt("CAFEBABEDEADBEEF", 16)
          val scalarBytes = (0 until 8).map(b => ((scalarData >> (b * 8)) & 0xff).toInt)
          val expected = foldBytes(scalarBytes ++ table.take(8))

          val s0 = dut.in.s0pipes(0)
          s0.valid.poke(true)
          s0.bits.opcode.poke(IndexGenOpcode.slideUp)
          s0.bits.placeIdx.poke(0); s0.bits.blockBase.poke(0); s0.bits.blockSize.poke(1)
          pSew(s0.bits.sew, sew); pSew(s0.bits.ei, 8)
          pLmul(s0.bits.lmul, 8)
          s0.bits.vl.poke(2)
          s0.bits.isSlide1Up.poke(true)
          dut.clock.step(); pS0Off(dut, 0)

          pS1(dut, 0, table, Prefill.default, scalarData = scalarData)
          dut.clock.step(); pS1Off(dut, 0)
          dut.clock.step()

          eWb(dut, 0, expected)
        }
      }
    }

    describe("Parallel pipes") {
      it("two independent LMUL=1 ops on different units") {
        simulate(new PermUnit[FakeRobPtr](FakeRobPtr())(GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1)
          val g = goldenGatherIdx(0, sew = 8, ei = 8, lmulFrac = 8, uopIdx = 0)
          val golden = goldenWb(table, g.map(_._1), g.map(_._2), g.map(_._3), g.map(_._4), Prefill.default, 0 until 1)
          pS0Gather(dut, 0, sew = 8, ei = 8, vs1 = 0, blockBase = 0)
          pS0Gather(dut, 2, sew = 8, ei = 8, vs1 = 0, blockBase = 2)
          dut.clock.step()
          pS0Off(dut, 0); pS0Off(dut, 2)
          pS1(dut, 0, table, Prefill.default)
          pS1(dut, 2, table, Prefill.default, blockBase = 2)
          dut.clock.step()
          pS1Off(dut, 0); pS1Off(dut, 2)
          dut.clock.step()
          eWb(dut, 0, golden)
          eWb(dut, 2, golden)
        }
      }
    }
  }
}
