package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.util.SpecBase
import yunsuan.vector.v2.Shuffle.GatherCommon.GatherParam

/** Segment store (zip) cases for [[PermUnit]].
  *
  * A segment store is split into one uop per fixed destination register
  * (`EMUL * seg` uops, D-29), and every group of `seg` uops belongs to one chunk: those `seg` uops
  * each present one field register on `vs2` and the ring has to zip them into one interleaved stream
  * of `seg` registers, `slice = uopIdx % blockSize` selecting which VLEN-wide piece a given uop
  * writes out.
  *
  * The case this spec was created for (emu 2026-09-14, `vsseg2e8_v-0`, LMUL=2): the ring emitted
  * only 2 of the 4 uops of the instruction - the two chunks are two independent 2-unit blocks and
  * the second block never delivered its results - which left the ROB waiting forever on the
  * instruction's borrowed PermUnit entries. The expected per-uop data below is
  * `interleave(field0, field1)[slice*VLENB .. slice*VLENB+VLENB)`, i.e. for seg 2 the low halves of
  * both field registers for slice 0 and the high halves for slice 1.
  */
class PermUnitSegStoreSpec extends SpecBase with GatherSpecUtils {

  val NumPipes = 4
  var cyc = 0

  def foldBytes(bytes: Seq[Int]): BigInt =
    bytes.foldRight(BigInt(0))((b, a) => (a << 8) | BigInt(b))

  def fieldLo(base: Int): Seq[Int] = Seq.tabulate(numElem)(base + _)
  def fieldHi(base: Int): Seq[Int] = Seq.tabulate(numElem)(base + 0x40 + _)

  /** Byte-wise interleave of the field registers of one chunk, as the store must place it. */
  def interleave(fields: Seq[Seq[Int]]): Seq[Int] =
    (0 until numElem).flatMap(i => fields.map(_(i))).toSeq

  def zipSlice(fields: Seq[Seq[Int]], slice: Int): BigInt =
    foldBytes(interleave(fields).slice(slice * numElem, (slice + 1) * numElem))

  // ---- Poke helpers (mirrors PermUnitSpec) ---------------------------------

  def pDefaults(dut: PermUnit[FakePermMeta], pipe: Int): Unit = {
    val p = dut.in.pipes(pipe)
    p.valid.poke(false)
    p.bits.placeIdx.poke(0); p.bits.blockBase.poke(0)
    val o = p.bits.op
    o.isGather.poke(false); o.isGatherEI16.poke(false); o.isGatherEI16Rvs3.poke(false)
    o.isSlideUp.poke(false); o.isSlide1Up.poke(false)
    o.isSlideDown.poke(false); o.isSlide1Down.poke(false)
    o.isZip.poke(false); o.isUnzip.poke(false); o.isCompress.poke(false)
    p.bits.uopIdx.poke(0)
    pSew(p.bits.sew, 8); pSew(p.bits.ei, 8); pLmul(p.bits.lmul, 8)
    p.bits.vl.poke(0)
    p.bits.vta.poke(false); p.bits.vma.poke(false)
    p.bits.vs1.poke(0); p.bits.vs2.poke(0); p.bits.vs3.poke(0); p.bits.rs1.poke(0)
    p.bits.mask.poke((BigInt(1) << numElem) - 1); p.bits.zipNum.poke(0)
    p.bits.meta.robIdx.poke(0); p.bits.meta.pdest.poke(0)
  }

  /** One segment store uop: `seg` of them share a `blockBase` and differ in `uopIdx`/`placeIdx`. */
  def pZip(
    dut: PermUnit[FakePermMeta], pipe: Int, field: Seq[Int],
    blockUnits: Int, uopIdx: Int, placeIdx: Int, zipNum: Int, pdest: Int
  ): Unit = {
    val p = dut.in.pipes(pipe)
    p.valid.poke(true)
    p.bits.op.isZip.poke(true)
    p.bits.placeIdx.poke(placeIdx); p.bits.blockBase.poke(0)
    p.bits.uopIdx.poke(uopIdx)
    pSew(p.bits.sew, 8); pLmul(p.bits.lmul, blockUnits * 8)
    p.bits.vl.poke(blockUnits * numElem)
    p.bits.vs2.poke(foldBytes(field))
    p.bits.zipNum.poke(zipNum)
    p.bits.meta.robIdx.poke(0); p.bits.meta.pdest.poke(pdest)
  }

  def pPipeOff(dut: PermUnit[FakePermMeta], pipe: Int): Unit =
    dut.in.pipes(pipe).valid.poke(false)

  def initDut(dut: PermUnit[FakePermMeta]): Unit = {
    for (p <- 0 until NumPipes) pDefaults(dut, p)
    dut.in.wbReady.foreach(_.poke(false))
    dut.in.flush.foreach(_.poke(false))
    dut.in.wakeAck.foreach(_.poke(false))
    dut.clock.step()
  }

  /** (pdest, data) of every writeback seen within `maxSteps`, keyed by pdest so the assertion does
    * not depend on which output pipe the ring arbitrates a given uop onto. */
  def collectWbByPdest(dut: PermUnit[FakePermMeta], maxSteps: Int, pdests: Seq[Int]): Map[Int, BigInt] = {
    var seen = Map.empty[Int, BigInt]
    for (_ <- 0 until maxSteps) {
      for (p <- 0 until NumPipes) {
        if (dut.out.pipes(p).valid.peekBoolean()) {
          val q = dut.out.pipes(p).bits.meta.pdest.peekValue().asBigInt.toInt
          if (pdests.contains(q) && !seen.contains(q)) seen += (q -> dut.out.pipes(p).bits.data.peekValue().asBigInt)
        }
      }
      dut.clock.step()
    }
    seen
  }

  // ---- Tests ---------------------------------------------------------------

  describe("zip") {
    describe("seg 2") {
      it("EMUL=1: one 2-unit block emits both slices") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val f0 = fieldLo(0x00); val f1 = fieldLo(0x10)
          val fields = Seq(f0, f1)
          pZip(dut, 0, f0, blockUnits = 2, uopIdx = 0, placeIdx = 0, zipNum = 1, pdest = 0)
          pZip(dut, 1, f1, blockUnits = 2, uopIdx = 1, placeIdx = 1, zipNum = 1, pdest = 1)
          dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
          val got = collectWbByPdest(dut, 40, Seq(0, 1))
          got(0) mustBe zipSlice(fields, 0)
          got(1) mustBe zipSlice(fields, 1)
        }
      }

      it("EMUL=2: two 2-unit blocks emit all four slices") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val c0 = Seq(fieldLo(0x00), fieldLo(0x10)) // chunk0: field 0 low, field 1 low
          val c1 = Seq(fieldHi(0x00), fieldHi(0x10)) // chunk1: field 0 high, field 1 high
          pZip(dut, 0, c0(0), blockUnits = 2, uopIdx = 0, placeIdx = 0, zipNum = 1, pdest = 0)
          pZip(dut, 1, c0(1), blockUnits = 2, uopIdx = 1, placeIdx = 1, zipNum = 1, pdest = 1)
          pZip(dut, 2, c1(0), blockUnits = 2, uopIdx = 0, placeIdx = 0, zipNum = 1, pdest = 2)
          pZip(dut, 3, c1(1), blockUnits = 2, uopIdx = 1, placeIdx = 1, zipNum = 1, pdest = 3)
          // chunk k works on the block at blockBase = k * blockSize - without this the four
          // tables of two chunks would all target block 0, which the ring rightly rejects.
          dut.in.pipes(2).bits.blockBase.poke(2); dut.in.pipes(3).bits.blockBase.poke(2)
          dut.clock.step(); for (p <- 0 until NumPipes) pPipeOff(dut, p)
          val got = collectWbByPdest(dut, 60, Seq(0, 1, 2, 3))
          got(0) mustBe zipSlice(c0, 0)
          got(1) mustBe zipSlice(c0, 1)
          got(2) mustBe zipSlice(c1, 0)
          got(3) mustBe zipSlice(c1, 1)
        }
      }

      // The two chunks of an EMUL=2 instruction land on different 2-unit blocks of the same ring:
      // chunk k uses blockBase = k * blockSize, which is what the main repo drives.
      it("EMUL=2: second chunk only (blockBase=2)") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val c1 = Seq(fieldHi(0x00), fieldHi(0x10))
          val p0 = dut.in.pipes(0); val p1 = dut.in.pipes(1)
          pZip(dut, 0, c1(0), blockUnits = 2, uopIdx = 0, placeIdx = 0, zipNum = 1, pdest = 0)
          pZip(dut, 1, c1(1), blockUnits = 2, uopIdx = 1, placeIdx = 1, zipNum = 1, pdest = 1)
          p0.bits.blockBase.poke(2); p1.bits.blockBase.poke(2)
          dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
          val got = collectWbByPdest(dut, 60, Seq(0, 1))
          got(0) mustBe zipSlice(c1, 0)
          got(1) mustBe zipSlice(c1, 1)
        }
      }

      // Same-cycle placement of two different blocks, each with placeIdx 0 (legal: each block owns
      // its own slots). Neither block can complete here - only one of its two tables arrives - so
      // this case only checks that the ring's table-writer assert stays quiet.
      it("EMUL=2: one field of each block in the same cycle") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          pZip(dut, 0, fieldLo(0x00), blockUnits = 2, uopIdx = 0, placeIdx = 0, zipNum = 1, pdest = 0)
          pZip(dut, 2, fieldHi(0x00), blockUnits = 2, uopIdx = 0, placeIdx = 0, zipNum = 1, pdest = 2)
          dut.in.pipes(2).bits.blockBase.poke(2)
          dut.clock.step(); for (p <- 0 until NumPipes) pPipeOff(dut, p)
          for (_ <- 0 until 20) dut.clock.step()
        }
      }

      it("EMUL=2: two blocks one cycle apart") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val c0 = Seq(fieldLo(0x00), fieldLo(0x10))
          val c1 = Seq(fieldHi(0x00), fieldHi(0x10))
          pZip(dut, 0, c0(0), blockUnits = 2, uopIdx = 0, placeIdx = 0, zipNum = 1, pdest = 0)
          pZip(dut, 1, c0(1), blockUnits = 2, uopIdx = 1, placeIdx = 1, zipNum = 1, pdest = 1)
          dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
          pZip(dut, 2, c1(0), blockUnits = 2, uopIdx = 0, placeIdx = 0, zipNum = 1, pdest = 2)
          pZip(dut, 3, c1(1), blockUnits = 2, uopIdx = 1, placeIdx = 1, zipNum = 1, pdest = 3)
          dut.in.pipes(2).bits.blockBase.poke(2); dut.in.pipes(3).bits.blockBase.poke(2)
          dut.clock.step(); for (p <- 0 until NumPipes) pPipeOff(dut, p)
          val got = collectWbByPdest(dut, 60, Seq(0, 1, 2, 3))
          got(0) mustBe zipSlice(c0, 0)
          got(1) mustBe zipSlice(c0, 1)
          got(2) mustBe zipSlice(c1, 0)
          got(3) mustBe zipSlice(c1, 1)
        }
      }
    }

    // TODO(seg 4/8): zipNum 3 / 7 with 4- and 8-unit blocks, and the seg 3/5/6/7 hole cases once
    // the block controller learns to accept an empty slot (task-yunsuan-seg3567.md).
    describe("seg 4") {
      it("EMUL=1: one 4-unit block emits all four slices") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val fields = Seq(fieldLo(0x00), fieldLo(0x10), fieldLo(0x20), fieldLo(0x30))
          for (u <- 0 until NumPipes)
            pZip(dut, u, fields(u), blockUnits = 4, uopIdx = u, placeIdx = u, zipNum = 3, pdest = u)
          dut.clock.step(); for (p <- 0 until NumPipes) pPipeOff(dut, p)
          val got = collectWbByPdest(dut, 80, Seq(0, 1, 2, 3))
          for (u <- 0 until 4) got(u) mustBe zipSlice(fields, u)
        }
      }
    }
  }
}
