package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.util.SpecBase
import yunsuan.vector.v2.Shuffle.GatherCommon.GatherParam

/** Segment store (zip) case for [[PermUnit]]: the emu failure of 2026-09-14 (`vsseg2e8_v-0`,
  * LMUL=2) replayed from the IT waveform.
  *
  * A segment store is split into one uop per fixed destination register (`EMUL * seg` uops, D-29) and
  * every group of `seg` uops belongs to one chunk; for this instruction the four uops form two
  * independent 2-unit blocks of the ring (blocks 0 and 2). The ring never released those units, so
  * the entries the instruction had borrowed from the VOQ were never returned and the ROB waited on it
  * forever. The replay below drives the exact stimulus measured in `logs/waves/seg2f_full.fst`
  * (tabulated in `emu-stimulus-seg2.md`) and asserts that every ring unit is idle again once the
  * instruction has drained.
  */
class PermUnitSegStoreSpec extends SpecBase with GatherSpecUtils {

  val NumPipes = 4

  def foldBytes(bytes: Seq[Int]): BigInt =
    bytes.foldRight(BigInt(0))((b, a) => (a << 8) | BigInt(b))

  def interleaveChunk(fields: Seq[Seq[Int]], chunk: Int): BigInt = {
    val elemsPerChunk = numElem / fields.length
    foldBytes((chunk * elemsPerChunk until (chunk + 1) * elemsPerChunk).flatMap(i => fields.map(_(i))))
  }

  // ---- Poke helpers (mirrors PermUnitSpec) ---------------------------------

  def pDefaults(dut: PermUnit[FakePermMeta], pipe: Int): Unit = {
    val p = dut.in.pipes(pipe)
    p.valid.poke(false)
    p.bits.placeIdx.poke(0)
    p.bits.blockBase.poke(0)
    val o = p.bits.op
    o.isGather.poke(false)
    o.isGatherEI16.poke(false)
    o.isGatherEI16Rvs3.poke(false)
    o.isSlideUp.poke(false)
    o.isSlide1Up.poke(false)
    o.isSlideDown.poke(false)
    o.isSlide1Down.poke(false)
    o.isZip.poke(false)
    o.isUnzip.poke(false)
    o.isCompress.poke(false)
    p.bits.uopIdx.poke(0)
    pSew(p.bits.sew, 8)
    pSew(p.bits.ei, 8)
    pLmul(p.bits.lmul, 8)
    p.bits.vl.poke(0)
    p.bits.vta.poke(false)
    p.bits.vma.poke(false)
    p.bits.vs1.poke(0)
    p.bits.vs2.poke(0)
    p.bits.vs3.poke(0)
    p.bits.rs1.poke(0)
    p.bits.mask.poke((BigInt(1) << numElem) - 1)
    p.bits.zipNum.poke(0)
    p.bits.meta.robIdx.poke(0)
    p.bits.meta.pdest.poke(0)
  }

  /** One segment store uop: `seg` of them share a `blockBase` and differ in `uopIdx`/`placeIdx`. */
  def pZip(
    dut: PermUnit[FakePermMeta], pipe: Int, field: Seq[Int],
    blockUnits: Int, uopIdx: Int, placeIdx: Int, zipNum: Int, pdest: Int
  ): Unit = {
    val p = dut.in.pipes(pipe)
    p.valid.poke(true)
    p.bits.op.isZip.poke(true)
    p.bits.placeIdx.poke(placeIdx)
    p.bits.blockBase.poke(0)
    p.bits.uopIdx.poke(uopIdx)
    pSew(p.bits.sew, 8)
    pLmul(p.bits.lmul, blockUnits * 8)
    p.bits.vl.poke(blockUnits * numElem)
    p.bits.vs2.poke(foldBytes(field))
    p.bits.zipNum.poke(zipNum)
    p.bits.meta.robIdx.poke(0)
    p.bits.meta.pdest.poke(pdest)
  }

  def pPipeOff(dut: PermUnit[FakePermMeta], pipe: Int): Unit =
    dut.in.pipes(pipe).valid.poke(false)

  /** Clock one cycle with the given pipes held, then release them. */
  def stepOff(dut: PermUnit[FakePermMeta], pipes: Int*): Unit = {
    dut.clock.step()
    pipes.foreach(pPipeOff(dut, _))
  }

  /** One segment store uop with the control values the emu drives for `vsseg2e8_v-0` LMUL=2. */
  def pEmuUop(dut: PermUnit[FakePermMeta], pipe: Int, vs2: Seq[Int], blockBase: Int,
              placeIdx: Int, uopIdx: Int, pdest: Int, robIdx: Int): Unit = {
    pZip(dut, pipe, vs2, blockUnits = 2, uopIdx = uopIdx, placeIdx = placeIdx, zipNum = 1, pdest = pdest)
    val b = dut.in.pipes(pipe).bits
    b.vl.poke(255)
    b.blockBase.poke(blockBase)
    b.meta.robIdx.poke(robIdx)
  }

  def initDut(dut: PermUnit[FakePermMeta]): Unit = {
    for (p <- 0 until NumPipes) {
      pDefaults(dut, p)
    }
    dut.in.wbReady.foreach(_.poke(false))
    dut.in.flush.foreach(_.poke(false))
    dut.in.wakeAck.foreach(_.poke(false))
    dut.clock.step()
  }

  // ---- Tests ---------------------------------------------------------------

  describe("zip") {
    describe("seg 2") {
      // Verbatim replay of the stimulus of the FAILING instruction in the emu run (logs/waves/
      // seg2f_full.fst, robIdx 0x88): the per-cycle valid pattern, every valid row's fields and the
      // payloads are copied from ...inner_vecRegion.permUnit.permUnit.in_pipes_*.
      //
      //   emu cycle  valid(p0,p1)  offers
      //   17409      1 0            p0 blockBase=0 uopIdx=0 placeIdx=0 vs2=a0
      //   17410      1 1            p0 blockBase=2 uopIdx=0 placeIdx=0 vs2=a2
      //                             p1 blockBase=0 uopIdx=1 placeIdx=0 vs2=b1
      //   17411      0 1            p1 blockBase=2 uopIdx=1 placeIdx=0 vs2=b3
      //   17412      0 0
      //
      // One 4-uop instruction = two independent 2-unit blocks (block 0 and block 2); pipe1 only
      // becomes valid in the second cycle. placeIdx counts same-cycle siblings *of the same block*
      // only (VecPermUnit.segChunkOffsetOf): the emu waveform shows 1 for the 17410 pipe1 uop because
      // the pre-fix generator counted its chunk-1 sibling from another block, which shifted the uop
      // off block 0's own unit and left block 0 incomplete forever (unit 0 never released).
      //
      // The payloads are the measured 128-bit vs2 values (each uop carries one field register, and
      // the four of them are the same stream shifted by 16 bits); the control fields are the
      // measured ones: vl=255, lmul=m2, sew=e8, ei=e8, vta=vma=0, mask=0xffff, zipNum=1, pdest=0.
      //
      // In the emu the instruction's tables were never all released, so the entries it had borrowed
      // from the VOQ were never returned and the ROB waited on it forever. The observable that
      // matters is unit occupancy: GatherRingUnit clears `hasTable` only on `in.clearTable &&
      // passedEntry` (or through the flow path), while `passedEntry` itself is only ever overwritten,
      // so a leftover `passedEntry=1` on an empty unit is benign. This case therefore asserts the
      // units are free again (hasTable=0) once the instruction has drained.
      it("EMUL=2: emu replay (vsseg2e8_v-0, ring unit release)") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val a0 = Seq(0x01, 0x00, 0xff, 0xf8, 0x7f, 0x0f, 0x02, 0x01, 0x00, 0xff, 0xf8, 0x7f, 0x0f, 0x02, 0x01, 0x00)
          val b1 = Seq(0x0f, 0x02, 0x01, 0x00, 0xff, 0xf8, 0x7f, 0x0f, 0x02, 0x01, 0x00, 0xff, 0xf8, 0x7f, 0x0f, 0x02)
          val a2 = Seq(0xf8, 0x7f, 0x0f, 0x02, 0x01, 0x00, 0xff, 0xf8, 0x7f, 0x0f, 0x02, 0x01, 0x00, 0xff, 0xf8, 0x7f)
          val b3 = Seq(0x00, 0xff, 0xf8, 0x7f, 0x0f, 0x02, 0x01, 0x00, 0xff, 0xf8, 0x7f, 0x0f, 0x02, 0x01, 0x00, 0xff)
          pEmuUop(dut, 0, a0, blockBase = 0, placeIdx = 0, uopIdx = 0, pdest = 0, robIdx = 0x88)
          stepOff(dut, 0)
          pEmuUop(dut, 0, a2, blockBase = 2, placeIdx = 0, uopIdx = 0, pdest = 2, robIdx = 0x88)
          pEmuUop(dut, 1, b1, blockBase = 0, placeIdx = 0, uopIdx = 1, pdest = 1, robIdx = 0x88)
          stepOff(dut, 0, 1)
          pEmuUop(dut, 1, b3, blockBase = 2, placeIdx = 0, uopIdx = 1, pdest = 3, robIdx = 0x88)
          stepOff(dut, 1)
          val expected = Seq(interleaveChunk(Seq(a0, b1), 0), interleaveChunk(Seq(a0, b1), 1), interleaveChunk(Seq(a2, b3), 0), interleaveChunk(Seq(a2, b3), 1))
          val actual = Array.fill[Option[BigInt]](expected.length)(None)
          dut.in.wbReady.foreach(_.poke(true))
          for (_ <- 0 until 80) {
            for (p <- 0 until dut.out.pipes.length) {
              if (dut.out.pipes(p).valid.peekBoolean()) {
                val uopIdx = dut.out.pipes(p).bits.meta.pdest.peekValue().asBigInt.toInt
                if (uopIdx < actual.length && actual(uopIdx).isEmpty) {
                  actual(uopIdx) = Some(dut.out.pipes(p).bits.data.peekValue().asBigInt)
                }
              }
            }
            dut.clock.step()
          }
          actual.toSeq mustBe expected.map(Some(_))
          val busy = (0 until dut.out.unitStatus.length).filter { u =>
            dut.out.unitStatus(u).hasTable.peek().litValue != 0
          }
          busy mustBe Seq.empty
        }
      }

      // Verbatim replay of the CURRENT emu stimulus (logs/waves/seg2e8_uopidx.fst, episode A,
      // cycle 17430-17432): identical to the case above except that the two chunk-1 uops carry
      // their chunk-inclusive uopIdx (2 and 3), which is what VecPermUnit drives since the
      // 2026-09-15 change (`ex0.bits.uopIdx := uop.bits.ctrl.uopIdx`). The zip IndexGen tables
      // only describe the slices of ONE chunk, so before PermUnit passed `uopIdx % seg` to
      // IndexGen the chunk-1 slices came out invalid and the ring emitted PREFILL zeros
      // (emu: sq18/sq19 data all-zero, REF expected interleave(v3, v5)).
      it("EMUL=2: chunk-inclusive uopIdx zips chunk 1 correctly") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val v2 = Seq(0x00, 0x01, 0x02, 0x0f, 0x7f, 0xf8, 0xff, 0x00, 0x01, 0x02, 0x0f, 0x7f, 0xf8, 0xff, 0x00, 0x01)
          val v4 = Seq(0x7f, 0xf8, 0xff, 0x00, 0x01, 0x02, 0x0f, 0x7f, 0xf8, 0xff, 0x00, 0x01, 0x02, 0x0f, 0x7f, 0xf8)
          val v3 = Seq(0x02, 0x0f, 0x7f, 0xf8, 0xff, 0x00, 0x01, 0x02, 0x0f, 0x7f, 0xf8, 0xff, 0x00, 0x01, 0x02, 0x0f)
          val v5 = Seq(0xff, 0x00, 0x01, 0x02, 0x0f, 0x7f, 0xf8, 0xff, 0x00, 0x01, 0x02, 0x0f, 0x7f, 0xf8, 0xff, 0x00)
          pEmuUop(dut, 0, v2, blockBase = 0, placeIdx = 0, uopIdx = 0, pdest = 0, robIdx = 0x88)
          stepOff(dut, 0)
          pEmuUop(dut, 0, v3, blockBase = 2, placeIdx = 0, uopIdx = 2, pdest = 2, robIdx = 0x88)
          pEmuUop(dut, 1, v4, blockBase = 0, placeIdx = 0, uopIdx = 1, pdest = 1, robIdx = 0x88)
          stepOff(dut, 0, 1)
          pEmuUop(dut, 1, v5, blockBase = 2, placeIdx = 0, uopIdx = 3, pdest = 3, robIdx = 0x88)
          stepOff(dut, 1)
          val expected = Seq(interleaveChunk(Seq(v2, v4), 0), interleaveChunk(Seq(v2, v4), 1), interleaveChunk(Seq(v3, v5), 0), interleaveChunk(Seq(v3, v5), 1))
          val actual = Array.fill[Option[BigInt]](expected.length)(None)
          dut.in.wbReady.foreach(_.poke(true))
          for (_ <- 0 until 80) {
            for (p <- 0 until dut.out.pipes.length) {
              if (dut.out.pipes(p).valid.peekBoolean()) {
                val pdest = dut.out.pipes(p).bits.meta.pdest.peekValue().asBigInt.toInt
                if (pdest < actual.length && actual(pdest).isEmpty) {
                  actual(pdest) = Some(dut.out.pipes(p).bits.data.peekValue().asBigInt)
                }
              }
            }
            dut.clock.step()
          }
          actual.toSeq mustBe expected.map(Some(_))
          val busy = (0 until dut.out.unitStatus.length).filter { u =>
            dut.out.unitStatus(u).hasTable.peek().litValue != 0
          }
          busy mustBe Seq.empty
        }
      }

      // Exact ROB 303 offers at cycles 21474-21478 from
      // logs/run/20260916-132452-3ab5c7775d-vsseg2e8-srcmap-wave/20260916-132452/
      // vsseg2e8_v-0_20260916-132452-571354.fst. Block 4 reaches the
      // ring out of order (uop5 before uop4) while its neighboring ring2 blocks are also flowing.
      // Before the fix, unit 4 simultaneously consumes unit 3's cross-block forward flow and unit
      // 5's ring2 loopback; Mux1H ORs both payloads and pdest 5 becomes
      // 0xfffff87f7f0f0f0202ff01f800ffffff.
      it("EMUL=4: neighboring ring2 blocks do not mix flows") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val fields = Seq(
            Seq(0x00, 0x01, 0x02, 0x0f, 0x7f, 0xf8, 0xff, 0x00, 0x01, 0x02, 0x0f, 0x7f, 0xf8, 0xff, 0x00, 0x01),
            Seq(0x01, 0x02, 0x0f, 0x7f, 0xf8, 0xff, 0x00, 0x01, 0x02, 0x0f, 0x7f, 0xf8, 0xff, 0x00, 0x01, 0x02),
            Seq(0x02, 0x0f, 0x7f, 0xf8, 0xff, 0x00, 0x01, 0x02, 0x0f, 0x7f, 0xf8, 0xff, 0x00, 0x01, 0x02, 0x0f),
            Seq(0x0f, 0x7f, 0xf8, 0xff, 0x00, 0x01, 0x02, 0x0f, 0x7f, 0xf8, 0xff, 0x00, 0x01, 0x02, 0x0f, 0x7f),
            Seq(0x7f, 0xf8, 0xff, 0x00, 0x01, 0x02, 0x0f, 0x7f, 0xf8, 0xff, 0x00, 0x01, 0x02, 0x0f, 0x7f, 0xf8),
            Seq(0xf8, 0xff, 0x00, 0x01, 0x02, 0x0f, 0x7f, 0xf8, 0xff, 0x00, 0x01, 0x02, 0x0f, 0x7f, 0xf8, 0xff),
            Seq(0xff, 0x00, 0x01, 0x02, 0x0f, 0x7f, 0xf8, 0xff, 0x00, 0x01, 0x02, 0x0f, 0x7f, 0xf8, 0xff, 0x00),
            Seq(0x00, 0x01, 0x02, 0x0f, 0x7f, 0xf8, 0xff, 0x00, 0x01, 0x02, 0x0f, 0x7f, 0xf8, 0xff, 0x00, 0x01),
          )
          def drive(pipe: Int, uopIdx: Int, placeIdx: Int = 0): Unit =
            pEmuUop(dut, pipe, fields(uopIdx), blockBase = uopIdx & ~1,
              placeIdx = placeIdx, uopIdx = uopIdx, pdest = uopIdx, robIdx = 303)

          drive(pipe = 1, uopIdx = 1)
          stepOff(dut, 1)
          drive(pipe = 0, uopIdx = 5)
          drive(pipe = 1, uopIdx = 3)
          stepOff(dut, 0, 1)
          drive(pipe = 0, uopIdx = 0)
          stepOff(dut, 0)
          drive(pipe = 0, uopIdx = 2)
          drive(pipe = 1, uopIdx = 4)
          stepOff(dut, 0, 1)
          drive(pipe = 0, uopIdx = 7)
          drive(pipe = 1, uopIdx = 6, placeIdx = 1)
          stepOff(dut, 0, 1)

          val expected = fields.grouped(2).zipWithIndex.flatMap { case (block, blockIdx) =>
            Seq(
              blockIdx * 2 -> interleaveChunk(block, 0),
              blockIdx * 2 + 1 -> interleaveChunk(block, 1),
            )
          }.toMap
          val actual = scala.collection.mutable.Map.empty[Int, BigInt]
          dut.in.wbReady.foreach(_.poke(true))
          for (_ <- 0 until 40) {
            for (p <- 0 until dut.out.pipes.length) {
              if (dut.out.pipes(p).valid.peekBoolean()) {
                val pdest = dut.out.pipes(p).bits.meta.pdest.peekValue().asBigInt.toInt
                if (expected.contains(pdest)) {
                  actual.getOrElseUpdate(pdest, dut.out.pipes(p).bits.data.peekValue().asBigInt)
                }
              }
            }
            dut.clock.step()
          }
          actual.toMap mustBe expected
          dut.out.unitStatus.foreach { status =>
            status.hasTable.expect(false.B)
            status.hasIndex.expect(false.B)
          }
        }
      }

      it("EMUL=2: wrong placeIdx leaves a ring unit occupied") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val field0 = Seq(0x01, 0x00, 0xff, 0xf8, 0x7f, 0x0f, 0x02, 0x01, 0x00, 0xff, 0xf8, 0x7f, 0x0f, 0x02, 0x01, 0x00)
          val field1 = Seq(0x0f, 0x02, 0x01, 0x00, 0xff, 0xf8, 0x7f, 0x0f, 0x02, 0x01, 0x00, 0xff, 0xf8, 0x7f, 0x0f, 0x02)
          pEmuUop(dut, 0, field0, blockBase = 0, placeIdx = 0, uopIdx = 0, pdest = 0, robIdx = 0x89)
          stepOff(dut, 0)
          pEmuUop(dut, 1, field1, blockBase = 0, placeIdx = 1, uopIdx = 1, pdest = 1, robIdx = 0x89)
          stepOff(dut, 1)
          dut.in.wbReady.foreach(_.poke(true))
          for (_ <- 0 until 40) {
            dut.clock.step()
          }
          val busy = (0 until dut.out.unitStatus.length).count { u =>
            dut.out.unitStatus(u).hasTable.peek().litValue != 0
          }
          busy must be > 0
        }
      }
    }

    describe("seg 4") {
      it("EMUL=2: two four-field blocks complete independently") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          val fields = (0 until 8).map { fieldIdx =>
            (0 until 16).map(elemIdx => fieldIdx * 0x10 + elemIdx + 1)
          }
          for (uop <- 0 until 8) {
            val pipe = uop % 4
            val blockBase = if (uop < 4) 0 else 4
            pZip(dut, pipe, fields(uop), blockUnits = 4, uopIdx = uop, placeIdx = 0, zipNum = 3, pdest = uop)
            dut.in.pipes(pipe).bits.blockBase.poke(blockBase)
            stepOff(dut, pipe)
          }
          for (pipe <- 0 until 4) {
            pPipeOff(dut, pipe)
          }
          dut.in.wbReady.foreach(_.poke(true))
          val expected = fields.grouped(4).flatMap { block =>
            (0 until 4).map(slice => interleaveChunk(block, slice))
          }.toSeq
          val actual = Array.fill[Option[BigInt]](expected.length)(None)
          for (_ <- 0 until 80) {
            for (p <- 0 until dut.out.pipes.length) {
              if (dut.out.pipes(p).valid.peekBoolean()) {
                val pdest = dut.out.pipes(p).bits.meta.pdest.peekValue().asBigInt.toInt
                if (pdest < actual.length && actual(pdest).isEmpty) {
                  actual(pdest) = Some(dut.out.pipes(p).bits.data.peekValue().asBigInt)
                }
              }
            }
            dut.clock.step()
          }
          actual.toSeq mustBe expected.map(Some(_))
          val busy = (0 until dut.out.unitStatus.length).filter { u =>
            dut.out.unitStatus(u).hasTable.peek().litValue != 0
          }
          busy mustBe Seq.empty
        }
      }
    }
  }
}
