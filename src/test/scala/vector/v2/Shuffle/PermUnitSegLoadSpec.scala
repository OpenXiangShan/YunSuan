package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.util.SpecBase
import yunsuan.vector.v2.Shuffle.GatherCommon.GatherParam

/** Focused segment-load unzip tests using the packed bytes observed at the load writeback boundary. */
class PermUnitSegLoadSpec extends SpecBase with GatherSpecUtils {

  private def foldBytes(bytes: Seq[Int]): BigInt =
    bytes.foldRight(BigInt(0))((byte, acc) => (acc << 8) | BigInt(byte))

  private def defaults(dut: PermUnit[FakePermMeta], pipe: Int): Unit = {
    val in = dut.in.pipes(pipe)
    in.valid.poke(false.B)
    in.bits.placeIdx.poke(0.U)
    in.bits.blockBase.poke(0.U)
    in.bits.op.isGather.poke(false.B)
    in.bits.op.isGatherEI16.poke(false.B)
    in.bits.op.isGatherEI16Rvs3.poke(false.B)
    in.bits.op.isSlideUp.poke(false.B)
    in.bits.op.isSlide1Up.poke(false.B)
    in.bits.op.isSlideDown.poke(false.B)
    in.bits.op.isSlide1Down.poke(false.B)
    in.bits.op.isZip.poke(false.B)
    in.bits.op.isUnzip.poke(false.B)
    in.bits.op.isCompress.poke(false.B)
    in.bits.uopIdx.poke(0.U)
    pSew(in.bits.sew, 8)
    pSew(in.bits.ei, 8)
    pLmul(in.bits.lmul, 16)
    in.bits.blockFieldNum.poke(2.U)
    in.bits.vl.poke(0.U)
    in.bits.vta.poke(false.B)
    in.bits.vma.poke(false.B)
    in.bits.vs1.poke(0.U)
    in.bits.vs2.poke(0.U)
    in.bits.vs3.poke(0.U)
    in.bits.rs1.poke(0.U)
    in.bits.mask.poke("hffff".U)
    in.bits.zipNum.poke(0.U)
    in.bits.meta.robIdx.poke(0.U)
    in.bits.meta.pdest.poke(0.U)
  }

  private def driveUnzip(
    dut: PermUnit[FakePermMeta],
    pipe: Int,
    uopIdx: Int,
    placeIdx: Int,
    packed: Seq[Int],
    oldVd: Seq[Int],
    zipNum: Int = 1,
    lmulFrac: Int = 16,
    fieldNum: Int = 2,
    vl: Int = 7,
  ): Unit = {
    val in = dut.in.pipes(pipe)
    in.valid.poke(true.B)
    in.bits.placeIdx.poke(placeIdx.U)
    in.bits.blockBase.poke(0.U)
    in.bits.op.isUnzip.poke(true.B)
    in.bits.uopIdx.poke(uopIdx.U)
    pSew(in.bits.sew, 8)
    pLmul(in.bits.lmul, lmulFrac)
    in.bits.blockFieldNum.poke(fieldNum.U)
    in.bits.vl.poke(vl.U)
    in.bits.vs2.poke(foldBytes(packed).U)
    in.bits.vs3.poke(foldBytes(oldVd).U)
    in.bits.zipNum.poke(zipNum.U)
    in.bits.meta.robIdx.poke(0x6e.U)
    in.bits.meta.pdest.poke(uopIdx.U)
  }

  /** One PermUnit input beat captured from an emu FST (idle cycles compressed). */
  case class SegLoadBeat(
    pipe: Int,
    uopIdx: Int,
    placeIdx: Int,
    blockBase: Int,
    vl: Int,
    vs2: BigInt,
    vs3: BigInt,
    pdest: Int,
  )

  private def driveBeat(
    dut: PermUnit[FakePermMeta],
    sew: Int,
    robIdx: Int,
    beat: SegLoadBeat,
  ): Unit = {
    val in = dut.in.pipes(beat.pipe)
    in.valid.poke(true.B)
    in.bits.placeIdx.poke(beat.placeIdx.U)
    in.bits.blockBase.poke(beat.blockBase.U)
    in.bits.op.isUnzip.poke(true.B)
    in.bits.uopIdx.poke(beat.uopIdx.U)
    pSew(in.bits.sew, sew)
    pLmul(in.bits.lmul, 32)
    in.bits.blockFieldNum.poke(4.U)
    in.bits.vl.poke(beat.vl.U)
    in.bits.vs2.poke(beat.vs2.U)
    in.bits.vs3.poke(beat.vs3.U)
    in.bits.zipNum.poke(3.U)
    in.bits.meta.robIdx.poke(robIdx.U)
    in.bits.meta.pdest.poke(beat.pdest.U)
  }

  /** Replay one captured segment-load group and check every output register.
   *
   * Stimuli and expected outputs come from emu waveforms
   * (logs/run/20260916-current-vlseg4e{32,64}-wave, emu banner Sep 17 2026 02:41:24).
   * The expected outputs were verified element-by-element against the architectural
   * deinterleave rule out(f)[j] = (j < vl_block) ? table_j[f] : oldVd[j] over all
   * 136 e32 and 112 e64 beats of the failing runs before being frozen here.
   */
  private def replayGroup(
    name: String,
    sew: Int,
    robIdx: Int,
    cycles: Seq[Seq[SegLoadBeat]],
    expected: Map[Int, BigInt],
  ): Unit = {
    simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
      for (pipe <- dut.in.pipes.indices) {
        defaults(dut, pipe)
      }
      dut.in.unitFlush.foreach(_.poke(false.B))
      dut.in.wbReady.foreach(_.poke(false.B))
      dut.in.wakeAck.foreach(_.poke(false.B))
      dut.clock.step()

      for (beats <- cycles) {
        for (beat <- beats) {
          driveBeat(dut, sew, robIdx, beat)
        }
        dut.clock.step()
        for (beat <- beats) {
          dut.in.pipes(beat.pipe).valid.poke(false.B)
        }
      }

      val actual = scala.collection.mutable.Map[Int, BigInt]()
      dut.in.wbReady.foreach(_.poke(true.B))
      dut.in.wakeAck.foreach(_.poke(true.B))
      for (_ <- 0 until 40) {
        for (pipe <- dut.out.pipes.indices if dut.out.pipes(pipe).valid.peekBoolean()) {
          val pdest = dut.out.pipes(pipe).bits.meta.pdest.peekValue().asBigInt.toInt
          if (!actual.contains(pdest)) {
            actual(pdest) = dut.out.pipes(pipe).bits.data.peekValue().asBigInt
          }
        }
        dut.clock.step()
      }

      withClue(s"$name: ") {
        actual.size mustBe expected.size
        for ((pdest, data) <- expected) {
          actual.get(pdest) mustBe Some(data)
        }
        dut.out.unitStatus.foreach { status =>
          status.hasTable.expect(false.B)
          status.hasIndex.expect(false.B)
        }
      }
    }
  }

  describe("segment load unzip") {
    it("deinterleaves vlseg2e8 vl=7 and preserves each field's old-vd tail") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        for (pipe <- dut.in.pipes.indices) {
          defaults(dut, pipe)
        }
        dut.in.unitFlush.foreach(_.poke(false.B))
        dut.in.wbReady.foreach(_.poke(false.B))
        dut.in.wakeAck.foreach(_.poke(false.B))
        dut.clock.step()

        val packed0 = (0 until 14).map(identity) ++ Seq.fill(2)(0xee)
        val packed1 = Seq.fill(16)(0)
        val old0 = (0 until 16).map(i => 0xa0 + i)
        val old1 = (0 until 16).map(i => 0xc0 + i)
        driveUnzip(dut, pipe = 0, uopIdx = 0, placeIdx = 0, packed0, old0)
        driveUnzip(dut, pipe = 1, uopIdx = 1, placeIdx = 1, packed1, old1)
        dut.clock.step()
        dut.in.pipes(0).valid.poke(false.B)
        dut.in.pipes(1).valid.poke(false.B)

        val expected = Seq(
          foldBytes((0 until 7).map(_ * 2) ++ old0.drop(7)),
          foldBytes((0 until 7).map(_ * 2 + 1) ++ old1.drop(7)),
        )
        val actual = Array.fill[Option[BigInt]](2)(None)
        dut.in.wbReady.foreach(_.poke(true.B))
        dut.in.wakeAck.foreach(_.poke(true.B))
        for (_ <- 0 until 40) {
          for (pipe <- dut.out.pipes.indices if dut.out.pipes(pipe).valid.peekBoolean()) {
            val pdest = dut.out.pipes(pipe).bits.meta.pdest.peekValue().asBigInt.toInt
            if (pdest < actual.length && actual(pdest).isEmpty) {
              actual(pdest) = Some(dut.out.pipes(pipe).bits.data.peekValue().asBigInt)
            }
          }
          dut.clock.step()
        }

        actual.toSeq mustBe expected.map(Some(_))
        dut.out.unitStatus.foreach { status =>
          status.hasTable.expect(false.B)
          status.hasIndex.expect(false.B)
        }
      }
    }

    it("deinterleaves vlseg3e8 vl=5 in a size-4 hole block and preserves old-vd tails") {
      simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
        for (pipe <- dut.in.pipes.indices) {
          defaults(dut, pipe)
        }
        dut.in.unitFlush.foreach(_.poke(false.B))
        dut.in.wbReady.foreach(_.poke(false.B))
        dut.in.wakeAck.foreach(_.poke(false.B))
        dut.clock.step()

        // vlseg3e8 vl=5: one chunk (15 interleaved bytes, all in table 0), 3 field
        // uops in a size-4 hole block (unit3 = passive index hole)
        val packed0 = (0 until 15).map(identity) :+ 0
        val packedZ = Seq.fill(16)(0)
        val olds = (0 until 3).map(f => (0 until 16).map(i => 0xa0 + f * 0x20 + i))
        for (f <- 0 until 3) {
          driveUnzip(dut, pipe = f, uopIdx = f, placeIdx = f, if (f == 0) packed0 else packedZ, olds(f),
            zipNum = 2, lmulFrac = 32, fieldNum = 3, vl = 5)
        }
        dut.clock.step()
        for (f <- 0 until 3) dut.in.pipes(f).valid.poke(false.B)

        val expected = Seq.tabulate(3)(f =>
          foldBytes((0 until 5).map(_ * 3 + f) ++ olds(f).drop(5))
        )
        val actual = Array.fill[Option[BigInt]](3)(None)
        dut.in.wbReady.foreach(_.poke(true.B))
        dut.in.wakeAck.foreach(_.poke(true.B))
        for (_ <- 0 until 40) {
          dut.out.wake.foreach(w => assert(!w.valid.peekBoolean(), "hole block must not wake (D3)"))
          for (pipe <- dut.out.pipes.indices if dut.out.pipes(pipe).valid.peekBoolean()) {
            val pdest = dut.out.pipes(pipe).bits.meta.pdest.peekValue().asBigInt.toInt
            if (pdest < actual.length && actual(pdest).isEmpty) {
              actual(pdest) = Some(dut.out.pipes(pipe).bits.data.peekValue().asBigInt)
            }
          }
          dut.clock.step()
        }

        actual.toSeq mustBe expected.map(Some(_))
        dut.out.unitStatus.foreach { status =>
          status.hasTable.expect(false.B)
          status.hasIndex.expect(false.B)
        }
      }
    }

    it("replays vlseg4e32 vl=2 single block (emu rob 0x9a)") {
      val old = BigInt("8344c0aa5b76129530ac75dc32808aa4", 16)
      replayGroup(
        name = "vlseg4e32-vl2",
        sew = 32,
        robIdx = 0x21,
        cycles = Seq(
          Seq(
            SegLoadBeat(0, 3, 0, 0, 2, BigInt("6807899f83bc182de7fc7d98384c503f", 16), old, 0x60),
            SegLoadBeat(1, 2, 1, 0, 2, BigInt("0e33b654d37c84774a7ff9297ea55e08", 16), old, 0x5f),
          ),
          Seq(
            SegLoadBeat(1, 0, 0, 0, 2, BigInt("0000000f000000020000000100000000", 16), old, 0x5d),
            SegLoadBeat(2, 1, 1, 0, 2, BigInt("00000000fffffffffffffff87fffffff", 16), old, 0x5e),
          ),
        ),
        expected = Map(
          0x5d -> BigInt("8344c0aa5b7612957fffffff00000000", 16),
          0x5e -> BigInt("8344c0aa5b761295fffffff800000001", 16),
          0x5f -> BigInt("8344c0aa5b761295ffffffff00000002", 16),
          0x60 -> BigInt("8344c0aa5b761295000000000000000f", 16),
        ),
      )
    }

    it("replays vlseg4e32 vl=7 with per-block vl 4/3 (emu rob 0x14d)") {
      val oldBlk0 = BigInt("5f03517aa30230eeb1b3d462ad678ccc", 16)
      val oldBlk1 = BigInt("0a6750f800ba35277dcf27ed2365efac", 16)
      replayGroup(
        name = "vlseg4e32-vl7",
        sew = 32,
        robIdx = 0x22,
        cycles = Seq(
          Seq(
            SegLoadBeat(2, 7, 0, 4, 3, BigInt("5972cd8fdcca5a0d68c8fc3b58465b4a", 16), oldBlk1, 0x1b),
          ),
          Seq(
            SegLoadBeat(0, 4, 0, 4, 3, BigInt("fffffff87fffffff0000000f00000002", 16), oldBlk1, 0x18),
            SegLoadBeat(1, 5, 1, 4, 3, BigInt("000000020000000100000000ffffffff", 16), oldBlk1, 0x19),
          ),
          Seq(
            SegLoadBeat(1, 6, 0, 4, 3, BigInt("fffffffffffffff87fffffff0000000f", 16), oldBlk1, 0x1a),
          ),
          Seq(
            SegLoadBeat(0, 2, 0, 0, 4, BigInt("7fffffff0000000f0000000200000001", 16), oldBlk0, 0x16),
            SegLoadBeat(2, 0, 1, 0, 4, BigInt("0000000f000000020000000100000000", 16), oldBlk0, 0x14),
          ),
          Seq(
            SegLoadBeat(2, 1, 0, 0, 4, BigInt("00000000fffffffffffffff87fffffff", 16), oldBlk0, 0x15),
          ),
          Seq(
            SegLoadBeat(2, 3, 0, 0, 4, BigInt("0000000100000000fffffffffffffff8", 16), oldBlk0, 0x0e),
          ),
        ),
        expected = Map(
          0x1b -> BigInt("0a6750f8ffffffff00000002fffffff8", 16),
          0x18 -> BigInt("0a6750f80000000fffffffff00000002", 16),
          0x19 -> BigInt("0a6750f87fffffff000000000000000f", 16),
          0x1a -> BigInt("0a6750f8fffffff8000000017fffffff", 16),
          0x16 -> BigInt("000000000000000fffffffff00000002", 16),
          0x14 -> BigInt("fffffff8000000017fffffff00000000", 16),
          0x15 -> BigInt("ffffffff00000002fffffff800000001", 16),
          0x0e -> BigInt("000000017fffffff000000000000000f", 16),
        ),
      )
    }

    it("replays vlseg4e64 with two vl=2 blocks (emu rob 0x142)") {
      val oldBlk0 = BigInt("5f03517aa30230eeb1b3d462ad678ccc", 16)
      val oldBlk1 = BigInt("0a6750f800ba35277dcf27ed2365efac", 16)
      replayGroup(
        name = "vlseg4e64-vl2x2",
        sew = 64,
        robIdx = 0x23,
        cycles = Seq(
          Seq(
            SegLoadBeat(0, 0, 0, 0, 2, BigInt("00000000000000010000000000000000", 16), oldBlk0, 0x30),
          ),
          Seq(
            SegLoadBeat(0, 1, 0, 0, 2, BigInt("000000000000000f0000000000000002", 16), oldBlk0, 0x31),
          ),
          Seq(
            SegLoadBeat(0, 2, 0, 0, 2, BigInt("fffffffffffffff87fffffffffffffff", 16), oldBlk0, 0x32),
            SegLoadBeat(1, 3, 1, 0, 2, BigInt("0000000000000000ffffffffffffffff", 16), oldBlk0, 0x33),
            SegLoadBeat(2, 4, 0, 4, 2, BigInt("00000000000000020000000000000001", 16), oldBlk1, 0x2f),
          ),
          Seq(
            SegLoadBeat(1, 5, 0, 4, 2, BigInt("7fffffffffffffff000000000000000f", 16), oldBlk1, 0x51),
            SegLoadBeat(2, 6, 1, 4, 2, BigInt("fffffffffffffffffffffffffffffff8", 16), oldBlk1, 0x52),
          ),
          Seq(
            SegLoadBeat(1, 7, 0, 4, 2, BigInt("00000000000000010000000000000000", 16), oldBlk1, 0x53),
          ),
        ),
        expected = Map(
          0x30 -> BigInt("7fffffffffffffff0000000000000000", 16),
          0x31 -> BigInt("fffffffffffffff80000000000000001", 16),
          0x32 -> BigInt("ffffffffffffffff0000000000000002", 16),
          0x33 -> BigInt("0000000000000000000000000000000f", 16),
          0x2f -> BigInt("fffffffffffffff80000000000000001", 16),
          0x51 -> BigInt("ffffffffffffffff0000000000000002", 16),
          0x52 -> BigInt("0000000000000000000000000000000f", 16),
          0x53 -> BigInt("00000000000000017fffffffffffffff", 16),
        ),
      )
    }

    it("replays vlseg4e64 with a vl=0 second block (emu rob 0x32)") {
      val oldBlk0 = BigInt("5f03517aa30230eeb1b3d462ad678ccc", 16)
      val oldBlk1 = BigInt("0a6750f800ba35277dcf27ed2365efac", 16)
      replayGroup(
        name = "vlseg4e64-blk1vl0",
        sew = 64,
        robIdx = 0x24,
        cycles = Seq(
          Seq(
            SegLoadBeat(0, 0, 0, 0, 2, BigInt("00000000000000010000000000000000", 16), oldBlk0, 0x66),
            SegLoadBeat(2, 1, 1, 0, 2, BigInt("000000000000000f0000000000000002", 16), oldBlk0, 0x68),
          ),
          Seq(
            SegLoadBeat(0, 2, 0, 0, 2, BigInt("fffffffffffffff87fffffffffffffff", 16), oldBlk0, 0x6b),
          ),
          Seq(
            SegLoadBeat(1, 5, 0, 4, 0, BigInt("3cf0f90adf4204df9f1d040aa8c90451", 16), oldBlk1, 0x6e),
            SegLoadBeat(2, 4, 1, 4, 0, BigInt("e36d46c8326984f1042a506f3394c993", 16), oldBlk1, 0x65),
          ),
          Seq(
            SegLoadBeat(0, 6, 0, 4, 0, BigInt("5db474ce5e78ee4f2137dfe148cd44d5", 16), oldBlk1, 0x71),
            SegLoadBeat(2, 7, 1, 4, 0, BigInt("47fc0b86f0f313e704c9d822cf3e57a8", 16), oldBlk1, 0x6a),
          ),
          Seq(
            SegLoadBeat(1, 3, 0, 0, 2, BigInt("0000000000000000ffffffffffffffff", 16), oldBlk0, 0x6d),
          ),
        ),
        expected = Map(
          0x66 -> BigInt("7fffffffffffffff0000000000000000", 16),
          0x68 -> BigInt("fffffffffffffff80000000000000001", 16),
          0x6b -> BigInt("ffffffffffffffff0000000000000002", 16),
          0x6d -> BigInt("0000000000000000000000000000000f", 16),
          0x6e -> oldBlk1,
          0x65 -> oldBlk1,
          0x71 -> oldBlk1,
          0x6a -> oldBlk1,
        ),
      )
    }
  }
}
