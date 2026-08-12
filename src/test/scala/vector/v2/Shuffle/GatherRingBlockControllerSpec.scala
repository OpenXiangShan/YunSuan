package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

import yunsuan.util.Cli.EmitFst

class GatherRingBlockControllerSpec extends AnyFunSpec with ChiselSim with Matchers with ParallelTestExecution with EmitFst {

  // ── Helper: single-cycle combinational check ──────────────────────────────
  // Returns per-pipe (valid, pos) for tables and indices, plus the willFlow mask
  // and the clearTable mask (both size bits).
  def simulateBlock(
    size:         Int,
    blockBase:    Int,
    tableValid:   Int,
    indexValid:   Int,
    pipePlaceIdx: Seq[(Boolean, Int)],
    counter:      Seq[Int] = Seq.fill(8)(0)
  )(
    check: (Seq[(Boolean, Int)], Seq[(Boolean, Int)], Int, Int) => Unit
  ): Unit = {
    simulate(
      new GatherRingBlockController(size, blockBase)(GatherCommon.GatherParam.default)
    ) { dut =>
      dut.in.tableValid.poke(BigInt(tableValid).U(size.W))
      dut.in.indexValid.poke(BigInt(indexValid).U(size.W))
      for (i <- pipePlaceIdx.indices) {
        dut.in.pipePlaceIdx(i).valid.poke(pipePlaceIdx(i)._1)
        dut.in.pipePlaceIdx(i).bits.poke(pipePlaceIdx(i)._2)
      }
      for (i <- 0 until size) {
        dut.in.counter(i).poke(if (i < counter.length) counter(i) else 0)
      }
      dut.clock.step()
      val tablePos = (0 until dut.in.pipePlaceIdx.length).map(
        i => (dut.out.tablePos(i).valid.peekValue().asBigInt != BigInt(0), dut.out.tablePos(i).bits.peekValue().asBigInt.toInt)
      )
      val indexPos = (0 until dut.in.pipePlaceIdx.length).map(
        i => (dut.out.indexPos(i).valid.peekValue().asBigInt != BigInt(0), dut.out.indexPos(i).bits.peekValue().asBigInt.toInt)
      )
      val willFlowMask = dut.out.willFlow.peekValue().asBigInt.toInt
      val clearTableMask = dut.out.clearTable.peekValue().asBigInt.toInt
      check(tablePos, indexPos, willFlowMask, clearTableMask)
    }
  }

  def boolSeq(n: Int): Seq[(Boolean, Int)] = Seq.fill(n)((false, 0))
  def pipeSeq(n: Int): Seq[(Boolean, Int)] = Seq.tabulate(n)(i => (true, i))

  // ── Placement ─────────────────────────────────────────────────────────────

  describe("placement") {

    describe("empty block (willFlow=0)") {
      // entryTable=0, entryIndex=0; tables placed in tableMap order backwards,
      // indices in linear order forwards.
      it("size=2") {
        simulateBlock(2, 0, 0, 0, pipeSeq(2)) { (t, i, _, _) =>
          assert(t.filter(_._1).map(_._2) == Seq(0, 1), s"tablePos=${t.filter(_._1).map(_._2)}")
          assert(i.filter(_._1).map(_._2) == Seq(0, 1), s"indexPos=${i.filter(_._1).map(_._2)}")
        }
      }
      it("size=4") {
        simulateBlock(4, 0, 0, 0, pipeSeq(4)) { (t, i, _, _) =>
          assert(t.map(_._2) == Seq(0, 3, 2, 1), s"tablePos=${t.map(_._2)}")
          assert(i.map(_._2) == Seq(0, 1, 2, 3), s"indexPos=${i.map(_._2)}")
        }
      }
      it("size=8") {
        // only NumPipeIn=4 placement ports: pokes placeIdx 0..3, positions mod 8.
        simulateBlock(8, 0, 0, 0, pipeSeq(4)) { (t, i, _, _) =>
          assert(t.filter(_._1).map(_._2) == Seq(0, 7, 6, 5), s"tablePos=${t.filter(_._1).map(_._2)}")
          assert(i.filter(_._1).map(_._2) == Seq(0, 1, 2, 3), s"indexPos=${i.filter(_._1).map(_._2)}")
        }
      }
      it("non-zero blockBase offsets positions") {
        simulateBlock(4, 4, 0, 0, pipeSeq(4)) { (t, i, _, _) =>
          assert(t.map(_._2) == Seq(4, 7, 6, 5), s"tablePos=${t.map(_._2)}")
          assert(i.map(_._2) == Seq(4, 5, 6, 7), s"indexPos=${i.map(_._2)}")
        }
      }
      it("LMUL group fills the block: distinct tables, distinct indices, every unit covered") {
        simulateBlock(4, 0, 0, 0, pipeSeq(4)) { (t, i, _, _) =>
          val tPos = t.map(_._2); val iPos = i.map(_._2)
          assert(tPos.distinct.length == 4, s"table positions distinct: $tPos")
          assert(iPos.distinct.length == 4, s"index positions distinct: $iPos")
          assert((tPos ++ iPos).forall(p => p < 4), s"positions within block: ${tPos ++ iPos}")
          assert(tPos.toSet == (0 until 4).toSet && iPos.toSet == (0 until 4).toSet,
            s"tables+indices cover the block: t=$tPos i=$iPos")
        }
      }
    }

    describe("occupied block") {
      // tableValid=0x1 (unit0 holds a table), no index.
      // block willFlow=1 (a table circulates); entryTable = tableMap(1) = 3.
      // tablePos(p) = entryTable + willFlow - p = 3 + 1 - p (mod 4) -> uses the
      // slot about to be vacated by the circulating table (position 0).
      // indexPos(p) = entryIndex + p = p.
      it("table at unit0, no index (willFlow=1)") {
        simulateBlock(4, 0, 0x1, 0, pipeSeq(4)) { (t, i, wf, _) =>
          assert(t.map(_._2) == Seq(0, 3, 2, 1), s"tablePos=${t.map(_._2)}")
          assert(i.map(_._2) == Seq(0, 1, 2, 3), s"indexPos=${i.map(_._2)}")
          assert(wf == 0, s"mask empty: no unit holds both T and I (got ${Integer.toBinaryString(wf)})")
        }
      }
      it("placeIdx wraps within the block") {
        // size=4, empty, single pipe at placeIdx=3: tablePos = 0 - 3 (mod 4) = 1.
        simulateBlock(4, 0, 0, 0, Seq((true, 3), (false, 0), (false, 0), (false, 0))) { (t, i, _, _) =>
          assert(t.head._2 == 1, s"tablePos=${t.head._2}")
          assert(i.head._2 == 3, s"indexPos=${i.head._2}")
        }
      }
    }
  }

  // ── willFlow ──────────────────────────────────────────────────────────────

  describe("willFlow") {
    // out.willFlow(i) = tableValid(i) & indexValid(i) & blockWillFlow.
    // Values below are the calibrated block-level outcomes for size=4.

    it("empty block -> no flow") {
      simulateBlock(4, 0, 0, 0, boolSeq(4)) { (_, _, wf, _) =>
        assert(wf == 0)
      }
    }

    it("table-only -> block flows but mask empty (no T&I unit)") {
      simulateBlock(4, 0, 0x1, 0, boolSeq(4)) { (_, _, wf, _) =>
        assert(wf == 0, s"block willFlow=1 but (T&I)=0, mask must be 0 (got ${Integer.toBinaryString(wf)})")
      }
    }

    it("table at head, index at tail -> block flows") {
      // T={0}, I={3}: block willFlow=1; mask empty because no unit has both.
      simulateBlock(4, 0, 0x1, 0x8, boolSeq(4)) { (_, _, wf, _) =>
        assert(wf == 0)
      }
    }

    it("table and index co-located -> no flow") {
      // T={0}, I={0}: table already at the index, no circulation needed.
      simulateBlock(4, 0, 0x1, 0x1, boolSeq(4)) { (_, _, wf, _) =>
        assert(wf == 0)
      }
    }

    it("index above table head -> no flow") {
      // T={0,1}, I={1}: highest index at pos1, table head free slot rotated to pos1.
      simulateBlock(4, 0, 0x3, 0x2, boolSeq(4)) { (_, _, wf, _) =>
        assert(wf == 0)
      }
    }

    it("all full -> every unit flows") {
      simulateBlock(4, 0, 0xf, 0xf, boolSeq(4)) { (_, _, wf, _) =>
        assert(wf == 0xf, s"mask=${Integer.toBinaryString(wf)}")
      }
    }

    it("mask gated by tableValid & indexValid") {
      // T={0,1}, I={0}: only unit0 holds both -> mask bit0 only.
      simulateBlock(4, 0, 0x3, 0x1, boolSeq(4)) { (_, _, wf, _) =>
        assert(wf == 0x1, s"mask=${Integer.toBinaryString(wf)}")
      }
    }

    it("mask follows co-located T&I units") {
      // T={0,2}, I={0,2}: units 0,2 hold both -> mask = 0x5.
      simulateBlock(4, 0, 0x5, 0x5, boolSeq(4)) { (_, _, wf, _) =>
        assert(wf == 0x5, s"mask=${Integer.toBinaryString(wf)}")
      }
    }

    it("table-only and index-only units never flow") {
      // T={2}, I={1}: no co-located unit, block willFlow=0 too.
      simulateBlock(4, 0, 0x2, 0x1, boolSeq(4)) { (_, _, wf, _) =>
        assert(wf == 0)
      }
    }
  }

  // ── clearTable ────────────────────────────────────────────────────────────

  describe("clearTable") {
    // clearTable(i) = AND_{j>i}(counter(j) < j-i), empty set (last unit) -> true.
    // The unit-level hasTable clear additionally requires passedEntry, so the
    // last unit's always-true clearTable is harmless (last unit is never entry).

    it("all counters zero -> every unit's table clearable") {
      simulateBlock(4, 0, 0xf, 0xf, boolSeq(4), Seq(0, 0, 0, 0)) { (_, _, _, ct) =>
        assert(ct == 0xf, s"clearTable=${Integer.toBinaryString(ct)}")
      }
    }

    it("all fresh (counter=3) -> only the last unit clearable") {
      simulateBlock(4, 0, 0xf, 0xf, boolSeq(4), Seq(3, 3, 3, 3)) { (_, _, _, ct) =>
        assert(ct == 0x8, s"clearTable=${Integer.toBinaryString(ct)}")
      }
    }

    it("decreasing 3,2,1,0 -> units 2 and 3 clearable") {
      simulateBlock(4, 0, 0xf, 0xf, boolSeq(4), Seq(3, 2, 1, 0)) { (_, _, _, ct) =>
        assert(ct == 0xc, s"clearTable=${Integer.toBinaryString(ct)}")
      }
    }

    it("increasing 0,1,2,3 -> only the last unit clearable") {
      simulateBlock(4, 0, 0xf, 0xf, boolSeq(4), Seq(0, 1, 2, 3)) { (_, _, _, ct) =>
        assert(ct == 0x8, s"clearTable=${Integer.toBinaryString(ct)}")
      }
    }

    it("0,0,1,2 -> units 0 and 3 clearable") {
      simulateBlock(4, 0, 0xf, 0xf, boolSeq(4), Seq(0, 0, 1, 2)) { (_, _, _, ct) =>
        assert(ct == 0x9, s"clearTable=${Integer.toBinaryString(ct)}")
      }
    }
  }
}

