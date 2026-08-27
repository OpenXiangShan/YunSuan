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
    counter:      Seq[Int] = Seq.fill(8)(0),
    s1TableValid: Int = 0,
    s1IndexValid: Int = 0
  )(
    check: (Seq[(Boolean, Int)], Seq[(Boolean, Int)], Int, Int) => Unit
  ): Unit = {
    simulate(
      new GatherRingBlockController(size, blockBase)(GatherCommon.GatherParam.default)
    ) { dut =>
      dut.in.unitTableValid.poke(BigInt(tableValid).U(size.W))
      dut.in.unitIndexValid.poke(BigInt(indexValid).U(size.W))
      // no s1 in-flight by default: placement sees unit valid only, clearTable
      // uses unit valid (registered) only.
      dut.in.s1TableValid.poke(BigInt(s1TableValid).U(size.W))
      dut.in.s1IndexValid.poke(BigInt(s1IndexValid).U(size.W))
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
      // The table circulates (unitWillFlow=1): next cycle it is predicted at
      // unit1 (unitTableValidNextCycle = rotateLeft(0x1) = 0x2). Placement must
      // avoid that predicted slot: entryTable resolves to 0, so
      // tablePos(p) = 0 + s1WillFlow - p = 1 - p (mod 4) -> [1,0,3,2].
      // indexPos(p) = entryIndex + p = p.
      it("table at unit0, no index (willFlow=1)") {
        simulateBlock(4, 0, 0x1, 0, pipeSeq(4)) { (t, i, wf, _) =>
          assert(t.map(_._2) == Seq(1, 0, 3, 2), s"tablePos=${t.map(_._2)}")
          assert(i.map(_._2) == Seq(0, 1, 2, 3), s"indexPos=${i.map(_._2)}")
          assert(wf == 0x1, s"table-only unit0 must flow (got ${Integer.toBinaryString(wf)})")
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
    // out.willFlow(i) = (tableValid(i) | indexValid(i)) & blockWillFlow.
    // Values below are the calibrated block-level outcomes for size=4.

    it("empty block -> no flow") {
      simulateBlock(4, 0, 0, 0, boolSeq(4)) { (_, _, wf, _) =>
        assert(wf == 0)
      }
    }

    it("table-only -> the table unit flows") {
      simulateBlock(4, 0, 0x1, 0, boolSeq(4)) { (_, _, wf, _) =>
        assert(wf == 0x1, s"table-only unit0 must flow (got ${Integer.toBinaryString(wf)})")
      }
    }

    it("table at head, index at tail -> both units flow") {
      // T={0}, I={3}: block willFlow=1; mask covers both the table and index units.
      simulateBlock(4, 0, 0x1, 0x8, boolSeq(4)) { (_, _, wf, _) =>
        assert(wf == 0x9, s"mask=${Integer.toBinaryString(wf)}")
      }
    }

    it("table and index co-located -> no flow") {
      // T={0}, I={0}: table already at the index, no circulation needed.
      simulateBlock(4, 0, 0x1, 0x1, boolSeq(4)) { (_, _, wf, _) =>
        assert(wf == 0)
      }
    }

    it("gapped index above table head -> flow stays alive") {
      // T={0,1}, I={1}: index is gapped (bit0 empty), so the contiguous-run
      // mask is empty (unitIndexMaskValidOH=0) and the first table (bit1) is
      // not covered -> flow stays alive (wf = T|I = 0x3). Under the old
      // highest-set-bit mask the index covered the table head (no flow).
      simulateBlock(4, 0, 0x3, 0x2, boolSeq(4)) { (_, _, wf, _) =>
        assert(wf == 0x3, s"mask=${Integer.toBinaryString(wf)}")
      }
    }

    it("all full -> every unit flows") {
      simulateBlock(4, 0, 0xf, 0xf, boolSeq(4)) { (_, _, wf, _) =>
        assert(wf == 0xf, s"mask=${Integer.toBinaryString(wf)}")
      }
    }

    it("mask gated by tableValid | indexValid") {
      // T={0,1}, I={0}: mask covers the union of table and index units.
      simulateBlock(4, 0, 0x3, 0x1, boolSeq(4)) { (_, _, wf, _) =>
        assert(wf == 0x3, s"mask=${Integer.toBinaryString(wf)}")
      }
    }

    it("gapped co-located T&I units -> no flow") {
      // T={0,2}, I={0,2}: the contiguous run from unit 0 ends at bit0 (gap at
      // bit1), which covers the first table (bit0) -> no circulation needed
      // (wf=0). Under the old highest-set-bit mask the index tail (bit2) was
      // not covered -> flow kept alive (mask = 0x5).
      simulateBlock(4, 0, 0x5, 0x5, boolSeq(4)) { (_, _, wf, _) =>
        assert(wf == 0, s"mask=${Integer.toBinaryString(wf)}")
      }
    }

    it("table-only and index-only units both flow") {
      // T={1}, I={0}: mask = T|I = {0,1}.
      simulateBlock(4, 0, 0x2, 0x1, boolSeq(4)) { (_, _, wf, _) =>
        assert(wf == 0x3, s"mask=${Integer.toBinaryString(wf)}")
      }
    }

    it("s1 index not yet placed -> no flow") {
      // Units 0-2 hold both a table and an index (0x7). Unit 3's table+index are
      // in s1 (0x8), NOT yet registered. The first table is at unit 2 (0x4); if
      // the block flows now, that table would circulate past unit 3 before its
      // index is filled (unconsumed), breaking the flow. So willFlow must be 0.
      // (unit-only view: free table at 3 sits just above the highest registered
      // index at 2 -> no circulation needed yet.)
      simulateBlock(4, 0, 0x7, 0x7, boolSeq(4), s1TableValid = 0x8, s1IndexValid = 0x8) { (_, _, wf, _) =>
        assert(wf == 0, s"willFlow must be 0 (got ${Integer.toBinaryString(wf)})")
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

