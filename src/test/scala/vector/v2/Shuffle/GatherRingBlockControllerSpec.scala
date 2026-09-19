package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

import yunsuan.util.Cli.EmitFst

class GatherRingBlockControllerSpec extends AnyFunSpec with ChiselSim with Matchers with ParallelTestExecution with EmitFst {

  // ── Helper: combinational check with optional latch phase ───────────────
  // Returns per-pipe (valid, pos) for tables and indices, plus the willFlow mask,
  // the clearTable mask, and the allTableValid mask (all size bits).
  // fieldNum < 0 => drive `size` (legacy full-block behavior). phase2 re-pokes
  // occupancy (tableValid, indexValid, counter) and steps once more, so latch
  // (allTableHasFilled / indexHasAllValid) set->clear sequences can be observed.
  def simulateBlock(
    size:         Int,
    blockBase:    Int,
    tableValid:   Int,
    indexValid:   Int,
    pipePlaceIdx: Seq[(Boolean, Int)],
    counter:      Seq[Int] = Seq.fill(8)(0),
    s1TableValid: Int = 0,
    s1IndexValid: Int = 0,
    fieldNum:     Int = -1,
    phase2:       Option[(Int, Int, Seq[Int])] = None
  )(
    check: (Seq[(Boolean, Int)], Seq[(Boolean, Int)], Int, Int, Int) => Unit
  ): Unit = {
    simulate(
      new GatherRingBlockController(size, blockBase)(GatherCommon.GatherParam.default)
    ) { dut =>
      def pokeOccupancy(tv: Int, iv: Int, ctr: Seq[Int]): Unit = {
        dut.in.unitTableValid.poke(BigInt(tv).U(size.W))
        dut.in.unitIndexValid.poke(BigInt(iv).U(size.W))
        for (i <- 0 until size) {
          dut.in.counter(i).poke(if (i < ctr.length) ctr(i) else 0)
        }
      }
      pokeOccupancy(tableValid, indexValid, counter)
      // no s1 in-flight by default: placement sees unit valid only, clearTable
      // uses unit valid (registered) only.
      dut.in.s1TableValid.poke(BigInt(s1TableValid).U(size.W))
      dut.in.s1IndexValid.poke(BigInt(s1IndexValid).U(size.W))
      dut.in.blockFieldNum.poke(BigInt(if (fieldNum < 0) size else fieldNum).U)
      for (i <- pipePlaceIdx.indices) {
        dut.in.pipePlaceIdx(i).valid.poke(pipePlaceIdx(i)._1)
        dut.in.pipePlaceIdx(i).bits.poke(pipePlaceIdx(i)._2)
      }
      dut.clock.step()
      phase2.foreach { case (tv, iv, ctr) =>
        pokeOccupancy(tv, iv, ctr)
        dut.clock.step()
      }
      val tablePos = (0 until dut.in.pipePlaceIdx.length).map(
        i => (dut.out.tablePos(i).valid.peekValue().asBigInt != BigInt(0), dut.out.tablePos(i).bits.peekValue().asBigInt.toInt)
      )
      val indexPos = (0 until dut.in.pipePlaceIdx.length).map(
        i => (dut.out.indexPos(i).valid.peekValue().asBigInt != BigInt(0), dut.out.indexPos(i).bits.peekValue().asBigInt.toInt)
      )
      val willFlowMask = dut.out.willFlow.peekValue().asBigInt.toInt
      val clearTableMask = dut.out.clearTable.peekValue().asBigInt.toInt
      val allTableValidMask = dut.out.allTableValid.peekValue().asBigInt.toInt
      check(tablePos, indexPos, willFlowMask, clearTableMask, allTableValidMask)
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
        simulateBlock(2, 0, 0, 0, pipeSeq(2)) { (t, i, _, _, _) =>
          assert(t.filter(_._1).map(_._2) == Seq(0, 1), s"tablePos=${t.filter(_._1).map(_._2)}")
          assert(i.filter(_._1).map(_._2) == Seq(0, 1), s"indexPos=${i.filter(_._1).map(_._2)}")
        }
      }
      it("size=4") {
        simulateBlock(4, 0, 0, 0, pipeSeq(4)) { (t, i, _, _, _) =>
          assert(t.map(_._2) == Seq(0, 3, 2, 1), s"tablePos=${t.map(_._2)}")
          assert(i.map(_._2) == Seq(0, 1, 2, 3), s"indexPos=${i.map(_._2)}")
        }
      }
      it("size=8") {
        // only NumPipeIn=4 placement ports: pokes placeIdx 0..3, positions mod 8.
        simulateBlock(8, 0, 0, 0, pipeSeq(4)) { (t, i, _, _, _) =>
          assert(t.filter(_._1).map(_._2) == Seq(0, 7, 6, 5), s"tablePos=${t.filter(_._1).map(_._2)}")
          assert(i.filter(_._1).map(_._2) == Seq(0, 1, 2, 3), s"indexPos=${i.filter(_._1).map(_._2)}")
        }
      }
      it("non-zero blockBase offsets positions") {
        simulateBlock(4, 4, 0, 0, pipeSeq(4)) { (t, i, _, _, _) =>
          assert(t.map(_._2) == Seq(4, 7, 6, 5), s"tablePos=${t.map(_._2)}")
          assert(i.map(_._2) == Seq(4, 5, 6, 7), s"indexPos=${i.map(_._2)}")
        }
      }
      it("LMUL group fills the block: distinct tables, distinct indices, every unit covered") {
        simulateBlock(4, 0, 0, 0, pipeSeq(4)) { (t, i, _, _, _) =>
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
      // tableValid=0x1 (unit0 holds a table), no index. Under the new
      // unitWillFlow semantics ((!A)&&B, B = first-table-unit index valid) a
      // table with NO index does not flow: the first table's unit has no valid
      // index, so flow stays off (unitWillFlow=0). Placement therefore does
      // NOT rotate: unitTableValidNextCycle = unitTableValid = 0x1, and
      // s1WillFlow is also 0 (no index), so tablePos(p) = entryTable - p with
      // entryTable = 3 (tableMap[3] = 1, the first free slot) -> [3,2,1,0].
      it("table at unit0, no index (willFlow=0)") {
        simulateBlock(4, 0, 0x1, 0, pipeSeq(4)) { (t, i, wf, _, _) =>
          assert(t.map(_._2) == Seq(3, 2, 1, 0), s"tablePos=${t.map(_._2)}")
          assert(i.map(_._2) == Seq(0, 1, 2, 3), s"indexPos=${i.map(_._2)}")
          assert(wf == 0, s"table-only unit0 must NOT flow (got ${Integer.toBinaryString(wf)})")
        }
      }
      it("placeIdx wraps within the block") {
        // size=4, empty, single pipe at placeIdx=3: tablePos = 0 - 3 (mod 4) = 1.
        simulateBlock(4, 0, 0, 0, Seq((true, 3), (false, 0), (false, 0), (false, 0))) { (t, i, _, _, _) =>
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
      simulateBlock(4, 0, 0, 0, boolSeq(4)) { (_, _, wf, _, _) =>
        assert(wf == 0)
      }
    }

    it("table-only -> no flow (no index)") {
      // T={0}, I={}: the first table's unit has no valid index, so under the
      // new (!A)&&B semantics B=0 -> unitWillFlow=0 -> no flow.
      simulateBlock(4, 0, 0x1, 0, boolSeq(4)) { (_, _, wf, _, _) =>
        assert(wf == 0, s"table-only unit0 must NOT flow (got ${Integer.toBinaryString(wf)})")
      }
    }

    it("table at head, index at tail -> no flow (not co-located)") {
      // T={0}, I={3}: the first table (unit0) is NOT on a unit with a valid
      // index (index is at unit3), so under (!A)&&B semantics B=0 -> no flow.
      simulateBlock(4, 0, 0x1, 0x8, boolSeq(4)) { (_, _, wf, _, _) =>
        assert(wf == 0, s"mask=${Integer.toBinaryString(wf)}")
      }
    }

    it("table and index co-located -> no flow") {
      // T={0}, I={0}: table already at the index, no circulation needed.
      simulateBlock(4, 0, 0x1, 0x1, boolSeq(4)) { (_, _, wf, _, _) =>
        assert(wf == 0)
      }
    }

    it("gapped index above table head -> flow stays alive") {
      // T={0,1}, I={1}: index is gapped (bit0 empty), so the contiguous-run
      // mask is empty (unitIndexMaskValidOH=0) and the first table (bit1) is
      // not covered -> flow stays alive (wf = T|I = 0x3). Under the old
      // highest-set-bit mask the index covered the table head (no flow).
      simulateBlock(4, 0, 0x3, 0x2, boolSeq(4)) { (_, _, wf, _, _) =>
        assert(wf == 0x3, s"mask=${Integer.toBinaryString(wf)}")
      }
    }

    it("all full -> every unit flows") {
      simulateBlock(4, 0, 0xf, 0xf, boolSeq(4)) { (_, _, wf, _, _) =>
        assert(wf == 0xf, s"mask=${Integer.toBinaryString(wf)}")
      }
    }

    it("mask gated by tableValid | indexValid -> no flow") {
      // T={0,1}, I={0}: the first table (unit0) lies at the index-region
      // boundary (bit0) AND unit0 holds a valid index, so both A and B hold
      // -> !(A&&B) under the (!A)&&B form means flow stops (wf=0).
      simulateBlock(4, 0, 0x3, 0x1, boolSeq(4)) { (_, _, wf, _, _) =>
        assert(wf == 0, s"mask=${Integer.toBinaryString(wf)}")
      }
    }

    it("gapped co-located T&I units -> no flow") {
      // T={0,2}, I={0,2}: the contiguous run from unit 0 ends at bit0 (gap at
      // bit1), which covers the first table (bit0) -> no circulation needed
      // (wf=0). Under the old highest-set-bit mask the index tail (bit2) was
      // not covered -> flow kept alive (mask = 0x5).
      simulateBlock(4, 0, 0x5, 0x5, boolSeq(4)) { (_, _, wf, _, _) =>
        assert(wf == 0, s"mask=${Integer.toBinaryString(wf)}")
      }
    }

    it("table-only and index-only units -> no flow (not co-located)") {
      // T={1}, I={0}: the first table (unit1) is NOT on the unit holding the
      // valid index (unit0), so B=0 -> no flow (wf=0).
      simulateBlock(4, 0, 0x2, 0x1, boolSeq(4)) { (_, _, wf, _, _) =>
        assert(wf == 0, s"mask=${Integer.toBinaryString(wf)}")
      }
    }

    it("qualified s1 index completes the block -> flow") {
      // Units 0-2 hold both a table and an index (0x7). Unit 3's table+index are
      // completing in s1 (0x8). s1IndexValid is already qualified by the
      // top-level (including the EI16E8 gather/rvs3 pair state), so the existing
      // tables may flow while the final index is registered on the same edge.
      simulateBlock(4, 0, 0x7, 0x7, boolSeq(4), s1TableValid = 0x8, s1IndexValid = 0x8) { (_, _, wf, _, _) =>
        assert(wf == 0x7, s"willFlow must be 111 (got ${Integer.toBinaryString(wf)})")
      }
    }
  }

  // ── clearTable ────────────────────────────────────────────────────────────

  describe("clearTable") {
    // clearTable(i) = AND_{j>i}(counter(j) < j-i), empty set (last unit) -> true.
    // The unit-level hasTable clear additionally requires passedEntry, so the
    // last unit's always-true clearTable is harmless (last unit is never entry).

    it("all counters zero -> every unit's table clearable") {
      simulateBlock(4, 0, 0xf, 0xf, boolSeq(4), Seq(0, 0, 0, 0)) { (_, _, _, ct, _) =>
        assert(ct == 0xf, s"clearTable=${Integer.toBinaryString(ct)}")
      }
    }

    it("all fresh (counter=3) -> only the last unit clearable") {
      simulateBlock(4, 0, 0xf, 0xf, boolSeq(4), Seq(3, 3, 3, 3)) { (_, _, _, ct, _) =>
        assert(ct == 0x8, s"clearTable=${Integer.toBinaryString(ct)}")
      }
    }

    it("decreasing 3,2,1,0 -> units 2 and 3 clearable") {
      simulateBlock(4, 0, 0xf, 0xf, boolSeq(4), Seq(3, 2, 1, 0)) { (_, _, _, ct, _) =>
        assert(ct == 0xc, s"clearTable=${Integer.toBinaryString(ct)}")
      }
    }

    it("increasing 0,1,2,3 -> only the last unit clearable") {
      simulateBlock(4, 0, 0xf, 0xf, boolSeq(4), Seq(0, 1, 2, 3)) { (_, _, _, ct, _) =>
        assert(ct == 0x8, s"clearTable=${Integer.toBinaryString(ct)}")
      }
    }

    it("0,0,1,2 -> units 0 and 3 clearable") {
      simulateBlock(4, 0, 0xf, 0xf, boolSeq(4), Seq(0, 0, 1, 2)) { (_, _, _, ct, _) =>
        assert(ct == 0x9, s"clearTable=${Integer.toBinaryString(ct)}")
      }
    }
  }

  // ── Hole blocks (segment fieldNum < size) ───────────────────────────────
  // Tables fill in the usual reversed tableMap order (0, size-1, ..., 1), so with
  // only f tables the table-less units are the LOW units 1..size-f (fill-order
  // tail). Indices fill linearly (entryIndex + placeIdx), so the index-less units
  // are the HIGH units f..size-1 — the "hole" units that never compute/wake/wb.
  // Index-hole forcing (hole units read as index-valid) is applied by
  // GatherRingController in the system; here it is emulated by poking the hole
  // bits of unitIndexValid directly.

  describe("hole block (fieldNum < size)") {

    describe("placement keeps the existing fill orders") {

      it("seg3 (size=4, f=3): tables reversed {0,3,2}, indices linear {0,1,2}") {
        simulateBlock(4, 0, 0, 0, pipeSeq(3) :+ (false, 0), fieldNum = 3) { (t, i, _, _, _) =>
          assert(t.filter(_._1).map(_._2) == Seq(0, 3, 2), s"tablePos=${t.map(_._2)}")
          assert(i.filter(_._1).map(_._2) == Seq(0, 1, 2), s"indexPos=${i.map(_._2)}")
        }
      }

      it("seg5 (size=8, f=5): tables reversed {0,7,6,5}, indices linear {0,1,2,3}") {
        simulateBlock(8, 0, 0, 0, pipeSeq(4), fieldNum = 5) { (t, i, _, _, _) =>
          assert(t.filter(_._1).map(_._2) == Seq(0, 7, 6, 5), s"tablePos=${t.map(_._2)}")
          assert(i.filter(_._1).map(_._2) == Seq(0, 1, 2, 3), s"indexPos=${i.map(_._2)}")
        }
      }
    }

    describe("flow / completion criteria (forcing emulated by pokes)") {

      it("seg3: forced index-hole flows with the block (willFlow all bits)") {
        simulateBlock(4, 0, 0xd, 0xf, boolSeq(4), Seq(3, 3, 3, 0), fieldNum = 3) { (_, _, wf, _, _) =>
          assert(wf == 0xf, s"willFlow=${Integer.toBinaryString(wf)}")
        }
      }

      it("seg3 empty block: no flow (fieldNum must not fake occupancy)") {
        simulateBlock(4, 0, 0, 0, boolSeq(4), fieldNum = 3) { (_, _, wf, _, _) =>
          assert(wf == 0x0, s"willFlow=${Integer.toBinaryString(wf)}")
        }
      }

      it("seg3 clearTable: forced index-hole (unit3) counts as done") {
        simulateBlock(4, 0, 0xd, 0xf, boolSeq(4), Seq(0, 0, 0, 0), fieldNum = 3) { (_, _, _, ct, _) =>
          assert(ct == 0xf, s"clearTable=${Integer.toBinaryString(ct)}")
        }
      }

      it("seg3 clearTable without forcing: unit3 blocks clearing") {
        simulateBlock(4, 0, 0xd, 0x7, boolSeq(4), Seq(0, 0, 0, 0), fieldNum = 3) { (_, _, _, ct, _) =>
          assert(ct == 0x8, s"clearTable=${Integer.toBinaryString(ct)}")
        }
      }

      it("seg3: allTableValid sets once the 3 real tables are placed (table-hole unit1 injected)") {
        simulateBlock(4, 0, 0xd, 0xf, boolSeq(4), Seq(3, 3, 3, 0), fieldNum = 3) { (_, _, _, _, atv) =>
          assert(atv == 0xf, s"allTableValid=${Integer.toBinaryString(atv)}")
        }
      }

      it("seg3 control: same occupancy with f=4 does NOT set allTableValid") {
        simulateBlock(4, 0, 0xd, 0xf, boolSeq(4), Seq(3, 3, 3, 3)) { (_, _, _, _, atv) =>
          assert(atv == 0x0, s"allTableValid=${Integer.toBinaryString(atv)}")
        }
      }

      it("seg3: allTableValid clears once every counter hits 0 (block done)") {
        simulateBlock(4, 0, 0xd, 0xf, boolSeq(4), Seq(3, 3, 3, 0), fieldNum = 3,
          phase2 = Some((0xd, 0xf, Seq(0, 0, 0, 0)))) { (_, _, _, _, atv) =>
          assert(atv == 0x0, s"allTableValid=${Integer.toBinaryString(atv)}")
        }
      }

      it("seg5 (size=8, f=5): allTableValid sets with tables at {0,4,5,6,7}") {
        simulateBlock(8, 0, 0xf1, 0xff, boolSeq(4), Seq(3, 3, 3, 3, 3, 0, 0, 0), fieldNum = 5) { (_, _, _, _, atv) =>
          assert(atv == 0xff, s"allTableValid=${Integer.toBinaryString(atv)}")
        }
      }
    }
  }
}

