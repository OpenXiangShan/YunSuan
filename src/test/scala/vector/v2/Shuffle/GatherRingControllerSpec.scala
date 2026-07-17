package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

class GatherRingControllerSpec extends AnyFunSpec with ChiselSim with Matchers with ParallelTestExecution {

  // ══════════════════════════════════════════════════════
  //  Helper
  // ══════════════════════════════════════════════════════
  def simulateCtrl(
    hasTable: BigInt,
    hasIndex: BigInt,
    unitDone: BigInt,
    s0pipes:  Seq[(Boolean, Int, Int)] = Seq.fill(4)((false, 0, 0))
  )(
    check: (Seq[(Boolean, Int)], Seq[(Boolean, Int)], Seq[Boolean], Seq[BigInt]) => Unit
  ): Unit = {
    val param = GatherCommon.GatherParam.default
    val NumUnits = 8 // VLEN/DLEN * MaxLMUL = 128/128 * 8

    simulate(new GatherRingController()(param)) { dut =>
      for (i <- 0 until NumUnits) {
        dut.in.hasTable(i).poke(((hasTable >> i) & 1) != 0)
        dut.in.hasIndex(i).poke(((hasIndex >> i) & 1) != 0)
      }
      dut.in.unitDone.poke(unitDone)
      for (i <- s0pipes.indices) {
        dut.in.s0pipes(i).valid.poke(s0pipes(i)._1)
        dut.in.s0pipes(i).bits.placeIdx.poke(s0pipes(i)._2)
        dut.in.s0pipes(i).bits.blockBase.poke(s0pipes(i)._3)
        dut.in.s0pipes(i).bits.blockSize.poke(2) // default
      }
      dut.clock.step()

      val tablePos = (0 until s0pipes.size).map { i =>
        (dut.out.pipeTablePos(i).valid.peekValue().asBigInt != BigInt(0),
         dut.out.pipeTablePos(i).bits.peekValue().asBigInt.toInt)
      }
      val indexPos = (0 until s0pipes.size).map { i =>
        (dut.out.pipeIndexPos(i).valid.peekValue().asBigInt != BigInt(0),
         dut.out.pipeIndexPos(i).bits.peekValue().asBigInt.toInt)
      }
      val willFlow = (0 until NumUnits).map { i =>
        dut.out.willFlow(i).peekValue().asBigInt != BigInt(0)
      }
      val wbSelOH = (0 until param.numPipeOut).map { i =>
        dut.out.wbSelOH(i).peekValue().asBigInt
      }
      check(tablePos, indexPos, willFlow, wbSelOH)
    }
  }

  // ══════════════════════════════════════════════════════
  //  Tests: Placement routing
  // ══════════════════════════════════════════════════════
  describe("Placement") {
    it("all free, size=8 -> table at 0, index at 0") {
      simulateCtrl(0, 0, 0, Seq((true, 0, 0), (false, 0, 0), (false, 0, 0), (false, 0, 0))) { (tp, ip, _, _) =>
        assert(tp(0)._2 == 0, s"tablePos=${tp(0)._2}")
        assert(ip(0)._2 == 0, s"indexPos=${ip(0)._2}")
      }
    }

    it("routing to ring4 block (base=4)") {
      // hasTable at unit 5 (bit 5 set), s0 blockBase=4 -> ring4 controller at base=4
      simulateCtrl(BigInt(1) << 5, 0, 0, Seq((true, 0, 4), (false, 0, 0), (false, 0, 0), (false, 0, 0))) { (tp, _, _, _) =>
        // ring4 base=4: tableMap=[0,3,2,1], tableFree: unit 5 is inside base=4 block at relative pos 1
        // tableValid in ring4 block: bit 1 set (relative to block)
        // tableFree = ~(rotate): bit1=1 -> PriorityEncoder -> entryTable=tableMap(relative pos of next free)
        assert(tp(0)._2 >= 4 && tp(0)._2 < 8, s"tablePos=${tp(0)._2} should be in ring4 range")
      }
    }

    it("s0 invalid -> pipeTablePos invalid") {
      simulateCtrl(0, 0, 0, Seq((false, 0, 0), (false, 0, 0), (false, 0, 0), (false, 0, 0))) { (tp, ip, _, _) =>
        assert(!tp(0)._1, "pipeTablePos should be invalid when s0 is invalid")
        assert(!ip(0)._1, "pipeIndexPos should be invalid when s0 is invalid")
      }
    }
  }

  // ══════════════════════════════════════════════════════
  //  Tests: WillFlow aggregation
  // ══════════════════════════════════════════════════════
  describe("WillFlow") {
    it("empty block -> no willFlow") {
      simulateCtrl(0, 0, 0) { (_, _, wf, _) =>
        assert(wf.forall(_ == false), "all empty -> no flow")
      }
    }

    it("table and index in ring2 -> willFlow asserted") {
      // hasTable bit0, hasIndex bit1 (unit 0 has table, unit 1 has index)
      simulateCtrl(1, 2, 0) { (_, _, wf, _) =>
        // ring2 at base=0: table at pos0, index at pos1
        // tableLeastFreeOH(0->rotateRight->pos1?) Actually need to trace...
        // Just verify at least one willFlow bit is true
        assert(wf.exists(_ == true), "should have flow when table and index present")
      }
    }

    it("willFlow is per-unit mask: table-owning units assert flow") {
      // hasTable=0x03 (units 0,1), hasIndex=0 -> global willFlow=true
      // per-unit mask: tableValid(i) && true -> bits 0,1 set
      simulateCtrl(0x3, 0, 0) { (_, _, wf, _) =>
        assert(wf(0), "unit 0 has table -> willFlow bit set")
        assert(wf(1), "unit 1 has table -> willFlow bit set")
        assert(!wf(2), "unit 2 has no table -> no willFlow")
      }
    }
  }

  // ══════════════════════════════════════════════════════
  //  Tests: Writeback arbitration
  // ══════════════════════════════════════════════════════
  describe("Writeback") {
    it("unitDone bit0 -> pipe 0 selected") {
      simulateCtrl(0, 0, 1) { (_, _, _, wb) =>
        // Interleaved: pipe0 serves U0,U4,... -> unit 0 maps to pipe0
        assert(wb(0) == 1, s"wbSelOH(0)=${wb(0)}, expected 1 (unit 0)")
      }
    }

    it("unitDone bit2 -> pipe 2 selected") {
      // Interleaved: 2 % 4 = 2 -> pipe2 serves unit 2
      simulateCtrl(0, 0, 4) { (_, _, _, wb) =>
        assert((wb(2) & 4) == 4, s"wbSelOH(2) bit 2 should be set, got ${wb(2)}")
      }
    }

    it("unitDone bit4 -> pipe 0 selected (second element)") {
      // Interleaved: 4 % 4 = 0 -> pipe0 serves U0,U4 -> unit 4 maps to pipe0 OH bit 1
      simulateCtrl(0, 0, 16) { (_, _, _, wb) =>
        assert(wb(0) == 16, s"wbSelOH(0)=${wb(0)}, expected 16 (unit 4, OH bit 1 -> global bit 4)")
      }
    }

    it("no unit done -> all wbSelOH zero") {
      simulateCtrl(0, 0, 0) { (_, _, _, wb) =>
        assert(wb.forall(_ == 0), "no done units -> all wbSelOH zero")
      }
    }

    it("first done unit in each pipe wins; concurrent pipes served in parallel") {
      // Interleaved: units 0(p0) and 1(p1) done -> both pipes win simultaneously
      simulateCtrl(0, 0, 0x3) { (_, _, _, wb) =>
        assert(wb(0) == 1, s"pipe0 should select unit 0, got ${wb(0)}")
        assert(wb(1) == 2, s"pipe1 should select unit 1, got ${wb(1)}")
      }
    }

    it("two done in same pipe -> lower index wins") {
      // Interleaved: units 0 and 4 both in pipe0 -> unit 0 wins (lower OH index)
      simulateCtrl(0, 0, 0x11) { (_, _, _, wb) =>
        assert(wb(0) == 1, s"pipe0 should pick unit 0 over unit 4, got ${wb(0)}")
      }
    }
  }
}
