package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

import yunsuan.util.Cli.EmitFst

class GatherRingControllerSpec extends AnyFunSpec with ChiselSim with Matchers with ParallelTestExecution with EmitFst {

  // ══════════════════════════════════════════════════════
  //  Helper
  // ══════════════════════════════════════════════════════
  def simulateCtrl(
    hasTable: BigInt,
    hasIndex: BigInt,
    wbValid : BigInt,
    s0pipes :  Seq[(Boolean, Int, Int)] = Seq.fill(4)((false, 0, 0)),
    unitBlockSize: Int = 2
  )(
    check: (Seq[(Boolean, Int)], Seq[(Boolean, Int)], Seq[Boolean], Seq[BigInt]) => Unit
  ): Unit = {
    val param = GatherCommon.GatherParam.default
    val NumUnits = param.numUnits

    simulate(new GatherRingController()(param)) { dut =>
      dut.in.acceptedWake.poke(0)
      dut.in.flush.poke(0)
      for (i <- 0 until NumUnits) {
        dut.in.hasTable(i).poke(((hasTable >> i) & 1) != 0)
        dut.in.hasIndex(i).poke(((hasIndex >> i) & 1) != 0)
        dut.in.blockSize(i).poke(unitBlockSize)
        dut.in.blockFieldNum(i).poke(unitBlockSize)
        dut.in.hasIndexNext(i).poke(false)
        dut.in.counter(i).poke(0)
      }
      dut.in.wbValid.poke(wbValid)
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
        ((dut.out.willFlow.peekValue().asBigInt >> i) & 1) != 0
      }
      val wbSelOH = (0 until param.numPipeOut).map { i =>
        dut.out.wbSelOH(i).peekValue().asBigInt
      }
      check(tablePos, indexPos, willFlow, wbSelOH)
    }
  }

  // Extended helper that also exposes blockSize and dbgPipeCtrlValid
  def simulateCtrlEx(
    blockSize: Int,
    blockBase: Int,
    s0valid:  Boolean = true
  )(
    check: Int => Unit  // receives dbgPipeCtrlValid(0) bitmap
  ): Unit = {
    val param = GatherCommon.GatherParam.default
    val NumUnits = param.numUnits

    simulate(new GatherRingController()(param)) { dut =>
      dut.in.acceptedWake.poke(0)
      dut.in.flush.poke(0)
      for (i <- 0 until NumUnits) {
        dut.in.hasTable(i).poke(false)
        dut.in.hasIndex(i).poke(false)
        dut.in.blockSize(i).poke(blockSize)
        dut.in.blockFieldNum(i).poke(blockSize)
        dut.in.hasIndexNext(i).poke(false)
        dut.in.counter(i).poke(0)
      }
      dut.in.wbValid.poke(0)
      dut.in.s0pipes(0).valid.poke(s0valid)
      dut.in.s0pipes(0).bits.placeIdx.poke(0)
      dut.in.s0pipes(0).bits.blockBase.poke(blockBase)
      dut.in.s0pipes(0).bits.blockSize.poke(blockSize)
      for (i <- 1 until param.numPipeIn) {
        dut.in.s0pipes(i).valid.poke(false)
      }
      dut.clock.step()

      val dbg = dut.out.dbgPipeCtrlValid(0).peekValue().asBigInt.toInt
      check(dbg)
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

    it("table and index in ring2, co-located -> flow") {
      // hasTable=0x3, hasIndex=0x3: units 0,1 hold both table and index (block full).
      simulateCtrl(0x3, 0x3, 0) { (_, _, wf, _) =>
        assert(wf(0) && wf(1), s"co-located T&I units must flow: $wf")
      }
    }

    it("willFlow is per-unit mask: only T&I units assert flow") {
      // hasTable=0x3, hasIndex=0x3 (units 0,1 hold both) -> mask bits 0,1 set.
      simulateCtrl(0x3, 0x3, 0) { (_, _, wf, _) =>
        assert(wf(0) && wf(1), s"units 0,1 hold T&I -> flow bits set: $wf")
        assert(!wf(2), "unit 2 holds nothing -> no flow")
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

  // ══════════════════════════════════════════════════════
  //  Tests: block controller mutual exclusion
  // ══════════════════════════════════════════════════════
  describe("Block controller mutual exclusion") {
    it("LMUL=1 (blockSize=1) -> no block controller active") {
      simulateCtrlEx(blockSize = 1, blockBase = 0) { dbg =>
        // dbg bits: [ring8, ring4, ring2]
        assert(dbg == 0, s"LMUL=1: no controller should be active, got dbg=0b${dbg.toBinaryString}")
      }
    }

    it("LMUL=2 (blockSize=2) -> only ring2 active") {
      simulateCtrlEx(blockSize = 2, blockBase = 0) { dbg =>
        assert(dbg == 1, s"LMUL=2: only ring2 should be active (0b001), got dbg=0b${dbg.toBinaryString}")
      }
    }

    it("LMUL=4 (blockSize=4) -> only ring4 active") {
      simulateCtrlEx(blockSize = 4, blockBase = 0) { dbg =>
        assert(dbg == 2, s"LMUL=4: only ring4 should be active (0b010), got dbg=0b${dbg.toBinaryString}")
      }
    }

    it("LMUL=8 (blockSize=8) -> only ring8 active") {
      simulateCtrlEx(blockSize = 8, blockBase = 0) { dbg =>
        assert(dbg == 4, s"LMUL=8: only ring8 should be active (0b100), got dbg=0b${dbg.toBinaryString}")
      }
    }

    it("LMUL=4 non-zero blockBase -> only ring4 active") {
      simulateCtrlEx(blockSize = 4, blockBase = 4) { dbg =>
        assert(dbg == 2, s"LMUL=4 base=4: only ring4 should be active, got dbg=0b${dbg.toBinaryString}")
      }
    }
  }
  // ══════════════════════════════════════════════════════
  //  Tests: unitAllTableValid (vperm wakeup gate)
  // ══════════════════════════════════════════════════════
  describe("unitAllTableValid") {
    it("m4 unit not driven by ring2 s1 of another block size") {
      val param = GatherCommon.GatherParam.default
      val NumUnits = param.numUnits
      simulate(new GatherRingController()(param)) { dut =>
        dut.in.acceptedWake.poke(0)
        dut.in.flush.poke(0)
        // U0/U2/U3: m4 block, only U0's table placed (block incomplete).
        // U1: m2 block with a registered table. An m2 s1 pipe targets U0:
        // ring2's allTableValid (U0-1) is raised via the in-flight s1 table,
        // but U0 is m4 and its block is not complete -> U0 must stay 0 (the
        // ring2 contribution is masked out by U0's blockSize).
        val blockSize = Seq(4, 2, 4, 4) ++ Seq.fill(NumUnits - 4)(0)
        for (i <- 0 until NumUnits) {
          dut.in.hasTable(i).poke(i < 2) // U0, U1 registered tables
          dut.in.hasIndex(i).poke(false)
          dut.in.blockSize(i).poke(blockSize(i))
          dut.in.blockFieldNum(i).poke(if (blockSize(i) == 0) 0 else blockSize(i))
          dut.in.hasIndexNext(i).poke(false)
          dut.in.counter(i).poke(0)
        }
        for (i <- 0 until param.numPipeIn) {
          val s1 = i == 0
          dut.in.s1pipes(i).valid.poke(s1)
          dut.in.s1pipes(i).bits.indexValid.poke(s1)
          dut.in.s1pipes(i).bits.tablePos.poke(if (s1) 0 else 0)
          dut.in.s1pipes(i).bits.indexPos.poke(if (s1) 0 else 0)
          dut.in.s1pipes(i).bits.blockSize.poke(if (s1) 2 else 0)
        }
        dut.in.wbValid.poke(0)
        dut.clock.step()
        val v = (0 until NumUnits).map(i => dut.out.unitAllTableValid(i).peekBoolean())
        assert(!v(0), s"U0 (m4, block incomplete) must not be allTableValid (got $v)")
        assert(!v(2) && !v(3), s"U2/U3 (m4, no table) must not be allTableValid (got $v)")
        assert((4 until NumUnits).forall(i => !v(i)), s"unused units must not be allTableValid (got $v)")
      }
    }
  }

  describe("segment hole release") {
    def setupHoleBlock(dut: GatherRingController, size: Int, base: Int, fieldNum: Int): Unit = {
      val param = GatherCommon.GatherParam.default
      for (i <- 0 until param.numUnits) {
        val inBlock = i >= base && i < base + size
        val realUnit = inBlock && i < base + fieldNum
        dut.in.hasTable(i).poke(inBlock)
        dut.in.hasIndex(i).poke(realUnit)
        dut.in.hasIndexNext(i).poke(false)
        dut.in.blockSize(i).poke(if (inBlock) size else 0)
        dut.in.blockFieldNum(i).poke(if (inBlock) fieldNum else 0)
        dut.in.counter(i).poke(0)
      }
      dut.in.acceptedWake.poke(0)
      dut.in.flush.poke(0)
      dut.in.wbValid.poke(0)
      for (i <- 0 until param.numPipeIn) {
        dut.in.s0pipes(i).valid.poke(false)
        dut.in.s1pipes(i).valid.poke(false)
      }
      dut.clock.step()
    }

    it("releases the ring4 hole after three accepted wakes, including same-cycle wakes") {
      val param = GatherCommon.GatherParam.default
      simulate(new GatherRingController()(param)) { dut =>
        setupHoleBlock(dut, size = 4, base = 0, fieldNum = 3)
        dut.in.acceptedWake.poke(0)
        dut.out.release.expect(0.U)
        dut.clock.step()

        dut.in.acceptedWake.poke(7)
        dut.out.release.expect(8.U)
        dut.clock.step()
        dut.in.acceptedWake.poke(0)
        dut.out.release.expect(0.U)
      }
    }

    def checkRing8Hole(fieldNum: Int, expected: Int): Unit = {
      val param = GatherCommon.GatherParam.default
      simulate(new GatherRingController()(param)) { dut =>
        setupHoleBlock(dut, size = 8, base = 0, fieldNum = fieldNum)
        dut.in.acceptedWake.poke((1 << fieldNum) - 1)
        dut.out.release.expect(expected.U)
        dut.clock.step()
        dut.in.acceptedWake.poke(0)
        dut.out.release.expect(0.U)
      }
    }

    it("releases the ring8 seg5 holes once") { checkRing8Hole(5, 0xe0) }
    it("releases the ring8 seg6 holes once") { checkRing8Hole(6, 0xc0) }
    it("releases the ring8 seg7 holes once") { checkRing8Hole(7, 0x80) }

    it("does not count an unaccepted wake and resets counters on block flush") {
      val param = GatherCommon.GatherParam.default
      simulate(new GatherRingController()(param)) { dut =>
        setupHoleBlock(dut, size = 4, base = 0, fieldNum = 3)
        dut.in.acceptedWake.poke(3)
        dut.clock.step()
        dut.in.acceptedWake.poke(4)
        dut.in.flush.poke(0xf)
        dut.out.release.expect(0.U)
        dut.clock.step()

        dut.in.flush.poke(0)
        dut.in.acceptedWake.poke(4)
        dut.out.release.expect(0.U, "a flushed block must restart its wake count")
      }
    }

    it("keeps counters independent for adjacent ring4 blocks") {
      val param = GatherCommon.GatherParam.default
      simulate(new GatherRingController()(param)) { dut =>
        setupHoleBlock(dut, size = 4, base = 0, fieldNum = 3)
        // Add a second seg3 block without disturbing the first block's state.
        for (i <- 4 until 8) {
          dut.in.hasTable(i).poke(true)
          dut.in.hasIndex(i).poke(i < 7)
          dut.in.blockSize(i).poke(4)
          dut.in.blockFieldNum(i).poke(3)
        }
        dut.clock.step()
        dut.in.acceptedWake.poke(0x77)
        dut.out.release.expect(0x88.U)
      }
    }
  }
}
