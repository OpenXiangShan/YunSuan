package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.ParallelTestExecution

class GatherRingSpec extends AnyFunSpec with ChiselSim with Matchers with ParallelTestExecution {

  val vlen = 128; val dlen = 128; val xlen = 64

  // ═══ Helper: drive one pipe with a vrgather identity request ═══
  def driveVrgatherIdentity(dut: GatherRing, pipe: Int, blkBase: Int, blkSize: Int,
                             table: Seq[Int], index: Seq[Int], indexValid: Seq[Boolean],
                             prefill: Seq[Int] = Seq.fill(16)(0)): Unit = {
    val p = dut.pipeIn(pipe)
    p.valid.poke(true)
    val req = p.bits.placeReq
    req.blockBase.poke(blkBase)
    req.tableIdx.poke(0)
    table.zipWithIndex.foreach { case (v, i) => req.table(i).poke(v) }
    index.zipWithIndex.foreach { case (v, i) => req.index(i).poke(v) }
    indexValid.zipWithIndex.foreach { case (v, i) => req.indexValid(i).poke(v) }
    prefill.zipWithIndex.foreach { case (v, i) => req.prefillData(i).poke(v) }
    (0 until 16).foreach { i => req.fillZeros(i).poke(false); req.fillScala(i).poke(false) }
    p.bits.placeIdx.poke(0)
  }

  def driveNoOp(dut: GatherRing, pipe: Int): Unit = {
    dut.pipeIn(pipe).valid.poke(false)
  }

  def initDut(dut: GatherRing, blkSize: Int = 8): Unit = {
    for (p <- 0 until 4) { dut.pipeIn(p).valid.poke(false) }
    for (p <- 0 until 4) { dut.writeback(p).ready.poke(false) }
    dut.flushRobIdx.poke(0)
    dut.blockSizeCfg.poke(blkSize)
    dut.clock.step()
  }

  // ═══ 阶段 A1: LMUL=1 vrgather identity (1 UOP) ══════
  describe("LMUL=1") {
    it("vrgather identity via pipe 0") { simulate(new GatherRing(vlen, dlen, xlen)) { dut =>
      initDut(dut, blkSize = 1)
      val t = (0 until 16).map(i => 0x10 + i)
      driveVrgatherIdentity(dut, 0, blkBase = 0, blkSize = 1, t, (0 until 16), Seq.fill(16)(true))
      dut.clock.step()
      dut.writeback(0).valid.expect(true)
      dut.writeback(0).bits.dest.expect(BigInt("1F1E1D1C1B1A19181716151413121110", 16))
    } }

    // A2: iterate all 8 units as standalone LMUL=1 blocks
    for (unit <- 0 until 8) {
      val u = unit
      it(s"vrgather identity on unit $u") { simulate(new GatherRing(vlen, dlen, xlen)) { dut =>
        initDut(dut, blkSize = 1)
        val t = (0 until 16).map(i => 0x10 + i)
        driveVrgatherIdentity(dut, 0, blkBase = u, blkSize = 1, t, (0 until 16), Seq.fill(16)(true))
        dut.clock.step()
        dut.writeback(0).valid.expect(true)
        dut.writeback(0).bits.dest.expect(BigInt("1F1E1D1C1B1A19181716151413121110", 16))
      } }
    }

    // A4: vslideup SEW=8 offset=3 (fillZeros)
    it("vslideup offset=3 fillZeros") { simulate(new GatherRing(vlen, dlen, xlen)) { dut =>
      initDut(dut, blkSize = 1)
      val t = (0 until 16).map(i => 0x10 + i); val offset = 3
      val fz = (0 until 16).map(_ < offset)
      val idx = (0 until 16).map(i => if (i < offset) 0 else i - offset)
      val iv  = (0 until 16).map(_ >= offset)
      val p = dut.pipeIn(0)
      p.valid.poke(true); p.bits.placeIdx.poke(0)
      val req = p.bits.placeReq
      req.blockBase.poke(0); req.tableIdx.poke(0)
      t.zipWithIndex.foreach{case(v,i)=>req.table(i).poke(v)}
      idx.zipWithIndex.foreach{case(v,i)=>req.index(i).poke(v)}
      iv.zipWithIndex.foreach{case(v,i)=>req.indexValid(i).poke(v)}
      (0 until 16).foreach(i=>req.prefillData(i).poke(0))
      fz.zipWithIndex.foreach{case(v,i)=>req.fillZeros(i).poke(v)}
      (0 until 16).foreach(i=>req.fillScala(i).poke(false))
      dut.clock.step()
      val bytes = (0 until 16).map(i=>if(i<offset)0 else 0x10+(i-offset))
      val exp = bytes.foldRight(BigInt(0))((b,a)=>(a<<8)|BigInt(b))
      dut.writeback(0).valid.expect(true)
      dut.writeback(0).bits.dest.expect(exp)
    } }
  }
}
