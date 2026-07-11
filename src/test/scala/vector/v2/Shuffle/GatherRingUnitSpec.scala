package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

class GatherRingUnitSpec extends AnyFunSpec with ChiselSim with Matchers {

  val vlen = 128; val dlen = 128; val maxTableNum = 8

  case class Cycle(pokes: Seq[GatherRingUnit => Unit], expects: Seq[GatherRingUnit => Unit])

  def runCycles(dut: GatherRingUnit, init: GatherRingUnit => Unit, cycles: Cycle*): Unit = {
    init(dut); for (c <- cycles) { c.pokes.foreach(_(dut)); dut.clock.step(); c.expects.foreach(_(dut)) }
  }

  def initUnit: GatherRingUnit => Unit = { dut =>
    dut.in.blockConfig.blockBase.poke(0); dut.in.blockConfig.blockSize.poke(8)
    dut.in.willFlow.poke(false)
    dut.in.flowPrev.valid.poke(false); dut.in.flowPrev.idx.poke(0); dut.in.flowPrev.data.foreach(_.poke(0)); dut.in.flowPrev.tag.isFirst.poke(false); dut.in.flowPrev.tag.passedEntry.poke(false)
    dut.in.flowWrap.valid.poke(false); dut.in.flowWrap.idx.poke(0); dut.in.flowWrap.data.foreach(_.poke(0)); dut.in.flowWrap.tag.isFirst.poke(false); dut.in.flowWrap.tag.passedEntry.poke(false)
    dut.in.place.valid.poke(false); dut.out.writeback.ready.poke(false)
    dut.flush.poke(true); dut.clock.step(); dut.flush.poke(false); dut.clock.step()
  }

  def eHasTable(v: Boolean):   GatherRingUnit => Unit = _.out.status.hasTable.expect(v)
  def eHasIndex(v: Boolean):   GatherRingUnit => Unit = _.out.status.hasIndex.expect(v)
  def eWb(valid: Boolean, dest: BigInt = 0): GatherRingUnit => Unit = { dut => dut.out.writeback.valid.expect(valid); if (valid) dut.out.writeback.bits.dest.expect(dest) }

  def pFlush: GatherRingUnit => Unit = _.flush.poke(true)
  def pPlace(blkSize: Int, tblIdx: Int, tbl: Seq[Int], idx: Seq[Int], iValid: Seq[Boolean],
             prefill: Seq[Int] = Seq.fill(16)(0), fScala: Seq[Boolean] = Seq.fill(16)(false), fZeros: Seq[Boolean] = Seq.fill(16)(false)): GatherRingUnit => Unit = { dut =>
    dut.in.blockConfig.blockSize.poke(blkSize); dut.in.place.valid.poke(true); dut.in.place.bits.blockBase.poke(0); dut.in.place.bits.tableIdx.poke(tblIdx)
    tbl.zipWithIndex.foreach { case (v, i) => dut.in.place.bits.table(i).poke(v) }; idx.zipWithIndex.foreach { case (v, i) => dut.in.place.bits.index(i).poke(v) }
    iValid.zipWithIndex.foreach { case (v, i) => dut.in.place.bits.indexValid(i).poke(v) }; prefill.zipWithIndex.foreach { case (v, i) => dut.in.place.bits.prefillData(i).poke(v) }
    fScala.zipWithIndex.foreach { case (v, i) => dut.in.place.bits.fillScala(i).poke(v) }; fZeros.zipWithIndex.foreach { case (v, i) => dut.in.place.bits.fillZeros(i).poke(v) }
  }
  def pNoPlace: GatherRingUnit => Unit = _.in.place.valid.poke(false)
  def pNoFlow:  GatherRingUnit => Unit = _.in.willFlow.poke(false)
  def pFlow: GatherRingUnit => Unit = { dut => dut.in.willFlow.poke(true)
    dut.in.flowPrev.valid.poke(true); dut.in.flowPrev.idx.poke(0); dut.in.flowPrev.data.foreach(_.poke(0)); dut.in.flowPrev.tag.isFirst.poke(false); dut.in.flowPrev.tag.passedEntry.poke(false)
    dut.in.flowWrap.valid.poke(true); dut.in.flowWrap.idx.poke(0); dut.in.flowWrap.data.foreach(_.poke(0)); dut.in.flowWrap.tag.isFirst.poke(false); dut.in.flowWrap.tag.passedEntry.poke(false)
  }
  def pFlowTbl(tblIdx: Int, tbl: Seq[Int]): GatherRingUnit => Unit = { dut => dut.in.willFlow.poke(true)
    dut.in.flowPrev.valid.poke(true); dut.in.flowPrev.idx.poke(tblIdx); dut.in.flowWrap.valid.poke(true); dut.in.flowWrap.idx.poke(tblIdx)
    tbl.zipWithIndex.foreach { case (v, i) => dut.in.flowPrev.data(i).poke(v); dut.in.flowWrap.data(i).poke(v) }
    dut.in.flowPrev.tag.isFirst.poke(false); dut.in.flowPrev.tag.passedEntry.poke(false); dut.in.flowWrap.tag.isFirst.poke(false); dut.in.flowWrap.tag.passedEntry.poke(false)
  }
  def pScala(v: BigInt): GatherRingUnit => Unit = _.in.scalaTable.poke(v)

  private val noOp = Seq.empty[GatherRingUnit => Unit]
  private val H1 = Seq(eHasTable(true)); private val H0 = Seq(eHasTable(false))
  private val I1 = Seq(eHasIndex(true));  private val I0 = Seq(eHasIndex(false))
  private val Wb0 = Seq(eWb(false)); def Wb(exp: BigInt) = Seq(eWb(true, exp))

  // ═══ 测试 1-4: FSM 基础 ══════════════════════════════
  describe("FSM") {
    it("Place sets hasTable/hasIndex") { simulate(new GatherRingUnit(vlen, dlen, maxTableNum, xlen = 64, unitIdx = 0)) { dut =>
      val e = Seq.fill(16)(0); val f = Seq.fill(16)(false)
      runCycles(dut, initUnit, Cycle(Seq(pNoFlow), H0++I0), Cycle(Seq(pPlace(8,0,e,e,f)), H1++I1++Wb0)) } }
    it("Flow decrements counter") { simulate(new GatherRingUnit(vlen, dlen, maxTableNum, xlen = 64, unitIdx = 0)) { dut =>
      val e = Seq.fill(16)(0); val f = Seq.fill(16)(false)
      runCycles(dut, initUnit, Cycle(Seq(pPlace(2,0,e,e,f)), Wb0), Cycle(Seq(pNoPlace,pFlow), Wb(0))) } }
    it("Hold keeps state") { simulate(new GatherRingUnit(vlen, dlen, maxTableNum, xlen = 64, unitIdx = 0)) { dut =>
      val e = Seq.fill(16)(0); val f = Seq.fill(16)(false)
      runCycles(dut, initUnit, Cycle(Seq(pPlace(8,0,e,e,f)), noOp), Cycle(Seq(pNoPlace,pNoFlow), H1++I1)) } }
    it("Flush clears") { simulate(new GatherRingUnit(vlen, dlen, maxTableNum, xlen = 64, unitIdx = 0)) { dut =>
      val e = Seq.fill(16)(0); val f = Seq.fill(16)(false)
      runCycles(dut, initUnit, Cycle(Seq(pPlace(8,0,e,e,f)), H1), Cycle(Seq(pNoPlace,pNoFlow,pFlush), H0++I0)) } }
  }

  // ═══ 测试 5-10: LMUL=1 运算 ═════════════════════════
  describe("LMUL=1") {
    it("identity 16 elem") { simulate(new GatherRingUnit(vlen, dlen, maxTableNum, xlen = 64, unitIdx = 0)) { dut =>
      val t = (0 until 16).map(i => 0x10 + i); val exp = BigInt("1F1E1D1C1B1A19181716151413121110", 16)
      runCycles(dut, initUnit, Cycle(Seq(pPlace(1,0,t,(0 until 16),Seq.fill(16)(true))), Wb(exp))) } }
    it("mixed found/notfound") { simulate(new GatherRingUnit(vlen, dlen, maxTableNum, xlen = 64, unitIdx = 0)) { dut =>
      val t = (0 until 16).map(i => 0xA0 + i); val idx = (0 until 16).map(i => if (i%2==0) i else 16+i); val pf = (0 until 16).map(i => if (i%2==0) 0 else 16+i)
      val bytes = (0 until 16).map(i => if (i%2==0) 0xA0+i else 16+i); val exp = bytes.foldRight(BigInt(0))((b,a)=>(a<<8)|BigInt(b))
      runCycles(dut, initUnit, Cycle(Seq(pPlace(1,0,t,idx,Seq.fill(16)(true),pf)), Wb(exp))) } }
    it("vslide1up SEW=8") { simulate(new GatherRingUnit(vlen, dlen, maxTableNum, xlen = 64, unitIdx = 0)) { dut =>
      val t = (0 until 16).map(i => 0x10 + i); val idx = (0 until 16).map(i => if (i==0) 0 else i-1); val iv = (0 until 16).map(_ != 0); val fs = (0 until 16).map(_ == 0)
      val bytes = (0 until 16).map(i => if (i==0) 0x5A else 0x10+(i-1)); val exp = bytes.foldRight(BigInt(0))((b,a)=>(a<<8)|BigInt(b))
      runCycles(dut, initUnit, Cycle(Seq(pPlace(1,0,t,idx,iv,fScala=fs), pScala(BigInt("5A5A5A5A5A5A5A5A",16))), Wb(exp))) } }
    it("vslide1down SEW=8") { simulate(new GatherRingUnit(vlen, dlen, maxTableNum, xlen = 64, unitIdx = 0)) { dut =>
      val t = (0 until 16).map(i => 0x10 + i); val idx = (0 until 16).map(i => if (i==15) 0 else i+1); val iv = (0 until 16).map(_ != 15); val fs = (0 until 16).map(_ == 15)
      val bytes = (0 until 16).map(i => if (i==15) 0x5A else 0x10+(i+1)); val exp = bytes.foldRight(BigInt(0))((b,a)=>(a<<8)|BigInt(b))
      runCycles(dut, initUnit, Cycle(Seq(pPlace(1,0,t,idx,iv,fScala=fs), pScala(BigInt("5A5A5A5A5A5A5A5A",16))), Wb(exp))) } }
  }

  // ═══ 测试 11-12: SEW=16/32 fillScala ════════════════
  describe("SEW=16 fillScala") { val sb = 2
    it("vslide1up") { simulate(new GatherRingUnit(vlen, dlen, maxTableNum, xlen = 64, unitIdx = 0)) { dut =>
      val t = (0 until 16).map(i => 0x10 + i); val idx = (0 until 16).map(i => if (i<sb) 0 else i-sb); val iv = (0 until 16).map(_ >= sb); val fs = (0 until 16).map(_ < sb)
      val bytes = (0 until 16).map(i => if (i<sb) 0x5A else 0x10+(i-sb)); val exp = bytes.foldRight(BigInt(0))((b,a)=>(a<<8)|BigInt(b))
      runCycles(dut, initUnit, Cycle(Seq(pPlace(1,0,t,idx,iv,fScala=fs), pScala(BigInt("5A5A5A5A5A5A5A5A",16))), Wb(exp))) } }
    it("vslide1down") { simulate(new GatherRingUnit(vlen, dlen, maxTableNum, xlen = 64, unitIdx = 0)) { dut =>
      val t = (0 until 16).map(i => 0x10 + i); val idx = (0 until 16).map(i => if (i>=16-sb) 0 else i+sb); val iv = (0 until 16).map(_ < 16-sb); val fs = (0 until 16).map(_ >= 16-sb)
      val bytes = (0 until 16).map(i => if (i>=16-sb) 0x5A else 0x10+(i+sb)); val exp = bytes.foldRight(BigInt(0))((b,a)=>(a<<8)|BigInt(b))
      runCycles(dut, initUnit, Cycle(Seq(pPlace(1,0,t,idx,iv,fScala=fs), pScala(BigInt("5A5A5A5A5A5A5A5A",16))), Wb(exp))) } }
  }
  describe("SEW=32 fillScala") { val sb = 4
    it("vslide1up") { simulate(new GatherRingUnit(vlen, dlen, maxTableNum, xlen = 64, unitIdx = 0)) { dut =>
      val t = (0 until 16).map(i => 0x10 + i); val idx = (0 until 16).map(i => if (i<sb) 0 else i-sb); val iv = (0 until 16).map(_ >= sb); val fs = (0 until 16).map(_ < sb)
      val bytes = (0 until 16).map(i => if (i<sb) 0x5A else 0x10+(i-sb)); val exp = bytes.foldRight(BigInt(0))((b,a)=>(a<<8)|BigInt(b))
      runCycles(dut, initUnit, Cycle(Seq(pPlace(1,0,t,idx,iv,fScala=fs), pScala(BigInt("5A5A5A5A5A5A5A5A",16))), Wb(exp))) } }
    it("vslide1down") { simulate(new GatherRingUnit(vlen, dlen, maxTableNum, xlen = 64, unitIdx = 0)) { dut =>
      val t = (0 until 16).map(i => 0x10 + i); val idx = (0 until 16).map(i => if (i>=16-sb) 0 else i+sb); val iv = (0 until 16).map(_ < 16-sb); val fs = (0 until 16).map(_ >= 16-sb)
      val bytes = (0 until 16).map(i => if (i>=16-sb) 0x5A else 0x10+(i+sb)); val exp = bytes.foldRight(BigInt(0))((b,a)=>(a<<8)|BigInt(b))
      runCycles(dut, initUnit, Cycle(Seq(pPlace(1,0,t,idx,iv,fScala=fs), pScala(BigInt("5A5A5A5A5A5A5A5A",16))), Wb(exp))) } }
  }

  // ═══ 测试 13-14: fillZeros ═══════════════════════════
  describe("fillZeros") {
    it("vslideup offset=3") { simulate(new GatherRingUnit(vlen, dlen, maxTableNum, xlen = 64, unitIdx = 0)) { dut =>
      val t = (0 until 16).map(i => 0x10 + i); val fz = (0 until 16).map(_ < 3); val idx = (0 until 16).map(i => if (i<3) 0 else i-3); val iv = (0 until 16).map(_ >= 3)
      val bytes = (0 until 16).map(i => if (i<3) 0 else 0x10+(i-3)); val exp = bytes.foldRight(BigInt(0))((b,a)=>(a<<8)|BigInt(b))
      runCycles(dut, initUnit, Cycle(Seq(pPlace(1,0,t,idx,iv,fZeros=fz)), Wb(exp))) } }
    it("vslidedown offset=2") { simulate(new GatherRingUnit(vlen, dlen, maxTableNum, xlen = 64, unitIdx = 0)) { dut =>
      val t = (0 until 16).map(i => 0x10 + i); val fz = (0 until 16).map(_ >= 14); val idx = (0 until 16).map(i => if (i>=14) 0 else i+2); val iv = (0 until 16).map(_ < 14)
      val bytes = (0 until 16).map(i => if (i>=14) 0 else 0x10+(i+2)); val exp = bytes.foldRight(BigInt(0))((b,a)=>(a<<8)|BigInt(b))
      runCycles(dut, initUnit, Cycle(Seq(pPlace(1,0,t,idx,iv,fZeros=fz)), Wb(exp))) } }
  }

  // ═══ 测试 15-16: Multi-Table ═════════════════════════
  describe("Multi-Table") {
    it("LMUL=2 identity") { simulate(new GatherRingUnit(vlen, dlen, maxTableNum, xlen = 64, unitIdx = 0)) { dut =>
      val s0 = (0 until 16).map(i => 0x10 + i); val s1 = (0 until 16).map(i => 0xA0 + i); val exp = s0.foldRight(BigInt(0))((b,a)=>(a<<8)|BigInt(b))
      runCycles(dut, initUnit, Cycle(Seq(pPlace(2,0,s0,(0 until 16),Seq.fill(16)(true))), Wb0), Cycle(Seq(pNoPlace,pFlowTbl(1,s1)), Wb(exp))) } }
    it("LMUL=4 identity") { simulate(new GatherRingUnit(vlen, dlen, maxTableNum, xlen = 64, unitIdx = 0)) { dut =>
      val s = (0 until 16).map(i => 0x10 + i); val exp = s.foldRight(BigInt(0))((b,a)=>(a<<8)|BigInt(b))
      runCycles(dut, initUnit, Cycle(Seq(pPlace(4,0,s,(0 until 16),Seq.fill(16)(true))), Wb0), Cycle(Seq(pNoPlace,pFlowTbl(1,s)), Wb0), Cycle(Seq(pNoPlace,pFlowTbl(2,s)), Wb0), Cycle(Seq(pNoPlace,pFlowTbl(3,s)), Wb(exp))) } }
  }

  // ═══ 测试 17-18: Tag ═════════════════════════════════
  describe("Tags") {
    it("tableOut valid after Place") { simulate(new GatherRingUnit(vlen, dlen, maxTableNum, xlen = 64, unitIdx = 0)) { dut =>
      val e = Seq.fill(16)(0); val f = Seq.fill(16)(false)
      runCycles(dut, initUnit, Cycle(Seq(pPlace(2,0,e,e,f)), noOp), Cycle(Seq(pNoPlace,pNoFlow), Seq(_.out.tableOut.valid.expect(true)))) } }
    it("passedEntry at entry unit after flow") { simulate(new GatherRingUnit(vlen, dlen, maxTableNum, xlen = 64, unitIdx = 0)) { dut =>
      val e = Seq.fill(16)(0); val f = Seq.fill(16)(false)
      runCycles(dut, initUnit, Cycle(Seq(pPlace(2,0,e,e,f)), noOp), Cycle(Seq(pNoPlace,pFlowTbl(1,e)), Seq(_.out.tableOut.tag.passedEntry.expect(true)))) } }
  }

  // ═══ 测试 19: Reuse ═════════════════════════════════
  describe("Reuse") {
    it("second instruction after flush") { simulate(new GatherRingUnit(vlen, dlen, maxTableNum, xlen = 64, unitIdx = 0)) { dut =>
      val t1 = (0 until 16).map(i => 0x10 + i); val e1 = t1.foldRight(BigInt(0))((b,a)=>(a<<8)|BigInt(b))
      val t2 = (0 until 16).map(i => 0xA0 + i); val e2 = t2.foldRight(BigInt(0))((b,a)=>(a<<8)|BigInt(b))
      runCycles(dut, initUnit, Cycle(Seq(pPlace(1,0,t1,(0 until 16),Seq.fill(16)(true))), Wb(e1)), Cycle(Seq(pNoPlace,pNoFlow,pFlush), noOp), Cycle(Seq(pNoPlace,pNoFlow,_.flush.poke(false)), noOp), Cycle(Seq(pPlace(1,0,t2,(0 until 16),Seq.fill(16)(true))), Wb(e2))) } }
  }
}
