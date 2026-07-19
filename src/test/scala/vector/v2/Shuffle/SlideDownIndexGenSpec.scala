package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.vector.v2.Shuffle.GatherCommon.GatherParam

class SlideDownIndexGenSpec extends AnyFunSpec with ChiselSim with Matchers with ParallelTestExecution with GatherSpecUtils {

  val maxLMUL = 8
  val vlmax = vlen * maxLMUL / 8


  // vlmax in 8b elements: (lmulFrac / 8) * VLENB = lmulFrac * vlen / 64
  def lmulVlmax(lmulFrac: Int): Int = lmulFrac * vlen / 64

  // Golden model: vslidedown
  def goldenSlideDown(tableIdx: Int, offset: Int, sew: Int, lmulFrac: Int): Seq[(Int, Boolean, Boolean, Boolean)] = {
    val e8Off = offset << sewShift(sew)
    val vlmax8b = lmulVlmax(lmulFrac)
    val tableBase = tableIdx * dlen / 8
    (0 until numElem).map { i =>
      val iPlusOffset = tableBase + i + e8Off
      val outOfRange = iPlusOffset >= vlmax8b
      (iPlusOffset, !outOfRange, outOfRange, false)
    }
  }

  // Golden model: vslide1down
  def goldenSlide1Down(tableIdx: Int, sew: Int, vl: Int): Seq[(Int, Boolean, Boolean, Boolean)] = {
    val sewb = sew / 8
    val tableBase = tableIdx * dlen / 8
    (0 until numElem).map { i =>
      val elemIdx = tableBase / sewb + i / sewb
      val isLastElem = elemIdx == vl - 1
      val iPlusOff = tableBase + i + sewb
      if (isLastElem)
        (iPlusOff, false, false, true)  // fillScala, index still computed
      else
        (iPlusOff, true, false, false)
    }
  }

  def pSew(dut: GatherSewBundle, sew: Int): Unit = {
    dut.e8.poke((sew == 8).B)
    dut.e16.poke((sew == 16).B)
    dut.e32.poke((sew == 32).B)
    dut.e64.poke((sew == 64).B)
  }

  def pLmul(dut: GatherLmulBundle, lmul: Int): Unit = {
    dut.mf8.poke((lmul == 1).B)
    dut.mf4.poke((lmul == 2).B)
    dut.mf2.poke((lmul == 4).B)
    dut.m1.poke((lmul == 8).B)
    dut.m2.poke((lmul == 16).B)
    dut.m4.poke((lmul == 32).B)
    dut.m8.poke((lmul == 64).B)
  }

  def pSlideDown(dut: SlideDownIndexGen, tableIdx: Int, offset: Int, sew: Int, lmulFrac: Int, vl: Int): Unit = {
    dut.in.valid.poke(true)
    dut.in.bits.isSlide1Down.poke(false)
    dut.in.bits.tableIdx.poke(tableIdx)
    dut.in.bits.offset.poke(offset)
    dut.in.bits.offsetOverflow.poke(false)
    dut.in.bits.vl.poke(vl)
    pLmul(dut.in.bits.lmul, lmulFrac)
    pSew(dut.in.bits.sew, sew)
  }

  def pSlide1Down(dut: SlideDownIndexGen, tableIdx: Int, sew: Int, vl: Int): Unit = {
    dut.in.valid.poke(true)
    dut.in.bits.isSlide1Down.poke(true)
    dut.in.bits.tableIdx.poke(tableIdx)
    dut.in.bits.offset.poke(0)
    dut.in.bits.offsetOverflow.poke(false)
    dut.in.bits.vl.poke(vl)
    pLmul(dut.in.bits.lmul, 8)
    pSew(dut.in.bits.sew, sew)
  }

  def eSlideDown(dut: SlideDownIndexGen, golden: Seq[(Int, Boolean, Boolean, Boolean)]): Unit = {
    dut.out.slideDown.valid.expect(true.B)
    for (i <- 0 until numElem) {
      val (idx, valid, fillZ, fillS) = golden(i)
      dut.out.slideDown.bits.index(i).expect(idx.U)
      dut.out.slideDown.bits.indexValid(i).expect(valid.B)
      dut.out.slideDown.bits.fillZeros(i).expect(fillZ.B)
      dut.out.slideDown.bits.fillScala(i).expect(fillS.B)
    }
  }

  def eSlide1Down(dut: SlideDownIndexGen, golden: Seq[(Int, Boolean, Boolean, Boolean)]): Unit = {
    dut.out.slide1Down.valid.expect(true.B)
    for (i <- 0 until numElem) {
      val (idx, valid, fillZ, fillS) = golden(i)
      dut.out.slide1Down.bits.index(i).expect(idx.U)
      dut.out.slide1Down.bits.indexValid(i).expect(valid.B)
      dut.out.slide1Down.bits.fillZeros(i).expect(fillZ.B)
      dut.out.slide1Down.bits.fillScala(i).expect(fillS.B)
    }
  }

  describe("vslidedown") {
    describe("SEW=8") {
      it("offset=3, LMUL=m1") {
        simulate(new SlideDownIndexGen()(GatherParam.default)) { dut =>
          val sew = 8; val lmulFrac = 8
          val golden = goldenSlideDown(tableIdx = 0, offset = 3, sew = sew, lmulFrac = lmulFrac)
          pSlideDown(dut, tableIdx = 0, offset = 3, sew = sew, lmulFrac = lmulFrac, vl = 14)
          dut.clock.step()
          eSlideDown(dut, golden)
        }
      }
      it("offset overflow") {
        simulate(new SlideDownIndexGen()(GatherParam.default)) { dut =>
          val sew = 8; val lmulFrac = 8
          val golden = goldenSlideDown(tableIdx = 0, offset = 16, sew = sew, lmulFrac = lmulFrac)
          pSlideDown(dut, tableIdx = 0, offset = 16, sew = sew, lmulFrac = lmulFrac, vl = 16)
          dut.clock.step()
          eSlideDown(dut, golden)
        }
      }
      it("tableIdx=1, offset=1, LMUL=m2") {
        simulate(new SlideDownIndexGen()(GatherParam.default)) { dut =>
          val sew = 8; val lmulFrac = 16
          val golden = goldenSlideDown(tableIdx = 1, offset = 1, sew = sew, lmulFrac = lmulFrac)
          pSlideDown(dut, tableIdx = 1, offset = 1, sew = sew, lmulFrac = lmulFrac, vl = 30)
          dut.clock.step()
          eSlideDown(dut, golden)
        }
      }
    }
    describe("SEW=32") {
      it("offset=1, LMUL=m2") {
        simulate(new SlideDownIndexGen()(GatherParam.default)) { dut =>
          val sew = 32; val lmulFrac = 16
          val golden = goldenSlideDown(tableIdx = 0, offset = 1, sew = sew, lmulFrac = lmulFrac)
          pSlideDown(dut, tableIdx = 0, offset = 1, sew = sew, lmulFrac = lmulFrac, vl = 7)
          dut.clock.step()
          eSlideDown(dut, golden)
        }
      }
    }
  }

  describe("vslide1down") {
    describe("SEW=8") {
      it("last element fillScala") {
        simulate(new SlideDownIndexGen()(GatherParam.default)) { dut =>
          val sew = 8
          val golden = goldenSlide1Down(tableIdx = 0, sew = sew, vl = 16)
          pSlide1Down(dut, tableIdx = 0, sew = sew, vl = 16)
          dut.clock.step()
          eSlide1Down(dut, golden)
        }
      }
    }
    describe("SEW=32") {
      it("last 4 bytes fillScala") {
        simulate(new SlideDownIndexGen()(GatherParam.default)) { dut =>
          val sew = 32
          val golden = goldenSlide1Down(tableIdx = 0, sew = sew, vl = 4)
          pSlide1Down(dut, tableIdx = 0, sew = sew, vl = 4)
          dut.clock.step()
          eSlide1Down(dut, golden)
        }
      }
    }
  }

  // ── ISA cross-validation ───────────────────────────────────────────




  /** ISA-level vslidedown: vd[i] = i+off&lt;vlmax ? vs2[i+off] : 0, for i&lt;vl */
  def isaVslidedown(vs2Sew: Seq[Int], offset: Int, vl: Int, vlmaxSew: Int): Seq[Int] = {
    vs2Sew.zipWithIndex.map { case (v, i) =>
      if (i < vl) {
        val src = i + offset
        if (src < vlmaxSew) vs2Sew(src) else 0
      } else 0
    }
  }

  def crossValidateSlideDown(sew: Int, offset: Int, lmulFrac: Int, vl: Int): Unit = {
    val sewb = sew / 8
    val nSewElems = dlen / sew
    val vlmaxSew = lmulFrac * vlen / 64 / sewb
    val e8Off = offset << sewShift(sew)
    val vs2Sew = (0 until vlmaxSew).map(i => (i * 0x11 + 0xAB) & ((1 << sew) - 1))

    val simpleGold = isaVslidedown(vs2Sew, offset, vl, vlmaxSew)

    val slideDown8b = goldenSlideDown(tableIdx = 0, offset, sew, lmulFrac)
    val table = sewTo8b(vs2Sew, sew)
    val nChunks = lmulFrac * 128 / 128
    val tableRange = 0 until nChunks
    val complexGold8b = (0 until numElem).map { i =>
      val (idx, valid, fillZ, _) = slideDown8b(i)
      if (fillZ) 0
      else if (valid && tableRange.contains(idx / numElem)) table(idx)
      else 0
    }
    val complexGold = eightBToSew(complexGold8b, sew).take(nSewElems)

    val matchOk = (0 until vl).forall(i => simpleGold(i) == complexGold(i))
    assert(matchOk,
      s"\nSEW=$sew offset=$offset LMUL=f$lmulFrac vl=$vl vlmax=$vlmaxSew" +
      s"\n  simple:  ${simpleGold.take(vl).mkString(",")}" +
      s"\n  complex: ${complexGold.take(vl).mkString(",")}")
  }

  /** ISA-level vslide1down: vd[i]=vs2[i+1] for i&lt;vl-1, vd[vl-1]=scalar */
  def isaVslide1down(vs2Sew: Seq[Int], vl: Int, scalar: Int): Seq[Int] = {
    vs2Sew.zipWithIndex.map { case (v, i) =>
      if (i < vl) {
        if (i == vl - 1) scalar else vs2Sew(i + 1)
      } else 0
    }
  }

  def crossValidateSlide1Down(sew: Int, vl: Int): Unit = {
    val sewb = sew / 8
    val nSewElems = dlen / sew
    val vs2Sew = (0 until nSewElems).map(i => (i * 0x11 + 0xAB) & ((1 << sew) - 1))
    val scalar = 0xCA

    val simpleGold = isaVslide1down(vs2Sew, vl, scalar)

    val slide1Down8b = goldenSlide1Down(tableIdx = 0, sew, vl)
    val table = sewTo8b(vs2Sew, sew)
    val tableRange = 0 until (128 / 128)
    val complexGold8b = (0 until numElem).map { i =>
      val (idx, valid, _, fillS) = slide1Down8b(i)
      if (fillS) scalar & 0xff
      else if (valid && tableRange.contains(idx / numElem)) table(idx)
      else 0
    }
    val complexGold = eightBToSew(complexGold8b, sew).take(nSewElems)

    val matchOk = (0 until vl).forall(i => (simpleGold(i) & ((1 << sew) - 1)) == (complexGold(i) & ((1 << sew) - 1)))
    assert(matchOk,
      s"\nSEW=$sew vl=$vl" +
      s"\n  simple:  ${simpleGold.take(vl).mkString(",")}" +
      s"\n  complex: ${complexGold.take(vl).mkString(",")}")
  }

  describe("cross-validate ISA") {
    it("vslidedown SEW=8 offset=3 LMUL=m1 vl=13") { crossValidateSlideDown(8, 3, 8, 13) }
    it("vslidedown SEW=32 offset=1 LMUL=m2 vl=2") { crossValidateSlideDown(32, 1, 16, 2) }
    it("vslidedown SEW=16 offset=2 LMUL=m4 vl=5") { crossValidateSlideDown(16, 2, 32, 5) }
    it("vslide1down SEW=8 vl=16") { crossValidateSlide1Down(8, 16) }
    it("vslide1down SEW=32 vl=4") { crossValidateSlide1Down(32, 4) }
  }
}
