package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.vector.v2.Shuffle.GatherCommon.GatherParam

class SlideUpIndexGenSpec extends AnyFunSpec with ChiselSim with Matchers with ParallelTestExecution {

  val dlen = 128
  val numElem = dlen / 8

  def sewShift(sew: Int): Int = sew match {
    case 8  => 0
    case 16 => 1
    case 32 => 2
    case 64 => 3
  }

  // Golden model: slideup index
  // i < min(vl_e8, offset_e8) -> stay (index = i), else -> shift down (index = i - offset_e8)
  def goldenSlideUp(offset: Int, sew: Int, vl: Int): Seq[(Int, Boolean, Boolean, Boolean)] = {
    val shift = sewShift(sew)
    val e8Off = offset << shift
    val e8Vl  = vl << shift
    (0 until numElem).map { i =>
      val isStay = e8Off > dlen || (i < e8Vl && i < e8Off)
      val idx = if (isStay) i else i - e8Off
      (idx.max(0), true, false, false) // (index, indexValid, fillZeros, fillScala)
    }
  }

  // Golden model: slide1up index
  def goldenSlide1Up(sew: Int): Seq[(Int, Boolean, Boolean, Boolean)] = {
    val sewb = sew / 8
    (0 until numElem).map { i =>
      val elemIdx = i / sewb
      val isFirst = elemIdx == 0
      val idx = (i - sewb).max(0)
      (idx, !isFirst, false, isFirst)
    }
  }

  def pSew(dut: GatherSewBundle, sew: Int): Unit = {
    dut.e8.poke((sew == 8).B)
    dut.e16.poke((sew == 16).B)
    dut.e32.poke((sew == 32).B)
    dut.e64.poke((sew == 64).B)
  }

  def pSlideUp(dut: SlideUpIndexGen, offset: Int, sew: Int, vl: Int, tableIdx: Int = 0): Unit = {
    dut.in.valid.poke(true)
    dut.in.bits.isSlide1Up.poke(false)
    pSew(dut.in.bits.sew, sew)
    dut.in.bits.offset.poke(offset)
    dut.in.bits.offsetOverflow.poke(offset > dlen)
    dut.in.bits.vl.poke(vl)
    dut.in.bits.tableIdx.poke(tableIdx)
  }

  def pSlide1Up(dut: SlideUpIndexGen, sew: Int, tableIdx: Int = 0): Unit = {
    dut.in.valid.poke(true)
    dut.in.bits.isSlide1Up.poke(true)
    pSew(dut.in.bits.sew, sew)
    dut.in.bits.offset.poke(0)
    dut.in.bits.offsetOverflow.poke(false)
    dut.in.bits.vl.poke(0)
    dut.in.bits.tableIdx.poke(tableIdx)
  }

  def pOff(dut: SlideUpIndexGen): Unit = {
    dut.in.valid.poke(false)
  }

  def eSlideUp(dut: SlideUpIndexGen, golden: Seq[(Int, Boolean, Boolean, Boolean)]): Unit = {
    dut.out.slideup.valid.expect(true.B)
    for (i <- 0 until numElem) {
      val (idx, valid, fillZ, fillS) = golden(i)
      dut.out.slideup.bits.index(i).expect(idx.U)
      dut.out.slideup.bits.indexValid(i).expect(valid.B)
      dut.out.slideup.bits.fillZeros(i).expect(fillZ.B)
      dut.out.slideup.bits.fillScala(i).expect(fillS.B)
    }
  }

  def eSlide1Up(dut: SlideUpIndexGen, golden: Seq[(Int, Boolean, Boolean, Boolean)]): Unit = {
    dut.out.slide1up.valid.expect(true.B)
    for (i <- 0 until numElem) {
      val (idx, valid, fillZ, fillS) = golden(i)
      dut.out.slide1up.bits.index(i).expect(idx.U)
      dut.out.slide1up.bits.indexValid(i).expect(valid.B)
      dut.out.slide1up.bits.fillZeros(i).expect(fillZ.B)
      dut.out.slide1up.bits.fillScala(i).expect(fillS.B)
    }
  }

  describe("vslideup") {
    describe("SEW=8") {
      it("offset=3, vl=16") {
        simulate(new SlideUpIndexGen()(GatherParam.default)) { dut =>
          val sew = 8
          val golden = goldenSlideUp(offset = 3, sew = sew, vl = 16)
          pSlideUp(dut, offset = 3, sew = sew, vl = 16)
          dut.clock.step()
          eSlideUp(dut, golden)
        }
      }
      it("offset=0") {
        simulate(new SlideUpIndexGen()(GatherParam.default)) { dut =>
          val sew = 8
          val golden = goldenSlideUp(offset = 0, sew = sew, vl = 16)
          pSlideUp(dut, offset = 0, sew = sew, vl = 16)
          dut.clock.step()
          eSlideUp(dut, golden)
        }
      }
      it("offset overflow") {
        simulate(new SlideUpIndexGen()(GatherParam.default)) { dut =>
          val sew = 8
          val golden = goldenSlideUp(offset = dlen + 1, sew = sew, vl = 16)
          pSlideUp(dut, offset = dlen + 1, sew = sew, vl = 16)
          dut.clock.step()
          eSlideUp(dut, golden)
        }
      }
    }
    describe("SEW=16") {
      it("offset=2, vl=8") {
        simulate(new SlideUpIndexGen()(GatherParam.default)) { dut =>
          val sew = 16
          val golden = goldenSlideUp(offset = 2, sew = sew, vl = 8)
          pSlideUp(dut, offset = 2, sew = sew, vl = 8)
          dut.clock.step()
          eSlideUp(dut, golden)
        }
      }
    }
    describe("SEW=32") {
      it("offset=1, vl=4") {
        simulate(new SlideUpIndexGen()(GatherParam.default)) { dut =>
          val sew = 32
          val golden = goldenSlideUp(offset = 1, sew = sew, vl = 4)
          pSlideUp(dut, offset = 1, sew = sew, vl = 4)
          dut.clock.step()
          eSlideUp(dut, golden)
        }
      }
    }
    describe("SEW=64") {
      it("offset=1, vl=2") {
        simulate(new SlideUpIndexGen()(GatherParam.default)) { dut =>
          val sew = 64
          val golden = goldenSlideUp(offset = 1, sew = sew, vl = 2)
          pSlideUp(dut, offset = 1, sew = sew, vl = 2)
          dut.clock.step()
          eSlideUp(dut, golden)
        }
      }
    }
  }

  describe("vslide1up") {
    describe("SEW=8") {
      it("first element fillScala") {
        simulate(new SlideUpIndexGen()(GatherParam.default)) { dut =>
          val sew = 8
          val golden = goldenSlide1Up(sew)
          pSlide1Up(dut, sew)
          dut.clock.step()
          eSlide1Up(dut, golden)
        }
      }
    }
    describe("SEW=16") {
      it("first 2 bytes fillScala") {
        simulate(new SlideUpIndexGen()(GatherParam.default)) { dut =>
          val sew = 16
          val golden = goldenSlide1Up(sew)
          pSlide1Up(dut, sew)
          dut.clock.step()
          eSlide1Up(dut, golden)
        }
      }
    }
    describe("SEW=32") {
      it("first 4 bytes fillScala") {
        simulate(new SlideUpIndexGen()(GatherParam.default)) { dut =>
          val sew = 32
          val golden = goldenSlide1Up(sew)
          pSlide1Up(dut, sew)
          dut.clock.step()
          eSlide1Up(dut, golden)
        }
      }
    }
    describe("SEW=64") {
      it("first 8 bytes fillScala") {
        simulate(new SlideUpIndexGen()(GatherParam.default)) { dut =>
          val sew = 64
          val golden = goldenSlide1Up(sew)
          pSlide1Up(dut, sew)
          dut.clock.step()
          eSlide1Up(dut, golden)
        }
      }
    }
  }

  // ── ISA cross-validation ───────────────────────────────────────────

  def toSewElems(value: BigInt, sew: Int, nElems: Int): Seq[Int] = {
    (0 until nElems).map { i =>
      ((value >> (i * sew)) & ((BigInt(1) << sew) - 1)).toInt
    }
  }

  def fromSewElems(elems: Seq[Int], sew: Int): BigInt = {
    elems.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (v, i)) =>
      acc | (BigInt(v & ((1 << sew) - 1)) << (i * sew))
    }
  }

  def sewTo8b(vs2Sew: Seq[Int], sew: Int): Seq[Int] = {
    val sewb = sew / 8
    vs2Sew.flatMap(v => (0 until sewb).map(b => (v >> (b * 8)) & 0xff))
  }

  def eightBToSew(bytes: Seq[Int], sew: Int): Seq[Int] = {
    val sewb = sew / 8
    bytes.grouped(sewb).map(g => g.zipWithIndex.foldLeft(0) { case (a, (b, i)) => a | (b << (i * 8)) }).toSeq
  }

  /** ISA-level vslideup: vd[i] = i&lt;offset ? vs2[i] : vs2[i-offset], for i&lt;vl */
  def isaVslideup(vs2Sew: Seq[Int], offset: Int, vl: Int): Seq[Int] = {
    vs2Sew.zipWithIndex.map { case (v, i) =>
      if (i < vl) {
        if (i < offset) v else vs2Sew(i - offset)
      } else 0
    }
  }

  def crossValidateSlideUp(sew: Int, offset: Int, vl: Int): Unit = {
    val sewb = sew / 8
    val nSewElems = dlen / sew
    val vs2Sew = (0 until nSewElems).map(i => (i * 0x11 + 0xAB) & ((1 << sew) - 1))
    val e8Off = offset << sewShift(sew)

    // Path 1: ISA golden
    val simpleGold = isaVslideup(vs2Sew, offset, vl)

    // Path 2: 8b golden via goldenSlideUp + gather
    val slideUp8b = goldenSlideUp(offset, sew, vl)
    val table = sewTo8b(vs2Sew, sew)
    val tableRange = 0 until (128 / 128)
    val complexGold8b = (0 until numElem).map { i =>
      val (idx, _, _, _) = slideUp8b(i)
      // slideup always has indexValid=true, fillZeros=false, fillScala=false
      if (tableRange.contains(idx / numElem)) table(idx) else 0
    }
    val complexGold = eightBToSew(complexGold8b, sew).take(nSewElems)

    val matchOk = (0 until vl).forall(i => simpleGold(i) == complexGold(i))
    assert(matchOk,
      s"\nSEW=$sew offset=$offset vl=$vl" +
      s"\n  simple:  ${simpleGold.take(vl).mkString(",")}" +
      s"\n  complex: ${complexGold.take(vl).mkString(",")}")
  }

  /** ISA-level vslide1up: vd[0]=scalar, vd[i]=vs2[i-1] for 0&lt;i&lt;vl */
  def isaVslide1up(vs2Sew: Seq[Int], vl: Int, scalar: Int): Seq[Int] = {
    vs2Sew.zipWithIndex.map { case (v, i) =>
      if (i < vl) {
        if (i == 0) scalar else vs2Sew(i - 1)
      } else 0
    }
  }

  def crossValidateSlide1Up(sew: Int, vl: Int): Unit = {
    val sewb = sew / 8
    val nSewElems = dlen / sew
    val vs2Sew = (0 until nSewElems).map(i => (i * 0x11 + 0xAB) & ((1 << sew) - 1))
    val scalar = 0xCA

    val simpleGold = isaVslide1up(vs2Sew, vl, scalar)

    val slide1Up8b = goldenSlide1Up(sew)
    val table = sewTo8b(vs2Sew, sew)
    val tableRange = 0 until (128 / 128)
    val complexGold8b = (0 until numElem).map { i =>
      val (idx, valid, _, fillS) = slide1Up8b(i)
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
    it("vslideup SEW=8 offset=3 vl=14") { crossValidateSlideUp(8, 3, 14) }
    it("vslideup SEW=16 offset=2 vl=6") { crossValidateSlideUp(16, 2, 6) }
    it("vslideup SEW=32 offset=1 vl=3") { crossValidateSlideUp(32, 1, 3) }
    it("vslideup SEW=64 offset=1 vl=1") { crossValidateSlideUp(64, 1, 1) }
    it("vslide1up SEW=8 vl=16") { crossValidateSlide1Up(8, 16) }
    it("vslide1up SEW=32 vl=4") { crossValidateSlide1Up(32, 4) }
  }
}
