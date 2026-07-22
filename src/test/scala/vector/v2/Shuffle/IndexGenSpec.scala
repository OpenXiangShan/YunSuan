package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.vector.v2.Shuffle.GatherCommon.GatherParam

class IndexGenSpec extends AnyFunSpec with ChiselSim with Matchers
    with ParallelTestExecution with GatherSpecBase with GatherSpecUtils {

  val dlenb = dlen / 8

  // -- Helpers: poke IndexGen.In fields ----------------------------------

  def pDefaults(dut: IndexGen): Unit = {
    dut.in.valid.poke(false)
    val o = dut.in.bits.op
    o.isGather.poke(false); o.isGatherEI16.poke(false)
    o.isSlideUp.poke(false); o.isSlide1Up.poke(false)
    o.isSlideDown.poke(false); o.isSlide1Down.poke(false)
    o.isZip.poke(false); o.isUnzip.poke(false); o.isCompress.poke(false)
    dut.in.bits.uopIdx.poke(0)
    pSew(dut.in.bits.sew, 8); pSew(dut.in.bits.ei, 8)
    pLmul(dut.in.bits.lmul, 8)
    dut.in.bits.vl.poke(16)
    dut.in.bits.vta.poke(false); dut.in.bits.vma.poke(false)
    dut.in.bits.vs1.poke(0); dut.in.bits.vs2.poke(0)
    dut.in.bits.vs3.poke(0); dut.in.bits.rs1.poke(0)
    dut.in.bits.mask.poke(0)
    dut.in.bits.zipNum.poke(0)
  }

  def eIndex(dut: IndexGen,
    goldenIdx: Seq[Int], goldenValid: Seq[Boolean],
    goldenFillZ: Seq[Boolean] = Seq.fill(numElem)(false),
    goldenFillS: Seq[Boolean] = Seq.fill(numElem)(false)
  ): Unit = {
    import ElemFlags._
    dut.out.valid.expect(true.B)
    for (i <- 0 until numElem) {
      val flags = if (goldenFillZ(i)) ZERO else if (goldenValid(i)) LOOKUP else PREFILL
      val data  = if (goldenFillZ(i)) 0 else goldenIdx(i)
      dut.out.elemFlags(i).expect(flags, s"elemFlags($i)")
      dut.out.elemData(i).expect(data.U, s"elemData($i)")
    }
  }

  // -- Golden Models -----------------------------------------------------

  def goldenGather(vs1: BigInt, sew: Int, ei: Int, lmulFrac: Int, uopIdx: Int): Seq[(Int, Boolean, Boolean, Boolean)] = {
    val s = sewb(sew)
    val nIdx = dlen / ei
    val vlmax = lmulFrac * vlen / 64
    val res = Array.fill(numElem)((0, false, false, false))
    for (ie <- 0 until nIdx; j <- 0 until s) {
      val raw = ((vs1 >> (ie * ei)) & ((BigInt(1) << ei) - 1)).toInt
      val pos = ie * s + j
      if (pos < numElem) {
        val idx = raw * s + j
        res(pos) = (idx, idx < vlmax, idx >= vlmax, false)
      }
    }
    res.toSeq
  }

  def goldenSlideUp(sew: Int, offset: Int, vl: Int, isSlide1Up: Boolean): Seq[(Int, Boolean, Boolean, Boolean)] = {
    val s = sewb(sew)
    val e8Off = offset << sewShift(sew)
    val e8Vl = vl << sewShift(sew)
    (0 until numElem).map { i =>
      if (isSlide1Up) {
        val idx = (i - s).max(0)
        (idx, i >= s, false, i < s)
      } else {
        val overflow = offset >= vl
        if (overflow || (i < e8Vl && i < e8Off)) (i, true, false, false)
        else (i - e8Off, true, false, false)
      }
    }
  }

  def goldenSlideDown(
    sew: Int, offset: Int, lmulFrac: Int, tableIdx: Int,
    isSlide1Down: Boolean, vl: Int = 16
  ): Seq[(Int, Boolean, Boolean, Boolean)] = {
    val s = sewb(sew)
    val e8Off = offset << sewShift(sew)
    val vlmax8b = lmulFrac * vlen / 64
    val idxBits = (vlen - 1).toBinaryString.length
    val sewElemsPerChunk = dlen / sew
    (0 until numElem).map { i =>
      if (isSlide1Down) {
        val iPlus = tableIdx * numElem + i + s
        val index = iPlus & ((1 << idxBits) - 1)
        val sewPos = tableIdx * sewElemsPerChunk + i / s
        (index, sewPos != (vl - 1), false, sewPos == (vl - 1))
      } else {
        val iPlus = tableIdx * numElem + i + e8Off
        val ge = iPlus >= vlmax8b
        (iPlus & ((1 << idxBits) - 1), !ge, ge, false)
      }
    }
  }

  def goldenZip(zipn: Int, sew: Int, uopIdx: Int): Seq[Option[Int]] = {
    val sb = sewb(sew); val ll = zipn * sb
    (0 until numElem).map { i =>
      val g = uopIdx * numElem + i
      if (g < vlenb * zipn) {
        val r = g % ll; val q = g / ll
        Some(q * sb + r / sb * vlenb + g % sb)
      } else None
    }
  }

  def goldenUnzip(unzipn: Int, sew: Int, uopIdx: Int): Seq[Option[Int]] = {
    val sb = sewb(sew); val times = unzipn * sb
    (0 until numElem).map { i =>
      val g = uopIdx * numElem + i
      if (g < unzipn * vlenb) Some(g % vlenb / sb * times + g / vlenb * sb + g % sb)
      else None
    }
  }

  def goldenCompress(mask: BigInt, uopIdx: Int): (Seq[(Int, Boolean)], Int) = {
    val splitted = 8 * vlen / dlen
    val sums = (0 until splitted).map { j =>
      (mask & ((BigInt(1) << (dlenb * (j + 1)).min(vlen)) - 1)).bitCount
    }
    val total = sums.last
    val idxBits = (vlen - 1).toBinaryString.length
    val results = (0 until numElem).map { i =>
      val g = uopIdx * numElem + i
      val gc = sums.count(s => g >= s)
      ((gc * numElem + i) & ((1 << idxBits) - 1), g < total)
    }
    (results, total)
  }

  // -- Tests -------------------------------------------------------------

  describe("IndexGen") {

    describe("Gather") {
      it("e64ei64 identity") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); val g = goldenGather(0, 64, 64, 8, 0)
          dut.in.valid.poke(true); dut.in.bits.op.isGather.poke(true)
          pSew(dut.in.bits.sew, 64); pSew(dut.in.bits.ei, 64)
          dut.clock.step()
          eIndex(dut, g.map(_._1), g.map(_._2), g.map(_._3), g.map(_._4))
        }
      }
      it("e32ei32 nonzero") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); val g = goldenGather(3, 32, 32, 8, 0)
          dut.in.valid.poke(true); dut.in.bits.op.isGather.poke(true)
          pSew(dut.in.bits.sew, 32); pSew(dut.in.bits.ei, 32); dut.in.bits.vs1.poke(3)
          dut.clock.step()
          eIndex(dut, g.map(_._1), g.map(_._2), g.map(_._3), g.map(_._4))
        }
      }
      it("e8ei8 passthrough") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut)
          val vs1 = BigInt("0102030405060708090a0b0c0d0e0f10", 16)
          val g = goldenGather(vs1, 8, 8, 8, 0)
          dut.in.valid.poke(true); dut.in.bits.op.isGather.poke(true)
          pSew(dut.in.bits.sew, 8); pSew(dut.in.bits.ei, 8); dut.in.bits.vs1.poke(vs1)
          dut.clock.step()
          eIndex(dut, g.map(_._1), g.map(_._2), g.map(_._3), g.map(_._4))
        }
      }
      it("out-of-range fillZeros") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); val g = goldenGather(1, 64, 64, 4, 0)
          dut.in.valid.poke(true); dut.in.bits.op.isGather.poke(true)
          pSew(dut.in.bits.sew, 64); pSew(dut.in.bits.ei, 64)
          pLmul(dut.in.bits.lmul, 4); dut.in.bits.vs1.poke(1)
          dut.clock.step()
          eIndex(dut, g.map(_._1), g.map(_._2), g.map(_._3), g.map(_._4))
        }
      }
    }

    describe("SlideUp") {
      it("vslideup offset=2 SEW=32") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); val g = goldenSlideUp(32, 2, 4, false)
          dut.in.valid.poke(true); dut.in.bits.op.isSlideUp.poke(true)
          pSew(dut.in.bits.sew, 32); dut.in.bits.rs1.poke(2); dut.in.bits.vl.poke(4)
          dut.clock.step()
          eIndex(dut, g.map(_._1), g.map(_._2), g.map(_._3), g.map(_._4))
        }
      }
      it("vslide1up SEW=64") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); val g = goldenSlideUp(64, 0, 2, true)
          dut.in.valid.poke(true); dut.in.bits.op.isSlide1Up.poke(true)
          pSew(dut.in.bits.sew, 64); dut.in.bits.vl.poke(2)
          dut.clock.step()
          eIndex(dut, g.map(_._1), g.map(_._2), g.map(_._3), g.map(_._4))
        }
      }
    }

    describe("SlideDown") {
      it("vslidedown offset=1 SEW=16") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); val g = goldenSlideDown(16, 1, 8, 0, false)
          dut.in.valid.poke(true); dut.in.bits.op.isSlideDown.poke(true)
          pSew(dut.in.bits.sew, 16); dut.in.bits.rs1.poke(1)
          dut.clock.step()
          eIndex(dut, g.map(_._1), g.map(_._2), g.map(_._3), g.map(_._4))
        }
      }
      it("vslide1down SEW=32") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); val g = goldenSlideDown(32, 0, 8, 0, true, 16)
          dut.in.valid.poke(true); dut.in.bits.op.isSlide1Down.poke(true)
          pSew(dut.in.bits.sew, 32)
          dut.clock.step()
          eIndex(dut, g.map(_._1), g.map(_._2), g.map(_._3), g.map(_._4))
        }
      }
      it("vslidedown SEW=8 boundary fillZeros") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); val g = goldenSlideDown(8, 15, 8, 0, false)
          dut.in.valid.poke(true); dut.in.bits.op.isSlideDown.poke(true)
          pSew(dut.in.bits.sew, 8); dut.in.bits.rs1.poke(15)
          dut.clock.step()
          eIndex(dut, g.map(_._1), g.map(_._2), g.map(_._3), g.map(_._4))
        }
      }
    }

    describe("Zip") {
      it("2-way SEW=8") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); val g = goldenZip(2, 8, 0)
          dut.in.valid.poke(true); dut.in.bits.op.isZip.poke(true)
          pSew(dut.in.bits.sew, 8); dut.in.bits.zipNum.poke(1)
          dut.clock.step()
          eIndex(dut, g.map(_.getOrElse(0)), g.map(_.nonEmpty))
        }
      }
      it("4-way SEW=16") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); val g = goldenZip(4, 16, 0)
          dut.in.valid.poke(true); dut.in.bits.op.isZip.poke(true)
          pSew(dut.in.bits.sew, 16); dut.in.bits.zipNum.poke(3)
          dut.clock.step()
          eIndex(dut, g.map(_.getOrElse(0)), g.map(_.nonEmpty))
        }
      }
    }

    describe("Unzip") {
      it("2-way SEW=8") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); val g = goldenUnzip(2, 8, 0)
          dut.in.valid.poke(true); dut.in.bits.op.isUnzip.poke(true)
          pSew(dut.in.bits.sew, 8); dut.in.bits.zipNum.poke(1)
          dut.clock.step()
          eIndex(dut, g.map(_.getOrElse(0)), g.map(_.nonEmpty))
        }
      }
      it("4-way SEW=32 uopIdx=1") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); val g = goldenUnzip(4, 32, 1)
          dut.in.valid.poke(true); dut.in.bits.op.isUnzip.poke(true)
          pSew(dut.in.bits.sew, 32); dut.in.bits.zipNum.poke(3); dut.in.bits.uopIdx.poke(1)
          dut.clock.step()
          eIndex(dut, g.map(_.getOrElse(0)), g.map(_.nonEmpty))
        }
      }
    }

    describe("Compress") {
      it("all active") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); val (g, t) = goldenCompress((BigInt(1) << numElem) - 1, 0)
          dut.in.valid.poke(true); dut.in.bits.op.isCompress.poke(true)
          dut.in.bits.vs1.poke((BigInt(1) << numElem) - 1)
          dut.clock.step()
          dut.out.valid.expect(true.B)
          for (i <- 0 until numElem) {
            val (idx, valid) = g(i)
            dut.out.elemFlags(i).expect(if (valid) ElemFlags.LOOKUP else ElemFlags.PREFILL)
            if (valid) dut.out.elemData(i).expect(idx.U)
          }
          dut.out.compressCount1s.expect(t.U)
        }
      }
      it("alternating mask") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); val (g, t) = goldenCompress(BigInt("5555", 16), 0)
          dut.in.valid.poke(true); dut.in.bits.op.isCompress.poke(true)
          dut.in.bits.vs1.poke(BigInt("5555", 16))
          dut.clock.step()
          for (i <- 0 until numElem) { val (idx, v) = g(i); dut.out.elemFlags(i).expect(if (v) ElemFlags.LOOKUP else ElemFlags.PREFILL); if (v) dut.out.elemData(i).expect(idx.U) }
          dut.out.compressCount1s.expect(t.U)
        }
      }
      it("none active") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut)
          dut.in.valid.poke(true); dut.in.bits.op.isCompress.poke(true)
          dut.clock.step()
          for (i <- 0 until numElem) dut.out.elemFlags(i).expect(ElemFlags.PREFILL)
          dut.out.compressCount1s.expect(0.U)
        }
      }
      it("vta=true, tail elements fill all-1s") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut)
          // vs3 = non-zero pattern, so we can distinguish vs3 from all-1s
          dut.in.bits.vs3.poke(BigInt("AABBCCDDEEFF00112233445566778899", 16))
          dut.in.bits.vta.poke(true); dut.in.bits.vma.poke(false)
          val mask = BigInt("5555", 16) // 8 bits set
          dut.in.valid.poke(true); dut.in.bits.op.isCompress.poke(true)
          dut.in.bits.vs1.poke(mask)
          dut.clock.step()
          // tail elements (i=8..15) should have elemData=0xFF
          for (i <- 8 until numElem) {
            dut.out.elemFlags(i).expect(ElemFlags.PREFILL)
            dut.out.elemData(i).expect(0xFF.U)
          }
        }
      }
      it("vta=false, tail elements fill vs3") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut)
          val vs3 = BigInt("AABBCCDDEEFF00112233445566778899", 16)
          dut.in.bits.vs3.poke(vs3)
          dut.in.bits.vta.poke(false); dut.in.bits.vma.poke(false)
          val mask = BigInt("FF00", 16) // first 8 bytes active
          dut.in.valid.poke(true); dut.in.bits.op.isCompress.poke(true)
          dut.in.bits.vs1.poke(mask)
          dut.clock.step()
          // tail elements (i=8..15) should have elemData = vs3 byte
          for (i <- 8 until numElem) {
            dut.out.elemFlags(i).expect(ElemFlags.PREFILL)
            val expectedByte = ((vs3 >> (i * 8)) & 0xFF).toInt
            dut.out.elemData(i).expect(expectedByte.U)
          }
        }
      }
    }

    describe("Invalid input") {
      it("valid=false -> output not valid") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); dut.clock.step()
          dut.out.valid.expect(false.B)
        }
      }
    }
  }
}
