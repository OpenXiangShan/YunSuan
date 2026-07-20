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

  // ── Helpers: poke IndexGen.In fields ──────────────────────────────────

  /** Set all IndexGen.In fields to safe defaults, then override specific fields */
  def pDefaults(dut: IndexGen): Unit = {
    dut.in.valid.poke(false)
    dut.in.bits.opcode.poke(0)
    dut.in.bits.uopIdx.poke(0)
    pSew(dut.in.bits.sew, 8)
    pSew(dut.in.bits.ei, 8)
    pLmul(dut.in.bits.lmul, 8) // m1
    dut.in.bits.vl.poke(16)
    dut.in.bits.isGatherEI16.poke(false)
    dut.in.bits.vs1.poke(0)
    dut.in.bits.mask.poke(0)
    dut.in.bits.tableIdx.poke(0)
    dut.in.bits.offset.poke(0)
    dut.in.bits.offsetOverflow.poke(false)
    dut.in.bits.isSlide1Up.poke(false)
    dut.in.bits.isSlide1Down.poke(false)
    dut.in.bits.zipNum.poke(0) // zipn=1 (2-way)
    dut.in.bits.byteMask.poke(0)
    for (i <- 0 until numElem) dut.in.bits.compressData(i).poke(0)
    dut.in.bits.vs3.poke(0)
  }

  def pOpcode(dut: IndexGen, op: UInt): Unit = dut.in.bits.opcode.poke(op)

  /** Expect IndexGen.Out fields match golden values */
  def eIndex(
    dut: IndexGen,
    goldenIdx: Seq[Int],
    goldenValid: Seq[Boolean],
    goldenFillZ: Seq[Boolean] = Seq.fill(numElem)(false),
    goldenFillS: Seq[Boolean] = Seq.fill(numElem)(false)
  ): Unit = {
    dut.out.valid.expect(true.B)
    for (i <- 0 until numElem) {
      dut.out.index.index(i).expect(goldenIdx(i).U, s"index($i)")
      dut.out.index.indexValid(i).expect(goldenValid(i).B, s"indexValid($i)")
      dut.out.index.fillZeros(i).expect(goldenFillZ(i).B, s"fillZeros($i)")
      dut.out.index.fillScala(i).expect(goldenFillS(i).B, s"fillScala($i)")
    }
  }

  // ── Golden Models ─────────────────────────────────────────────────────

  /** Golden for vrgather: SEW-level index → 8b granularity indices */
  def goldenGather(vs1: BigInt, sew: Int, ei: Int, lmulFrac: Int, uopIdx: Int): Seq[(Int, Boolean, Boolean, Boolean)] = {
    val sewbVal = sewb(sew)
    val nIdxElems = dlen / ei
    val vlmax = lmulFrac * vlen / 64
    val results = Array.fill(numElem)((0, false, false, false))
    for (idxElem <- 0 until nIdxElems) {
      val rawIdx = ((vs1 >> (idxElem * ei)) & ((BigInt(1) << ei) - 1)).toInt
      for (j <- 0 until sewbVal) {
        val eightBIdx = rawIdx * sewbVal + j
        val outPos = idxElem * sewbVal + j
        if (outPos < numElem) {
          val geVlmax = eightBIdx >= vlmax
          results(outPos) = (eightBIdx, !geVlmax, geVlmax, false)
        }
      }
    }
    results.toSeq
  }

  /** Golden for vslideup */
  def goldenSlideUp(
    sew: Int, offset: Int, vl: Int, tableIdx: Int, offsetOverflow: Boolean, isSlide1Up: Boolean
  ): Seq[(Int, Boolean, Boolean, Boolean)] = {
    val s = sewb(sew)
    val e8Off = offset << sewShift(sew)
    val e8Vl = vl << sewShift(sew)
    (0 until numElem).map { i =>
      if (isSlide1Up) {
        // vslide1up: vd[i] = (i==0) ? scalar : vs2[i-1]
        val idx = (i - s).max(0)
        (idx, i >= s, false, i < s)
      } else {
        // vslideup: vd[i] = (i < offset) ? vs2[i] : vs2[i-offset]
        if (offsetOverflow || (i < e8Vl && i < e8Off))
          (i, true, false, false)
        else
          (i - e8Off, true, false, false)
      }
    }
  }

  /** Golden for vslidedown.
    * Index = (tableIdx*NumElem + i + e8Off) truncated to IndexWidth bits.
    * slidedown: fillZeros when iPlus >= vlmax.
    * slide1down: fillScala when SEW-level position == vl-1.
    */
  def goldenSlideDown(
    sew: Int, offset: Int, lmulFrac: Int, tableIdx: Int, offsetOverflow: Boolean,
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
        // iIsVlM1: SEW-level position == vl-1 (vl is in SEW elements)
        val sewPos = tableIdx * sewElemsPerChunk + i / s
        val isLastSew = sewPos == (vl - 1)
        (index, !isLastSew, false, isLastSew)
      } else {
        val iPlus = tableIdx * numElem + i + e8Off
        val geBoundary = iPlus >= vlmax8b
        val index = iPlus & ((1 << idxBits) - 1)
        (index, !geBoundary, geBoundary, false)
      }
    }
  }

  /** Golden for zip interleave: index = q * sewb + r / sewb * vlenb + i % sewb */
  def goldenZip(zipn: Int, sew: Int, uopIdx: Int): Seq[Option[Int]] = {
    val sewbVal = sewb(sew)
    val loopLength = zipn * sewbVal
    (0 until numElem).map { i =>
      val globalI = uopIdx * numElem + i
      if (globalI < vlenb * zipn) {
        val r = globalI % loopLength
        val q = globalI / loopLength
        Some(q * sewbVal + r / sewbVal * vlenb + globalI % sewbVal)
      } else None
    }
  }

  /** Golden for unzip deinterleave: index = i % vlenb / sewb * times + i / vlenb * sewb + i % sewb */
  def goldenUnzip(unzipn: Int, sew: Int, uopIdx: Int): Seq[Option[Int]] = {
    val sewbVal = sewb(sew)
    val times = unzipn * sewbVal
    (0 until numElem).map { i =>
      val globalI = uopIdx * numElem + i
      if (globalI < unzipn * vlenb) {
        Some(globalI % vlenb / sewbVal * times + globalI / vlenb * sewbVal + globalI % sewbVal)
      } else None
    }
  }

  /** Golden for vcompress: index = geCount * dlenb + i, truncated to IndexWidth bits.
    * valid = globalPos < total (popcount of full mask).
    */
  def goldenCompress(mask: BigInt, uopIdx: Int): (Seq[(Int, Boolean)], Int) = {
    val splitted = 8 * vlen / dlen
    val sums = (0 until splitted).map { j =>
      val bits = (dlenb * (j + 1)).min(vlen)
      (mask & ((BigInt(1) << bits) - 1)).bitCount
    }
    val total = sums.last
    val idxBits = (vlen - 1).toBinaryString.length // IndexWidth = log2Ceil(VLEN)
    val results = (0 until numElem).map { i =>
      val globalPos = uopIdx * numElem + i
      val geCount = sums.count(s => globalPos >= s)
      val fullIndex = geCount * numElem + i
      val index = fullIndex & ((1 << idxBits) - 1) // MSB truncation
      (index, globalPos < total)
    }
    (results, total)
  }

  // ── Tests ─────────────────────────────────────────────────────────────

  describe("IndexGen") {

    describe("Gather (vrgather)") {
      it("e64ei64, identity index") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); pOpcode(dut, IndexGenOpcode.gather)
          val vs1 = BigInt(0) // element 0→0, element 1→0
          val golden = goldenGather(vs1, sew = 64, ei = 64, lmulFrac = 8, uopIdx = 0)
          dut.in.valid.poke(true)
          pSew(dut.in.bits.sew, 64); pSew(dut.in.bits.ei, 64)
          dut.in.bits.vs1.poke(vs1)
          dut.clock.step()
          eIndex(dut,
            golden.map(_._1), golden.map(_._2),
            golden.map(_._3), golden.map(_._4))
        }
      }
      it("e32ei32, nonzero index") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); pOpcode(dut, IndexGenOpcode.gather)
          val vs1 = BigInt(3) // element 0→3
          val golden = goldenGather(vs1, sew = 32, ei = 32, lmulFrac = 8, uopIdx = 0)
          dut.in.valid.poke(true)
          pSew(dut.in.bits.sew, 32); pSew(dut.in.bits.ei, 32)
          dut.in.bits.vs1.poke(vs1)
          dut.clock.step()
          eIndex(dut, golden.map(_._1), golden.map(_._2), golden.map(_._3), golden.map(_._4))
        }
      }
      it("e8ei8, passthrough") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); pOpcode(dut, IndexGenOpcode.gather)
          val vs1 = BigInt("0102030405060708090a0b0c0d0e0f10", 16)
          val golden = goldenGather(vs1, sew = 8, ei = 8, lmulFrac = 8, uopIdx = 0)
          dut.in.valid.poke(true)
          pSew(dut.in.bits.sew, 8); pSew(dut.in.bits.ei, 8)
          dut.in.bits.vs1.poke(vs1)
          dut.clock.step()
          eIndex(dut, golden.map(_._1), golden.map(_._2), golden.map(_._3), golden.map(_._4))
        }
      }
      it("e64ei64, out-of-range → fillZeros") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); pOpcode(dut, IndexGenOpcode.gather)
          val vs1 = BigInt(1) // element 0→1, >= vlmax for mf2
          val golden = goldenGather(vs1, sew = 64, ei = 64, lmulFrac = 4, uopIdx = 0)
          dut.in.valid.poke(true)
          pSew(dut.in.bits.sew, 64); pSew(dut.in.bits.ei, 64)
          pLmul(dut.in.bits.lmul, 4) // mf2
          dut.in.bits.vs1.poke(vs1)
          dut.clock.step()
          eIndex(dut, golden.map(_._1), golden.map(_._2), golden.map(_._3), golden.map(_._4))
        }
      }
    }

    describe("SlideUp") {
      it("vslideup offset=2, SEW=32") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); pOpcode(dut, IndexGenOpcode.slideUp)
          val sew = 32; val offset = 2
          val golden = goldenSlideUp(sew, offset, vl = 4, tableIdx = 0, offsetOverflow = false, isSlide1Up = false)
          dut.in.valid.poke(true)
          pSew(dut.in.bits.sew, sew)
          dut.in.bits.offset.poke(offset)
          dut.in.bits.vl.poke(4)
          dut.clock.step()
          eIndex(dut, golden.map(_._1), golden.map(_._2), golden.map(_._3), golden.map(_._4))
        }
      }
      it("vslide1up, SEW=64") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); pOpcode(dut, IndexGenOpcode.slideUp)
          val sew = 64
          val golden = goldenSlideUp(sew, offset = 0, vl = 2, tableIdx = 0, offsetOverflow = false, isSlide1Up = true)
          dut.in.valid.poke(true)
          pSew(dut.in.bits.sew, sew)
          dut.in.bits.isSlide1Up.poke(true)
          dut.in.bits.vl.poke(2)
          dut.clock.step()
          eIndex(dut, golden.map(_._1), golden.map(_._2), golden.map(_._3), golden.map(_._4))
        }
      }
    }

    describe("SlideDown") {
      it("vslidedown offset=1, SEW=16, LMUL=m1") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); pOpcode(dut, IndexGenOpcode.slideDown)
          val sew = 16; val offset = 1
          val golden = goldenSlideDown(sew, offset, lmulFrac = 8, tableIdx = 0, offsetOverflow = false, isSlide1Down = false)
          dut.in.valid.poke(true)
          pSew(dut.in.bits.sew, sew)
          dut.in.bits.offset.poke(offset)
          dut.clock.step()
          eIndex(dut, golden.map(_._1), golden.map(_._2), golden.map(_._3), golden.map(_._4))
        }
      }
      it("vslide1down, SEW=32") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); pOpcode(dut, IndexGenOpcode.slideDown)
          val sew = 32; val vl = 16
          val golden = goldenSlideDown(sew, offset = 0, lmulFrac = 8, tableIdx = 0,
            offsetOverflow = false, isSlide1Down = true, vl = vl)
          dut.in.valid.poke(true)
          pSew(dut.in.bits.sew, sew)
          dut.in.bits.isSlide1Down.poke(true)
          dut.clock.step()
          eIndex(dut, golden.map(_._1), golden.map(_._2), golden.map(_._3), golden.map(_._4))
        }
      }
      it("vslidedown SEW=8, last element → fillZeros") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); pOpcode(dut, IndexGenOpcode.slideDown)
          val sew = 8; val offset = 15 // near boundary
          val golden = goldenSlideDown(sew, offset, lmulFrac = 8, tableIdx = 0, offsetOverflow = false, isSlide1Down = false)
          dut.in.valid.poke(true)
          pSew(dut.in.bits.sew, sew)
          dut.in.bits.offset.poke(offset)
          dut.clock.step()
          eIndex(dut, golden.map(_._1), golden.map(_._2), golden.map(_._3), golden.map(_._4))
        }
      }
    }

    describe("Zip") {
      it("2-way, SEW=8") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); pOpcode(dut, IndexGenOpcode.zip)
          val golden = goldenZip(zipn = 2, sew = 8, uopIdx = 0)
          dut.in.valid.poke(true)
          pSew(dut.in.bits.sew, 8)
          dut.in.bits.zipNum.poke(1) // zipn-1 = 1 → 2-way
          dut.clock.step()
          val valid = golden.map(_.nonEmpty)
          val idx   = golden.map(_.getOrElse(0))
          eIndex(dut, idx, valid)
        }
      }
      it("4-way, SEW=16") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); pOpcode(dut, IndexGenOpcode.zip)
          val golden = goldenZip(zipn = 4, sew = 16, uopIdx = 0)
          dut.in.valid.poke(true)
          pSew(dut.in.bits.sew, 16)
          dut.in.bits.zipNum.poke(3) // 4-way
          dut.clock.step()
          val valid = golden.map(_.nonEmpty)
          val idx   = golden.map(_.getOrElse(0))
          eIndex(dut, idx, valid)
        }
      }
    }

    describe("Unzip") {
      it("2-way, SEW=8") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); pOpcode(dut, IndexGenOpcode.unzip)
          val golden = goldenUnzip(unzipn = 2, sew = 8, uopIdx = 0)
          dut.in.valid.poke(true)
          pSew(dut.in.bits.sew, 8)
          dut.in.bits.zipNum.poke(1) // 2-way
          dut.clock.step()
          val valid = golden.map(_.nonEmpty)
          val idx   = golden.map(_.getOrElse(0))
          eIndex(dut, idx, valid)
        }
      }
      it("4-way, SEW=32, uopIdx=1") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); pOpcode(dut, IndexGenOpcode.unzip)
          val golden = goldenUnzip(unzipn = 4, sew = 32, uopIdx = 1)
          dut.in.valid.poke(true)
          pSew(dut.in.bits.sew, 32)
          dut.in.bits.zipNum.poke(3) // 4-way
          dut.in.bits.uopIdx.poke(1)
          dut.clock.step()
          val valid = golden.map(_.nonEmpty)
          val idx   = golden.map(_.getOrElse(0))
          eIndex(dut, idx, valid)
        }
      }
    }

    describe("Compress") {
      it("all bytes active") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); pOpcode(dut, IndexGenOpcode.compress)
          val mask = (BigInt(1) << numElem) - 1
          val (golden, total) = goldenCompress(mask, uopIdx = 0)
          dut.in.valid.poke(true)
          dut.in.bits.byteMask.poke(mask)
          dut.clock.step() // CompressIndexGen has 1-cycle pipeline
          dut.out.valid.expect(true.B)
          for (i <- 0 until numElem) {
            val (idx, valid) = golden(i)
            dut.out.index.index(i).expect(idx.U)
            dut.out.index.indexValid(i).expect(valid.B)
          }
          dut.out.compressCount1s.expect(total.U)
        }
      }
      it("alternating mask") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); pOpcode(dut, IndexGenOpcode.compress)
          val mask = BigInt("5555", 16)
          val (golden, total) = goldenCompress(mask, uopIdx = 0)
          dut.in.valid.poke(true)
          dut.in.bits.byteMask.poke(mask)
          dut.clock.step()
          dut.out.valid.expect(true.B)
          for (i <- 0 until numElem) {
            val (idx, valid) = golden(i)
            dut.out.index.index(i).expect(idx.U)
            dut.out.index.indexValid(i).expect(valid.B)
          }
          dut.out.compressCount1s.expect(total.U)
        }
      }
      it("no bytes active") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut); pOpcode(dut, IndexGenOpcode.compress)
          dut.in.valid.poke(true)
          dut.in.bits.byteMask.poke(0)
          dut.clock.step()
          dut.out.valid.expect(true.B)
          for (i <- 0 until numElem) {
            dut.out.index.indexValid(i).expect(false.B)
          }
          dut.out.compressCount1s.expect(0.U)
        }
      }
    }

    describe("Invalid input") {
      it("valid=false → output not valid") {
        simulate(new IndexGen()(GatherParam.default)) { dut =>
          pDefaults(dut)
          dut.in.valid.poke(false)
          dut.clock.step()
          dut.out.valid.expect(false.B)
        }
      }
    }
  }
}
