package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.vector.v2.Shuffle.GatherCommon.GatherParam

class GatherIndexGenSpec extends AnyFunSpec with ChiselSim with Matchers with ParallelTestExecution {

  val dlen = 128
  val vlen = 128
  val numElem = dlen / 8

  // SEW -> number of 8b elements per SEW element
  def sewb(sew: Int): Int = sew / 8
  // SEW -> number of Ei elements per SEI element
  def eib(ei: Int): Int = ei / 8

  // Golden: for given SEW and EI, expand vs1 indices to 8b granularity
  // Each SEW-level index idx produces sewb consecutive 8b indices: idx*sewb, idx*sewb+1, ..., idx*sewb+sewb-1
  // For e.g., e64ei64: vs1[0]=5 → indices 40..47
  def goldenGather(vs1: BigInt, sew: Int, ei: Int, lmulFrac: Int, uopIdx: Int): Seq[(Int, Boolean, Boolean, Boolean)] = {
    val sewbVal = sewb(sew)
    val eibVal = eib(ei)
    val nIdxElems = dlen / ei  // number of index elements in this DLEN chunk
    val nDataElems = dlen / sew // number of data elements covered by this chunk's indices
    val vlmax = lmulFrac * vlen / 64  // vlmax in 8b elements
    val base = uopIdx * dlen / ei  // base index element number

    val results = Array.fill(numElem)((0, false, false, false))
    for (idxElem <- 0 until nIdxElems) {
      val eiMask = (BigInt(1) << ei) - 1
      val shift = idxElem * ei
      val rawIdx = ((vs1 >> shift) & eiMask).toInt
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

  def pSew(dut: GatherSewBundle, sew: Int): Unit = {
    dut.e8.poke((sew == 8).B)
    dut.e16.poke((sew == 16).B)
    dut.e32.poke((sew == 32).B)
    dut.e64.poke((sew == 64).B)
  }

  def pLmul(dut: GatherLmulBundle, lmulFrac: Int): Unit = {
    dut.mf8.poke((lmulFrac == 1).B)
    dut.mf4.poke((lmulFrac == 2).B)
    dut.mf2.poke((lmulFrac == 4).B)
    dut.m1.poke((lmulFrac == 8).B)
    dut.m2.poke((lmulFrac == 16).B)
    dut.m4.poke((lmulFrac == 32).B)
    dut.m8.poke((lmulFrac == 64).B)
  }

  def pGather(dut: GatherIndexGen, vs1: BigInt, sew: Int, ei: Int, lmulFrac: Int, uopIdx: Int): Unit = {
    dut.in.valid.poke(true)
    dut.in.bits.isGatherEI16.poke(false)
    dut.in.bits.vs1.poke(vs1)
    dut.in.bits.uopIdx.poke(uopIdx)
    dut.in.bits.mask.poke((BigInt(1) << numElem) - 1)
    pSew(dut.in.bits.sew, sew)
    pSew(dut.in.bits.ei, ei)
    pLmul(dut.in.bits.lmul, lmulFrac)
  }

  def pGatherEI16(dut: GatherIndexGen, vs1: BigInt, sew: Int, lmulFrac: Int, uopIdx: Int): Unit = {
    dut.in.valid.poke(true)
    dut.in.bits.isGatherEI16.poke(true)
    dut.in.bits.vs1.poke(vs1)
    dut.in.bits.uopIdx.poke(uopIdx)
    dut.in.bits.mask.poke((BigInt(1) << numElem) - 1)
    pSew(dut.in.bits.sew, sew)
    pSew(dut.in.bits.ei, 16)
    pLmul(dut.in.bits.lmul, lmulFrac)
  }

  def eGather(dut: GatherIndexGen, golden: Seq[(Int, Boolean, Boolean, Boolean)]): Unit = {
    dut.out.gather.valid.expect(true.B)
    for (i <- 0 until numElem) {
      val (idx, valid, fillZ, fillS) = golden(i)
      dut.out.gather.bits.index(i).expect(idx.U)
      dut.out.gather.bits.indexValid(i).expect(valid.B)
      dut.out.gather.bits.fillZeros(i).expect(fillZ.B)
      dut.out.gather.bits.fillScala(i).expect(fillS.B)
    }
  }

  def eGatherEI16(dut: GatherIndexGen): Unit = {
    dut.out.gatherei16.valid.expect(true.B)
  }

  // vs1 with one non-zero index at element 0
  def vs1OneIdx(idx: Int, ei: Int): BigInt = BigInt(idx) & ((BigInt(1) << ei) - 1)

  describe("vrgather.vv") {
    describe("e64ei64") {
      it("identity index") {
        simulate(new GatherIndexGen()(GatherParam.default)) { dut =>
          val sew = 64; val ei = 64; val lmulFrac = 8
          // vs1[0]=0 → bytes 0..7, vs1[1]=1 → bytes 8..15
          val vs1 = BigInt(1) << 64 | BigInt(0)  // element 0→0, element 1→1
          val golden = goldenGather(vs1, sew, ei, lmulFrac, uopIdx = 0)
          pGather(dut, vs1, sew, ei, lmulFrac, uopIdx = 0)
          dut.clock.step()
          eGather(dut, golden)
        }
      }
      it("nonzero index, LMUL=m1") {
        simulate(new GatherIndexGen()(GatherParam.default)) { dut =>
          val sew = 64; val ei = 64; val lmulFrac = 8
          val vs1 = BigInt(5)  // vs1[0]=5 → bytes 40..47, vs1[1]=0
          val golden = goldenGather(vs1, sew, ei, lmulFrac, uopIdx = 0)
          pGather(dut, vs1, sew, ei, lmulFrac, uopIdx = 0)
          dut.clock.step()
          eGather(dut, golden)
        }
      }
      it("out of range index, LMUL=mf2") {
        simulate(new GatherIndexGen()(GatherParam.default)) { dut =>
          val sew = 64; val ei = 64; val lmulFrac = 4
          // mf2: vlmax = 4*128/64/8 = 1? No...
          // mf2: vlmax in 8b = lmulFrac*vlen/64 = 4*128/64 = 8 bytes = 1 E64 element
          // index 1 → bytes 8..15 → >= 8 → fillZeros
          val vs1 = BigInt(1)
          val golden = goldenGather(vs1, sew, ei, lmulFrac, uopIdx = 0)
          pGather(dut, vs1, sew, ei, lmulFrac, uopIdx = 0)
          dut.clock.step()
          eGather(dut, golden)
        }
      }
    }
    describe("e32ei32") {
      it("nonzero index") {
        simulate(new GatherIndexGen()(GatherParam.default)) { dut =>
          val sew = 32; val ei = 32; val lmulFrac = 8
          // vs1[0]=3 → bytes 12..15
          val vs1 = BigInt(3)
          val golden = goldenGather(vs1, sew, ei, lmulFrac, uopIdx = 0)
          pGather(dut, vs1, sew, ei, lmulFrac, uopIdx = 0)
          dut.clock.step()
          eGather(dut, golden)
        }
      }
    }
    describe("e16ei16") {
      it("nonzero index") {
        simulate(new GatherIndexGen()(GatherParam.default)) { dut =>
          val sew = 16; val ei = 16; val lmulFrac = 8
          val vs1 = BigInt(7)
          val golden = goldenGather(vs1, sew, ei, lmulFrac, uopIdx = 0)
          pGather(dut, vs1, sew, ei, lmulFrac, uopIdx = 0)
          dut.clock.step()
          eGather(dut, golden)
        }
      }
    }
    describe("e8ei8") {
      it("passthrough index") {
        simulate(new GatherIndexGen()(GatherParam.default)) { dut =>
          val sew = 8; val ei = 8; val lmulFrac = 8
          val vs1 = BigInt("0102030405060708090a0b0c0d0e0f10", 16)
          val golden = goldenGather(vs1, sew, ei, lmulFrac, uopIdx = 0)
          pGather(dut, vs1, sew, ei, lmulFrac, uopIdx = 0)
          dut.clock.step()
          eGather(dut, golden)
        }
      }
    }
  }

  describe("vrgatherei16") {
    it("e64ei16, valid") {
      simulate(new GatherIndexGen()(GatherParam.default)) { dut =>
        val sew = 64
        val vs1 = BigInt(0)
        pGatherEI16(dut, vs1, sew, lmulFrac = 8, uopIdx = 0)
        dut.clock.step()
        eGatherEI16(dut)
      }
    }
  }
}
