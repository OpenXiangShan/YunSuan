package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.util.SpecBase
import yunsuan.vector.v2.Shuffle.GatherCommon.GatherParam

class GatherIndexGenSpec extends SpecBase with GatherSpecUtils {


  // SEW -> number of 8b elements per SEW element
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


  def pGather(dut: GatherIndexGen, vs1: BigInt, sew: Int, ei: Int, lmulFrac: Int, uopIdx: Int): Unit = {
    dut.in.valid.poke(true)
    dut.in.bits.isGatherEI16.poke(false)
    dut.in.bits.vs1.poke(vs1)
    dut.in.bits.uopIdx.poke(uopIdx)
    pSew(dut.in.bits.sew, sew)
    pSew(dut.in.bits.ei, ei)
    pLmul(dut.in.bits.lmul, lmulFrac)
  }

  def pGatherEI16(dut: GatherIndexGen, vs1: BigInt, sew: Int, lmulFrac: Int, uopIdx: Int): Unit = {
    dut.in.valid.poke(true)
    dut.in.bits.isGatherEI16.poke(true)
    dut.in.bits.vs1.poke(vs1)
    dut.in.bits.uopIdx.poke(uopIdx)
    pSew(dut.in.bits.sew, sew)
    pSew(dut.in.bits.ei, 16)
    pLmul(dut.in.bits.lmul, lmulFrac)
  }

  def eGather(dut: GatherIndexGen, golden: Seq[(Int, Boolean, Boolean, Boolean)]): Unit = {
    dut.out.gather.valid.expect(true.B)
    for (i <- 0 until numElem) {
      val (idx, valid, fillZ, fillS) = golden(i)
      // index is don't-care when fillZeros; only check validity flags
      if (!fillZ) {
        dut.out.gather.bits.index(i).expect(idx.U)
      }
      dut.out.gather.bits.indexValid(i).expect(valid.B)
      dut.out.gather.bits.fillZeros(i).expect(fillZ.B)
      dut.out.gather.bits.fillScala(i).expect(fillS.B)
    }
  }

  // Golden for vrgatherei16 (EI=16, SEW>16): vs1 is sliced by uopIdx (e32: 1/2,
  // e64: 1/4); each sliced e16 index i produces sewb consecutive 8b indices
  // i*sewb .. i*sewb+sewb-1; OOR if the e16 index >= vlmax (in SEW elements).
  def goldenGatherEI16(vs1: BigInt, sew: Int, lmulFrac: Int, uopIdx: Int): Seq[(Int, Boolean, Boolean, Boolean)] = {
    val sewbVal = sewb(sew)                        // 4 (e32) / 8 (e64)
    val sliceBits = dlen / (sew / 16)              // 64 (e32) / 32 (e64)
    // slice is reused every dlen/sliceBits uops (e32: 2, e64: 4)
    val sliceOff = (uopIdx % (dlen / sliceBits)) * sliceBits
    val slice = (vs1 >> sliceOff) & ((BigInt(1) << sliceBits) - 1)
    val vlmaxSew = lmulFrac * vlen / 64 / sewbVal  // vlmax in SEW elements
    (0 until numElem).map { k =>
      val i = k / sewbVal
      val j = k % sewbVal
      val rawIdx = ((slice >> (i * 16)) & 0xffff).toInt
      val eightBIdx = rawIdx * sewbVal + j
      val geVlmax = rawIdx >= vlmaxSew
      (eightBIdx, !geVlmax, geVlmax, false)
    }
  }

  // vrgatherei16 drives out.gather.bits (the merged flags that become elemFlags in
  // the wrapper IndexGen), but out.gather.valid is false (gatherei16.valid pulses).
  def eGatherEI16Bits(dut: GatherIndexGen, golden: Seq[(Int, Boolean, Boolean, Boolean)]): Unit = {
    dut.out.gatherei16.valid.expect(true.B)
    for (i <- 0 until numElem) {
      val (idx, valid, fillZ, fillS) = golden(i)
      if (!fillZ) {
        dut.out.gather.bits.index(i).expect(idx.U)
      }
      dut.out.gather.bits.indexValid(i).expect(valid.B)
      dut.out.gather.bits.fillZeros(i).expect(fillZ.B)
      dut.out.gather.bits.fillScala(i).expect(fillS.B)
    }
  }

  // Named canary for the emu vrgatherei16_vv-1 difftest failure (v9_high =
  // 0xfffffff8): an out-of-range e16 index (0xfff8/0xffff, >= vlmax) must become
  // fillZeros, not a table lookup. The all-configs test below covers the general
  // case; this keeps an explicit reproduction of the fixed bug.
  describe("vrgatherei16 (SEW>16)") {
    it("e32ei16 uop1 out-of-range 0xfff8/0xffff -> fillZeros (emu v9_high bug)") {
      simulate(new GatherIndexGen()(GatherParam.default)) { dut =>
        val sew = 32; val lmulFrac = 64
        // uop1 slice = high 64b = e16 [3,0,0xfff8,0xffff]; 0xfff8/0xffff >= vlmax(32)
        val vs1 = fromSewElems(Seq(0, 0, 1, 0, 3, 0, 0xfff8, 0xffff), 16)
        val golden = goldenGatherEI16(vs1, sew, lmulFrac, uopIdx = 1)
        pGatherEI16(dut, vs1, sew, lmulFrac, uopIdx = 1)
        dut.clock.step()
        eGatherEI16Bits(dut, golden)
      }
    }
  }

  // Stateless (combinational) DUT: cover every (sew, lmul) config of both
  // vrgather.vv (ei=sew) and vrgatherei16 (ei=16) in ONE `it` by re-poking. For
  // each config feed boundary indices around vlmax plus deterministic random
  // indices, split into per-uop chunks, and check every slot against the golden.
  // Configs with fractional VLMAX (< 1 SEW element, e.g. e64/mf8) are excluded:
  // the byte-granular OOR is undefined there.
  it("all sew/lmul configs (ei=sew and ei=16)") {
    simulate(new GatherIndexGen()(GatherParam.default)) { dut =>
      val sewList = Seq(8, 16, 32, 64)
      val lmulList = Seq(1, 2, 4, 8, 16, 32, 64) // lmulFrac: mf8..m8
      def genIndices(sew: Int, lmulFrac: Int): Seq[Int] = {
        val vlmaxSew = (lmulFrac * vlen / 64 / (sew / 8)).max(1)
        val rnd = new scala.util.Random(sew * 131 + lmulFrac * 17)
        // boundary around vlmax, the byte-index truncation wrap (element-level OOR,
        // e.g. e64 index 16 -> byte 0-7), a huge stress index, + deterministic random
        Seq(0, 1, vlmaxSew / 2, (vlmaxSew - 1).max(0), vlmaxSew, vlmaxSew + 1,
          (vlmaxSew * 3) / 2, (vlmaxSew * 2 - 1).max(0), vlmaxSew * 2, vlmaxSew * 2 + 1,
          vlmaxSew * 4, dlen * 8 / sew, 0xfff8) ++ Seq.fill(8)(rnd.nextInt(vlmaxSew * 3 + 2))
      }
      // vrgather.vv (ei = sew): each uop reads the full DLEN vs1
      for (sew <- sewList; lmulFrac <- lmulList if (lmulFrac * vlen / 64) % (sew / 8) == 0) {
        genIndices(sew, lmulFrac).grouped(dlen / sew).foreach { chunk =>
          var vs1 = BigInt(0)
          chunk.zipWithIndex.foreach { case (idx, k) =>
            vs1 |= (BigInt(idx) & ((BigInt(1) << sew) - 1)) << (k * sew)
          }
          val golden = goldenGather(vs1, sew, sew, lmulFrac, 0)
          pGather(dut, vs1, sew, sew, lmulFrac, 0)
          dut.clock.step()
          eGather(dut, golden)
        }
      }
      // vrgatherei16 (ei = 16, sew >= 16): vs1 is sliced by uopIdx
      for (sew <- Seq(16, 32, 64); lmulFrac <- lmulList if (lmulFrac * vlen / 64) % (sew / 8) == 0) {
        val sliceBits = dlen / (sew / 16)
        genIndices(sew, lmulFrac).grouped(dlen / sew).zipWithIndex.foreach { case (chunk, u) =>
          val sliceOff = (u % (dlen / sliceBits)) * sliceBits
          var vs1 = BigInt(0)
          chunk.zipWithIndex.foreach { case (idx, k) =>
            vs1 |= BigInt(idx & 0xffff) << (sliceOff + k * 16)
          }
          val golden = goldenGatherEI16(vs1, sew, lmulFrac, u)
          pGatherEI16(dut, vs1, sew, lmulFrac, u)
          dut.clock.step()
          eGatherEI16Bits(dut, golden)
        }
      }
    }
  }

  // ── ISA cross-validation ───────────────────────────────────────────

  /** Convert DLEN-bit BigInt to Seq of SEW-width elements (little-endian within DLEN) */

  /** Convert Seq of SEW-width elements back to DLEN-bit BigInt */

  /** Flatten SEW-level vs2 values to 8b table */

  /** Assemble 8b results back to SEW-level */

  /** ISA-level vrgather golden: vd[i] = (i&lt;vl && idx&lt;vlmax) ? vs2[idx] : 0 */
  def isaVrgather(vs2Sew: Seq[Int], vs1Sew: Seq[Int], vl: Int, vlmax: Int): Seq[Int] = {
    vs1Sew.zipWithIndex.map { case (idx, i) =>
      if (i < vl && idx < vlmax) vs2Sew(idx) else 0
    }
  }

  /** Cross-validate: ISA golden vs goldenGather + Res.gather */
  def crossValidateVrgather(sew: Int, lmulFrac: Int, vl: Int): Unit = {
    val sewb = sew / 8
    val nSewElems = dlen / sew
    val vlmaxSew = lmulFrac * vlen / 64 / sewb  // vlmax in SEW elements
    val vlmax8b = lmulFrac * vlen / 64

    // Generate test data: vs2 spanning all LMUL chunks
    val vs2Sew = (0 until vlmaxSew).map(i => (i * 0x11 + 0xAB) & ((1 << sew) - 1))
    val vs1Sew = (0 until nSewElems).map { i =>
      if (i < vl) {
        // indices that span both within and beyond vlmax
        val raw = (i * 3 + 1) % (vlmaxSew + 2)
        if (raw >= vlmaxSew) vlmaxSew + 1 else raw
      } else 0
    }

    // Path 1: ISA-level simple golden
    val simpleGold = isaVrgather(vs2Sew, vs1Sew, vl, vlmaxSew)

    // Path 2: 8b-pipeline complex golden
    // Pack vs1 as DLEN-bit BigInt (SEW-level indices packed in little-endian)
    val vs1BigInt = fromSewElems(vs1Sew, sew)
    val eightBIndices = goldenGather(vs1BigInt, sew, sew, lmulFrac, uopIdx = 0)

    val table = sewTo8b(vs2Sew, sew)
    val tableRange = 0 until (lmulFrac * vlen / dlen)  // number of DLEN chunks
    val complexGold8b = (0 until numElem).map { i =>
      val (idx, valid, fillZ, _) = eightBIndices(i)
      if (valid && tableRange.contains(idx / numElem)) table(idx)
      else if (fillZ) 0
      else 0  // prefill=0 for simplicity
    }

    // Reassemble to SEW
    val complexGold = eightBToSew(complexGold8b, sew).take(nSewElems)

    // Compare only elements within vl (tail elements differ due to prefill vs zero)
    val matchOk = (0 until vl).forall(i => simpleGold(i) == complexGold(i))
    assert(matchOk,
      s"\nSEW=$sew LMUL=lmulFrac=$lmulFrac vl=$vl vlmax=$vlmaxSew" +
      s"\n  simple:  ${simpleGold.take(vl).mkString(",")}" +
      s"\n  complex: ${complexGold.take(vl).mkString(",")}")
  }

  describe("cross-validate ISA") {
    val sewLmulVls = Seq(
      // mf8
      (8, 1, 1), (16, 1, 1),
      // mf4
      (8, 2, 4), (16, 2, 2), (32, 2, 1),
      // mf2
      (8, 4, 8), (16, 4, 4), (32, 4, 1), (64, 4, 1),
      // m1
      (8, 8, 16), (8, 8, 8), (16, 8, 8), (16, 8, 4), (32, 8, 4), (32, 8, 2), (64, 8, 2), (64, 8, 1),
      // m2: single-chunk only
      (16, 16, 8), (32, 16, 4), (64, 16, 2),
      // m4: single-chunk only
      (32, 32, 4), (64, 32, 2),
      // m8: single-chunk only
      (64, 64, 2),
    )
    for ((sew, lmulFrac, vl) <- sewLmulVls) {
      it(s"vrgather SEW=$sew LMUL=f$lmulFrac vl=$vl") {
        crossValidateVrgather(sew, lmulFrac, vl)
      }
    }
  }
}
