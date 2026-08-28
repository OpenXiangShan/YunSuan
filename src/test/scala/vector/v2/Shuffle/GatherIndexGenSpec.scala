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

  def eGatherEI16(dut: GatherIndexGen): Unit = {
    dut.out.gatherei16.valid.expect(true.B)
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

  // Boundary-coverage helper: pack each batch of e16 indices into that uop's vs1
  // slice and check every slot's indexValid/fillZeros against the golden (OOR iff
  // the e16 index >= vlmax in SEW elements).
  def checkEi16Boundaries(sew: Int, lmulFrac: Int, batches: Seq[(Int, Seq[Int])]): Unit = {
    simulate(new GatherIndexGen()(GatherParam.default)) { dut =>
      batches.foreach { case (uopIdx, indices) =>
        val sliceBits = dlen / (sew / 16)
        val sliceOff = (uopIdx % (dlen / sliceBits)) * sliceBits
        var vs1 = BigInt(0)
        indices.zipWithIndex.foreach { case (idx, k) =>
          vs1 |= BigInt(idx & 0xffff) << (sliceOff + k * 16)
        }
        val golden = goldenGatherEI16(vs1, sew, lmulFrac, uopIdx)
        pGatherEI16(dut, vs1, sew, lmulFrac, uopIdx)
        dut.clock.step()
        eGatherEI16Bits(dut, golden)
      }
    }
  }

  // Boundary-coverage helper for vrgather.vv (EI=SEW): each batch packs up to
  // dlen/sew SEW-level indices into vs1 and checks every slot's flags against the
  // golden (OOR iff the index >= vlmax in SEW elements).
  def checkGatherBoundaries(sew: Int, lmulFrac: Int, batches: Seq[Seq[Int]]): Unit = {
    simulate(new GatherIndexGen()(GatherParam.default)) { dut =>
      batches.foreach { indices =>
        var vs1 = BigInt(0)
        indices.zipWithIndex.foreach { case (idx, k) =>
          vs1 |= (BigInt(idx) & ((BigInt(1) << sew) - 1)) << (k * sew)
        }
        val golden = goldenGather(vs1, sew, sew, lmulFrac, uopIdx = 0)
        pGather(dut, vs1, sew, sew, lmulFrac, uopIdx = 0)
        dut.clock.step()
        eGather(dut, golden)
      }
    }
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
      it("element value with upper bits set is out-of-range") {
        simulate(new GatherIndexGen()(GatherParam.default)) { dut =>
          val sew = 64; val ei = 64; val lmulFrac = 8  // LMUL=m1, VLMAX=2 elem
          // vs1[0] = 16 (0x10) → lower 4 bits = 0, but full value 16 >= VLMAX=2
          // RTL truncates to 4 bits → gets idx 0, incorrectly treating as valid
          // Spec: element offset 16 >= 2 → should return 0 (fillZeros)
          val vs1 = BigInt(16)
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
    // EI=SEW boundary coverage around vlmax (in SEW elements =
    // lmulFrac*vlen/64/sewb): 0, 1, vlmax/2, vlmax-1, vlmax, vlmax+1, vlmax*1.5,
    // vlmax*2-1, vlmax*2, vlmax*2+1, vlmax*4.
    describe("boundary indices around vlmax") {
      it("e8ei8 m1 vlmax=16") {
        checkGatherBoundaries(8, 8, Seq(
          Seq(0, 1, 8, 15, 16, 17, 24, 31, 32, 33, 64, 0, 0, 0, 0, 0),
        ))
      }
      it("e16ei16 m1 vlmax=8") {
        checkGatherBoundaries(16, 8, Seq(
          Seq(0, 1, 4, 7, 8, 9, 12, 15),
          Seq(16, 17, 32, 0, 0, 0, 0, 0),
        ))
      }
      it("e32ei32 m1 vlmax=4") {
        checkGatherBoundaries(32, 8, Seq(
          Seq(0, 1, 2, 3),
          Seq(4, 5, 6, 7),
          Seq(8, 9, 16, 0),
        ))
      }
      it("e64ei64 m1 vlmax=2") {
        checkGatherBoundaries(64, 8, Seq(
          Seq(0, 1),
          Seq(2, 3),
          Seq(4, 5),
          Seq(8, 0),
        ))
      }
    }
  }

  describe("vrgatherei16 (SEW>16)") {
    it("e64ei16 uop0 in-range identity") {
      simulate(new GatherIndexGen()(GatherParam.default)) { dut =>
        val sew = 64; val lmulFrac = 64
        val vs1 = fromSewElems(Seq(0, 0, 0, 0, 0, 0, 0, 0), 16) // all indices 0
        val golden = goldenGatherEI16(vs1, sew, lmulFrac, uopIdx = 0)
        pGatherEI16(dut, vs1, sew, lmulFrac, uopIdx = 0)
        dut.clock.step()
        eGatherEI16Bits(dut, golden)
      }
    }
    it("e32ei16 uop0 in-range indices -> LOOKUP with byte idx*4+j") {
      simulate(new GatherIndexGen()(GatherParam.default)) { dut =>
        val sew = 32; val lmulFrac = 64
        // uop0 slice = low 64b = e16 [0,0,1,0] -> bytes 0-3, 0-3, 4-7, 0-3
        val vs1 = fromSewElems(Seq(0, 0, 1, 0, 3, 0, 0xfff8, 0xffff), 16)
        val golden = goldenGatherEI16(vs1, sew, lmulFrac, uopIdx = 0)
        pGatherEI16(dut, vs1, sew, lmulFrac, uopIdx = 0)
        dut.clock.step()
        eGatherEI16Bits(dut, golden)
      }
    }
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
    it("e32ei16 wrap index 0x20 (32) -> fillZeros (m8: byte-level skipped)") {
      simulate(new GatherIndexGen()(GatherParam.default)) { dut =>
        val sew = 32; val lmulFrac = 64
        // 32 >= vlmax_e32(32); truncates to 5b -> byte 0-3, would LOOKUP on old RTL
        val vs1 = fromSewElems(Seq(0x20, 0, 0, 0, 0, 0, 0, 0), 16)
        val golden = goldenGatherEI16(vs1, sew, lmulFrac, uopIdx = 0)
        pGatherEI16(dut, vs1, sew, lmulFrac, uopIdx = 0)
        dut.clock.step()
        eGatherEI16Bits(dut, golden)
      }
    }
    it("e64ei16 uop1 wrap index 0x10 (16) -> fillZeros (m8: byte-level skipped)") {
      simulate(new GatherIndexGen()(GatherParam.default)) { dut =>
        val sew = 64; val lmulFrac = 64
        // uop1 slice = bits[32:63] = e16 [0x10, 0]; 16 >= vlmax_e64(16)
        val vs1 = fromSewElems(Seq(0, 0, 0x10, 0, 0, 0, 0, 0), 16)
        val golden = goldenGatherEI16(vs1, sew, lmulFrac, uopIdx = 1)
        pGatherEI16(dut, vs1, sew, lmulFrac, uopIdx = 1)
        dut.clock.step()
        eGatherEI16Bits(dut, golden)
      }
    }
    it("e32ei16 m2 index >= vlmax(8) -> fillZeros (small lmul)") {
      simulate(new GatherIndexGen()(GatherParam.default)) { dut =>
        val sew = 32; val lmulFrac = 16 // m2: vlmax_e32 = 8
        // uop1 slice = [0x10(16), 0, 0, 0]; 16 >= 8 -> fillZeros on slots 0-3
        val vs1 = fromSewElems(Seq(0, 0, 0, 0, 0x10, 0, 0, 0), 16)
        val golden = goldenGatherEI16(vs1, sew, lmulFrac, uopIdx = 1)
        pGatherEI16(dut, vs1, sew, lmulFrac, uopIdx = 1)
        dut.clock.step()
        eGatherEI16Bits(dut, golden)
      }
    }
    it("e32ei16 m8 boundary indices around vlmax=32") {
      checkEi16Boundaries(32, 64, Seq(
        (0, Seq(0, 1, 16, 31)),   // 0, 1, vlmax/2, vlmax-1: valid
        (1, Seq(32, 33, 48, 63)), // vlmax, vlmax+1, vlmax*1.5, vlmax*2-1: OOR
        (2, Seq(64, 65, 128, 0)), // vlmax*2, vlmax*2+1, vlmax*4: OOR
      ))
    }
    it("e32ei16 m2 boundary indices around vlmax=8") {
      checkEi16Boundaries(32, 16, Seq(
        (0, Seq(0, 1, 4, 7)),   // valid
        (1, Seq(8, 9, 12, 15)), // vlmax, +1, *1.5, *2-1: OOR
        (2, Seq(16, 17, 32, 0)), // vlmax*2, +1, *4: OOR
      ))
    }
    it("e64ei16 m8 boundary indices around vlmax=16") {
      checkEi16Boundaries(64, 64, Seq(
        (0, Seq(0, 1)),     // valid
        (1, Seq(8, 15)),    // vlmax/2, vlmax-1: valid
        (2, Seq(16, 17)),   // vlmax, vlmax+1: OOR
        (3, Seq(24, 31)),   // vlmax*1.5, vlmax*2-1: OOR
        (0, Seq(32, 33)),   // vlmax*2, vlmax*2+1: OOR
        (1, Seq(64, 0)),    // vlmax*4: OOR
      ))
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
