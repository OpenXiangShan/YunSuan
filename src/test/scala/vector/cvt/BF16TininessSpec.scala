package yunsuan.vector.cvt

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import _root_.yunsuan.vector.VectorConvert.CVT_bf16

/** Independent hand anchors: this suite never derives expectations from a
  * production rounding helper, BF16Reference or NativeConvertReference.
  * RISC-V BF16 v1.0, Underflow: tininess uses a separate rounding with an
  * unbounded exponent, so a minimum-normal result can still raise UF.
  */
class BF16TininessSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "BF16 underflow at the subnormal to normal boundary"
  it should "distinguish result rounding from tininess rounding in every mode and sign" in {
    // Columns are RNE, RTZ, RDN, RUP, RMM for positive magnitudes.
    val anchors = Seq(
      ("007f8000", Seq(0x80, 0x7f, 0x7f, 0x80, 0x80), Seq(3, 3, 3, 3, 3)),
      ("007f8001", Seq(0x80, 0x7f, 0x7f, 0x80, 0x80), Seq(3, 3, 3, 1, 3)),
      ("007fc000", Seq(0x80, 0x7f, 0x7f, 0x80, 0x80), Seq(1, 3, 3, 1, 1)),
      ("007f0000", Seq.fill(5)(0x7f), Seq.fill(5)(0)),
      ("007f0001", Seq(0x7f, 0x7f, 0x7f, 0x80, 0x7f), Seq.fill(5)(3)),
      ("007f7fff", Seq(0x7f, 0x7f, 0x7f, 0x80, 0x7f), Seq.fill(5)(3)),
      ("007fbfff", Seq(0x80, 0x7f, 0x7f, 0x80, 0x80), Seq(3, 3, 3, 1, 3)),
      ("007fc001", Seq(0x80, 0x7f, 0x7f, 0x80, 0x80), Seq(1, 3, 3, 1, 1)),
      ("007fffff", Seq(0x80, 0x7f, 0x7f, 0x80, 0x80), Seq(1, 3, 3, 1, 1)),
      ("00800000", Seq.fill(5)(0x80), Seq.fill(5)(0)))
    test(new CVT_bf16(32)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.io.fire.poke(false.B); dut.io.src.poke(0.U)
      dut.io.opType.poke(0xdd.U); dut.io.sew.poke(2.U); dut.io.rm.poke(0.U)
      dut.io.input1H.poke(4.U); dut.io.output1H.poke(2.U)
      dut.io.isFpToVecInst.poke(false.B); dut.io.isFround.poke(0.U); dut.io.isFcvtmod.poke(false.B)
      dut.clock.step(2)
      var comparisons = 0
      for ((raw, results, flags) <- anchors; negative <- Seq(false, true); rm <- 0 to 4) {
        // Sign reversal exchanges the two directed modes, preserving nearest/RTZ.
        val magnitudeRM = if (negative && rm == 2) 3 else if (negative && rm == 3) 2 else rm
        val src = BigInt(raw, 16) | (if (negative) BigInt(1) << 31 else BigInt(0))
        val expected = BigInt(results(magnitudeRM)) | (if (negative) BigInt(1) << 15 else BigInt(0))
        val expectedFlags = flags(magnitudeRM)
        dut.io.src.poke(src.U); dut.io.rm.poke(rm.U); dut.io.fire.poke(true.B)
        dut.clock.step(); dut.io.fire.poke(false.B); dut.clock.step()
        val actual = dut.io.result.peek().litValue
        val actualFlags = dut.io.fflags.peek().litValue
        if (comparisons == 0) println(s"BF16_UF_REPRO src=0x${src.toString(16)} rm=$rm actual=0x${actual.toString(16)} expected=0x${expected.toString(16)} actual_flags=$actualFlags expected_flags=$expectedFlags")
        assert(actual == expected && actualFlags == expectedFlags,
          s"BF16_UF src=0x${src.toString(16)} rm=$rm result=0x${actual.toString(16)}/0x${expected.toString(16)} flags=$actualFlags/$expectedFlags")
        comparisons += 1
      }
      assert(comparisons == 100)
      println(s"BF16_UF_BOUNDARY_PASS hand_anchors=10 signs=2 rounding_modes=5 comparisons=$comparisons")
    }
  }
}
