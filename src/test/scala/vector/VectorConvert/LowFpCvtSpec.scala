package yunsuan.vector.VectorConvert

import chisel3._
import chiseltest._
import chiseltest.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.VfcvtType
import yunsuan.vector.VectorConvert.RoundingModle._

class LowFpCvtSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "FP8 and FP4 vector conversions"

  private val annos = Seq(VerilatorBackendAnnotation)

  private def driveLowFpBase(dut: CVT_fp8_fp4): Unit = {
    dut.io.fire.poke(false.B)
    dut.io.src.poke(0.U)
    dut.io.opType.poke(0.U)
    dut.io.sew.poke(0.U)
    dut.io.rm.poke(RNE)
    dut.io.altfmt.poke(false.B)
    dut.io.input1H.poke(0.U)
    dut.io.output1H.poke(0.U)
    dut.io.isFpToVecInst.poke(false.B)
    dut.io.isFround.poke(0.U)
    dut.io.isFcvtmod.poke(false.B)
  }

  private def issueLowFp(
    dut: CVT_fp8_fp4,
    src: BigInt,
    opType: UInt,
    altfmt: Boolean = false,
    rm: UInt = RNE
  ): Unit = {
    dut.io.src.poke(src.U(64.W))
    dut.io.opType.poke(opType)
    dut.io.sew.poke(0.U)
    dut.io.rm.poke(rm)
    dut.io.altfmt.poke(altfmt.B)
    dut.io.fire.poke(true.B)
    dut.clock.step()
    dut.io.fire.poke(false.B)
    dut.clock.step(2)
  }

  private def expectLowFp(
    dut: CVT_fp8_fp4,
    src: BigInt,
    opType: UInt,
    expected: BigInt,
    expectedFflags: Int,
    altfmt: Boolean = false,
    rm: UInt = RNE
  ): Unit = {
    issueLowFp(dut, src, opType, altfmt, rm)
    dut.io.result.expect(expected.U)
    dut.io.fflags.expect(expectedFflags.U)
  }

  private def driveVectorBase(dut: VectorCvt): Unit = {
    dut.io.fire.poke(false.B)
    dut.io.src.poke(0.U)
    dut.io.opType.poke(0.U)
    dut.io.sew.poke(0.U)
    dut.io.rm.poke(RNE)
    dut.io.altfmt.poke(false.B)
    dut.io.isFpToVecInst.poke(false.B)
    dut.io.isFround.poke(0.U)
    dut.io.isFcvtmod.poke(false.B)
  }

  private def issueVector(
    dut: VectorCvt,
    src: BigInt,
    opType: UInt,
    altfmt: Boolean = false,
    rm: UInt = RNE
  ): Unit = {
    dut.io.src.poke(src.U(64.W))
    dut.io.opType.poke(opType)
    dut.io.sew.poke(0.U)
    dut.io.rm.poke(rm)
    dut.io.altfmt.poke(altfmt.B)
    dut.io.fire.poke(true.B)
    dut.clock.step()
    dut.io.fire.poke(false.B)
    dut.clock.step(2)
  }

  it should "convert OFP8 E4M3 values to BF16 exactly without fflags" in {
    test(new CVT_fp8_fp4(64)).withAnnotations(annos) { dut =>
      driveLowFpBase(dut)
      expectLowFp(dut, 0x00, VfcvtType.vfwcvtbf16_ffv, 0x0000, 0)
      expectLowFp(dut, 0x38, VfcvtType.vfwcvtbf16_ffv, 0x3f80, 0)
      expectLowFp(dut, 0xb8, VfcvtType.vfwcvtbf16_ffv, 0xbf80, 0)
      expectLowFp(dut, 0x01, VfcvtType.vfwcvtbf16_ffv, 0x3b00, 0)
      expectLowFp(dut, 0x7f, VfcvtType.vfwcvtbf16_ffv, 0x7fc0, 0)
    }
  }

  it should "convert OFP8 E5M2 values to BF16 exactly without fflags" in {
    test(new CVT_fp8_fp4(64)).withAnnotations(annos) { dut =>
      driveLowFpBase(dut)
      expectLowFp(dut, 0x3c, VfcvtType.vfwcvtbf16_ffv, 0x3f80, 0, altfmt = true)
      expectLowFp(dut, 0xbc, VfcvtType.vfwcvtbf16_ffv, 0xbf80, 0, altfmt = true)
      expectLowFp(dut, 0x01, VfcvtType.vfwcvtbf16_ffv, 0x3780, 0, altfmt = true)
      expectLowFp(dut, 0x7c, VfcvtType.vfwcvtbf16_ffv, 0x7f80, 0, altfmt = true)
      expectLowFp(dut, 0x7f, VfcvtType.vfwcvtbf16_ffv, 0x7fc0, 0, altfmt = true)
    }
  }

  it should "convert BF16 to OFP8 E4M3 with rounding, NaN, infinity, and saturation behavior" in {
    test(new CVT_fp8_fp4(64)).withAnnotations(annos) { dut =>
      driveLowFpBase(dut)
      expectLowFp(dut, 0x3f80, VfcvtType.vfncvtbf16_ffw, 0x38, 0)
      expectLowFp(dut, 0xbf80, VfcvtType.vfncvtbf16_ffw, 0xb8, 0)
      expectLowFp(dut, 0x3f88, VfcvtType.vfncvtbf16_ffw, 0x38, 1, rm = RNE)
      expectLowFp(dut, 0x3f88, VfcvtType.vfncvtbf16_ffw, 0x39, 1, rm = RMM)
      expectLowFp(dut, 0x7fc0, VfcvtType.vfncvtbf16_ffw, 0x7f, 0)
      expectLowFp(dut, 0x7f80, VfcvtType.vfncvtbf16_ffw, 0x7f, 0)
      expectLowFp(dut, 0x7f80, VfcvtType.vfncvtbf16_sat_ffw, 0x7e, 0)
      expectLowFp(dut, 0xff80, VfcvtType.vfncvtbf16_sat_ffw, 0xfe, 0)
    }
  }

  it should "convert BF16 to OFP8 E5M2 with altfmt selecting the E5M2 encoding" in {
    test(new CVT_fp8_fp4(64)).withAnnotations(annos) { dut =>
      driveLowFpBase(dut)
      expectLowFp(dut, 0x3f80, VfcvtType.vfncvtbf16_ffw, 0x3c, 0, altfmt = true)
      expectLowFp(dut, 0xbf80, VfcvtType.vfncvtbf16_ffw, 0xbc, 0, altfmt = true)
      expectLowFp(dut, 0x3f90, VfcvtType.vfncvtbf16_ffw, 0x3c, 1, altfmt = true, rm = RNE)
      expectLowFp(dut, 0x3f90, VfcvtType.vfncvtbf16_ffw, 0x3d, 1, altfmt = true, rm = RMM)
      expectLowFp(dut, 0x7f80, VfcvtType.vfncvtbf16_ffw, 0x7c, 0, altfmt = true)
      expectLowFp(dut, 0x7fc0, VfcvtType.vfncvtbf16_ffw, 0x7f, 0, altfmt = true)
    }
  }

  it should "convert FP32 directly to OFP8 formats and report invalid for signaling NaNs" in {
    test(new CVT_fp8_fp4(64)).withAnnotations(annos) { dut =>
      driveLowFpBase(dut)
      expectLowFp(dut, 0x3f800000L, VfcvtType.vfncvt_ffq, 0x38, 0)
      expectLowFp(dut, 0xbf800000L, VfcvtType.vfncvt_ffq, 0xb8, 0)
      expectLowFp(dut, 0x3f880000L, VfcvtType.vfncvt_ffq, 0x38, 1, rm = RNE)
      expectLowFp(dut, 0x3f880000L, VfcvtType.vfncvt_ffq, 0x39, 1, rm = RMM)
      expectLowFp(dut, 0x7f800000L, VfcvtType.vfncvt_ffq, 0x7f, 0)
      expectLowFp(dut, 0x7f800000L, VfcvtType.vfncvt_sat_ffq, 0x7e, 0)
      expectLowFp(dut, 0x7fa00000L, VfcvtType.vfncvt_ffq, 0x7f, 0x10)

      expectLowFp(dut, 0x3f800000L, VfcvtType.vfncvt_ffq, 0x3c, 0, altfmt = true)
      expectLowFp(dut, 0x7f800000L, VfcvtType.vfncvt_ffq, 0x7c, 0, altfmt = true)
      expectLowFp(dut, 0x7fa00000L, VfcvtType.vfncvt_ffq, 0x7f, 0x10, altfmt = true)
    }
  }

  it should "extend OFP4 nibbles to OFP8 E4M3 without fflags" in {
    test(new CVT_fp8_fp4(64)).withAnnotations(annos) { dut =>
      driveLowFpBase(dut)
      expectLowFp(dut, 0x0, VfcvtType.vfext_vf2, 0x00, 0)
      expectLowFp(dut, 0x1, VfcvtType.vfext_vf2, 0x30, 0)
      expectLowFp(dut, 0x2, VfcvtType.vfext_vf2, 0x38, 0)
      expectLowFp(dut, 0x3, VfcvtType.vfext_vf2, 0x3c, 0)
      expectLowFp(dut, 0xf, VfcvtType.vfext_vf2, 0xcc, 0)
    }
  }

  it should "route packed OFP8 lanes through VectorCvt using SEW=8 and altfmt" in {
    test(new VectorCvt(64)).withAnnotations(annos) { dut =>
      driveVectorBase(dut)
      issueVector(dut, BigInt("b87f0138", 16), VfcvtType.vfwcvtbf16_ffv)
      dut.io.result.expect(BigInt("bf807fc03b003f80", 16).U)
      dut.io.fflags.expect(0.U)

      issueVector(dut, BigInt("bc7c013c", 16), VfcvtType.vfwcvtbf16_ffv, altfmt = true)
      dut.io.result.expect(BigInt("bf807f8037803f80", 16).U)
      dut.io.fflags.expect(0.U)
    }
  }

  it should "route packed BF16 and OFP4 lanes through VectorCvt special cases" in {
    test(new VectorCvt(64)).withAnnotations(annos) { dut =>
      driveVectorBase(dut)
      issueVector(dut, BigInt("3f887f80bf803f80", 16), VfcvtType.vfncvtbf16_ffw)
      dut.io.result.expect(BigInt("387fb838", 16).U)
      dut.io.fflags.expect(BigInt("8000", 16).U)

      issueVector(dut, BigInt("f210", 16), VfcvtType.vfext_vf2)
      dut.io.result.expect(BigInt("cc383000", 16).U)
      dut.io.fflags.expect(0.U)
    }
  }
}
