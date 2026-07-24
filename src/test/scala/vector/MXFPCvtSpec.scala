package yunsuan.vector.VectorConvert

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class MXFPCvtSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "FP32 to MXFP conversion"

  private def issue(
    dut: CVT_mxfp,
    src: BigInt,
    factor: Int = 127,
    isFp4: Boolean = false,
    rm: Int = 0
  ): Unit = {
    dut.io.src.poke(src.U)
    dut.io.factor.poke(factor.U)
    dut.io.isFp4.poke(isFp4.B)
    dut.io.rm.poke(rm.U)
    dut.io.fire.poke(true.B)
    dut.clock.step()
    dut.io.fire.poke(false.B)
    dut.clock.step(2)
  }

  it should "scale E4M3 by subtracting the UE8M0 exponent" in {
    test(new CVT_mxfp) { dut =>
      issue(dut, BigInt("3f800000", 16), factor = 128)
      dut.io.result.expect("h30".U)
      dut.io.fflags.expect(0.U)
    }
  }

  it should "round E4M3 and report NX" in {
    test(new CVT_mxfp) { dut =>
      issue(dut, BigInt("3f880000", 16)) // 1.0625, halfway between 1.0 and 1.125
      dut.io.result.expect("h38".U)
      dut.io.fflags.expect("b00001".U)
    }
  }

  it should "saturate E4M3 overflow and report OF and NX" in {
    test(new CVT_mxfp) { dut =>
      issue(dut, BigInt("43f00000", 16)) // 480.0
      dut.io.result.expect("h7e".U)
      dut.io.fflags.expect("b00101".U)
    }
  }

  it should "distinguish exact and inexact E4M3 subnormal results" in {
    test(new CVT_mxfp) { dut =>
      issue(dut, BigInt("3b000000", 16)) // 2^-9, exact minimum subnormal
      dut.io.result.expect("h01".U)
      dut.io.fflags.expect(0.U)

      issue(dut, BigInt("3a800000", 16)) // 2^-10, ties to zero under RNE
      dut.io.result.expect(0.U)
      dut.io.fflags.expect("b00011".U)
    }
  }

  it should "canonicalize E4M3 NaNs and raise NV only for signaling NaN" in {
    test(new CVT_mxfp) { dut =>
      issue(dut, BigInt("7fc00000", 16))
      dut.io.result.expect("h7f".U)
      dut.io.fflags.expect(0.U)

      issue(dut, BigInt("7f800001", 16))
      dut.io.result.expect("h7f".U)
      dut.io.fflags.expect("b10000".U)
    }
  }

  it should "convert E2M1 normal, overflow, and underflow cases" in {
    test(new CVT_mxfp) { dut =>
      issue(dut, BigInt("3fc00000", 16), isFp4 = true) // 1.5
      dut.io.result.expect("h03".U)
      dut.io.fflags.expect(0.U)

      issue(dut, BigInt("40e00000", 16), isFp4 = true) // 7.0, ties to 8.0
      dut.io.result.expect("h07".U)
      dut.io.fflags.expect("b00101".U)

      issue(dut, BigInt("3e800000", 16), isFp4 = true) // 0.25, ties to zero
      dut.io.result.expect(0.U)
      dut.io.fflags.expect("b00011".U)
    }
  }
}

class MXFPVectorCvtSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "VectorCvt MXFP packing"

  private def driveBase(dut: VectorCvt): Unit = {
    dut.io.fire.poke(false.B)
    dut.io.src.poke(0.U)
    dut.io.opType.poke(0.U)
    dut.io.sew.poke(0.U)
    dut.io.rm.poke(0.U)
    dut.io.isFpToVecInst.poke(false.B)
    dut.io.isFround.poke(0.U)
    dut.io.isFcvtmod.poke(false.B)
    dut.io.isMXFP.poke(true.B)
    dut.io.factor.poke(127.U)
  }

  private def issue(dut: VectorCvt, src: BigInt, sew: Int): Unit = {
    dut.io.src.poke(src.U)
    dut.io.sew.poke(sew.U)
    dut.io.fire.poke(true.B)
    dut.clock.step()
    dut.io.fire.poke(false.B)
    dut.clock.step(2)
  }

  it should "pack two E4M3 results into the low 16 bits" in {
    test(new VectorCvt(64)) { dut =>
      driveBase(dut)
      issue(dut, BigInt("400000003f800000", 16), sew = 0)
      dut.io.result.expect("h0000000000004038".U)
      dut.io.fflags.expect(0.U)
    }
  }

  it should "pack two E2M1 results into the low byte" in {
    test(new VectorCvt(64)) { dut =>
      driveBase(dut)
      issue(dut, BigInt("400000003f800000", 16), sew = 1)
      dut.io.result.expect("h0000000000000042".U)
      dut.io.fflags.expect(0.U)
    }
  }
}
