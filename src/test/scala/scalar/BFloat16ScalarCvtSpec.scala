package yunsuan.scalar

import chisel3._
import chiseltest._
import chiseltest.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.VfcvtType

class BFloat16ScalarCvtSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "FPCVT BF16 scalar conversions"

  private def driveBase(dut: FPCVT): Unit = {
    dut.io.fire.poke(false.B)
    dut.io.src.poke(0.U)
    dut.io.opType.poke(0.U)
    dut.io.sew.poke(0.U)
    dut.io.rm.poke(0.U)
    dut.io.altfmt.poke(false.B)
    dut.io.isFpToVecInst.poke(true.B)
    dut.io.isFround.poke(0.U)
    dut.io.isFcvtmod.poke(false.B)
  }

  private def issue(dut: FPCVT, src: BigInt, opType: UInt, sew: UInt): Unit = {
    dut.io.src.poke(src.U(64.W))
    dut.io.opType.poke(opType)
    dut.io.sew.poke(sew)
    dut.io.fire.poke(true.B)
    dut.clock.step()
    dut.io.fire.poke(false.B)
  }

  it should "convert boxed FP32 zero to boxed BF16 zero for fcvt.bf16.s" in {
    test(new FPCVT(64)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      driveBase(dut)
      issue(dut, BigInt("ffffffff00000000", 16), VfcvtType.fcvt_bf16_s, "b10".U)
      dut.clock.step(2)
      dut.io.result.expect(BigInt("ffffffffffff0000", 16).U)
      dut.io.fflags.expect(0.U)
    }
  }

  it should "route scalar fcvt.bf16.s through the BF16 converter independent of sew" in {
    test(new FPCVT(64)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      driveBase(dut)
      issue(dut, BigInt("ffffffff00000000", 16), VfcvtType.fcvt_bf16_s, "b01".U)
      dut.clock.step(2)
      dut.io.result.expect(BigInt("ffffffffffff0000", 16).U)
      dut.io.fflags.expect(0.U)
    }
  }

  it should "convert boxed BF16 zero to boxed FP32 zero for fcvt.s.bf16" in {
    test(new FPCVT(64)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      driveBase(dut)
      issue(dut, BigInt("ffffffffffff0000", 16), VfcvtType.fcvt_s_bf16, "b01".U)
      dut.clock.step(2)
      dut.io.result.expect(BigInt("ffffffff00000000", 16).U)
      dut.io.fflags.expect(0.U)
    }
  }

  it should "canonicalize to boxed FP32 NaN when fcvt.s.bf16 sees a non-BF16-boxed source" in {
    test(new FPCVT(64)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      driveBase(dut)
      issue(dut, BigInt("ffffffff00000000", 16), VfcvtType.fcvt_s_bf16, "b01".U)
      dut.clock.step(2)
      dut.io.result.expect(BigInt("ffffffff7fc00000", 16).U)
      dut.io.fflags.expect(0.U)
    }
  }
}
