package yunsuan.vector

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import yunsuan.encoding.Opcode.Opcodes.FCvtOpcode
import yunsuan.vector.VectorConvert.VectorCvt

class VectorCvtSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "VectorCvt"

  it should "produce FP16 subnormal vfrec7 estimates in every lane" in {
    test(new VectorCvt(64)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.io.in.fire.poke(false.B)
      dut.io.in.ctrl.opType.poke(FCvtOpcode.vfrec7_fp16.encode.value.U)
      dut.io.in.ctrl.rm.poke(0.U)
      dut.io.in.ctrl.inSew1H.poke(2.U)
      dut.io.in.ctrl.outSew1H.poke(2.U)
      dut.io.in.ctrl.cvt64UseWidenSrc2.poke(false.B)
      dut.io.in.ctrl.cvt32UseWidenSrc2.poke(false.B)
      dut.io.in.ctrl.cvt16UseWidenSrc2.poke(false.B)
      dut.io.in.data.src2.poke(0.U)
      dut.io.in.data.widenSrc2.poke(0.U)
      dut.io.in.data.narrowSrc2.poke(0.U)
      dut.io.in.data.narrowSrc1.poke(0.U)
      dut.clock.step()

      def pack(elems: Seq[Int]): BigInt = {
        elems.zipWithIndex.foldLeft(BigInt(0)) { case (bits, (elem, i)) =>
          bits | (BigInt(elem) << (16 * i))
        }
      }

      def check(inputs: Seq[Int], outputs: Seq[Int]): Unit = {
        dut.io.in.data.src2.poke(pack(inputs).U)
        dut.io.in.fire.poke(true.B)
        dut.clock.step()
        dut.io.out.ex1.res.expect(pack(outputs).U)
        dut.io.out.ex1.fflagsE8.foreach(_.expect(0.U))
      }

      // V 1.0 vfrec7 table endpoints and the normal/subnormal output boundary.
      val cases = Seq(
        0x3c00 -> 0x3bf8,
        0x7000 -> 0x07f8,
        0x73ff -> 0x0400,
        0x7400 -> 0x03fc,
        0x77ff -> 0x0200,
        0x7800 -> 0x01fe,
        0x7bff -> 0x0100
      )
      for (rm <- 0 to 4) {
        dut.io.in.ctrl.rm.poke(rm.U)
        for ((input, output) <- cases; sign <- Seq(0, 0x8000)) {
          check(Seq.fill(4)(input | sign), Seq.fill(4)(output | sign))
        }
        val inputs = Seq(0x7400, 0xf800, 0x7bff, 0x7000)
        val outputs = Seq(0x03fc, 0x81fe, 0x0100, 0x07f8)
        for (i <- 0 until 4) {
          check(inputs.drop(i) ++ inputs.take(i), outputs.drop(i) ++ outputs.take(i))
        }
      }
    }
  }
}
