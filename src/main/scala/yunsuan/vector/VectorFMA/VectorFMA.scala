package yunsuan.vector.VectorFMA

import chisel3._
import chisel3.util._

class VectorFMA extends VectorFMA.Module {
  val in = IO(Input(new VectorFMA.In))
  val out = IO(Output(new VectorFMA.Out))

  private val s0 = Module(new VectorFMAS0)
  private val s1 = Module(new VectorFMAS1)
  private val s2 = Module(new VectorFMAS2)
  private val s3 = Module(new VectorFMAS3)

  s0.in := in.ex0.bits
  val s0ToS1 = RegEnable(s0.out, in.ex0.valid)

  s1.in := s0ToS1
  val s1ToS2 = RegEnable(s1.out, in.ex1Valid)

  s2.in := s1ToS2
  val s2ToS3 = RegEnable(s2.out, in.ex2Valid)

  s3.in := s2ToS3

  out.fp_result := s3.out.fp_result
  out.fflags := s3.out.fflags
}

object VectorFMA {
  trait Config {
    val exponentWidth = 11
    val significandWidth = 53
    val floatWidth = exponentWidth + significandWidth

    val csaWidth = 2 * significandWidth + 1
    val rShiftWindowWidth = csaWidth + significandWidth + 3 // 3 :G/R/S
    val fpCRshiftValueInvWidth = (rShiftWindowWidth + 1 - 3) + 1 //first1:S 3:G/R/S second1:invert/complement
    val adderWidth = 3 * significandWidth + 5
    val laneFlagWidth = 4
    val eGreaterWidth = 24
  }

  class Bundle extends chisel3.Bundle with Config
  class Module extends chisel3.Module with Config

  class In extends Bundle {
    val ex0 = ValidIO(new InEx0)
    val ex1Valid = Bool()
    val ex2Valid = Bool()
  }

  class Out extends Bundle {
    val fp_result = UInt(floatWidth.W)
    val fflags = Vec(floatWidth / 8, UInt(5.W))
  }

  class InEx0 extends Bundle {
    val ctrl = new InCtrlEx0
    val data = new InDataEx0
  }

  class InCtrlEx0 extends Bundle {
    val isVfmul = Bool()
    val isVfmacc = Bool()
    val isVfnmacc = Bool()
    val isVfmsac = Bool()
    val isVfnmsac = Bool()
    val isVfmadd = Bool()
    val isVfnmadd = Bool()
    val isVfmsub = Bool()
    val isVfnmsub = Bool()
    val round_mode = UInt(3.W)
    val sel16 = Bool()
    val sel32 = Bool()
    val sel64 = Bool()
    val res_widening = Bool()
  }

  class InDataEx0 extends Bundle {
    val fp_a = UInt(floatWidth.W)
    val fp_b = UInt(floatWidth.W)
    val fp_c = UInt(floatWidth.W)
    val widen_a = UInt(floatWidth.W)
    val widen_b = UInt(floatWidth.W)
    val fp_aIsFpCanonicalNAN = Bool()
    val fp_bIsFpCanonicalNAN = Bool()
    val fp_cIsFpCanonicalNAN = Bool()
  }

}
