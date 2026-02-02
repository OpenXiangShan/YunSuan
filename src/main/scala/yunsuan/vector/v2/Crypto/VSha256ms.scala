package yunsuan.vector.v2.Crypto

import _root_.circt.stage._
import chisel3.{experimental, _}
import chisel3.util._
import chisel3.experimental.conversions._
import yunsuan.vector.Common._
import yunsuan.vector.v2.Crypto.Utils.Zvknhb.SHA256

import scala.language.implicitConversions


class VSha256ms extends Module {
  import VSha256ms._

  val in = IO(Input(new In))
  val out = IO(Output(new Out))

  val vs1Regs = Reg(Vec(EGS, UInt(EEW.W)))
  val vs2Regs = Reg(Vec(EGS, UInt(EEW.W)))
  val vs3Regs = Reg(Vec(EGS, UInt(EEW.W)))
  val tmpRegs = Reg(Vec(4, UInt(EEW.W)))
  val stages = Reg(Vec(4, Bool()))

  vs1Regs zip in.vs1.splitToVecN(EGS) foreach { case (reg, input) => when (in.valid) {reg := input} }
  vs2Regs zip in.vs2.splitToVecN(EGS) foreach { case (reg, input) => when (in.valid) {reg := input} }
  vs3Regs zip in.vs3.splitToVecN(EGS) foreach { case (reg, input) => when (in.valid) {reg := input} }

  stages zip (in.valid +: stages.dropRight(1)) foreach {
    case (reg, next) =>
      when (reg || next) {
        reg := next
      }
  }

  val mod = Module(new VSha256msInner)

  val src4, src3, src2, src1, src0 = Wire(UInt())
  (src4, src3, src2, src1, src0) := Mux1H(Seq(
    stages(0) -> (vs2Regs(1), vs3Regs(2), vs3Regs(1), vs3Regs(1), vs3Regs(0)),
    stages(1) -> (vs2Regs(2), vs1Regs(3), vs1Regs(2), tmpRegs(1), tmpRegs(0)),
    stages(2) -> (vs2Regs(3), vs2Regs(0), vs3Regs(3), vs3Regs(3), vs3Regs(2)),
    stages(3) -> (tmpRegs(1), tmpRegs(0), vs1Regs(0), tmpRegs(3), tmpRegs(2)),
  ))

  mod.in.isVSha2ms0 := stages(0) || stages(2)
  mod.in.srcs := VecInit(src0, src1, src2, src3, src4)

  val t1, t3 = mod.out.res1
  val t0, t2 = mod.out.res0

  when (stages(0) || stages(1)) {
    tmpRegs(0) := t0
    tmpRegs(1) := t1
  }
  when (stages(2) || stages(3)) {
    tmpRegs(2) := t2
    tmpRegs(3) := t3
  }
  out.valid := stages(3)
  out.vd    := Cat(tmpRegs.reverse)
}

object VSha256ms {
  def main(args: Array[String]): Unit = {
    println("Generating the VSha2ms0 hardware")

    val firtoolOpts = Array(
      "--target=systemverilog",
      "-O=release",
      "--disable-annotation-unknown",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
    )
    val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

    (new ChiselStage).execute(
      Array("--target-dir", "build/vector") ++ args,
      chisel3.stage.ChiselGeneratorAnnotation(() => new VSha256ms) +: firtoolAnno
    )
  }

  val DLEN: Int = 128
  val EGW: Int = 128
  val EGS: Int = 4
  val EEW: Int = 32

  class In extends Bundle {
    val valid = Bool()
    val vs1 = UInt(DLEN.W)
    val vs2 = UInt(DLEN.W)
    val vs3 = UInt(DLEN.W)
  }

  class Out extends Bundle {
    val valid = Bool()
    val vd = UInt(DLEN.W)
  }
}

class VSha256msInner extends Module {
  import VSha256msInner._

  val in = IO(Input(new In))
  val out = IO(Output(new Out))

  val isVSha2ms0 = in.isVSha2ms0
  val w0 :: w1 :: _   :: w2  :: w9  :: Nil = in.srcs.toList
  val t0 :: t1 :: w14 :: w15 :: w10 :: Nil = in.srcs.toList

  val add2operands: experimental.HWTuple2[UInt, UInt] = Mux(isVSha2ms0, (w1, w2), (t0, w14))
  val add3operands: experimental.HWTuple3[UInt, UInt, UInt] = Mux(isVSha2ms0, (w0, w1, w9), (t1, w15, w10))

  val add2res = add2operands._1 + SHA256.sig0(add2operands._2)
  val add3res = add3operands._1 + SHA256.sig1(add3operands._2) + add3operands._3

  (out.res0, out.res1) := Mux(
    isVSha2ms0,
    (add3res, add2res),
    (add2res, add3res),
  )
}

object VSha256msInner {
  val EGW: Int = 128
  val EGS: Int = 4
  val EEW: Int = 32

  class In extends Bundle {
    val isVSha2ms0 = Bool()
    val srcs = Vec(5, UInt(EEW.W))
  }

  class Out extends Bundle {
    val res0, res1: UInt = UInt(EEW.W)
  }
}