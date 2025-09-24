package yunsuan.vector.v2.Crypto

import _root_.circt.stage.FirtoolOption
import chisel3.experimental.conversions._
import chisel3.{experimental, _}
import yunsuan.vector.v2.Crypto.Utils.Zvknhb.SHA512

import scala.language.implicitConversions

/**
 * Only for "vsha2ms.vv" of sha512
 */
class VSha512ms extends Module {
  import VSha512ms._

  val in = IO(Input(new In))
  val out = IO(Output(new Out))

  val isVSha2ms0 = in.isVSha2ms0
  val w0 :: w1 :: _   :: w2  :: w9  :: Nil = in.srcs.toList
  val t0 :: t1 :: w14 :: w15 :: w10 :: Nil = in.srcs.toList

  val add2operands: experimental.HWTuple2[UInt, UInt] = Mux(isVSha2ms0, (w1, w2), (t0, w14))
  val add3operands: experimental.HWTuple3[UInt, UInt, UInt] = Mux(isVSha2ms0, (w0, w1, w9), (t1, w15, w10))

  val add2res = add2operands._1 + SHA512.sig0(add2operands._2)
  val add3res = add3operands._1 + SHA512.sig1(add3operands._2) + add3operands._3

  (out.res0, out.res1) := Mux(
    isVSha2ms0,
    (add3res, add2res),
    (add2res, add3res),
  )
}

object VSha512ms {
  def main(args: Array[String]): Unit = {
    println("Generating the VSha2ms0 hardware")

    val firtoolOpts = Array(
      "--target=systemverilog",
      "-O=release",
      "--disable-annotation-unknown",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
    )
    val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

    (new chisel3.stage.ChiselStage).execute(
      Array("--target-dir", "build/vector") ++ args,
      chisel3.stage.ChiselGeneratorAnnotation(() => new VSha512ms) +: firtoolAnno
    )

  }

  val EGW: Int = 256
  val EGS: Int = 4
  val EEW: Int = 64

  class In extends Bundle {
    val isVSha2ms0 = Bool()
    val srcs = Vec(5, UInt(EEW.W))
  }

  class Out extends Bundle {
    val res0, res1: UInt = UInt(EEW.W)
  }
}
