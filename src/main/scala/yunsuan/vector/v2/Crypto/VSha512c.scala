package yunsuan.vector.v2.Crypto

import _root_.circt.stage.FirtoolOption
import chisel3._
import chisel3.util._
import yunsuan.vector.Common._
import yunsuan.vector.v2.Crypto.Utils.Zvknhb.SHA512

import scala.language.implicitConversions

class VSha512c extends Module {
  import VSha512c._
  val EGW: Int = 256
  val EGS: Int = 4
  val EEW: Int = 64
  val in = IO(Input(new In))
  val toCtrl = IO(Output(new IterOut))
  val out = IO(Output(new Out))

  private val stages = in.stages
  private val h :: g :: f :: e :: d :: c :: b :: a :: Nil = in.abcdefgh.splitToVecByWidth(EEW).toList
  private val kw0 :: kw1 :: Nil = in.kw1kw0.splitToVecN(2).toList
  private val inT1 :: inT2 :: Nil = in.t2t1.splitToVecN(2).toList

  //  stages(0), stages(2):
  //    t1 = h + sum1(e) + ch(e,f,g) + W0
  //    t2 = sum0(a) + maj(a,b,c)
  val kw = Mux1H(Seq(
    stages(0) -> kw0,
    stages(2) -> kw1,
  ))
  val iterT1: UInt = SHA512.sum1ch(h, g, f, e, kw)
  val iterT2: UInt = SHA512.sum0maj(c, b, a)
  toCtrl.t2t1 := Cat(Seq(iterT2, iterT1))

  //  stages(1), stages(3):
  //    a = t2 + t1
  //    e = d + t1
  val dPlusT1 = d + inT1
  val t2PlusT1 = inT2 + inT1

  // update stage1 stage3 abcdefgh
  toCtrl.abcdefgh := t2PlusT1 ## a ## b ## c ## dPlusT1 ## e ## f ## g

  out.ab := t2PlusT1 ## a
  out.ef := dPlusT1 ## e
}

object VSha512c {
  def main(args: Array[String]): Unit = {
    println("Generating the VSha512c hardware")

    val firtoolOpts = Array(
      "--target=systemverilog",
      "-O=release",
      "--disable-annotation-unknown",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
    )
    val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

    (new chisel3.stage.ChiselStage).execute(
      Array("--target-dir", "build/vector") ++ args,
      chisel3.stage.ChiselGeneratorAnnotation(() => new VSha512c()) +: firtoolAnno
    )

    println("done")
  }

  val EGW: Int = 256
  val EGS: Int = 4
  val EEW: Int = 64

  class In extends Bundle {
    val stages: Vec[Bool] = Vec(4, Bool())
    val abcdefgh = UInt((EGW * 2).W)
    val kw1kw0 = UInt((EEW * 2).W)
    val t2t1 = UInt((EEW * 2).W)
  }

  class IterOut extends Bundle {
    val t2t1 = UInt((EEW * 2).W)
    val abcdefgh = UInt((EGW * 2).W) // {a, b, c, d, e, f, g, h}
  }

  class Out extends Bundle {
    val ef = UInt((EEW * 2).W)
    val ab = UInt((EEW * 2).W)
  }
}
