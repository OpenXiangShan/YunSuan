package yunsuan.vector.v2.Crypto

import _root_.circt.stage._
import chisel3._

import scala.language.implicitConversions

class CryptoIteration extends Module {
  import CryptoIteration._

  val in = IO(Input(new Bundle {
    val pipe0 = new InPipe
    val pipe1 = new InPipe
    val clear = Bool()
  }))
  val out = IO(Output(new Bundle {
    val pipe0 = new OutPipe
    val pipe1 = new OutPipe
  }))

  val ctrlMod = Module(new CryptoIterationCtrl)
  val vsha256c = Module(new VSha256c)
  val vsha512c = Module(new VSha512c)
  val vsm3c = Module(new VSm3c)

  ctrlMod.in.pipe0 := in.pipe0
  ctrlMod.in.pipe1 := in.pipe1
  ctrlMod.in.clear := in.clear
  ctrlMod.fromVSha256c.iterOut := vsha256c.toCtrl
  ctrlMod.fromVSha256c.out := vsha256c.out
  ctrlMod.fromVSha512c.iterOut := vsha512c.toCtrl
  ctrlMod.fromVSha512c.out := vsha512c.out
  ctrlMod.fromVSm3c.iterOut := vsm3c.toCtrl
  ctrlMod.fromVSm3c.out := vsm3c.out

  vsha256c.in := ctrlMod.toVSha256c
  vsha512c.in := ctrlMod.toVSha512c
  vsm3c.in := ctrlMod.toVSm3c

  out.pipe0 := ctrlMod.out.pipe0
  out.pipe1 := ctrlMod.out.pipe1
  ctrlMod.fromVSha512c.out := vsha512c.out
}

object CryptoIteration {
  def main(args: Array[String]): Unit = {
    println("Generating the CryptoIteration hardware")

    val firtoolOpts = Array(
      "--target=systemverilog",
      "-O=release",
      "--disable-annotation-unknown",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
    )
    val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

    (new ChiselStage).execute(
      Array("--target-dir", "build/vector") ++ args,
      chisel3.stage.ChiselGeneratorAnnotation(() => new CryptoIteration()) +: firtoolAnno
    )

    println("done")
  }

  class InPipe extends Bundle {
    val DLEN = 128

    val valid = Bool()
    val vs1 = UInt(DLEN.W)
    val vs2 = UInt(DLEN.W)
    val vs3 = UInt(DLEN.W)
    val uop = new UopOH
    val uimm = UInt(5.W)
  }

  class OutPipe extends Bundle {
    val DLEN = 128
    val valid = Bool()
    val vd  = UInt(DLEN.W)
  }

  class UopOH extends Bundle {
    val vsha512c = Bool()
    val vsha256c = Bool()
    val vsha2cl = Bool()
    val vsm3c = Bool()
  }
}
