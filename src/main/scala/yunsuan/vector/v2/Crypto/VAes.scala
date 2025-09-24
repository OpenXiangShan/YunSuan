package yunsuan.vector.v2.Crypto

import _root_.circt.stage.FirtoolOption
import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.util._
import yunsuan.vector.Common._
import yunsuan.vector.v2.Crypto.Utils.Zvkned.subBytes

import scala.collection.immutable.SeqMap
import scala.language.implicitConversions


class VAes extends Module {
  import VAes._
  import yunsuan.vector.v2.Crypto.Utils.Zvkned._

  val in = IO(Input(ValidIO(new In)))
  val out = IO(Output(new Out))
  val op = in.op
  val state = in.vs3
  val rkey = in.vs2
  val ensb = subBytes(state)
  val ensr = shiftRows(ensb)
  val enmix = mixColumns(ensr)
  val enark = Mux1H(Seq(
    op.em -> enmix,
    op.ef -> ensr,
  )) ^ rkey

  val desr = shiftRowsInv(state)
  val desb = subBytesInv(desr)
  val deark = desb ^ rkey
  val demix = mixColumnsInv(deark)

  out.vd := Mux1H(Seq(
    (op.em || op.ef) -> enark,
    op.dm -> demix,
    op.df -> deark,
  ))
}

class SubBytes extends Module {
  import VAes._

  val state = IO(Input(UInt(DLEN.W)))
  val sb = IO(Output(UInt(DLEN.W)))

  sb := subBytes(state)
}

object SubBytes {
  def main(args: Array[String]): Unit = {
    println("Generating the SubBytes hardware")

    val firtoolOpts = Array(
      "--target=systemverilog",
      "-O=release",
      "--disable-annotation-unknown",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
    )
    val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

    (new chisel3.stage.ChiselStage).execute(
      Array("--target-dir", "build/vector") ++ args,
      chisel3.stage.ChiselGeneratorAnnotation(() => new SubBytes()) +: firtoolAnno
    )

    println("done")
  }
}

object VAes {
  def main(args: Array[String]): Unit = {
    println("Generating the VAes hardware")

    val firtoolOpts = Array(
      "--target=systemverilog",
      "-O=release",
      "--disable-annotation-unknown",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
    )
    val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

    (new chisel3.stage.ChiselStage).execute(
      Array("--target-dir", "build/vector") ++ args,
      chisel3.stage.ChiselGeneratorAnnotation(() => new VAes()) +: firtoolAnno
    )

    println("done")
  }

  val DLEN = 128

  class In extends Bundle {
    val op = new Op
    // round state
    val vs3 = UInt(DLEN.W)
    // round key
    val vs2 = UInt(DLEN.W)
  }

  class Out extends Bundle {
    // new round state
    val vd = UInt(DLEN.W)
  }

  class Op extends Bundle {
    val em, ef, dm, df = Bool()
  }
}
