package yunsuan.vector.v2.Crypto

import _root_.circt.stage._
import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.util._
import yunsuan.vector.Common._
import chisel3.experimental.conversions._

import scala.collection.immutable.SeqMap
import scala.language.implicitConversions

class VClmul extends Module {
  val in = IO(Input(new Bundle {
    val a = UInt(64.W)
    val b = UInt(64.W)
  }))
  val out = IO(Output(new Bundle {
    val ch = UInt(64.W)
    val cl = UInt(64.W)
  }))

  val a, b = Wire(UInt())
  (a, b) := (in.a, in.b)
  val res = Wire(Vec(127, Bool()))

  for (i <- 0 until 127) {
    res(i) := Cat((0.max(i - 63) to i.min(63)).map(x => a(x) & b(i - x))).xorR
  }

  out.cl := res.asUInt.take(64)
  out.ch := res.asUInt.drop(64)
}

object VClmul {
  def main(args: Array[String]): Unit = {
    println("Generating the VClmul hardware")

    val firtoolOpts = Array(
      "--target=systemverilog",
      "-O=release",
      "--disable-annotation-unknown",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
    )
    val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

    (new ChiselStage).execute(
      Array("--target-dir", "build/vector") ++ args,
      chisel3.stage.ChiselGeneratorAnnotation(() => new VClmul()) +: firtoolAnno
    )

    println("done")
  }
}
