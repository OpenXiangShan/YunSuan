package yunsuan.fpulite

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage._

object GenTest extends App {
  val path = """./generated/fpulite/FloatDivider"""
  (new ChiselStage).execute(
    Array("--target-dir", path),
    Seq(ChiselGeneratorAnnotation(() => new FloatDivider), FirtoolOption("--disable-all-randomization"))
  )
}
