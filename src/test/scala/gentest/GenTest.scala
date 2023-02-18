package yunsuan.gentest

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import yunsuan.vector.{FloatAdderReductionF32WidenF16, VectorFloatAdder, VectorFloatDivider, VectorFloatFMA, VectorIntAdder}

object GenTest extends App {
  val path = """./generated/VectorFloatAdder"""
  (new ChiselStage).execute(Array("--emission-options=disableMemRandomization,disableRegisterRandomization",
    "--target-dir", path), Seq(ChiselGeneratorAnnotation(() => new VectorFloatDivider())))
}
