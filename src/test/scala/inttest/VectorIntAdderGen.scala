package inttest

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import intfunction.VectorIntAdder

object VectorIntAdderGen extends App {
  val path = """./generated/VectorIntAdder"""
  (new ChiselStage).execute(Array("--emission-options=disableMemRandomization,disableRegisterRandomization",
    "--target-dir", path), Seq(ChiselGeneratorAnnotation(() => new VectorIntAdder())))
}
