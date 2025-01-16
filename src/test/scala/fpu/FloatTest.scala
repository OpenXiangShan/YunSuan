import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage._
import yunsuan.fpu.FloatFMA

object GenTest extends App {
  val path = """./generated/FloatFMA"""
  (new ChiselStage).execute(
    Array("--target-dir", path),
    Seq(ChiselGeneratorAnnotation(() => new FloatFMA), FirtoolOption("--disable-all-randomization"))
  )
}
