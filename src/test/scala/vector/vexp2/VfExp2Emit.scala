package yunsuan.vector

import circt.stage.ChiselStage
import chisel3.stage.ChiselGeneratorAnnotation

object VfExp2Emit extends App {
  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => new VfExp2(64))
  ))
}
