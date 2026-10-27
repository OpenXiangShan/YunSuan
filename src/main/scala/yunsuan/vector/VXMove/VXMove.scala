package yunsuan.vector.VXMove

import chisel3._
import chisel3.util._
import yunsuan.vector.BitsExtend
import yunsuan.vector.Common.VSew._

object VXMove {
  class In extends Bundle {
    val vsew = UInt(2.W)
    val vs2  = UInt(128.W)
  }

  class Out extends Bundle {
    val res = UInt(64.W)
  }
}

class VXMove extends Module {
  val in = IO(Input(new VXMove.In))
  val out = IO(Output(new VXMove.Out))

  out.res := Mux1H(Seq(
    (in.vsew === e8)  -> BitsExtend(in.vs2( 7, 0), 64, true.B),
    (in.vsew === e16) -> BitsExtend(in.vs2(15, 0), 64, true.B),
    (in.vsew === e32) -> BitsExtend(in.vs2(31, 0), 64, true.B),
    (in.vsew === e64) -> in.vs2(63, 0),
  ))
}
