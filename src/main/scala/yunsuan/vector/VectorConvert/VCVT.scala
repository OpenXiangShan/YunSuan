package yunsuan.vector.VectorConvert

import chisel3._
import yunsuan.encoding.Opcode.Opcodes.FCvtOpcode
import yunsuan.vector.Common._

object VCVT {
  class InCtrl extends Bundle {
    val opType    = FCvtOpcode()
    val rm        = Frm()
    val inSew1H   = Sew()
    val outSew1H  = Sew()
    val isScalarFpInst = Bool()
  }

  class InData(width: Int) extends Bundle {
    val src = UInt(width.W)
  }

  class In(width: Int) extends Bundle {
    val fire = Bool()
    val ctrl = new InCtrl
    val data = new InData(width)
  }

  class OutData(width: Int) extends Bundle {
    val res = UInt(width.W)
    val fflags = Fflags()
  }

  class Out(width: Int) extends Bundle {
    val ex1 = new OutData(width)
    val ex2 = new OutData(width)
  }

  class IO(width: Int) extends Bundle {
    val in  = Input(new In(width))
    val out = Output(new Out(width))
  }
}

abstract class CVT(width: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(new VCVT.In(width))
    val out = Output(new VCVT.Out(width))

    def fire = in.fire
    def src = in.data.src
    def opType = in.ctrl.opType
    def rm = in.ctrl.rm
    def inSew1H = in.ctrl.inSew1H
    def outSew1H = in.ctrl.outSew1H
  })
}