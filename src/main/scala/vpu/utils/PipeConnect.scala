package race.vpu

import chisel3._
import chisel3.util._

class PipeConnectPipe[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(gen.cloneType))
    val out = DecoupledIO(gen.cloneType)
    val flush = Input(Bool())
  })

  PipeConnect.connect(io.in, io.out, io.flush)
}

object PipeConnect {
  def connect[T <: Data](
    in: DecoupledIO[T],
    out: DecoupledIO[T],
    flush: Bool,
  ): T = {
    val valid = RegInit(false.B)
    val data = Reg(out.bits.cloneType)
    in.ready := !valid || out.ready
    val regEnable = in.fire && !flush
    when (regEnable) {
      valid := true.B
    }.elsewhen (out.ready || flush) {
      valid := false.B
    }
    when (regEnable) { data := in.bits }
    out.valid := valid
    out.bits := data
    out.bits
  }

  def apply[T <: Data](
    in: DecoupledIO[T],
    out: DecoupledIO[T],
    flush: Bool,
    moduleName: Option[String] = None
  ): Option[T] = {
    if (moduleName.isDefined) {
      val pipeline = Module(new PipeConnectPipe(in.bits))
      pipeline.suggestName(moduleName.get)
      pipeline.io.in <> in
      pipeline.io.out <> out
      pipeline.io.flush := flush
      None
    }
    else {
      // do not use module here to please DCE
      Some(connect(in, out, flush))
    }
  }
}