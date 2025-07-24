package yunsuan.util

import chisel3._
import chisel3.util._

object ModuleWrapper {
  private class Mux1HModule(n: Int, w: Int) extends Module {
    override def desiredName: String = s"Mux${n}to1UInt${w}"
    val in = IO(Input(Vec(n, UInt(w.W))))
    val sel = IO(Input(Vec(n, Bool())))
    val out = IO(Output(UInt(w.W)))

    out := Mux1H(sel, in)
  }

  object ModuleMux1H {
    def apply[T <: Data](sel: Seq[Bool], in: Seq[T]): T =
      apply(sel.zip(in))
    def apply[T <: Data](inputs: Iterable[(Bool, T)]): T = {
      val mod = Module(new Mux1HModule(inputs.size, inputs.head._2.getWidth))
      for (((inputCond, inputData), i) <- inputs.zipWithIndex) {
        mod.in(i) := inputData
        mod.sel(i) := inputCond
      }
      mod.out.asTypeOf(inputs.head._2)
    }
    def apply[T <: Data](sel: UInt, in: Seq[T]): T =
      apply(in.indices.map(sel(_)), in)
    def apply(sel: UInt, in: UInt): Bool = (sel & in).orR
  }

  private class ModuleVec[T <: Data](n: Int, gen: => T) extends Module {
    override def desiredName: String = s"Vec${n}to1UInt${gen.getWidth}"

    val in = IO(Input(Vec(n, gen)))
    val index = IO(Input(UInt(log2Up(n).W)))
    val out = IO(Output(gen))

    out := ModuleMux1H(
      in.indices.map(i => (index === i.U) -> in(i))
    )
  }

  object ModuleVec {
    def apply[T <: Data](vec: Vec[T])(index: UInt): T = {
      val mod = Module(new ModuleVec(vec.length, vec.head.cloneType))
      mod.suggestName(mod.desiredName)
      mod.in := vec
      mod.index := index
      mod.out
    }

//    def apply[T <: Data](vec: Vec[T])(indices: Vec[T]): T = {
//
//    }
  }
}
