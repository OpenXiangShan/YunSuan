package yunsuan.vector.v2.VectorInt

import chisel3._
import chisel3.util._


class VectorIntBitOp64b extends Module {
  val io = IO(new Bundle {
    val in = Input(new Bundle {
      val op = new Bundle {
        val and = Bool()
        val nand = Bool()
        val andn = Bool()
        val xor = Bool()
        val or = Bool()
        val nor = Bool()
        val orn = Bool()
        val xnor = Bool()
        // Todo: clz, first, etc.
      }
      val vs1 = UInt(64.W)
      val vs2 = UInt(64.W)
    })
    val out = Output(new Bundle{
      val vd = UInt(64.W)
    })
  })

  private val op = io.in.op
  private val vs2 = io.in.vs2
  private val vs1 = io.in.vs1

  io.out.vd := Mux1H(Seq(
    op.and  -> (vs2 & vs1),
    op.nand -> (~(vs2 & vs1)).asUInt,
    op.andn -> (vs2 & (~vs1).asUInt),
    op.xor  -> (vs2 ^ vs1),
    op.or   -> (vs2 | vs1),
    op.nor  -> (~(vs2 | vs1)).asUInt,
    op.orn  -> (vs2 | (~vs1).asUInt),
    op.xnor -> (~(vs2 ^ vs1)).asUInt,
  ))
}
