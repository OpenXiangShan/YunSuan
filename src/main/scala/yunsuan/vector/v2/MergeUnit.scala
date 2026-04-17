package yunsuan.vector.v2

import chisel3._
import chisel3.util._

object MergeUnit {
  def apply(vlen: Int): MergeUnit = {
    val mgu = new MergeUnit(vlen)
    mgu
  }
  class In(vlen: Int) extends Bundle {
    val ctrl = new InCtrl
    val data = new InData(vlen)
  }
  class Out(vlen: Int) extends Bundle {
    val vlenb = vlen / 8
    val activeEn   = UInt(vlenb.W)
    val agnosticEn = UInt(vlenb.W)
    val res        = Vec(vlenb, UInt(8.W))
  }
  class InCtrl extends Bundle {
    val vma = Bool()
    val vta = Bool()
  }
  class InData(vlen: Int) extends Bundle {
    val vlenb = vlen / 8

    val mask  = UInt(vlenb.W)
    val begin = UInt(log2Ceil(vlenb + 1).W)
    val end   = UInt(log2Ceil(vlenb + 1).W)
    val oldVd = Vec(vlenb, UInt(8.W))
    val vd    = Vec(vlenb, UInt(8.W))
  }
}

class MergeUnit(vlen: Int = 128) extends Module {
  val vlenb = vlen / 8
  val in  = IO(Input(new MergeUnit.In(vlen)))
  val out = IO(Output(new MergeUnit.Out(vlen)))

  private val vma    = in.ctrl.vma
  private val vta    = in.ctrl.vta
  private val maskEn = in.data.mask
  private val begin  = in.data.begin
  private val end    = in.data.end
  private val vd     = in.data.vd
  private val oldVd  = in.data.oldVd

  assert(begin <= vlenb.U, "begin should less than or equal to vlenb")
  assert(end   <= vlenb.U, "begin should less than or equal to vlenb")

  private val beginMask = Wire(UInt(vlenb.W))
  private val endMask   = Wire(UInt(vlenb.W))
  private val tailMask  = Wire(UInt(vlenb.W))
  beginMask := (Fill(vlenb, 1.U) << begin)((vlenb << 1) - 1, vlenb)
  endMask   := (Fill(vlenb, 1.U) << end)((vlenb << 1) - 1, vlenb)
  tailMask  := ~endMask

  private val bodyMask = (~beginMask).asUInt & endMask

  private val maskOffEn = (~maskEn).asUInt

  private val activeMask = bodyMask & maskEn
  private val agnosticMask = Fill(vlenb, vma) & bodyMask & maskOffEn | Fill(vlenb, vta) & tailMask

  private val bytes1s = (~0.U(8.W)).asUInt

  // oldVd and byte1s have good timing
  private val resTmp = Wire(Vec(vlenb, UInt(8.W)))
  for (i <- 0 until vlenb) {
    resTmp(i) := Mux(agnosticMask(i), bytes1s, oldVd(i))
  }

  out.activeEn := activeMask
  out.agnosticEn := agnosticMask

  for (i <- 0 until vlenb) {
    out.res(i) := Mux(activeMask(i), vd(i), resTmp(i))
  }
}