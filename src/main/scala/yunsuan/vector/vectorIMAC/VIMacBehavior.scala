package yunsuan.vector.mac

import chisel3._
import chisel3.util._
import yunsuan.util.MultiplyBehavior

/** Element arithmetic; source swapping and architectural mask/merge stay outside. */
object VIMacBehavior {
  def apply(in: VIMac64bStage1Input): VIMac64bStage3Output = {
    val formats = Seq(8, 16, 32, 64).map { width =>
      val lanes = 64 / width
      val results = (0 until lanes).map { lane =>
        val a = in.vs2((lane+1)*width-1, lane*width)
        val b = in.vs1((lane+1)*width-1, lane*width)
        val product = MultiplyBehavior.product(a, b, in.srcType(0)(2), in.srcType(1)(2))
        val oldNarrow = in.oldVd((lane+1)*width-1, lane*width).pad(2*width)
        val oldWide = if (width == 64) oldNarrow else {
          val index = lane % (lanes / 2)
          in.oldVd((index+1)*2*width-1, index*2*width)
        }
        val old = Mux(in.widen, oldWide, oldNarrow)
        val accumulated = Mux(in.isSub, old - product, old + product)
        val normal = Mux(in.isMacc, accumulated, product)
        val low = normal(width-1, 0)
        val high = normal(2*width-1, width)
        val shift = width-1
        val guard = product(shift-1)
        val sticky = product(shift-2, 0).orR
        val lsb = product(shift)
        val increment = MuxLookup(in.info.vxrm, false.B)(Seq(
          0.U -> guard, 1.U -> (guard && (sticky || lsb)),
          2.U -> false.B, 3.U -> (!lsb && (guard || sticky))))
        val saturated = product(2*width-1, 2*width-2) === 1.U
        val rounded = product(2*width-2, width-1) + increment
        val fixed = Mux(saturated, ((BigInt(1) << (width-1))-1).U(width.W), rounded)
        (Mux(in.isFixP, fixed, Mux(in.highHalf, high, low)), normal,
          Fill(width/8, in.isFixP && saturated))
      }
      val narrow = Cat(results.reverse.map(_._1))
      val wide = if (width == 64) narrow else {
        val half = lanes/2
        Mux(in.info.uopIdx(0), Cat(results.drop(half).reverse.map(_._2)),
          Cat(results.take(half).reverse.map(_._2)))
      }
      (Mux(in.widen, wide, narrow), Cat(results.reverse.map(_._3)))
    }
    val result = Wire(new VIMac64bStage3Output)
    result.vd := VecInit(formats.map(_._1))(in.srcType(0)(1, 0))
    result.vxsat := VecInit(formats.map(_._2))(in.srcType(0)(1, 0))
    result
  }
}

/** Packed pure lane-arithmetic carrier for the grouped compute lowering. */
class VIMacComputeCarrier(val modelName: String = "vec-int-mac-lane") extends RawModule {
  override def desiredName: String =
    "WolvrixBehaviorComputeGroup_v1_" + modelName.replace('-', '_')
  val io = IO(new Bundle {
    val in = Input(UInt(238.W))
    val out = Output(UInt(72.W))
  })
  val request = Wire(new VIMac64bStage1Input)
  request.isFixP := io.in(0)
  request.widen := io.in(1)
  request.isSub := io.in(2)
  request.isMacc := io.in(3)
  request.highHalf := io.in(4)
  request.oldVd := io.in(68, 5)
  request.vs2 := io.in(132, 69)
  request.vs1 := io.in(196, 133)
  request.vdType := io.in(200, 197)
  request.srcType(1) := io.in(204, 201)
  request.srcType(0) := io.in(208, 205)
  request.info.vxrm := io.in(210, 209)
  request.info.uopIdx := io.in(216, 211)
  request.info.vstart := io.in(223, 217)
  request.info.vl := io.in(231, 224)
  request.info.vlmul := io.in(234, 232)
  request.info.ta := io.in(235)
  request.info.ma := io.in(236)
  request.info.vm := io.in(237)
  val result = VIMacBehavior(request)
  io.out := Cat(result.vxsat, result.vd)
}
