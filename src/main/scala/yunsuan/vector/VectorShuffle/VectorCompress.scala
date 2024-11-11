package yunsuan.vector.VectorShuffle

import chisel3._
import chisel3.util._
import yunsuan.util.{LiteralCat, ParallelOperation}
import yunsuan.util.Reflect.{getUIntMaxWidthOfObject, getUIntWidthOfObject}
import yunsuan.vector.Common._


class Compress(length: Int, gen: => UInt) extends Module {
  val io = IO(new Bundle {
    val vecT = Input(Vec(length, gen))
    val mask = Input(UInt(length.W))
    val res = Output(Vec(length, ValidIO(gen)))
    val count = Output(UInt(log2Up(length).W))
  })

  private val vec = io.vecT
  private val mask = io.mask

  private val res: Vec[ValidIO[UInt]] = io.res
  private val popcVec = (0 until length).map(i => PopCount(mask(i, 0)))
  private val validPopcVec = mask.asBools lazyZip popcVec map { case (m, popc) => Mux(m, popc, 0.U) }
  private val popcOHVec = validPopcVec.map(x => (UIntToOH(x, length + 1) >> 1.U).asUInt.take(length))

  res.zipWithIndex.foreach {
    case (r, i) =>
      r.valid := i.U < popcVec.last
      r.bits := Mux1H(popcOHVec.map(_(i)), vec)
  }

  io.count := popcVec.last
}

object Compress {
  def apply[T <: Data](mask: UInt, vec: Vec[T], count: Option[UInt]): Vec[ValidIO[T]] = {
    val length = vec.length
    require(length == mask.getWidth)
    require(count.forall(_.getWidth == log2Up(length + 1)))
    val mod = Module(new Compress(length, vec.head.asUInt)).suggestName(s"Compress_${length}bits")
    mod.io.mask := mask
    mod.io.vecT := vec.map(_.asUInt)
    count.foreach(_ := mod.io.count)
    val res = Wire(Vec(length, ValidIO(vec.head.cloneType)))
    res lazyZip mod.io.res foreach { case (sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits.asTypeOf(sink.bits)
    }
    res
  }
}

class VectorCompress(vlen: Int) extends VectorShuffleBaseModule(vlen) {
  ////////
  // stage 0: compress each input vector elements into DREG
  //

  val s0_uopIdx = s0.bits.uopIdx
  val s0_vs8bVec = WireInit(s0.bits.src2.to8bitVec)
  val s0_mask = WireInit(s0.bits.mask)
  val s0_countVec = Wire(Vec(8, UInt((ElemIdxWidth + 1).W)))
  val s0_allMaskVec = s0.bits.allMask.toVf8Vec

  val s0_compressed8bVec: Vec[ValidIO[UInt]] = Compress(s0_mask, s0_vs8bVec, Some(s0_countVec.head))
  val s0_isUop0 = s0.valid && s0_uopIdx === 0.U

  for (i <- 1 until 8) {
    s0_countVec(i) := PopCount(s0_allMaskVec(i))
  }

  for (dregE8IdxHigh <- 0 until 8) {
    for (dregE8IdxLow <- 0 until VLENB) {
      wdataVec(dregE8IdxHigh)(dregE8IdxLow).valid := s0.valid && s0_uopIdx === dregE8IdxHigh.U
      wdataVec(dregE8IdxHigh)(dregE8IdxLow).bits := s0_compressed8bVec(dregE8IdxLow).bits
    }
  }

  ////////
  // stage 1: sum and rotate slide up data to right position
  //

  val s1_sumNext  = Wire(UInt(log2Up(vlen + 1).W))
  val s1_isUop0   = RegEnable(s0_isUop0,        s0.valid)
  val s1_countCur = RegEnable(s0_countVec.head, s0.valid)
  val s1_countVec = RegEnable(s0_countVec,      s0.valid && s0_isUop0)
  val s2_sum      = RegEnable(s1_sumNext,       s1.valid)
  val s2_sumBf    = RegEnable(s2_sum,           s1.valid)

  when(s1_uopIdx === 0.U) {
    s1_sumNext := s1_count
  }.elsewhen {
    s1_sumNext := s2_sum + s1_countCur
  }

  val s1_uopIdx = s1.bits.uopIdx
  val s1_rotatedVec8b = dreg8bMatrix(s1_uopIdx).rotateUp(s1_sumNext.take(VLENB))

  ////////
  // stage 2: copy to out reg
  //

  val countSum = RegEnable(ParallelOperation(s1_countVec, (a, b) => a +& b), s1.valid && s1_isUop0)

  val s2_rotatedVec8b = RegEnable(s1_rotatedVec8b, s1.valid)
  val s2_thisBegin = s2_sumBf.take(ElemIdxWidth)
  val s2_nextEnd = s2_sum.take(ElemIdxWidth)
  val sumBfHigh = s2_sumBf.drop(ElemIdxWidth)
  val sumHigh   = s2_sum.drop(ElemIdxWidth)

  val s2_nextReg = sumBfHigh =/= sumHigh

  ////////
  // Stage 3: output
  //

  val s3_nextReg = RegEnable(s2_nextReg, s2.valid)
  val s3_sumBf   = RegEnable(s2_sumBf,   s2.valid)
  val s3_sum     = RegEnable(s2_sum,     s2.valid)
  val s3_regVec1 = Reg(VecE8)
  val s3_regVec2 = Reg(VecE8)

  for (i <- 0 until VLENB) {
    when(s3_nextReg && i.U < s2_thisBegin) {
      s3_regVec1(i) := s3_regVec2(i)
    }.elsewhen(i.U >= s2_thisBegin) {
      s3_regVec1(i) := s2_rotatedVec8b(i)
    }

    when(s2_nextReg && i.U < s2_nextEnd) {
      s3_regVec2(i) := s2_rotatedVec8b(i)
    }
  }

  val outIsTail = !s3_nextReg
  val tailVdPtr = Reg(UInt(3.W))
  when (s1.valid && s1_isUop0) {
    tailVdPtr := 7.U // last vd idx
  }.elsewhen (outIsTail) {
    tailVdPtr := tailVdPtr - 1.U
  }

  val vd = s3_regVec1.asUInt
  val s3_uopIdx = Wire(UInt(3.W))
  s3_uopIdx := Mux(
    s3_nextReg,
    sumBfHigh,
    tailVdPtr,
  )

  io.out.valid      := s3.valid
  io.out.bits.dest  := vd
  io.out.bits.vdIdx := s3_uopIdx
}
