package yunsuan.vector.VectorShuffle

import chisel3._
import chisel3.util._
import yunsuan.util.LiteralCat
import yunsuan.util.Reflect.{getUIntMaxWidthOfObject, getUIntWidthOfObject}
import yunsuan.vector.Common._

import scala.language.{existentials, postfixOps, implicitConversions}

class VectorSlideUp(vlen: Int) extends VectorShuffleBaseModule(vlen) {
  ////////
  // stage 0: rotate up to be aligned in vreg, and the elements in vreg in range[e8offsetInVreg,VLENB) is the final result
  //

  val s0_uopIdx = s0.bits.uopIdx
  val s0_vs8bVec = WireInit(s0.bits.src2.to8bitVec)

  val s0_alignedVs8bVec = s0_vs8bVec.rotateUp(e8offset)

  for (dregE8IdxHigh <- 0 until 8) {
    for (dregE8IdxLow <- 0 until VLENB) {
      wdataVec(dregE8IdxHigh)(dregE8IdxLow).valid := s0.valid && (
        (dregE8IdxHigh.U < e8offsetVreg && dregE8IdxHigh.U === s0_uopIdx) ||
        ((dregE8IdxHigh != 7).B && dregE8IdxHigh.U >= e8offsetVreg && dregE8IdxHigh.U === (s0_uopIdx +& e8offsetVreg)) ||
        ((dregE8IdxHigh == 7).B && dregE8IdxHigh.U >= e8offsetVreg && dregE8IdxHigh.U === (s0_uopIdx +& e8offsetVreg) && dregE8IdxLow.U >= e8offsetInVreg) ||
        ((dregE8IdxHigh == 7).B && s0_uopIdx === e8offsetVreg && dregE8IdxLow.U < e8offsetInVreg)
      )
      wdataVec(dregE8IdxHigh)(dregE8IdxLow).bits := Mux1H(Seq(
        (dregE8IdxHigh.U < e8offsetVreg && dregE8IdxHigh.U === s0_uopIdx) -> s0_vs8bVec(dregE8IdxLow),
        ((dregE8IdxHigh != 7).B && dregE8IdxHigh.U >= e8offsetVreg && dregE8IdxHigh.U === (s0_uopIdx +& e8offsetVreg)) -> s0_alignedVs8bVec(dregE8IdxLow),
        ((dregE8IdxHigh == 7).B && dregE8IdxHigh.U >= e8offsetVreg && dregE8IdxHigh.U === (s0_uopIdx +& e8offsetVreg) && dregE8IdxLow.U >= e8offsetInVreg) -> s0_alignedVs8bVec(dregE8IdxLow),
        ((dregE8IdxHigh == 7).B && s0_uopIdx === e8offsetVreg && dregE8IdxLow.U < e8offsetInVreg) -> s0_vs8bVec(dregE8IdxLow),
      ))
    }
  }

  ////////
  // stage 1: seleft final result
  //

  val s1_uopIdx = s1.bits.uopIdx

  val vd = Wire(Vec(vlen / 8, UInt(8.W)))

  val dregIdx = s1.bits.uopIdx
  val dregGapOffset = e8offsetInVreg

  for (i <- 0 until VLENB) {
    val vdE8Idx = Cat(s1_uopIdx, i.U(ElemIdxWidth.W))
    vd(i) := Mux1H(Seq(
      (s1_uopIdx < e8offsetVreg) -> dreg8bMatrix(dregIdx)(i),
      (s1_uopIdx === e8offsetVreg && i.U  < e8offsetInVreg) -> dreg8bMatrix(7)(i),
      (s1_uopIdx === e8offsetVreg && i.U >= e8offsetInVreg) -> dreg8bMatrix(dregIdx)(i),
      (s1_uopIdx > e8offsetVreg && vdE8Idx < e8vl) ->
        Mux1H(Seq(
          (i.U >= e8offsetInVreg) -> dreg8bMatrix(dregIdx)(i),
          (i.U <  e8offsetInVreg) -> dreg8bMatrix(dregIdx - 1.U)(i),
        )),
      (vdE8Idx >= e8vl) -> fill8b1s,
    ))
  }

  io.out.valid      := s1.valid
  io.out.bits.dest  := vd.asTypeOf(io.out.bits.dest)
  io.out.bits.vdIdx := s1_uopIdx
}

class RotateUp(gen: Vec[UInt]) extends Module {
  private val length = gen.length
  private val width = gen.head.getWidth
  override def desiredName: String = s"RotateUpW${width}L${length}"

  val io = IO(new Bundle {
    val inVec = Input(chiselTypeOf(gen))
    val n     = Input(UInt(log2Up(length).W))
    val outVec = Output(chiselTypeOf(gen))
  })

  private val vec = io.inVec

  io.outVec := this.rotateUp(io.n)

  def rotateUp(n: Int): Vec[UInt] = {
    n.ensuring(n >= 0) match {
      case _ if (n == 0) => vec
      case _ if (n >= length) => rotateUp(n % length)
      case _ => VecInit(vec.takeRight(n) ++ vec.dropRight(n))
    }
  }

  def rotateUp(n: UInt): Vec[UInt] = {
    val slideNum = n.take(log2Up(length))
    VecInit.tabulate(length)(l => this.rotateUp(l))(slideNum)
  }
}

object RotateUp {
  def apply[T <: Data](vec: Vec[T], n: UInt): Vec[T] = {
    val length = vec.length
    val width = vec.head.getWidth

    val mod = Module(new RotateUp(VecInit(vec.map(_.asUInt))))

    mod.io.inVec := vec
    mod.io.n := n

    val res = Wire(Vec(length, chiselTypeOf(vec.head)))
    res := mod.io.outVec.asTypeOf(res)
    res
  }
}
