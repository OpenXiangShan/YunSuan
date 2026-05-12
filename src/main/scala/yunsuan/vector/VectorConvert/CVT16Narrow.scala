package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import yunsuan.util._
import yunsuan.encoding.Opcode.Opcodes.FCvtOpcode
import yunsuan.vector.Common._
import yunsuan.vector.VectorConvert.Bundles._
import yunsuan.vector.VectorConvert.RoundingModle._
import yunsuan.vector.VectorConvert.util._
import yunsuan.vector.VectorConvert.utils._

class CVT16NarrowConvert(width: Int = 16) extends Module {
  val io = IO(new Bundle {
    val in = Input(new VCVT.In(width))
    val out = Output(new VCVT.Out(width))
  })
  val fireS1 = GatedValidRegNext(io.in.fire)
  val s0 = Module(new CVT16ModuleS0(width))
  s0.io.s0In.src := io.in.data.src
  s0.io.s0In.opType := io.in.ctrl.opType
  s0.io.s0In.rm := io.in.ctrl.rm
  s0.io.s0In.inSew1H := io.in.ctrl.inSew1H
  s0.io.s0In.outSew1H := io.in.ctrl.outSew1H
  val s1 = Module(new CVT16NarrowModuleS1(width))
  CommonConnect(s1.io.s1In, s0.io.s0Out, io.in.fire)
  io.out.ex1.res := s1.io.s1Out.result
  io.out.ex1.fflags := s1.io.s1Out.fflags
  io.out.ex2.res := RegEnable(s1.io.s1Out.result, 0.U(width.W), fireS1)
  io.out.ex2.fflags := RegEnable(s1.io.s1Out.fflags, 0.U.asTypeOf(Fflags()), fireS1)
}

class CVT16NarrowModuleS1(width: Int = 16) extends Module {
  val io = IO(new CVT16BundleS1(width))
  val s1In = io.s1In
  val special = io.s1In.special
  val expIsOnes = special.expIsOnes
  val isZero = special.isZero
  val isInf = special.isInf
  val isNaN = special.isNaN
  val expInS0 = s1In.exp
  val expPlus1Enable = s1In.expPlus1Enable
  val outSew1H = s1In.outSew1H
  val signSrc = s1In.signSrc
  val rm = s1In.rm
  val hasSignInt = s1In.hasSignInt
  val inRounder = s1In.inRounder
  val sticky = s1In.sticky
  val intParamMap = (0 to 1).map(i => (1 << i) * 8)
  val rounder = Module(new RoundingUnit(width))
  rounder.io.in := inRounder.head(f16.width)
  rounder.io.roundIn := inRounder(0)
  rounder.io.stickyIn := sticky
  rounder.io.signIn := signSrc
  rounder.io.rm := rm
  val nxRounded = rounder.io.inexact
  val upRounded = rounder.io.r_up
  val exp = expInS0 + Mux(expPlus1Enable, 1.U, 0.U)
  val resultRounded = Mux(upRounded, rounder.io.in + 1.U, rounder.io.in)
  val normalResult = Mux(signSrc && resultRounded.orR, (~resultRounded).asUInt + 1.U, resultRounded)
  val int1HOut = outSew1H(1, 0)
  val hasSignInt1HOut = int1HOut.asBools.map(oh => Seq(oh && !hasSignInt, oh && hasSignInt)).flatten
  val isOnesRounderInputMapFp2Int =
    intParamMap.map(intType => Seq(intType, intType - 1)).flatten.map(intType => rounder.io.in.tail(f16.width - intType).andR)
  val cout = upRounded && Mux1H(hasSignInt1HOut, isOnesRounderInputMapFp2Int).asBool
  val isZeroRounded = !resultRounded.orR
  val ofExpRounded = !exp.head(1) && Mux1H(int1HOut,
    (3 to 4).map(i =>
      Mux1H(UIntToOH(hasSignInt ## cout), VecInit((0 to 3).map {
        case 0 => exp(exp.getWidth - 2, i).orR
        case 1 => exp(exp.getWidth - 2, i).orR || exp(i - 1, 0).andR
        case 2 => exp(exp.getWidth - 2, i).orR || exp(i - 1, 0).andR
        case 3 => exp(exp.getWidth - 2, i).orR || exp(i - 1, 1).andR
      }))
    )
  )
  val excludeFrac = Mux1H(int1HOut,
    intParamMap.map(intType => resultRounded(intType - 1) && !resultRounded(intType - 2, 0).orR))
  val excludeExp = Mux1H(int1HOut,
    (3 to 4).map(i => !exp.head(exp.getWidth - i).orR &&
      Mux(cout, exp(i - 1, 1).andR && !exp(0), exp(i - 1, 0).andR)
    )
  )
  val signNonNan = signSrc && !isNaN
  val toUnv = ofExpRounded || expIsOnes || signSrc && !(isZero || isZeroRounded && !ofExpRounded)
  val toUnx = !toUnv && nxRounded
  val toInv = ofExpRounded && !(signSrc && excludeExp && excludeFrac) || expIsOnes
  val toInx = !toInv && nxRounded
  val fp2IntNv = Mux(hasSignInt, toInv, toUnv)
  val fp2IntNx = Mux(hasSignInt, toInx, toUnx)
  io.s1Out.result := Mux1H(Seq(
    ((!hasSignInt && !toUnv) || (hasSignInt && !toInv)) -> normalResult,
    (!hasSignInt && toUnv && (isNaN || !signSrc && (isInf || ofExpRounded))) -> (~0.U(width.W)).asUInt,
    (!hasSignInt && toUnv && signSrc && !isNaN) -> 0.U(width.W),
    (hasSignInt && toInv) -> Mux1H(int1HOut, intParamMap.map(intType => signNonNan ## Fill(intType - 1, !signNonNan)))
  ))
  io.s1Out.fflags := Cat(fp2IntNv, false.B, false.B, false.B, fp2IntNx)
}