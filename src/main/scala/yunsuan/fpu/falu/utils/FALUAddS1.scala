package yunsuan.fpu.falu.utils

import yunsuan.fpu.FloatParams
import yunsuan.fpu.falu.utils.FALUAddS0ToS1Bundle
import chisel3._
import chisel3.util._

class FALUAddS1(val totalWidth: Int) extends Module with FloatParams{
  val io = IO(new Bundle() {
    val fromS0 = Input(new FALUAddS0ToS1Bundle(totalWidth))
    val outRes = Output(UInt(totalWidth.W))
    val outFlags = Output(UInt(flagsWidth.W))
  })
  val sign = io.fromS0.signS0
  val isSub = io.fromS0.isSub
  val expBase = io.fromS0.expS0
  val expBaseIsZero = !expBase.orR
  val fracS0 = io.fromS0.fracS0
  val fracBase = io.fromS0.fracS0(decimalWidth - 1, 0)
  val fracOFBase = io.fromS0.fracS0(decimalWidth, 1)
  val isClosePath = io.fromS0.isClosePath
  val roundUp = io.fromS0.roundUp
  val roundUpOF = io.fromS0.roundUpOF
  val roundUpUF = io.fromS0.roundUpUF
  val expBaseAdd1 = expBase + 1.U
  val expBaseAdd2 = expBase + 2.U
  val fracBaseAdd1 = fracBase + 1.U
  val fracBaseOFAdd1 = fracOFBase + 1.U
  val isFracOF = fracS0.head(1).asBool || isClosePath && expBaseIsZero || fracBase.andR && roundUp
  val denormalExpOF = Mux(isSub, fracS0.head(1).asBool || fracOFBase.tail(1).andR && roundUpOF, fracBase.head(1).asBool || fracBase.tail(1).andR && roundUp)
  val denormalExpOFUF = Mux(isSub, fracOFBase.tail(1).andR && roundUpUF, fracBase.tail(1).andR && roundUpUF)
  val isExpOF = Mux(!expBaseIsZero, isFracOF, denormalExpOF)
  val isExpOF2 = fracS0.head(1).asBool && fracOFBase.andR && roundUpOF
  val resultIsNaN = io.fromS0.resultIsNaN
  val resultIsInf = io.fromS0.resultIsInf
  val resultIsOF = !resultIsNaN && (
      expBase(exponentWidth - 1, 1).andR && (expBase(0) || isFracOF) ||
      expBase(exponentWidth - 1, 2).andR && (expBase(1,0).orR && isExpOF2) ||
      resultIsInf || io.fromS0.flagsOF
    )
  val dec = Mux(isFracOF, Mux(roundUpOF, fracBaseOFAdd1, fracOFBase), Mux(roundUp, fracBaseAdd1, fracBase))
  val frac = dec(significandWidth - 1, 0)
  val exp = Mux(isExpOF, Mux(isExpOF2, expBaseAdd2, expBaseAdd1), expBase)
  val resIsNaNOrInf = resultIsNaN || resultIsOF
  val rm = io.fromS0.rm
  val resultOF = Mux(((rm === RTZ.U) || sign && (rm === RUP.U) || !sign && (rm === RDN.U)) && !resultIsInf, maxNormal.U, infNoSign.U)
  val resNaNOrInf = Mux1H(Seq(resultIsNaN, resultIsOF), Seq(cNaN.U, Cat(sign, resultOF)))
  io.outRes := Mux(resIsNaNOrInf, resNaNOrInf, Cat(sign, exp, frac))
  val flagsNV = io.fromS0.flagsNV
  val flagsDZ = false.B
  val flagsOF = !resultIsNaN && !resultIsInf && resultIsOF
  val flagsNX = !resultIsNaN && !resultIsInf && Mux(isFracOF, io.fromS0.flagsNXOF, io.fromS0.flagsNX) || flagsOF
  val canUF = expBaseIsZero && Mux(isSub, !fracS0.head(1), !fracS0.head(2).orR)
  val flagsUF = !resultIsNaN && !resultIsInf && canUF && !denormalExpOFUF && flagsNX
  io.outFlags := Cat(flagsNV, flagsDZ, flagsOF, flagsUF, flagsNX)
  dontTouch(sign)
  dontTouch(exp)
  dontTouch(frac)
  dontTouch(isFracOF)
  dontTouch(isExpOF)
  dontTouch(isExpOF2)
  dontTouch(roundUp)
  dontTouch(roundUpOF)
  dontTouch(fracBase)
  dontTouch(fracOFBase)
  dontTouch(denormalExpOF)
  dontTouch(denormalExpOFUF)
  dontTouch(canUF)
}
