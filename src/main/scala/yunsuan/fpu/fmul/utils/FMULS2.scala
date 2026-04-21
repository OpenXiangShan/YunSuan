package yunsuan.fpu.fmul.utils
import chisel3._
import chisel3.util._
import yunsuan.fpu._
import yunsuan.fpu.fmul.utils.FMULS1ToS2Bundle

class FMULS2(val totalWidth: Int, val version: Int = 0) extends Module with FloatParams {
  override def desiredName = (this.getClass.getName + s"_$version").split("\\.").last
  val io = IO(new Bundle() {
    val inFromS1 = Input(new FMULS1ToS2Bundle(totalWidth))
    val outRes = Output(UInt(totalWidth.W))
    val outFlags = Output(UInt(flagsWidth.W))
  })
  val rm = io.inFromS1.rm
  val sign = io.inFromS1.resForRounding.head(signWidth).asBool
  val exp = io.inFromS1.resForRounding.tail(signWidth).head(exponentWidth)
  val frac = io.inFromS1.resForRounding.tail(signWidth + exponentWidth).head(significandWidth)
  val guard = frac(0)
  val round = io.inFromS1.resForRounding(significandWidth)
  val sticky = io.inFromS1.resForRounding(significandWidth - 1, 0).orR || io.inFromS1.sticky
  val guard_uf = round
  val round_uf = io.inFromS1.resForRounding(significandWidth - 1)
  val sticky_uf = io.inFromS1.resForRounding(significandWidth - 2, 0).orR || io.inFromS1.sticky
  val expAdd1 = exp + 1.U
  val fracAdd1 = frac + 1.U
  val isRNE = rm === RNE.U
  val isRTZ = rm === RTZ.U
  val isRDN = rm === RDN.U
  val isRUP = rm === RUP.U
  val isRMM = rm === RMM.U
  val roundUp = isRNE & round & (guard | sticky) |
                isRDN & sign & (round | sticky) |
                isRUP & (!sign) & (round | sticky) |
                isRMM & round
  val roundUp_uf = isRNE & round_uf & (guard_uf | sticky_uf) |
                isRDN & sign & (round_uf | sticky_uf) |
                isRUP & (!sign) & (round_uf | sticky_uf) |
                isRMM & round_uf
  val fracRoundUp = Mux(roundUp, fracAdd1, frac)
  val expIsRoundUp = frac.andR & roundUp
  val expIsRoundUp_uf = frac.andR & guard_uf & roundUp_uf
  val expIsOverflow = Mux(expIsRoundUp, exp(exp.getWidth - 1, 1).andR, exp.andR)
  val expForOverflow = Cat(Fill(exponentWidth - 1, 1.U), !(isRTZ | !sign & isRDN | sign & isRUP))
  val fracForOverflow = Fill(significandWidth, isRTZ | !sign & isRDN | sign & isRUP)
  val expFinal = Mux(expIsOverflow, expForOverflow, Mux(expIsRoundUp, expAdd1, exp))
  val fracFinal = Mux(expIsOverflow, fracForOverflow, fracRoundUp)
  val zero = Cat(sign, 0.U((totalWidth - 1).W))
  val inf = Cat(sign, Fill(exponentWidth, 1.U), 0.U(significandWidth.W))
  val isConst = io.inFromS1.resIsNAN || io.inFromS1.resIsZero || io.inFromS1.resIsInf
  val const = Mux1H(
    Seq(io.inFromS1.resIsNAN, io.inFromS1.resIsZero, io.inFromS1.resIsInf),
    Seq(cNaN.U, zero, inf)
  )
  io.outRes := Mux(isConst, const, Cat(sign, expFinal, fracFinal))
  val flagsNV = io.inFromS1.flagsNV
  val flagsDZ = false.B
  val flagsOF = expIsOverflow
  val flagsNX = expIsOverflow || (round || sticky)
  val flagsUF = (exp === 0.U) && !expIsRoundUp_uf && flagsNX
  io.outFlags := Mux(isConst, Cat(flagsNV, 0.U((flagsWidth - 1).W)), Cat(0.U, flagsDZ, flagsOF, flagsUF, flagsNX))

  dontTouch(sign)
  dontTouch(rm)
  dontTouch(frac)
  dontTouch(fracAdd1)
  dontTouch(roundUp)
  dontTouch(guard)
  dontTouch(round)
  dontTouch(sticky)
  dontTouch(fracFinal)
  dontTouch(exp)
  dontTouch(expAdd1)
  dontTouch(expIsRoundUp)
  dontTouch(expIsOverflow)
  dontTouch(expForOverflow)
  dontTouch(fracForOverflow)
  dontTouch(expFinal)
  dontTouch(fracFinal)
}