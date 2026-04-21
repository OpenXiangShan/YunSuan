package yunsuan.fpu.falu.utils

import yunsuan.fpu.FloatParams
import chisel3._
import chisel3.util._
import yunsuan.fpu.falu.utils.util._
import yunsuan.fpu.fmul.utils.FMULToFADDCtrlBundle

class FALUAddS0ToS1Bundle(val totalWidth: Int) extends Bundle with FloatParams {
//  val roundMode = Input(UInt(rmWidth.W))
  val flagsNV = Output(Bool())
  val flagsOF = Output(Bool())
  val resultIsNaN = Output(Bool())
  val resultIsInf = Output(Bool())
  val isSub = Output(Bool())
  val isClosePath = Output(Bool())
  val signS0 = Output(Bool())
  val expS0 = Output(UInt(exponentWidth.W))
  val fracS0 = Output(UInt((decimalWidth + 1).W))
  val roundUp = Output(Bool())
  val roundUpOF = Output(Bool())
  val roundUpUF = Output(Bool())
  val flagsNX = Output(Bool())
  val flagsNXOF = Output(Bool())
  val closeOF = Output(Bool())
  val rm = Output(UInt(rmWidth.W))
}
class FALUAddS0(val totalWidth: Int) extends Module with FloatParams{
  val io = IO(new Bundle() {
    // fpA is vs2/rs1/fmulTempResult
    val fpA, fpB = Input(UInt(totalWidth.W))
    val fpAisCanonicalNaN      = Input(Bool())
    val fpBisCanonicalNaN      = Input(Bool())
    val isSub    = Input(Bool())
    val rm = Input(UInt(rmWidth.W))
    val toS1 = Output(new FALUAddS0ToS1Bundle(totalWidth))
    val fpAAppend = Input(UInt(decimalWidth.W))
    val inCtrlFromFMUL = Input(new FMULToFADDCtrlBundle(totalWidth))
  })
  val isFMA = io.inCtrlFromFMUL.isFMA
  val fmaCtrl = io.inCtrlFromFMUL
  val rm = io.rm
  val signA = io.fpA.head(1).asBool
  val signB = io.fpB.head(1).asBool
  val isSub = signA ^ (signB ^ io.isSub)
  val expA = io.fpA.tail(1).head(exponentWidth)
  val expB = io.fpB.tail(1).head(exponentWidth)
  val expAIsZero = expA === 0.U
  val expBIsZero = expB === 0.U
  val expAIsOne = expA === 1.U
  val expBIsOne = expB === 1.U
  val expAIsAllOne = expA.andR
  val expBIsAllOne = expB.andR
  val expAEB = expA === expB
  val expAG1 = expA === expB +& 1.U
  val expBG1 = expB === expA +& 1.U
  val fracA = io.fpA.tail(1 + exponentWidth)
  val fracB = io.fpB.tail(1 + exponentWidth)
  val fracAIsZero = !fracA.orR
  val fracBIsZero = !fracB.orR
  val fpAisCanonicalNaN = io.fpAisCanonicalNaN
  val fpBisCanonicalNaN = io.fpBisCanonicalNaN
  val fpAIsZero = Mux(isFMA, fmaCtrl.resIsZero, !fpAisCanonicalNaN && expAIsZero && fracAIsZero)
  val fpBIsZero = !fpBisCanonicalNaN && expBIsZero && fracBIsZero
  val fpAIsInf = Mux(isFMA, fmaCtrl.resIsInf, !fpAisCanonicalNaN && expAIsAllOne && fracAIsZero)
  val fpBIsInf = !fpBisCanonicalNaN && expBIsAllOne && fracBIsZero
  val fpAIsNaN = Mux(isFMA, fmaCtrl.resIsNaN, fpAisCanonicalNaN || (expAIsAllOne && !fracAIsZero))
  val fpBIsNaN = fpBisCanonicalNaN || (expBIsAllOne && !fracBIsZero)
  val fpAIsSNaN = Mux(isFMA, fmaCtrl.flagsNV, !fpAisCanonicalNaN && fpAIsNaN && !fracA.head(1))
  val fpBIsSNaN = !fpBisCanonicalNaN && fpBIsNaN && !fracB.head(1)
  val decA = Cat(!expAIsZero, fracA)
  val decAApend = io.fpAAppend
  val decAApendTail1IsZero = !decAApend.tail(1).orR
  val decAApendIsZero = decAApendTail1IsZero && !decAApend.head(1)
  val decB = Cat(!expBIsZero, fracB)
  val expASubExpBCarry = expA +& (~expB).asUInt + 1.U
  val expAGE = expASubExpBCarry.head(1).asBool
  val expG = Mux(expAGE, expA, expB)
  val isClosePath = isSub && (expAEB || expAG1 || expBG1)

  // far path begin
  val expFar = Mux(isSub, expG - 1.U, expG)
  // suppose expA is equal to or greater than expB
  val shiftBitsWidthAdd1 = shiftBitsWidth + 1 // f64 is 7
  val maxRS = (1 << shiftBitsWidthAdd1) - 1 // f64 is 127
  val expASubExpB = expASubExpBCarry(exponentWidth - 1, 0)
  val expAIsTooLarge = expASubExpB > (2 * decimalWidth - 1).U
  val decBRSBits = expASubExpB(shiftBitsWidthAdd1 - 1, 0)
  val decBNeedLS1 = !expAIsZero && expBIsZero
  val decBRSTotal = Wire(UInt((decimalWidth + maxRS).W))
  decBRSTotal := RightShiftLowFirst((RightShiftLowFirst(Cat(decB, 0.U(maxRS.W)), decBRSBits(2, 0)) << decBNeedLS1).asUInt, Cat(decBRSBits(shiftBitsWidthAdd1 - 1, 3), 0.U(3.W)))
  val decBShiftedMask = ((Cat(0.U((2 * decimalWidth).W), Fill(maxRS, 1.U)) << decBRSBits) >> decBNeedLS1)(2 * decimalWidth + maxRS - 1, decimalWidth + maxRS)
  val decBShiftedSticky = (decB & decBShiftedMask).orR

  // suppose expB is equal to or greater than expB
  val expBSubExpA = expB + (~expA).asUInt + 1.U
  val expBIsTooLarge = expBSubExpA > (2 * decimalWidth - 1).U
  val decARSBits = expBSubExpA(shiftBitsWidthAdd1 - 1, 0)
  val decANeedLS1 = !expBIsZero && expAIsZero
  val decARSTotal = Wire(UInt((decimalWidth + maxRS).W))
  val decAApendRSTotal = Wire(UInt((2 * decimalWidth + maxRS).W))
  decAApendRSTotal := RightShiftLowFirst((RightShiftLowFirst(Cat(decA, decAApend, 0.U(maxRS.W)), decARSBits(2, 0)) << decANeedLS1).asUInt, Cat(decARSBits(shiftBitsWidthAdd1 - 1, 3), 0.U(3.W)))
  decARSTotal := decAApendRSTotal.head(decimalWidth + maxRS)
  val decAShiftedMask = ((Cat(0.U((2 * decimalWidth).W), Fill(maxRS, 1.U)) << decARSBits) >> decANeedLS1)(2 * decimalWidth + maxRS - 1, maxRS)
  val decAShiftedSticky = (Cat(decA, decAApend) & decAShiftedMask).orR

  val decRSTooLarge = Mux(expAGE, expAIsTooLarge, expBIsTooLarge)
  val decRSIsZero = Mux(expAGE, fpBIsZero, fpAIsZero)
  val decRSSticky = Mux(expAGE, decBShiftedSticky, decAShiftedSticky || fmaCtrl.sticky)
  val decRSTotal = Mux(expAGE, decBRSTotal, decARSTotal)
  val decRSTotalFix = Mux(decRSTooLarge, 0.U((decimalWidth + maxRS).W), decRSTotal)
  val decRS = decRSTotalFix.head(2 * decimalWidth)
  val decRSTail = decRSTotalFix.tail(2 * decimalWidth)
  val decGE = Mux(expAGE, decA, decB)
  // 2 * decimalWidth + 1
  val decGEAlignBase = Cat(decGE, Mux(isFMA && expAGE, decAApend, 0.U(decimalWidth.W)))
  val decGEAlign = Mux(isSub, Cat(decGEAlignBase, 0.U), Cat(0.U, decGEAlignBase))
  val decLEAlign = Mux(isSub, Cat(~decRS, 1.U), Cat(0.U, decRS))
  val farNeedAdd1 = Mux(isSub, Mux(decRSTooLarge, decRSIsZero, !decRSSticky), 0.U)
  val fracLong = decGEAlign + decLEAlign + farNeedAdd1
  val fracFar = fracLong(2 * decimalWidth, decimalWidth)
  val fracFarLow = fracLong(decimalWidth - 1, 0)
  // far grs logic
  // debug signal, ignore
  val decRSTailGEForAdd = Mux(isFMA && expAGE, Cat(decAApend, 0.U((maxRS - decimalWidth).W)), 0.U(maxRS.W))
  val decRSTailAdd = decRSTailGEForAdd +& decRSTail
  val decRSTailAddOF = decRSTailAdd.head(1).asBool
  val decRSTailAddHighBit = decRSTailAdd.tail(1).head(1).asBool
  
  val guardFar = fracFar(0)
  val roundFar = fracFarLow.head(1).asBool
  val stickyFarBase = Mux(decRSTooLarge, !decRSIsZero, decRSSticky) || isFMA && fmaCtrl.sticky
  val stickyFar = fracFarLow.tail(1).orR || stickyFarBase
  val roundUpFar = RoundGen(rm, guardFar, roundFar, stickyFar, io.toS1.signS0)
  val guardFarUF = fracFarLow.head(1). asBool
  val roundFarUF = fracFarLow.tail(1).head(1).asBool
  val stickyFarBaseUF = Mux(decRSTooLarge, !decRSIsZero, decRSSticky) || isFMA && fmaCtrl.sticky
  val stickyFarUF = fracFarLow.tail(2).orR || stickyFarBase
  val roundUpFarUF = guardFarUF && RoundGen(rm, guardFarUF, roundFarUF, stickyFarUF, io.toS1.signS0)

  val guardFarOF = fracFar(1)
  val roundFarOF = fracFar(0)
  val stickyFarOF = fracFarLow.orR || stickyFarBase
  val roundUpFarOF = RoundGen(rm, guardFarOF, roundFarOF, stickyFarOF, io.toS1.signS0)

  //close path begin
  val expNE = expA(0) ^ expB(0) // exp not equal
  // suppose expA is equal to or 1 greater than expB
  val fracAAlign = Cat(1.U, fracA, decAApend, 0.U)
  val fracBAlign = Cat(1.U, fracB, 0.U(decimalWidth.W), 0.U)
  val fracARS1 = Cat(0.U, 1.U, fracA, decAApend)
  val fracBRS1 = Cat(0.U, 1.U, fracB, 0.U(decimalWidth.W))
  val lzaSeqAE  = LZASeqGen(fracAAlign, fracBAlign)
  val lzaSeqAG1 = LZASeqGen(fracAAlign, fracBRS1)
  val expAMask = ExpMaskGen(expA, fracAAlign.getWidth)
  // lzd = lza + 0/1
  // suppose lza is 1 bit little than lzd
  val lzd0 = Module(new lzc_parameters(2 * decimalWidth + 2))
  lzd0.io.X := Cat(0.U, Mux(expNE, lzaSeqAG1, lzaSeqAE) | expAMask)
  val lzaAGE = lzd0.io.Z
//  val lzaAGE = LZD(Cat(0.U, Mux(expNE, lzaSeqAG1, lzaSeqAE) | expAMask))

  // suppose expB is equal to or 1 greater than expA
  val lzaSeqBE  = LZASeqGen(fracBAlign, fracAAlign)
  val lzaSeqBG1 = LZASeqGen(fracBAlign, fracARS1)
  val expBMask = ExpMaskGen(expB, fracBAlign.getWidth)
  // suppose lza is 1 bit little than lzd
  val lzd1 = Module(new lzc_parameters(2 * decimalWidth + 2))
  lzd1.io.X := Cat(0.U, Mux(expNE, lzaSeqBG1, lzaSeqBE) | expBMask)
  val lzaBGE = lzd1.io.Z
//  val lzaBGE = LZD(Cat(0.U, Mux(expNE, lzaSeqBG1, lzaSeqBE) | expBMask))

  val fracASubB = fracA +& (~fracB).asUInt + 1.U
  val fracAGE = fracASubB.head(1).asBool
  val AGEB = expAEB && fracAGE || !expAEB && expAGE
  val lza = Mux(AGEB, lzaAGE, lzaBGE)

  // expClose is expG - lza
  val maskEn = expG === lza // mask enable
  // lza is >= 1, 0 - 1 will overflow
  val absFpAEB = (io.fpA.tail(1) === io.fpB.tail(1)) && Mux(isFMA, decAApendIsZero && !fmaCtrl.sticky, true.B)
  val resultIsZero = isSub && absFpAEB
  val expClose = Mux((expG === 0.U) || resultIsZero, 0.U, expG - lza)

  // decA and decB's width append 1 because grs logic need
  val decAClose = Cat(decA, decAApend.head(1))
  val decBClose = Cat(decB, 0.U)
  val isAshift = expNE && !expAIsZero
  val decAShift = Mux(isAshift, Cat(0.U, decA), decAClose)
  val isBshift = expNE && !expBIsZero
  val decBShift = Mux(isBshift, Cat(0.U, decB), decBClose)
  val decASubdecB = decAClose + (~decBShift).asUInt + 1.U
  val decBSubdecANeedAdd1 = Mux(isAshift, decAApendIsZero, decAApendTail1IsZero) && !fmaCtrl.sticky
  val decBSubdecA = decBClose + (~decAShift).asUInt + decBSubdecANeedAdd1
  val subFrac = Mux(AGEB, decASubdecB, decBSubdecA)
  val const = 1.U << (decimalWidth + 1) // width = decimalWidth + 2
  val maskOF = (const >> lza)(decimalWidth, 0)
  val closeOF = (subFrac & maskOF).orR
  val decAApendNeg = (~decAApend).asUInt + !fmaCtrl.sticky
  val fracCloseTail = Mux(AGEB, Cat(decAApend.tail(1), 0.U), Mux(isAshift, decAApendNeg, Cat(decAApendNeg.tail(1), 0.U)))
  val subFracForShift = Cat(subFrac, fracCloseTail) // width = 2 * decimalWidth + 1
  val subFracShift = LeftShiftHighFirst(Cat(0.U, subFracForShift), lza)
  val fracClose = subFracShift(2 * decimalWidth + 1, decimalWidth + 1)
  val guardClose = fracClose(0)
  val roundClose = subFracShift(decimalWidth)
  val stickyClose = fmaCtrl.sticky || subFracShift(decimalWidth - 1, 0).orR
  val roundUpClose = RoundGen(rm, guardClose, roundClose, stickyClose, io.toS1.signS0)
  val guardCloseOF = fracClose(1)
  val roundCloseOF = fracClose(0)
  val stickyCloseOF = fmaCtrl.sticky || subFracShift(decimalWidth, 0).orR
  val roundUpCloseOF = RoundGen(rm, guardCloseOF, roundCloseOF, stickyCloseOF, io.toS1.signS0)
  val guardCloseUF = fracClose(0)
  val roundCloseUF = subFracShift(decimalWidth)
  val stickyCloseUF = fmaCtrl.sticky || subFracShift(decimalWidth - 1, 0).orR
  val roundUpCloseUF = guardCloseUF && RoundGen(rm, guardCloseUF, roundCloseUF, stickyCloseUF, io.toS1.signS0)
  //close path end
  val infSign = fpAIsInf && signA || fpBIsInf && (signB ^ io.isSub)
  val normalSign = Mux(isSub, Mux(AGEB, Mux(absFpAEB, rm === RDN.U, signA), (signB ^ io.isSub)), signA)
  io.toS1.isClosePath := isClosePath
  io.toS1.isSub := isSub
  io.toS1.signS0 := Mux(fpAIsInf || fpBIsInf, infSign, normalSign)
  io.toS1.expS0 := Mux(isClosePath, expClose, expFar)
  io.toS1.fracS0 := Mux(isClosePath, fracClose, fracFar)
  io.toS1.roundUp := Mux(isClosePath, roundUpClose, roundUpFar)
  io.toS1.roundUpOF := Mux(isClosePath, roundUpCloseOF, roundUpFarOF)
  io.toS1.roundUpUF := Mux(isClosePath, roundUpCloseUF, roundUpFarUF)
  io.toS1.flagsNX := Mux(isClosePath, roundClose || stickyClose, roundFar || stickyFar)
  io.toS1.flagsNXOF := Mux(isClosePath, roundCloseOF || stickyCloseOF, roundFarOF || stickyFarOF)
  io.toS1.flagsNV := isSub && fpAIsInf && fpBIsInf || fpAIsSNaN || fpBIsSNaN
  io.toS1.flagsOF := fmaCtrl.flagsOF
  io.toS1.resultIsNaN := isSub && fpAIsInf && fpBIsInf || fpAIsNaN || fpBIsNaN
  io.toS1.resultIsInf := !io.toS1.resultIsNaN && (!isSub && (fpAIsInf || fpBIsInf) || isSub && (fpAIsInf ^ fpBIsInf))
  io.toS1.closeOF := closeOF
  io.toS1.rm := rm

  dontTouch(signA)
  dontTouch(signB)
  dontTouch(expA)
  dontTouch(expB)
  dontTouch(fracA)
  dontTouch(fracB)
  dontTouch(decA)
  dontTouch(decB)
  dontTouch(decGE)
  dontTouch(expAGE)
  dontTouch(decARSBits)
  dontTouch(decBRSBits)
  dontTouch(decARSTotal)
  dontTouch(decBRSTotal)
  dontTouch(subFrac)
  dontTouch(subFracShift)
  dontTouch(fracClose)
  dontTouch(decASubdecB)
  dontTouch(decBSubdecA)
  dontTouch(decBSubdecANeedAdd1)
  dontTouch(isAshift)
  dontTouch(isBshift)
  dontTouch(lzaAGE)
  dontTouch(lzaBGE)
  dontTouch(expAMask)
  dontTouch(expBMask)
  dontTouch(lzaSeqAE)
  dontTouch(lzaSeqAG1)
  dontTouch(lzaSeqBE)
  dontTouch(lzaSeqBG1)
  dontTouch(lzaSeqBG1)
  dontTouch(fracAAlign)
  dontTouch(fracBAlign)
  dontTouch(fracARS1)
  dontTouch(guardClose)
  dontTouch(roundClose)
  dontTouch(stickyClose)
  dontTouch(guardCloseUF)
  dontTouch(roundCloseUF)
  dontTouch(stickyCloseUF)
  dontTouch(guardCloseOF)
  dontTouch(roundCloseOF)
  dontTouch(stickyCloseOF)
  dontTouch(guardFar)
  dontTouch(roundFar)
  dontTouch(stickyFarBase)
  dontTouch(stickyFar)
  dontTouch(guardFarUF)
  dontTouch(roundFarUF)
  dontTouch(stickyFarBaseUF)
  dontTouch(stickyFarUF)
  dontTouch(guardFarOF)
  dontTouch(roundFarOF)
  dontTouch(stickyFarOF)
  dontTouch(decRSTooLarge)
  dontTouch(fracFar)
  dontTouch(maskOF)
  dontTouch(AGEB)
  dontTouch(fracAGE)
  dontTouch(maskEn)
  dontTouch(decGEAlignBase)
  dontTouch(decGEAlign)
  dontTouch(decLEAlign)
  dontTouch(farNeedAdd1)
  dontTouch(expBIsTooLarge)
  dontTouch(expAIsTooLarge)
  dontTouch(decRSSticky)
  dontTouch(decAApendRSTotal)
  dontTouch(decRSTailGEForAdd)
  dontTouch(decAApend)
  dontTouch(decRSTailAdd)
  dontTouch(decRSTailAddOF)
  dontTouch(fracCloseTail)
  dontTouch(subFracForShift)
  dontTouch(roundUpFar)
  dontTouch(roundUpFarOF)
  dontTouch(roundUpClose)
  dontTouch(roundUpCloseOF)
  dontTouch(decRSTail)
  dontTouch(expClose)
  dontTouch(fracFarLow)
  dontTouch(decAShiftedMask)
  dontTouch(decAShiftedSticky)
  dontTouch(fracLong)
  dontTouch(decBShiftedMask)
  dontTouch(decBShiftedSticky)
  dontTouch(infSign)
  dontTouch(normalSign)
}
