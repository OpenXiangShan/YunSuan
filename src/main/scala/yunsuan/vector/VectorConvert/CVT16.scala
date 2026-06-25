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

class CVT16(width: Int = f16.width) extends CVT(width) {
  /** cycle0                                                              | cycle1                  |   cycle2
   * fp2int   in(16) raw_in(17)  left,right    ShiftRightJam(37)          | RoundingUnit(11)  adder |
   * int2fp   in(16) in_abs(16)  lzc in_shift  exp_raw                    | RoundingUnit(10)  adder |  -> result & fflags
   * vfr      in(16)             lzc exp_nor   sig_nor  clz out_exp adder | Table                   |
   *
   */
  val (fire, src, opType, rm, inSew1H, outSew1H) = (
    io.fire, io.src, io.opType, io.rm, io.inSew1H, io.outSew1H
  )
  val fireS1 = GatedValidRegNext(fire)
  val cvt16ModuleS0 = Module(new CVT16ModuleS0(width))
  cvt16ModuleS0.io.s0In.src := src
  cvt16ModuleS0.io.s0In.opType := opType
  cvt16ModuleS0.io.s0In.rm := rm
  cvt16ModuleS0.io.s0In.inSew1H := inSew1H
  cvt16ModuleS0.io.s0In.outSew1H := outSew1H
  val cvt16ModuleS1 = Module(new CVT16ModuleS1(width))
  CommonConnect(cvt16ModuleS1.io.s1In, cvt16ModuleS0.io.s0Out, fire)
  val cvt16ModuleS2 = Module(new CVT16ModuleS2(width))
  CommonConnect(cvt16ModuleS2.io.s2In, cvt16ModuleS1.io.s1Out, fireS1)
  io.out.ex1.res := cvt16ModuleS1.io.s1Out.result
  io.out.ex1.fflags := cvt16ModuleS1.io.s1Out.fflags
  io.out.ex2.res := cvt16ModuleS2.io.s2Out.result
  io.out.ex2.fflags := cvt16ModuleS2.io.s2Out.fflags
}

class CVT16BundleInputS0(width: Int) extends Bundle {
  val src      = UInt(width.W)
  val opType   = FCvtOpcode()
  val inSew1H  = Sew()
  val outSew1H = Sew()
  val rm       = Frm()
}

class CVT16BundleOutputS0(width: Int) extends Bundle {
  val exp            = UInt(f16.expAdderWidth.W)
  val expPlus1Enable = Bool()
  val shiftLeft      = UInt(width.W)
  val fracSrcLeft    = UInt(width.W)
  val inRounder      = UInt((width + 1).W)
  val sticky         = Bool()
  val special        = new Special
  val isFp2Int       = Bool()
  val isInt2Fp       = Bool()
  val isEstimate7    = Bool()
  val isRec          = Bool()
  val outSew1H       = Sew()
  val signSrc        = Bool()
  val rm             = Frm()
  val hasSignInt     = Bool()
  // int2fp
  val intSignSrc     = Bool()
  val isZeroIntSrc   = Bool()
  val intExp         = UInt(f16.expAdderWidth.W)
  val absIntSrc      = UInt(width.W)
  val intLeadZeros   = UInt(log2Up(width).W)
  val intShiftLeft   = UInt(width.W)
  // est
  val isSubnormalRec0 = Bool()
  val isSubnormalRec1 = Bool()
  val isSubnormalRec2 = Bool()
  val isNormalRec0    = Bool()
  val isNormalRec1    = Bool()
  val isNormalRec2    = Bool()
  val estExp          = UInt(f16.expAdderWidth.W)
  val estExpNormaled0 = Bool()

}

class CVT16BundleS0(width: Int) extends Bundle {
  val s0In = Input(new CVT16BundleInputS0(width))
  val s0Out = Output(new CVT16BundleOutputS0(width))
}

class CVT16ModuleS0(width: Int = 16) extends Module {
  val io = IO(new CVT16BundleS0(width))
  val s0In = io.s0In
  val s0Out = io.s0Out
  val (src, opType, rm, inSew1H, outSew1H) = (s0In.src, s0In.opType, s0In.rm, s0In.inSew1H, s0In.outSew1H)

  val outIsFp = FCvtOpcode.outIsFp(opType)
  val isEstimate7 = FCvtOpcode.isEstimate7(opType)
  val isRec = FCvtOpcode.isRec(opType)
  val hasSignInt = FCvtOpcode.isSignInt(opType)

  val fpSignSrc = src(f16.width - 1)
  val expSrc    = src(f16.width - 2, f16.fracWidth)
  val fracSrc   = src(f16.fracWidth - 1, 0)

  val (expNotZeroSrc, expIsOnes, fracNotZero) = (expSrc.orR, expSrc.andR, fracSrc.orR)
  val isnormalSrc     = !expIsOnes && expNotZeroSrc
  val isSubnormalSrc  = !expNotZeroSrc && fracNotZero
  val isNaNSrc        = expIsOnes && fracNotZero
  val isZeroSrc       = !expNotZeroSrc && !fracNotZero
  val isInfSrc        = expIsOnes && !fracNotZero
  val isSNaNSrc       = isNaNSrc && !fracSrc.head(1)
  val isQNaNSrc       = isNaNSrc && fracSrc.head(1).asBool
  val isSubnormalRec2 = isSubnormalSrc && !fracSrc.head(2).orR

  val isFp2Int   = FCvtOpcode.isF2I(opType)
  val isInt2Fp   = FCvtOpcode.isI2F(opType)

  // exp
  val expAdderIn0 = Wire(UInt(f16.expAdderWidth.W))
  val expAdderIn1 = Wire(UInt(f16.expAdderWidth.W))
  val exp = Wire(UInt(f16.expAdderWidth.W))

  val leadZeros = Lzc((fracSrc << (f16.width - f16.fracWidth)).asUInt).data
  val bias = f16.bias.U
  val minusExp = extend((~(false.B ## bias)), f16.expAdderWidth).asUInt
  expAdderIn0 := Mux(isSubnormalSrc, false.B ## 1.U, false.B ## expSrc)
  val expPlus1Enable = isFp2Int
  expAdderIn1 := Mux(expPlus1Enable, minusExp, expSrc)
  exp := expAdderIn0 + expAdderIn1
  s0Out.exp := exp
  s0Out.expPlus1Enable := expPlus1Enable

  // frac
  val fracSrcLeft = Wire(UInt(width.W))
  fracSrcLeft := fracSrc << (f16.width - f16.fracWidth)
  val shiftLeft = (fracSrcLeft.asUInt << 1) << leadZeros
  s0Out.fracSrcLeft := fracSrcLeft
  s0Out.shiftLeft := shiftLeft

  val fracImplict1Src = (expNotZeroSrc && !expIsOnes) ## fracSrc
  val shamtIn = fracImplict1Src ## 0.U((f16.width - 1 - f16.fracWidth).W) ## false.B
  val shamtWidth = (f16.width - 1 + f16.bias).U + (~expSrc).asUInt
  val shamtWidthPlus1 = shamtWidth + 1.U
  val shamt = Mux(shamtWidth.andR, 0.U, Mux(shamtWidth(4), (f16.width+1).U, shamtWidthPlus1))

  val (inRounder, sticky) = ShiftRightJam(shamtIn, shamt)
  s0Out.inRounder := inRounder
  s0Out.sticky := sticky

  s0Out.special.expNotZero := expNotZeroSrc
  s0Out.special.expIsOnes := expIsOnes
  s0Out.special.fracNotZero := fracNotZero
  s0Out.special.isInf := isInfSrc
  s0Out.special.isZero := isZeroSrc
  s0Out.special.isSubnormal := isSubnormalSrc
  s0Out.special.isnormal := isnormalSrc
  s0Out.special.isNaN := isNaNSrc
  s0Out.special.isSNaN := isSNaNSrc
  s0Out.isFp2Int := isFp2Int
  s0Out.isInt2Fp := isInt2Fp
  s0Out.outSew1H := outSew1H
  s0Out.signSrc := fpSignSrc
  s0Out.rm := rm
  s0Out.hasSignInt := hasSignInt

  // int2fp
  val intIn = Mux1H(Seq(
    (inSew1H === SewOH.e8)  -> int16Extend(src(7, 0),  hasSignInt && src(7)).asUInt,
    (inSew1H === SewOH.e16) -> int16Extend(src(15, 0), hasSignInt && src(15)).asUInt,
  ))
  val intSignSrc = intIn(f16.width)
  val intDataSrc = intIn(f16.width - 1, 0)
  val absIntSrc = Mux(intSignSrc, (~intDataSrc).asUInt + 1.U, intDataSrc)
  val isZeroIntSrc = !absIntSrc.orR
  // clz
  val intLeadZeros = Lzc(absIntSrc).data
  // exp
  val intExpAdderIn0 = Wire(UInt(f16.expAdderWidth.W))
  val intExpAdderIn1 = Wire(UInt(f16.expAdderWidth.W))
  val intMinuxExp = extend((~(false.B ## intLeadZeros)).asUInt, f16.expAdderWidth).asUInt
  intExpAdderIn0 := (f16.bias + f16.width - 1).U
  intExpAdderIn1 := intMinuxExp
  val intExp = Wire(UInt(f16.expAdderWidth.W))
  intExp := intExpAdderIn0 + intExpAdderIn1

  s0Out.intSignSrc := intSignSrc
  s0Out.isZeroIntSrc := isZeroIntSrc
  s0Out.intExp := intExp
  s0Out.absIntSrc := absIntSrc
  s0Out.intLeadZeros := intLeadZeros
  s0Out.intShiftLeft := (absIntSrc << 1) << intLeadZeros

  // est
  val isNormalRec0 = expSrc(f16.expWidth - 1, 1).andR && !expSrc(0)
  val isNormalRec1 = expSrc(f16.expWidth - 1, 2).andR && !expSrc(1) && expSrc(0)
  val isNormalRec2 = expNotZeroSrc && !expIsOnes && !isNormalRec0 && !isNormalRec1
  val isSubnormalRec0 = isSubnormalSrc && fracSrc(f16.fracWidth - 1)
  val isSubnormalRec1 = isSubnormalSrc && !fracSrc(f16.fracWidth - 1) && fracSrc(f16.fracWidth - 2)

  val estExpAdderIn0 = Wire(UInt(f16.expAdderWidth.W))
  val estExpAdderIn1 = Wire(UInt(f16.expAdderWidth.W))
  val estMinusExp = extend((~(false.B ## expSrc)).asUInt, f16.expAdderWidth).asUInt
  estExpAdderIn0 := Mux(isRec, (2 * f16.bias - 1).U, (3 * f16.bias - 1).U)
  estExpAdderIn1 := Mux(isSubnormalSrc, leadZeros, estMinusExp)
  val estExp = Wire(UInt(f16.expAdderWidth.W))
  estExp := estExpAdderIn0 + estExpAdderIn1

  val estExpNormaled = Mux(isSubnormalSrc, leadZeros(0), expSrc(0)) // only the last bit is needed

  s0Out.isEstimate7 := isEstimate7
  s0Out.isRec := isRec
  s0Out.special.isQNaN := isQNaNSrc
  s0Out.isNormalRec0 := isNormalRec0
  s0Out.isNormalRec1 := isNormalRec1
  s0Out.isNormalRec2 := isNormalRec2
  s0Out.isSubnormalRec0 := isSubnormalRec0
  s0Out.isSubnormalRec1 := isSubnormalRec1
  s0Out.isSubnormalRec2 := isSubnormalRec2
  s0Out.estExp := estExp
  s0Out.estExpNormaled0 := estExpNormaled
}

class CVT16BundleInputS1(width: Int) extends CVT16BundleOutputS0(width)

class CVT16BundleOutputS1(width: Int = 16) extends Bundle {
  val result = UInt(width.W)
  val fflags = Fflags()
}

class CVT16BundleS1(width: Int) extends Bundle {
  val s1In = Input(new CVT16BundleInputS1(width))
  val s1Out = Output(new CVT16BundleOutputS1(width))
}

class CVT16ModuleS1(width: Int = 16) extends Module {
  val io = IO(new CVT16BundleS1(width))
  val s1In = io.s1In
  val s1Out = io.s1Out
  val special = io.s1In.special
  val expIsOnes =  special.expIsOnes
  val fracNotZero = special.fracNotZero
  val isZero = special.isZero
  val isInf = special.isInf
  val isnormal = special.isnormal
  val isSubnormal = special.isSubnormal
  val isSNaN = special.isSNaN
  val isQNaN = special.isQNaN
  val isNaN = special.isNaN
  val expInS0 = s1In.exp
  val expPlus1Enable = s1In.expPlus1Enable
  val isFp2Int  = s1In.isFp2Int
  val isInt2Fp = s1In.isInt2Fp
  val isEstimate7 = s1In.isEstimate7
  val isRec = s1In.isRec
  val outSew1H = s1In.outSew1H
  val shiftLeft = s1In.shiftLeft
  val fracSrcLeft = s1In.fracSrcLeft
  val signSrc = s1In.signSrc
  val rm = s1In.rm
  val hasSignInt = s1In.hasSignInt

  val intSignSrc = s1In.intSignSrc
  val isZeroIntSrc = s1In.isZeroIntSrc
  val intExpInS0 = s1In.intExp
  val intShiftLeft = s1In.intShiftLeft

  val estExpInS0 = s1In.estExp
  val expNormaled0 = s1In.estExpNormaled0
  val isNormalRec0 = s1In.isNormalRec0
  val isNormalRec1 = s1In.isNormalRec1
  val isNormalRec2 = s1In.isNormalRec2
  val isSubnormalRec0 = s1In.isSubnormalRec0
  val isSubnormalRec1 = s1In.isSubnormalRec1
  val isSubnormalRec2 = s1In.isSubnormalRec2

  // output
  val nv, dz, of, uf, nx = Wire(Bool())
  val result = Wire(UInt(width.W))
  val fflags = Wire(Fflags())

  val float1HOut = outSew1H(1)

  val fracNormaled = Wire(UInt(width.W))
  fracNormaled := Mux(isSubnormal, shiftLeft, fracSrcLeft)

  val inRounder = s1In.inRounder
  val sticky = s1In.sticky

  val rounderInput = inRounder.head(f16.width)
  val rounderRoundIn = inRounder(0)
  val rounderStickyIn = sticky

  val rounder = Module(new RoundingUnit(width))
  rounder.io.in := rounderInput
  rounder.io.roundIn := rounderRoundIn
  rounder.io.stickyIn := rounderStickyIn
  rounder.io.signIn := signSrc
  rounder.io.rm := rm

  val nxRounded = rounder.io.inexact
  val upRounded = rounder.io.r_up
  val exp = expInS0 + Mux(expPlus1Enable, 1.U, 0.U)
  val expIncrease = expInS0 + Mux(expPlus1Enable, 2.U, 1.U)
  val rounderInputIncrease = rounderInput + 1.U

  // for fp2int
  // 2bit => u16, i16
  val hasSignInt1HOut = Seq(!hasSignInt, hasSignInt)
  val isOnesRounderInputMapFp2Int = Seq(rounderInput.andR, rounderInput.tail(1).andR)//Remove the sign bit
  val cout = upRounded && Mux1H(hasSignInt1HOut, isOnesRounderInputMapFp2Int).asBool
  val expRounded = Wire(UInt(f16.expWidth.W))
  expRounded := Mux(cout, expIncrease, exp)
  val fracRounded = Mux(upRounded, rounderInputIncrease, rounderInput)
  val rmin = rm === RTZ || (signSrc && rm === RUP) || (!signSrc && rm === RDN)
  val signNonNan = signSrc && !isNaN

  // for int2fp
  val intRounderInput = intShiftLeft.head(f16.fracWidth)
  val intRoundIn      = intShiftLeft.tail(f16.fracWidth).head(1)
  val intStickyIn     = intShiftLeft.tail(f16.fracWidth + 1).orR
  val intRounder = Module(new RoundingUnit(width))
  intRounder.io.in       := intRounderInput
  intRounder.io.roundIn  := intRoundIn
  intRounder.io.stickyIn := intStickyIn
  intRounder.io.signIn   := intSignSrc
  intRounder.io.rm       := rm
  val intExp = intExpInS0 + 1.U
  val intExpIncrease = intExpInS0 + 2.U
  val intRounderInputIncrease = intRounderInput + 1.U

  val intNxRounded = intRounder.io.inexact
  val intUpRounded = intRounder.io.r_up
  val intCout = intUpRounded && intShiftLeft.head(f16.fracWidth).andR
  val intExpRounded = Wire(UInt(f16.expWidth.W))
  intExpRounded := Mux(intCout, intExpIncrease, intExp)
  val intFracRounded = Mux(intUpRounded, intRounderInputIncrease, intRounderInput)
  val intRmin =
    rm === RTZ || (intSignSrc && rm === RUP) || (!intSignSrc && rm === RDN)

  /**
   * int->fp   any int/uint -> any fp
    */
  // Mux(cout, exp > FP.maxExp - 1, exp > FP.maxExp)
  val intOfRounded = !intExp.head(1).asBool && Mux(intCout,
    intExp(f16.expWidth - 1, 1).andR || intExp(intExp.getWidth - 2, f16.expWidth).orR,
    intExp(f16.expWidth - 1, 0).andR || intExp(intExp.getWidth - 2, f16.expWidth).orR
  )

  // for est
  val estExpPlus1 = estExpInS0 + 1.U
  val estExp = Mux(isSubnormal, estExpInS0, estExpPlus1)
  val rsqrt7Table = Module(new Rsqrt7Table)
  rsqrt7Table.src := expNormaled0 ## fracNormaled.head(6)
  val rec7Table = Module(new Rec7Table)
  rec7Table.src := fracNormaled.head(7)
  val fracEstimate = Mux(isRec, rec7Table.out, rsqrt7Table.out)

  when (isInt2Fp) {
    nv := false.B
    dz := false.B
    of := intOfRounded
    uf := false.B
    nx := intOfRounded || intNxRounded

    val result1H = Seq(
      intOfRounded && intRmin,
      intOfRounded && !intRmin,
      isZeroIntSrc,
      !intOfRounded && !isZeroIntSrc
    )

    def int2FpResultGen(fp: FloatFormat): Seq[UInt] = {
      VecInit((0 to 3).map {
        case 0 => intSignSrc ## fp.maxExp.U(fp.expWidth.W) ## ~0.U(fp.fracWidth.W) // GNF
        case 1 => intSignSrc ## ~0.U(fp.expWidth.W) ## 0.U(fp.fracWidth.W) // INF
        case 2 => intSignSrc ## 0.U((fp.width - 1).W) // 0
        case 3 => intSignSrc ## intExpRounded(fp.expWidth - 1, 0) ## intFracRounded(fp.fracWidth - 1, 0) // normal
      })
    }

    val int2FpResult = Mux1H(result1H, int2FpResultGen(f16))
    result := int2FpResult
  }.elsewhen(isEstimate7) {
    nv := Mux(isRec, isSNaN, (signSrc && !isZero && !isQNaN) | isSNaN)
    dz := isZero
    of := isRec && isSubnormalRec2
    uf := false.B
    nx := of

    val recResult1H = Seq(
      isNaN,
      isInf,
      isZero || isSubnormalRec2 && !rmin,
      isNormalRec0,
      isNormalRec1,
      isNormalRec2 || isSubnormalRec0 || isSubnormalRec1,
      isSubnormalRec2 && rmin
    )
    def recResultMapGen(fp: FloatFormat): Seq[UInt] = {
      VecInit((0 to 6).map {
        case 0 => false.B ## ~0.U(fp.expWidth.W) ## true.B ## 0.U((fp.fracWidth - 1).W) // can
        case 1 => signSrc ## 0.U((fp.width - 1).W) // 0
        case 2 => signSrc ## ~0.U(fp.expWidth.W) ## 0.U(fp.fracWidth.W) // INF
        case 3 => signSrc ## 0.U(fp.expWidth.W) ## 1.U(2.W) ## fracEstimate ## 0.U((fp.fracWidth - 2 - 7).W)
        case 4 => signSrc ## 0.U(fp.expWidth.W) ## 1.U(1.W) ## fracEstimate ## 0.U((fp.fracWidth - 1 - 7).W)
        case 5 => signSrc ## estExp(fp.expWidth - 1, 0) ## fracEstimate ## 0.U((fp.fracWidth - 7).W)
        case 6 => signSrc ## fp.maxExp.U(fp.expWidth.W) ## ~0.U(fp.fracWidth.W) // GNF
      })
    }
    val recReult = Mux1H(recResult1H, recResultMapGen(f16))

    val sqrtResult1H = Seq(
      signSrc & !isZero | isNaN,
      isZero,
      !signSrc & !isZero & !expIsOnes,
      !signSrc & isInf
    )
    def sqrtResultMapGen(fp: FloatFormat): Seq[UInt] = {
      VecInit((0 to 3).map {
        case 0 => false.B ## ~0.U(fp.expWidth.W) ## true.B ## 0.U((fp.fracWidth - 1).W)
        case 1 => signSrc ## ~0.U(fp.expWidth.W) ## 0.U(fp.fracWidth.W)
        case 2 => signSrc ## estExp(fp.expWidth, 1) ## fracEstimate ## 0.U((fp.fracWidth - 7).W) // exp/2 => >>1
        case 3 => 0.U(fp.width.W)
      })
    }
    val sqrtResult = Mux1H(sqrtResult1H, sqrtResultMapGen(f16))

    result := Mux(isRec, recReult, sqrtResult)
  }.otherwise {
    /**
     * out is int, any fp->any int/uint
     * drop the shift left!
     */
    val resultRounded = fracRounded
    val isNotZeroRounded = resultRounded.orR
    val isZeroRounded = !isNotZeroRounded
    val normalResult = Mux(signSrc && isNotZeroRounded, (~resultRounded).asUInt + 1.U, resultRounded) // exclude 0
    val ofExpRounded = !exp.head(1) && Mux1H(UIntToOH(hasSignInt ## cout), VecInit(Seq(
      exp(exp.getWidth - 2, log2Ceil(width)).orR,                                          // >= width   unsign & non cout
      exp(exp.getWidth - 2, log2Ceil(width)).orR || exp(log2Ceil(width) - 1, 0).andR,      // >= width-1 unsign & cout
      exp(exp.getWidth - 2, log2Ceil(width)).orR || exp(log2Ceil(width) - 1, 0).andR,      // >= width-1 sign   & non cout
      exp(exp.getWidth - 2, log2Ceil(width)).orR || exp(log2Ceil(width) - 1, 1).andR       // >= width-2 sign   & cout
    )))
    val excludeFrac = resultRounded(width - 1) && !resultRounded(width - 2, 0).orR // 1000***000
    val excludeExp = !exp.head(exp.getWidth - log2Ceil(width)).orR && Mux(
      cout,
      exp(log2Ceil(width) - 1, 1).andR && !exp(0), // == width-2
      exp(log2Ceil(width) - 1, 0).andR             // == width-1
    )
    val toUnv = ofExpRounded || expIsOnes ||
      signSrc && !(isZero || isZeroRounded && !ofExpRounded) // exclude 0 & -0 after rounding
    val toUnx = !toUnv && nxRounded
    val toInv = ofExpRounded && !(signSrc && excludeExp && excludeFrac) || expIsOnes // nv has included inf & nan
    val toInx = !toInv && nxRounded

    nv := Mux(hasSignInt, toInv, toUnv)
    dz := false.B
    of := false.B
    uf := false.B
    nx := Mux(hasSignInt, toInx, toUnx)

    val result1H = Seq(
      (!hasSignInt && !toUnv) || (hasSignInt && !toInv), // toUnv include nan & inf
      !hasSignInt && toUnv && (isNaN || !signSrc && (isInf || ofExpRounded)),
      !hasSignInt && toUnv && signSrc && !isNaN,
      hasSignInt && toInv,
    )
    result := Mux1H(result1H, Seq(
      normalResult,
      (~0.U(width.W)).asUInt,
      0.U(width.W),
      signNonNan ## Fill(width - 1, !signNonNan)
    ))
  }
  fflags := Cat(nv, dz, of, uf, nx)
  s1Out.result := result
  s1Out.fflags := fflags
}

class CVT16BundleS2(width: Int = 16) extends Bundle {
  val s2In = Input(new CVT16BundleOutputS1(width))
  val s2Out = Output(new CVT16BundleOutputS1(width))
}

class CVT16ModuleS2(width: Int = 16) extends Module {
  val io = IO(new CVT16BundleS2(width))
  io.s2Out := io.s2In
}