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


class CVT32(width: Int = 32) extends Module {
  /** cycle0                                     |         cycle1                  |   cycle2
   * fp2int/fp2fp   in(32)   lzc    left         |         RoundingUnit(32)  adder |
   *                          ..    adder        |                                 |
   *                  adder  ShiftRightJam(33)   |                                 |
   * int2fp   in(32) in_abs(32)  lzc  adder      | left    RoundingUnit(32)  adder |  -> result & fflags
   * vfr      in(32)             lzc  adder      | Table                           |
   */
  val io = IO(new Bundle {
    val in = Input(new VCVT.In(width))
    val out = Output(new VCVT.Out(width))
  })
  val (fire, src, opType, rm, inSew1H, outSew1H) = (
    io.in.fire, io.in.data.src, io.in.ctrl.opType, io.in.ctrl.rm, io.in.ctrl.inSew1H, io.in.ctrl.outSew1H
  )
  val fireS1 = GatedValidRegNext(fire)
  val cvt32ModuleS0 = Module(new CVT32ModuleS0(width))
  cvt32ModuleS0.io.s0In.src := src
  cvt32ModuleS0.io.s0In.opType := opType
  cvt32ModuleS0.io.s0In.rm := rm
  cvt32ModuleS0.io.s0In.inSew1H := inSew1H
  cvt32ModuleS0.io.s0In.outSew1H := outSew1H
  val cvt32ModuleS1 = Module(new CVT32ModuleS1(width))
  CommonConnect(cvt32ModuleS1.io.s1In, cvt32ModuleS0.io.s0Out, fire)
  val cvt32ModuleS2 = Module(new CVT32ModuleS2(width))
  CommonConnect(cvt32ModuleS2.io.s2In, cvt32ModuleS1.io.s1Out, fireS1)
  io.out.ex1.res := cvt32ModuleS1.io.s1Out.result
  io.out.ex1.fflags := cvt32ModuleS1.io.s1Out.fflags
  io.out.ex2.res := cvt32ModuleS2.io.s2Out.result
  io.out.ex2.fflags := cvt32ModuleS2.io.s2Out.fflags
}

class CVT32BundleInputS0(width: Int) extends Bundle {
  val src      = UInt(width.W)
  val opType   = FCvtOpcode()
  val inSew1H  = Sew()
  val outSew1H = Sew()
  val rm       = Frm()
}

class CVT32BundleOutputS0(width: Int) extends Bundle {
  val exp            = UInt(f32.expAdderWidth.W)
  val expPlus1Enable = Bool()
  val shiftLeft      = UInt(width.W)
  val fracSrcLeft    = UInt(width.W)
  val inRounder      = UInt((width + 1).W)
  val sticky         = Bool()
  val special        = new Special
  val isFpWiden      = Bool()
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
  val intExp         = UInt(f32.expAdderWidth.W)
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
  val estExp          = UInt(f32.expAdderWidth.W)
  val estExpNormaled0 = Bool()

}

class CVT32BundleS0(width: Int) extends Bundle {
  val s0In = Input(new CVT32BundleInputS0(width))
  val s0Out = Output(new CVT32BundleOutputS0(width))
}

class CVT32ModuleS0(width: Int = 32) extends Module {
  val io = IO(new CVT32BundleS0(width))
  val s0In = io.s0In
  val s0Out = io.s0Out
  val (src, opType, rm, inSew1H, outSew1H) = (s0In.src, s0In.opType, s0In.rm, s0In.inSew1H, s0In.outSew1H)

  val isEstimate7 = FCvtOpcode.isEstimate7(opType)
  val isRec = FCvtOpcode.isRec(opType)
  val hasSignInt = FCvtOpcode.isSignInt(opType)
  val float1HSrc = inSew1H(2, 1) // exclude f8, f64
  val float1HOut = outSew1H(2, 1) // exclude f8, f64

  val srcMap = (0 to 2).map(i => src((1 << i) * 8 - 1, 0))
  val floatMap = srcMap.zipWithIndex.map{ case (float ,i) => float32Extend(float, i)}.drop(1)
  val expSrcMap = floatMap.map(_.tail(1).head(f32.expWidth))
  val fracSrcMap = floatMap.map(_.tail(1 + f32.expWidth).head(f32.fracWidth))
  val fpSignSrcMap = floatMap.map(_.head(1).asBool)
  val fpSignSrc = Mux1H(float1HSrc, fpSignSrcMap)
  val expSrc = Mux1H(float1HSrc, expSrcMap)
  val fracSrc = Mux1H(float1HSrc, fracSrcMap)
  val decodeFloatSrcMap = fpParam.fp16AndFp32Formats.zip(expSrcMap).zip(fracSrcMap).map { case ((fp, exp), frac) =>
    VecInit(exp(fp.expWidth-1,0).orR, exp(fp.expWidth-1,0).andR, frac.head(fp.fracWidth).orR).asUInt
  }
  val decodeFloatSrc = Mux1H(float1HSrc, decodeFloatSrcMap)
  val (expNotZeroSrc, expIsOnes, fracNotZero) = (decodeFloatSrc(0), decodeFloatSrc(1), decodeFloatSrc(2))
  val isnormalSrc = !expIsOnes && expNotZeroSrc
  val isSubnormalSrc = !expNotZeroSrc && fracNotZero
  val isNaNSrc = expIsOnes && fracNotZero
  val isZeroSrc = !expNotZeroSrc && !fracNotZero
  val isInfSrc = expIsOnes && !fracNotZero
  val isSNaNSrc = isNaNSrc && !fracSrc.head(1)
  val isQNaNSrc = isNaNSrc && fracSrc.head(1).asBool
  val isSubnormalRec2 = isSubnormalSrc && !fracSrc.head(2).orR

  val fpInputWidth    = FCvtOpcode.getInputDataWidth(opType)
  val fpOutputWidth   = FCvtOpcode.getOutputDataWidth(opType)
  val isFpWiden       = FCvtOpcode.isF2F(opType) && fpOutputWidth > fpInputWidth
  val isFp2Int        = FCvtOpcode.isF2I(opType)
  val isInt2Fp        = FCvtOpcode.isI2F(opType)

  // exp
  val expAdderIn0 = Wire(UInt(f32.expAdderWidth.W))
  val expAdderIn1 = Wire(UInt(f32.expAdderWidth.W))
  val exp = Wire(UInt(f32.expAdderWidth.W))

  val leadZerosMap = fracSrcMap.map(frac => Lzc((frac << (32 - f32.fracWidth)).asUInt).data)
  val leadZeros = Mux1H(float1HSrc, leadZerosMap)
  val biasDelta = (f32.bias - f16.bias).U
  val bias = Mux1H(float1HSrc, fpParam.fp16AndFp32Formats.map(fp => fp.bias.U))
  val minusExp = extend((~(false.B ## Mux1H(
    Seq(
      isFpWiden -> leadZeros,
      isFp2Int -> bias
    )))), f32.expAdderWidth).asUInt
  expAdderIn0 := Mux1H(Seq(
    isFpWiden -> biasDelta,
    isFp2Int -> Mux(isSubnormalSrc, false.B ## 1.U, false.B ## expSrc)
  ))
  val expPlus1Enable = isFp2Int || (isFpWiden && isSubnormalSrc)
  expAdderIn1 := Mux(expPlus1Enable, minusExp, expSrc)
  exp := expAdderIn0 + expAdderIn1
  s0Out.exp := exp
  s0Out.expPlus1Enable := expPlus1Enable

  // frac
  val fracSrcLeftMap = fracSrcMap.map(frac => frac << (32 - f32.fracWidth))
  val fracSrcLeft = Mux1H(float1HSrc, fracSrcLeftMap)
  val shiftLeftMap = fracSrcLeftMap.zip(leadZerosMap).map { case (fracLeft, leadZeros) =>
    (fracLeft.asUInt << 1) << leadZeros
  }
  val shiftLeft = Mux1H(float1HSrc, shiftLeftMap)
  s0Out.fracSrcLeft := fracSrcLeft
  s0Out.shiftLeft := shiftLeft

  val shamtInMap = decodeFloatSrcMap.zip(fracSrcMap).map { case (decode, frac) =>
    val expNotZero = decode(0)
    val expIsOnes = decode(1)
    val fracImplict1 = (expNotZero && !expIsOnes) ## frac
    fracImplict1 ## 0.U(8.W) ## false.B
  }
  val shamtMap = fpParam.fp16AndFp32Formats.zip(expSrcMap).map { case (fp, exp) =>
    val shamtWidth = (31 + fp.bias).U + (~exp).asUInt
    val shamtWidthPlus1 = shamtWidth + 1.U
    Mux(shamtWidth.andR, 0.U, Mux(shamtWidth(7,5).orR, 33.U, shamtWidthPlus1))
  }
  val inRounderStickyMap = shamtInMap.zip(shamtMap).map { case (shamtIn, shamt) =>
    ShiftRightJam(shamtIn, shamt)
  }
  val inRounder = Mux1H(float1HSrc, inRounderStickyMap.map(_._1))
  val sticky = Mux1H(float1HSrc, inRounderStickyMap.map(_._2))
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
  s0Out.isFpWiden := isFpWiden
  s0Out.isFp2Int := isFp2Int
  s0Out.isInt2Fp := isInt2Fp
  s0Out.outSew1H := outSew1H
  s0Out.signSrc := fpSignSrc
  s0Out.rm := rm
  s0Out.hasSignInt := hasSignInt

  // int2fp
  val int1HSrc = inSew1H(2, 0)
  val intWidthMap = Seq(8, 16, 32)
  val intSignSrcMap = srcMap.map(int => hasSignInt && int.head(1).asBool)
  val absIntSrcRawMap = srcMap.zip(intSignSrcMap).map { case (int, sign) =>
    val neg = (~int).asUInt + 1.U
    Mux(sign, neg(int.getWidth - 1, 0), int)
  }
  val absIntSrcMap = absIntSrcRawMap.zip(intWidthMap).map { case (absInt, intWidth) =>
    if (intWidth == width) absInt else 0.U((width - intWidth).W) ## absInt
  }
  val intLeadZerosMap = absIntSrcRawMap.zip(intWidthMap).map { case (absInt, intWidth) =>
    if (intWidth == width) {
      Lzc(absInt).data
    } else {
      ((width - intWidth).U(log2Up(width).W) + Lzc(absInt).data)(log2Up(width) - 1, 0)
    }
  }
  val intShiftLeftMap = absIntSrcMap.zip(intLeadZerosMap).map { case (absInt, leadZeros) =>
    (absInt << 1) << leadZeros
  }
  val intExpBase = Mux1H(float1HOut, fpParam.fp16AndFp32Formats.map(fp => (fp.bias + 31).U(f32.expAdderWidth.W)))
  val intExpMap = intLeadZerosMap.map { leadZeros =>
    intExpBase + extend((~(false.B ## leadZeros)).asUInt, f32.expAdderWidth).asUInt
  }
  val intSignSrc = Mux1H(int1HSrc, intSignSrcMap)
  val absIntSrc = Mux1H(int1HSrc, absIntSrcMap)
  val isZeroIntSrc = Mux1H(int1HSrc, absIntSrcRawMap.map(absInt => !absInt.orR))
  val intLeadZeros = Mux1H(int1HSrc, intLeadZerosMap)
  val intExp = Mux1H(int1HSrc, intExpMap)
  val intShiftLeft = Mux1H(int1HSrc, intShiftLeftMap)

  s0Out.intSignSrc := intSignSrc
  s0Out.isZeroIntSrc := isZeroIntSrc
  s0Out.intExp := intExp
  s0Out.absIntSrc := absIntSrc
  s0Out.intLeadZeros := intLeadZeros
  s0Out.intShiftLeft := intShiftLeft

  // est
  val decodeFloatSrcRec = Mux1H(float1HSrc,
    expSrcMap.zip(fpParam.fp16AndFp32Formats.map(fp => fp.expWidth)).map { case (exp, expWidth) =>
      VecInit(
        exp.head(expWidth-1).andR && !exp(0),
        exp.head(expWidth-2).andR && !exp(1) && exp(0)
      ).asUInt
    }
  )
  val (isNormalRec0, isNormalRec1) = (decodeFloatSrcRec(0), decodeFloatSrcRec(1))
  val isNormalRec2 = expNotZeroSrc && !expIsOnes && !isNormalRec0 && !isNormalRec1
  val isSubnormalRec0 = isSubnormalSrc && fracSrc.head(1).asBool
  val isSubnormalRec1 = isSubnormalSrc && !fracSrc.head(1) && fracSrc.tail(1).head(1).asBool

  val estExpAdderIn0 = Wire(UInt(f32.expAdderWidth.W))
  val estExpAdderIn1 = Wire(UInt(f32.expAdderWidth.W))
  val estMinusExp = extend((~(false.B ## expSrc)).asUInt, f32.expAdderWidth).asUInt
  estExpAdderIn0 := Mux1H(float1HOut, fpParam.fp16AndFp32Formats.map(fp => Mux(isRec, (2 * fp.bias - 1).U, (3 * fp.bias - 1).U)))
  estExpAdderIn1 := Mux(isSubnormalSrc, leadZeros, estMinusExp)
  val estExp = Wire(UInt(f32.expAdderWidth.W))
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

class CVT32BundleInputS1(width: Int) extends CVT32BundleOutputS0(width)

class CVT32BundleOutputS1(width: Int = 32) extends Bundle {
  val result = UInt(width.W)
  val fflags = Fflags()
}

class CVT32BundleS1(width: Int) extends Bundle {
  val s1In = Input(new CVT32BundleInputS1(width))
  val s1Out = Output(new CVT32BundleOutputS1(width))
}

class CVT32ModuleS1(width: Int = 32) extends Module {
  val io = IO(new CVT32BundleS1(width))
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
  val isFpWiden = s1In.isFpWiden
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
  val fflags = Wire(UInt(5.W))

  val intParamMap = (0 to 2).map(i => (1 << i) * 8)

  val float1HOut = outSew1H(2, 1)

  val fracNormaled = Wire(UInt(width.W))
  fracNormaled := Mux(isSubnormal, shiftLeft, fracSrcLeft)

  val inRounder = s1In.inRounder
  val sticky = s1In.sticky

  val rounderInput = inRounder.head(32)
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
  // 6bit => u32, i32, u16, i16, u8, i8
  val int1HOut = outSew1H(2, 0)
  val hasSignInt1HOut = int1HOut.asBools.map(oh => Seq(oh && !hasSignInt, oh && hasSignInt)).flatten
  val isOnesRounderInputMapFp2Int =
    intParamMap.map(intType => Seq(intType, intType - 1)).flatten.map(intType => rounderInput.tail(32 - intType).andR)
  val cout = upRounded && Mux1H(hasSignInt1HOut, isOnesRounderInputMapFp2Int).asBool
  val expRounded = Wire(UInt(f32.expWidth.W))
  expRounded := Mux(cout, expIncrease, exp)
  val fracRounded = Mux(upRounded, rounderInputIncrease, rounderInput)
  val rmin = rm === RTZ || (signSrc && rm === RUP) || (!signSrc && rm === RDN)
  val signNonNan = signSrc && !isNaN

  // for int2fp
  val roundMapIn = Wire(UInt(width.W))
  roundMapIn := intShiftLeft
  val intRounderMap = fpParam.fp16AndFp32Formats.map(fp => Seq(
    roundMapIn.head(fp.fracWidth),
    roundMapIn.tail(fp.fracWidth).head(1),
    roundMapIn.tail(fp.fracWidth + 1).orR,
    roundMapIn.head(fp.fracWidth).andR
  )).transpose

  val (intRounderInputMap, intRounderInMap, intRounderStickyMap, intIsOnesRounderInputMap) = {
    (intRounderMap(0), intRounderMap(1), intRounderMap(2), intRounderMap(3))
  }
  val intRounderInput = Mux1H(float1HOut, intRounderInputMap)
  val intRounder = Module(new RoundingUnit(width))
  intRounder.io.in := intRounderInput
  intRounder.io.roundIn := Mux1H(float1HOut, intRounderInMap)
  intRounder.io.stickyIn := Mux1H(float1HOut, intRounderStickyMap)
  intRounder.io.signIn := intSignSrc
  intRounder.io.rm := rm
  val intExp = intExpInS0 + 1.U
  val intExpIncrease = intExpInS0 + 2.U
  val intRounderInputIncrease = intRounderInput + 1.U

  val intNxRounded = intRounder.io.inexact
  val intUpRounded = intRounder.io.r_up
  val intCout = intUpRounded && Mux1H(float1HOut, intIsOnesRounderInputMap).asBool
  val intExpRounded = Wire(UInt(f32.expWidth.W))
  intExpRounded := Mux(intCout, intExpIncrease, intExp)
  val intFracRounded = Mux(intUpRounded, intRounderInputIncrease, intRounderInput)
  val intRmin =
    rm === RTZ || (intSignSrc && rm === RUP) || (!intSignSrc && rm === RDN)

  /**
   * int->fp   any int/uint -> any fp
    */
  // Mux(cout, exp > FP.maxExp - 1, exp > FP.maxExp)
  val intOfRounded = !intExp.head(1).asBool && Mux1H(float1HOut,
    fpParam.fp16AndFp32Formats.map(fp => Mux(intCout,
      intExp(fp.expWidth - 1, 1).andR || intExp(intExp.getWidth - 2, fp.expWidth).orR,
      intExp(fp.expWidth - 1, 0).andR || intExp(intExp.getWidth - 2, fp.expWidth).orR
    ))
  )

  // for est
  val estExpPlus1 = estExpInS0 + 1.U
  val estExp = Mux(isSubnormal, estExpInS0, estExpPlus1)
  val rsqrt7Table = Module(new Rsqrt7Table)
  rsqrt7Table.src := expNormaled0 ## fracNormaled.head(6)
  val rec7Table = Module(new Rec7Table)
  rec7Table.src := fracNormaled.head(7)
  val fracEstimate = Mux(isRec, rec7Table.out, rsqrt7Table.out)
  when (isFpWiden) {
    nv := isSNaN
    dz := false.B
    of := false.B
    uf := false.B
    nx := false.B

    val fpWidenResult = Mux1H(Seq(
      expIsOnes -> signNonNan ## ~0.U(f32.expWidth.W) ## fracNotZero ## 0.U((f32.fracWidth - 1).W),
      isZero -> signNonNan ## 0.U((f32.width - 1).W),
      (isnormal || isSubnormal) -> signNonNan ## exp(f32.expWidth - 1, 0) ## fracNormaled.head(f32.fracWidth)
    ))
    result := fpWidenResult
  }.elsewhen(isInt2Fp) {
    nv := false.B
    dz := false.B
    of := intOfRounded
    uf := false.B
    nx := intOfRounded || intNxRounded

    val int2FpResultMap: Seq[UInt] = fpParam.fp16AndFp32Formats.map { fp =>
      Mux1H(Seq(
        (intOfRounded && intRmin)         -> (intSignSrc ## fp.maxExp.U(fp.expWidth.W) ## ~0.U(fp.fracWidth.W)), // GNF
        (intOfRounded && !intRmin)        -> (intSignSrc ## ~0.U(fp.expWidth.W) ## 0.U(fp.fracWidth.W)), // INF
        isZeroIntSrc                      -> (intSignSrc ## 0.U((fp.width - 1).W)), // 0
        (!intOfRounded && !isZeroIntSrc)  -> (intSignSrc ## intExpRounded(fp.expWidth - 1, 0) ## intFracRounded(fp.fracWidth - 1, 0)) // normal
      ))
    }
    result := Mux1H(float1HOut, int2FpResultMap)
  }.elsewhen(isEstimate7) {
    nv := Mux(isRec, isSNaN, (signSrc && !isZero && !isQNaN) | isSNaN)
    dz := isZero
    of := isRec && isSubnormalRec2
    uf := false.B
    nx := of

    val recResultMap: Seq[UInt] = fpParam.fp16AndFp32Formats.map { fp =>
      Mux1H(Seq(
        isNaN                                                -> (false.B ## ~0.U(fp.expWidth.W) ## true.B ## 0.U((fp.fracWidth - 1).W)), // can
        isInf                                                -> (signSrc ## 0.U((fp.width - 1).W)), // 0
        (isZero || isSubnormalRec2 && !rmin)                 -> (signSrc ## ~0.U(fp.expWidth.W) ## 0.U(fp.fracWidth.W)), // INF
        isNormalRec0                                         -> (signSrc ## 0.U(fp.expWidth.W) ## 1.U(2.W) ## fracEstimate ## 0.U((fp.fracWidth - 2 - 7).W)),
        isNormalRec1                                         -> (signSrc ## 0.U(fp.expWidth.W) ## 1.U(1.W) ## fracEstimate ## 0.U((fp.fracWidth - 1 - 7).W)),
        (isNormalRec2 || isSubnormalRec0 || isSubnormalRec1) -> (signSrc ## estExp(fp.expWidth - 1, 0) ## fracEstimate ## 0.U((fp.fracWidth - 7).W)),
        (isSubnormalRec2 && rmin)                            -> (signSrc ## fp.maxExp.U(fp.expWidth.W) ## ~0.U(fp.fracWidth.W)) // GNF
      ))
    }

    val sqrtResultMap: Seq[UInt] = fpParam.fp16AndFp32Formats.map { fp =>
      Mux1H(Seq(
        (signSrc & !isZero | isNaN)       -> (false.B ## ~0.U(fp.expWidth.W) ## true.B ## 0.U((fp.fracWidth - 1).W)),
        isZero                            -> (signSrc ## ~0.U(fp.expWidth.W) ## 0.U(fp.fracWidth.W)),
        (!signSrc & !isZero & !expIsOnes) -> (signSrc ## estExp(fp.expWidth, 1) ## fracEstimate ## 0.U((fp.fracWidth - 7).W)), // exp/2 => >>1
        (!signSrc & isInf)                -> 0.U(fp.width.W)
      ))
    }

    result := Mux(isRec, Mux1H(float1HOut, recResultMap), Mux1H(float1HOut, sqrtResultMap))
  }.otherwise {
    /**
     * out is int, any fp->any int/uint
     * drop the shift left!
     */
    val resultRounded = fracRounded
    val isNotZeroRounded = resultRounded.orR
    val isZeroRounded = !isNotZeroRounded
    val normalResult = Mux(signSrc && isNotZeroRounded, (~resultRounded).asUInt + 1.U, resultRounded) // exclude 0
    // i = log2(intType)
    val ofExpRounded = !exp.head(1) && Mux1H(int1HOut,
      (3 to 5).map(i =>
        Mux1H(UIntToOH(hasSignInt ## cout), VecInit((0 to 3).map {
          case 0 => exp(exp.getWidth - 2, i).orR                        // >= intType   unsign & non cout
          case 1 => exp(exp.getWidth - 2, i).orR || exp(i-1, 0).andR    // >= intType-1 unsign & cout
          case 2 => exp(exp.getWidth - 2, i).orR || exp(i-1, 0).andR    // >= intType-1 sign   & non cout
          case 3 => exp(exp.getWidth - 2, i).orR || exp(i-1, 1).andR    // >= intType-2 sign   & cout
        }))
      )
    )
    val excludeFrac = Mux1H(int1HOut,
      intParamMap.map(intType => resultRounded(intType - 1) && !resultRounded(intType - 2, 0).orR)) // 1000***000
    // i=log2(intType)
    val excludeExp = Mux1H(int1HOut,
      (3 to 5).map(i => !exp.head(exp.getWidth - i).orR &&
        Mux(cout,
          exp(i-1, 1).andR && !exp(0), // == intType-2
          exp(i-1, 0).andR             // == intType-1
        )
      )
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
      (~0.U(32.W)).asUInt,
      0.U(32.W),
      Mux1H(int1HOut, intParamMap.map(intType => signNonNan ## Fill(intType - 1, !signNonNan)))
    ))
  }
  fflags := Cat(nv, dz, of, uf, nx)
  s1Out.result := result
  s1Out.fflags := fflags
}

class CVT32BundleS2(width: Int = 32) extends Bundle {
  val s2In = Input(new CVT32BundleOutputS1(width))
  val s2Out = Output(new CVT32BundleOutputS1(width))
}

class CVT32ModuleS2(width: Int = 32) extends Module {
  val io = IO(new CVT32BundleS2(width))
  io.s2Out := io.s2In
}