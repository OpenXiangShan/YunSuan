package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import yunsuan.util._
import yunsuan.vector.VectorConvert.utils._
import yunsuan.vector.VectorConvert.util._
import yunsuan.vector.VectorConvert.RoundingModle._


class CVT32(width: Int = 32) extends CVT(width) {
  /** cycle0                                     |         cycle1                  |   cycle2
   * fp2int/fp2fp   in(32)   lzc    left         |         RoundingUnit(32)  adder |
   *                          ..    adder        |                                 |
   *                  adder  ShiftRightJam(33)   |                                 |
   * int2fp   in(32) in_abs(32)  lzc  adder      | left    RoundingUnit(32)  adder |  -> result & fflags
   * vfr      in(32)             lzc  adder      | Table                           |
   */
  val (fire, src, sew, opType, rm, iSize1H, oSize1H) = (
    io.fire, io.src, io.sew, io.opType, io.rm, io.input1H, io.output1H
  )
  val fireS1 = GatedValidRegNext(fire)
  val cvt32ModuleS0 = Module(new CVT32ModuleS0(width))
  cvt32ModuleS0.io.s0In.src := src
  cvt32ModuleS0.io.s0In.opType := opType
  cvt32ModuleS0.io.s0In.rm := rm
  cvt32ModuleS0.io.s0In.iSize1H := iSize1H
  cvt32ModuleS0.io.s0In.oSize1H := oSize1H
  val cvt32ModuleS1 = Module(new CVT32ModuleS1(width))
  cvt32Bundles.CommonConnect(cvt32ModuleS1.io.s1In, cvt32ModuleS0.io.s0Out, fire)
  val cvt32ModuleS2 = Module(new CVT32ModuleS2(width))
  cvt32Bundles.CommonConnect(cvt32ModuleS2.io.s2In, cvt32ModuleS1.io.s1Out, fireS1)
  io.result := cvt32ModuleS2.io.s2Out.result
  io.fflags := cvt32ModuleS2.io.s2Out.fflags
}

object cvt32Bundles {
  def CommonConnect(sink: Bundle, source: Bundle, valid: Bool) = {
    sink := RegEnable(source, valid)
  }
}

class Special extends Bundle {
  val expNotZero = Bool()
  val expIsOnes = Bool()
  val fracNotZero = Bool()
  val isInf = Bool()
  val isZero = Bool()
  val isSubnormal = Bool()
  val isnormal = Bool()
  val isNaN = Bool()
  val isSNaN = Bool()
  val isQNaN = Bool()
}

class CVT32BundleInputS0(width: Int) extends Bundle {
  val src = UInt(width.W)
  val opType = UInt(8.W)
  val iSize1H = UInt(4.W)
  val oSize1H = UInt(4.W)
  val rm = UInt(3.W)
}

class CVT32BundleOutputS0(width: Int) extends Bundle {
  val exp = UInt(10.W)
  val expPlus1Enable = Bool()
  val shiftLeft = UInt(width.W)
  val fracSrcLeft = UInt(width.W)
  val inRounder = UInt(33.W)
  val sticky = Bool()
  val special = new Special()
  val isFpWiden = Bool()
  val isFpNarrow = Bool()
  val isFp2Int = Bool()
  val isInt2Fp = Bool()
  val isEstimate7 = Bool()
  val isRec = Bool()
  val oSize1H = UInt(4.W)
  val signSrc = Bool()
  val rm = UInt(3.W)
  val hasSignInt = Bool()
  val trunSticky = Bool()
  // int2fp
  val intSignSrc = Bool()
  val isZeroIntSrc = Bool()
  val intExp = UInt(10.W)
  val absIntSrc = UInt(width.W)
  val intLeadZeros = UInt(log2Up(width).W)
  // est
  val isSubnormalRec0 = Bool()
  val isSubnormalRec1 = Bool()
  val isSubnormalRec2 = Bool()
  val isNormalRec0 = Bool()
  val isNormalRec1 = Bool()
  val isNormalRec2 = Bool()
  val estExp = UInt(10.W)
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
  val (src, opType, rm, iSize1H, oSize1H) = (s0In.src, s0In.opType, s0In.rm, s0In.iSize1H, s0In.oSize1H)

  val isWiden = !opType(4) && opType(3)
  val isNarrow = opType(4) && !opType(3)
  val inIsFp = opType(7)
  val outIsFp = opType(6)
  val isEstimate7 = opType(5)
  val isRec = opType(5) && opType(0)
  val hasSignInt = opType(0)
  val float1HSrc = iSize1H.head(3).tail(1) // exclude f8, f64
  val float1HOut = oSize1H.head(3).tail(1) // exclude f8, f64

  val srcMap = (0 to 2).map(i => src((1 << i) * 8 - 1, 0))
  val floatMap = srcMap.zipWithIndex.map{ case (float ,i) => float32Extend(float, i)}.drop(1)
  val fpIn = Mux1H(float1HSrc, floatMap)
  val fpSignSrc = fpIn.head(1)
  val expSrc = fpIn.tail(1).head(f32.expWidth)
  val fracSrc = fpIn.tail(1+f32.expWidth).head(f32.fracWidth)
  val decodeFloatSrc = Mux1H(float1HSrc, fpParam.fpMap.take(2).map(fp =>
    VecInit(expSrc(fp.expWidth-1,0).orR, expSrc(fp.expWidth-1,0).andR, fracSrc.head(fp.fracWidth).orR).asUInt
  ))
  val (expNotZeroSrc, expIsOnes, fracNotZero) = (decodeFloatSrc(0), decodeFloatSrc(1), decodeFloatSrc(2))
  val isnormalSrc = !expIsOnes && expNotZeroSrc
  val isSubnormalSrc = !expNotZeroSrc && fracNotZero
  val isNaNSrc = expIsOnes && fracNotZero
  val isZeroSrc = !expNotZeroSrc && !fracNotZero
  val isInfSrc = expIsOnes && !fracNotZero
  val isSNaNSrc = isNaNSrc && !fracSrc.head(1)
  val isQNaNSrc = isNaNSrc && fracSrc.head(1).asBool
  val isSubnormalRec2 = isSubnormalSrc && !fracSrc.head(2).orR
  val trunSticky = fracSrc.tail(f16.fracWidth).orR

  val (isFpWiden, isFpNarrow, isFp2Int, isInt2Fp) = (inIsFp && outIsFp && isWiden, inIsFp && outIsFp && isNarrow, !outIsFp, !inIsFp)

  // exp
  val widthExpAdder = 10
  val expAdderIn0 = Wire(UInt(widthExpAdder.W))
  val expAdderIn1 = Wire(UInt(widthExpAdder.W))
  val exp = Wire(UInt(widthExpAdder.W))

  val leadZeros = Lzc((fracSrc << (32 - f32.fracWidth)).asUInt)
  val biasDelta = (f32.bias - f16.bias).U
  val bias = Mux1H(float1HSrc, fpParam.fpMap.take(2).map(fp => fp.bias.U))
  val minusExp = extend((~(false.B ## Mux1H(
    Seq(
      isFpWiden -> leadZeros,
      isFpNarrow -> biasDelta,
      isFp2Int -> bias
    )))), widthExpAdder).asUInt
  expAdderIn0 := Mux1H(Seq(
    isFpWiden -> biasDelta,
    (isFpNarrow || isFp2Int) -> Mux(isSubnormalSrc, false.B ## 1.U, false.B ## expSrc)
  ))
  val expPlus1Enable = isFpNarrow || isFp2Int || (isFpWiden && isSubnormalSrc)
  expAdderIn1 := Mux(expPlus1Enable, minusExp, expSrc)
  exp := expAdderIn0 + expAdderIn1
  s0Out.exp := exp
  s0Out.expPlus1Enable := expPlus1Enable

  // frac
  val fracSrcLeft = Wire(UInt(32.W))
  fracSrcLeft := fracSrc << (32 - f32.fracWidth)
  val shiftLeft = (fracSrcLeft.asUInt << 1) << leadZeros
  s0Out.fracSrcLeft := fracSrcLeft
  s0Out.shiftLeft := shiftLeft

  val fracImplict1Src = (expNotZeroSrc && !expIsOnes) ## fracSrc
  val shamtIn = fracImplict1Src ## 0.U(8.W) ## false.B
  val shamtWidth = Mux(outIsFp,
    (f32.bias - f16.bias + 1).U,
    Mux1H(float1HSrc, fpParam.fpMap.take(2).map(fp => (31 + fp.bias).U))
  ) + (~expSrc).asUInt
  val shamtWidthPlus1 = shamtWidth + 1.U
  val shamt = Mux(shamtWidth.andR, 0.U, Mux(shamtWidth(7,5).orR, 33.U, shamtWidthPlus1))

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
  s0Out.isFpWiden := isFpWiden
  s0Out.isFpNarrow := isFpNarrow
  s0Out.isFp2Int := isFp2Int
  s0Out.isInt2Fp := isInt2Fp
  s0Out.oSize1H := oSize1H
  s0Out.signSrc := fpSignSrc
  s0Out.rm := rm
  s0Out.hasSignInt := hasSignInt
  s0Out.trunSticky := trunSticky

  // int2fp
  val int1HSrc = iSize1H.tail(1)
  val intMap = srcMap.map(int => int32Extend(int, hasSignInt && int.head(1).asBool))
  val intIn = Mux1H(int1HSrc, intMap)
  val intSignSrc = intIn.head(1).asBool
  val absIntSrc = Mux(intSignSrc, (~intIn.tail(1)).asUInt + 1.U, intIn.tail(1))
  val isZeroIntSrc = !absIntSrc.orR
  // clz
  val intLeadZeros = Lzc(absIntSrc)
  // exp
  val intExpAdderIn0 = Wire(UInt(widthExpAdder.W))
  val intExpAdderIn1 = Wire(UInt(widthExpAdder.W))
  val intMinuxExp = extend((~(false.B ## intLeadZeros)).asUInt, widthExpAdder).asUInt
  intExpAdderIn0 := Mux1H(float1HOut, fpParam.fpMap.take(2).map(fp => (fp.bias + 31).U))
  intExpAdderIn1 := intMinuxExp
  val intExp = Wire(UInt(widthExpAdder.W))
  intExp := intExpAdderIn0 + intExpAdderIn1

  s0Out.intSignSrc := intSignSrc
  s0Out.isZeroIntSrc := isZeroIntSrc
  s0Out.intExp := intExp
  s0Out.absIntSrc := absIntSrc
  s0Out.intLeadZeros := intLeadZeros

  // est
  val decodeFloatSrcRec = Mux1H(float1HSrc,
    fpParam.fpMap.take(2).map(fp => expSrc(fp.expWidth - 1, 0)).zip(fpParam.fpMap.take(2).map(fp => fp.expWidth)).map { case (exp, expWidth) =>
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

  val estExpAdderIn0 = Wire(UInt(widthExpAdder.W))
  val estExpAdderIn1 = Wire(UInt(widthExpAdder.W))
  val estMinusExp = extend((~(false.B ## expSrc)).asUInt, widthExpAdder).asUInt
  estExpAdderIn0 := Mux1H(float1HOut, fpParam.fpMap.take(2).map(fp => Mux(isRec, (2 * fp.bias - 1).U, (3 * fp.bias - 1).U)))
  estExpAdderIn1 := Mux(isSubnormalSrc, leadZeros, estMinusExp)
  val estExp = Wire(UInt(widthExpAdder.W))
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
  val fflags = UInt(5.W)
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
  val isFpNarrow = s1In.isFpNarrow
  val isFp2Int  = s1In.isFp2Int
  val isInt2Fp = s1In.isInt2Fp
  val isEstimate7 = s1In.isEstimate7
  val isRec = s1In.isRec
  val oSize1H = s1In.oSize1H
  val shiftLeft = s1In.shiftLeft
  val fracSrcLeft = s1In.fracSrcLeft
  val signSrc = s1In.signSrc
  val rm = s1In.rm
  val hasSignInt = s1In.hasSignInt
  val trunSticky = s1In.trunSticky

  val intSignSrc = s1In.intSignSrc
  val isZeroIntSrc = s1In.isZeroIntSrc
  val intExpInS0 = s1In.intExp
  val absIntSrc = s1In.absIntSrc
  val intLeadZeros = s1In.intLeadZeros

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

  val float1HOut = oSize1H.head(3).tail(1)

  val fracNormaled = Wire(UInt(width.W))
  fracNormaled := Mux(isSubnormal, shiftLeft, fracSrcLeft)

  val inRounder = s1In.inRounder
  val sticky = s1In.sticky

  val rounderMapIn = Wire(UInt(width.W))
  rounderMapIn := Mux(isFpNarrow, fracSrcLeft, shiftLeft)

  val rounderMap = fpParam.fpMap.take(2).map(fp => Seq(
    rounderMapIn.head(fp.fracWidth),
    rounderMapIn.tail(fp.fracWidth).head(1),
    rounderMapIn.tail(fp.fracWidth + 1).orR,
    rounderMapIn.head(fp.fracWidth).andR
  )).transpose

  val (rounderInputMap, rounderInMap, rounderStickyMap, isOnesRounderInputMap) = {
    (rounderMap(0), rounderMap(1), rounderMap(2), rounderMap(3))
  }

  val selectInRounder = isFp2Int
  val rounderInput = Mux(selectInRounder, inRounder.head(32), Mux1H(float1HOut, rounderInputMap))
  val rounderRoundIn = Mux(selectInRounder, inRounder(0), Mux1H(float1HOut, rounderInMap))
  val rounderStickyIn = Mux(selectInRounder, sticky, Mux1H(float1HOut, rounderStickyMap))

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
  val int1HOut = oSize1H.tail(1)
  val hasSignInt1HOut = int1HOut.asBools.map(oh => Seq(oh && !hasSignInt, oh && hasSignInt)).flatten
  val isOnesRounderInputMapFp2Int =
    intParamMap.map(intType => Seq(intType, intType - 1)).flatten.map(intType => rounderInput.tail(32 - intType).andR)
  val cout = upRounded && Mux(isFp2Int,
    Mux1H(hasSignInt1HOut, isOnesRounderInputMapFp2Int),
    Mux1H(float1HOut, isOnesRounderInputMap)
  ).asBool
  val expRounded = Wire(UInt(f32.expWidth.W))
  expRounded := Mux(cout, expIncrease, exp)
  val fracRounded = Mux(upRounded, rounderInputIncrease, rounderInput)
  val rmin = rm === RTZ || (signSrc && rm === RUP) || (!signSrc && rm === RDN)
  val signNonNan = signSrc && !isNaN

  // for int2fp
  val intShiftLeft = Wire(UInt(width.W))
  intShiftLeft := (absIntSrc << 1) << intLeadZeros
  val roundMapIn = Wire(UInt(width.W))
  roundMapIn := intShiftLeft
  val intRounderMap = fpParam.fpMap.take(2).map(fp => Seq(
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
    fpParam.fpMap.take(2).map(fp => Mux(intCout,
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
  }.elsewhen(isFpNarrow) {
    /**
     * note: IEEE754 uf: exp in (-b^emin, b^emin), after rounding(RiscV!!!)
     * note: IEEE754 uf: exp in (-b^emin, b^emin), before rounding(other)
     */
    // Mux(cout, exp > FP.maxExp - 1, exp > FP.maxExp)
    val ofRounded = !exp.head(1).asBool && Mux(cout,
      exp(f16.expWidth - 1, 1).andR || exp(exp.getWidth - 2, f16.expWidth).orR,
      exp(f16.expWidth - 1, 0).andR || exp(exp.getWidth - 2, f16.expWidth).orR
    )
    // val ufExpRounded = Mux(cout, interExp < 0.S, interExp < 1.S)
    val maybeSub = exp.head(1).asBool || !exp.orR
    val ufExpRounded = Mux(cout, exp.head(1).asBool, maybeSub)
    val nxOfRounded = nxRounded || ofRounded
    /** dest is Subnormal
     * dest: 1-toBias, src: srcExp - srcBias
     * src->dest: exp = srcExp - srcBias + toBias
     */
    // val maybeSub = exp < 1.S
    val subFracRounded = Wire(UInt(f16.fracWidth.W))
    val subExpRounded = Wire(UInt(f16.expWidth.W))

    val (subFrac, shiftSticky) = (inRounder, sticky)
    val subRounderMap = Seq(
      subFrac.tail(1).head(f16.fracWidth),
      subFrac.tail(1+f16.fracWidth).head(1), // 1+toFracWidth+1 => drop head & tail
      trunSticky || shiftSticky || subFrac.tail(f16.fracWidth+2).orR,
      subFrac.tail(1).head(f16.fracWidth).andR
    )

    val (subRounderInput, subRounderIn, subRounderSticky, subIsOnesRounderInput) = {
      (subRounderMap(0), subRounderMap(1), subRounderMap(2), subRounderMap(3))
    }

    val subRounder = Module(new RoundingUnit(f16.fracWidth))
    subRounder.io.in := subRounderInput
    subRounder.io.roundIn := subRounderIn
    subRounder.io.stickyIn := subRounderSticky
    subRounder.io.signIn := signSrc
    subRounder.io.rm := rm
    val subNxRounded = subRounder.io.inexact
    val subUpRounded = subRounder.io.r_up
    subFracRounded := Mux(subUpRounded, subRounderInput + 1.U, subRounderInput)
    val subCout = subUpRounded && subIsOnesRounderInput.asBool
    subExpRounded := Mux(subCout, 1.U, 0.U)

    nv := isSNaN
    dz := false.B
    of := !expIsOnes && ofRounded
    uf := !expIsOnes && maybeSub && ufExpRounded && subNxRounded
    nx := !expIsOnes && (!maybeSub && nxOfRounded || maybeSub && subNxRounded)

    val result1H = Seq(
      expIsOnes,
      !expIsOnes && !maybeSub && ofRounded && (rmin || (rm === RTO)),
      !expIsOnes && !maybeSub && ofRounded && !(rmin || (rm === RTO)),
      !expIsOnes && !maybeSub && !ofRounded,
      !expIsOnes && maybeSub
    )

    def fpNarrowResultGen: Seq[UInt] = Seq(
      signNonNan ## ~0.U(f16.expWidth.W) ## fracNotZero ## 0.U((f16.fracWidth - 1).W),    // Inf or NaN->QNaN
      signNonNan ## f16.maxExp.U(f16.expWidth.W) ## ~0.U(f16.fracWidth.W),                // of => GNF
      signNonNan ## (f16.maxExp + 1).U(f16.expWidth.W) ## 0.U(f16.fracWidth.W),           // of => INF
      signNonNan ## expRounded(f16.expWidth - 1, 0) ## fracRounded(f16.fracWidth - 1, 0), // normal
      signNonNan ## subExpRounded(f16.expWidth - 1, 0) ## subFracRounded(f16.fracWidth - 1, 0) // sub or uf
    )

    val fpNarrowResult = Mux1H(result1H, fpNarrowResultGen)
    result := fpNarrowResult
  }.elsewhen(isInt2Fp) {
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

    val int2FpResultMap: Seq[UInt] = fpParam.fpMap.take(2).map(fp => Mux1H(result1H, int2FpResultGen(fp)))
    result := Mux1H(float1HOut, int2FpResultMap)
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
    val recReultMap: Seq[UInt] = fpParam.fpMap.take(2).map(fp => Mux1H(recResult1H, recResultMapGen(fp)))

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
    val sqrtResultMap: Seq[UInt] = fpParam.fpMap.take(2).map(fp => Mux1H(sqrtResult1H, sqrtResultMapGen(fp)))

    result := Mux(isRec, Mux1H(float1HOut, recReultMap), Mux1H(float1HOut, sqrtResultMap))
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
