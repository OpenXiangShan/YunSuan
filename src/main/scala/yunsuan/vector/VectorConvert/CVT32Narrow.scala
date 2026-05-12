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

class CVT32NarrowBundleOutputS0(width: Int) extends CVT32BundleOutputS0(width) {
  val trunSticky = Bool()
  val isFpNarrow = Bool()
}

class CVT32NarrowBundleS0(width: Int) extends Bundle {
  val s0In = Input(new CVT32BundleInputS0(width))
  val s0Out = Output(new CVT32NarrowBundleOutputS0(width))
}

class CVT32NarrowBundleInputS1(width: Int) extends CVT32NarrowBundleOutputS0(width)

class CVT32BundleNarrowInput(width: Int = 32) extends Bundle {
  val exp = UInt(f32.expAdderWidth.W)
  val cout = Bool()
  val nxRounded = Bool()
  val expRounded = UInt(f32.expWidth.W)
  val fracRounded = UInt(width.W)
  val inRounder = UInt((width + 1).W)
  val sticky = Bool()
  val trunSticky = Bool()
  val signSrc = Bool()
  val signNonNan = Bool()
  val rm = Frm()
  val expIsOnes = Bool()
  val fracNotZero = Bool()
  val isSNaN = Bool()
}

class CVT32NarrowBundleOutputS1(width: Int = 32) extends CVT32BundleOutputS1(width) {
  val narrow = new CVT32BundleNarrowInput(width)
}

class CVT32NarrowBundleS1(width: Int) extends Bundle {
  val s1In = Input(new CVT32NarrowBundleInputS1(width))
  val s1Out = Output(new CVT32NarrowBundleOutputS1(width))
}

class CVT32Narrow(width: Int = 32) extends Module {
  val io = IO(new Bundle {
    val exp = Input(UInt(f32.expAdderWidth.W))
    val cout = Input(Bool())
    val nxRounded = Input(Bool())
    val expRounded = Input(UInt(f32.expWidth.W))
    val fracRounded = Input(UInt(width.W))
    val inRounder = Input(UInt((width + 1).W))
    val sticky = Input(Bool())
    val trunSticky = Input(Bool())
    val signSrc = Input(Bool())
    val signNonNan = Input(Bool())
    val rm = Input(Frm())
    val expIsOnes = Input(Bool())
    val fracNotZero = Input(Bool())
    val isSNaN = Input(Bool())
    val result = Output(UInt(width.W))
    val fflags = Output(Fflags())
  })

  val ofRounded = !io.exp.head(1).asBool && Mux(io.cout,
    io.exp(f16.expWidth - 1, 1).andR || io.exp(io.exp.getWidth - 2, f16.expWidth).orR,
    io.exp(f16.expWidth - 1, 0).andR || io.exp(io.exp.getWidth - 2, f16.expWidth).orR
  )
  val maybeSub = io.exp.head(1).asBool || !io.exp.orR
  val ufExpRounded = Mux(io.cout, io.exp.head(1).asBool, maybeSub)
  val nxOfRounded = io.nxRounded || ofRounded
  val (subFrac, shiftSticky) = (io.inRounder, io.sticky)
  val subRounderMap = Seq(
    subFrac.tail(1).head(f16.fracWidth),
    subFrac.tail(1+f16.fracWidth).head(1),
    io.trunSticky || shiftSticky || subFrac.tail(f16.fracWidth+2).orR,
    subFrac.tail(1).head(f16.fracWidth).andR
  )
  val (subRounderInput, subRounderIn, subRounderSticky, subIsOnesRounderInput) = {
    (subRounderMap(0), subRounderMap(1), subRounderMap(2), subRounderMap(3))
  }
  val subRounder = Module(new RoundingUnit(f16.fracWidth))
  subRounder.io.in := subRounderInput
  subRounder.io.roundIn := subRounderIn
  subRounder.io.stickyIn := subRounderSticky
  subRounder.io.signIn := io.signSrc
  subRounder.io.rm := io.rm

  val subNxRounded = subRounder.io.inexact
  val subUpRounded = subRounder.io.r_up
  val subFracRounded = Mux(subUpRounded, subRounderInput + 1.U, subRounderInput)
  val subCout = subUpRounded && subIsOnesRounderInput.asBool
  val subExpRounded = Mux(subCout, 1.U(f16.expWidth.W), 0.U(f16.expWidth.W))
  val rmin = io.rm === RTZ || (io.signSrc && io.rm === RUP) || (!io.signSrc && io.rm === RDN)

  val nv = io.isSNaN
  val dz = false.B
  val of = !io.expIsOnes && ofRounded
  val uf = !io.expIsOnes && maybeSub && ufExpRounded && subNxRounded
  val nx = !io.expIsOnes && (!maybeSub && nxOfRounded || maybeSub && subNxRounded)

  val fpNarrowResult = Mux1H(Seq(
    io.expIsOnes -> (io.signNonNan ## ~0.U(f16.expWidth.W) ## io.fracNotZero ## 0.U((f16.fracWidth - 1).W)),
    (!io.expIsOnes && !maybeSub && ofRounded && (rmin || (io.rm === ROD))) ->
      (io.signNonNan ## f16.maxExp.U(f16.expWidth.W) ## ~0.U(f16.fracWidth.W)),
    (!io.expIsOnes && !maybeSub && ofRounded && !(rmin || (io.rm === ROD))) ->
      (io.signNonNan ## (f16.maxExp + 1).U(f16.expWidth.W) ## 0.U(f16.fracWidth.W)),
    (!io.expIsOnes && !maybeSub && !ofRounded) ->
      (io.signNonNan ## io.expRounded(f16.expWidth - 1, 0) ## io.fracRounded(f16.fracWidth - 1, 0)),
    (!io.expIsOnes && maybeSub) ->
      (io.signNonNan ## subExpRounded(f16.expWidth - 1, 0) ## subFracRounded(f16.fracWidth - 1, 0))
  ))

  io.result := fpNarrowResult
  io.fflags := Cat(nv, dz, of, uf, nx)
}

class CVT32NarrowModuleS0(width: Int = 32) extends Module {
  val io = IO(new CVT32NarrowBundleS0(width))
  val s0In = io.s0In
  val s0Out = io.s0Out
  val (src, opType, rm, inSew1H, outSew1H) = (s0In.src, s0In.opType, s0In.rm, s0In.inSew1H, s0In.outSew1H)

  val isEstimate7 = FCvtOpcode.isEstimate7(opType)
  val isRec = FCvtOpcode.isRec(opType)
  val hasSignInt = FCvtOpcode.isSignInt(opType)
  val float1HSrc = inSew1H(2, 1)
  val float1HOut = outSew1H(2, 1)

  val srcMap = (0 to 2).map(i => src((1 << i) * 8 - 1, 0))
  val floatMap = srcMap.zipWithIndex.map { case (float, i) => float32Extend(float, i) }.drop(1)
  val fpIn = Mux1H(float1HSrc, floatMap)
  val fpSignSrc = fpIn.head(1)
  val expSrc = fpIn.tail(1).head(f32.expWidth)
  val fracSrc = fpIn.tail(1+f32.expWidth).head(f32.fracWidth)
  val decodeFloatSrc = Mux1H(float1HSrc, fpParam.fp16AndFp32Formats.map(fp =>
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

  val fpInputWidth = FCvtOpcode.getInputDataWidth(opType)
  val fpOutputWidth = FCvtOpcode.getOutputDataWidth(opType)
  val isFpWiden = FCvtOpcode.isF2F(opType) && fpOutputWidth > fpInputWidth
  val isFpNarrow = FCvtOpcode.isF2F(opType) && fpOutputWidth < fpInputWidth
  val isFp2Int = FCvtOpcode.isF2I(opType)
  val isInt2Fp = FCvtOpcode.isI2F(opType)

  val expAdderIn0 = Wire(UInt(f32.expAdderWidth.W))
  val expAdderIn1 = Wire(UInt(f32.expAdderWidth.W))
  val exp = Wire(UInt(f32.expAdderWidth.W))

  val leadZeros = Lzc((fracSrc << (32 - f32.fracWidth)).asUInt).data
  val biasDelta = (f32.bias - f16.bias).U
  val bias = Mux1H(float1HSrc, fpParam.fp16AndFp32Formats.map(fp => fp.bias.U))
  val minusExp = extend((~(false.B ## Mux1H(
    Seq(
      isFpWiden -> leadZeros,
      isFpNarrow -> biasDelta,
      isFp2Int -> bias
    )))), f32.expAdderWidth).asUInt
  expAdderIn0 := Mux1H(Seq(
    isFpWiden -> biasDelta,
    (isFpNarrow || isFp2Int) -> Mux(isSubnormalSrc, false.B ## 1.U, false.B ## expSrc)
  ))
  val expPlus1Enable = isFpNarrow || isFp2Int || (isFpWiden && isSubnormalSrc)
  expAdderIn1 := Mux(expPlus1Enable, minusExp, expSrc)
  exp := expAdderIn0 + expAdderIn1
  s0Out.exp := exp
  s0Out.expPlus1Enable := expPlus1Enable

  val fracSrcLeft = Wire(UInt(32.W))
  fracSrcLeft := fracSrc << (32 - f32.fracWidth)
  val shiftLeft = (fracSrcLeft.asUInt << 1) << leadZeros
  s0Out.fracSrcLeft := fracSrcLeft
  s0Out.shiftLeft := shiftLeft

  val fracImplict1Src = (expNotZeroSrc && !expIsOnes) ## fracSrc
  val shamtIn = fracImplict1Src ## 0.U(8.W) ## false.B
  val shamtWidth = Mux(
    FCvtOpcode.outIsFp(opType),
    (f32.bias - f16.bias + 1).U,
    Mux1H(float1HSrc, fpParam.fp16AndFp32Formats.map(fp => (31 + fp.bias).U))
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
  s0Out.outSew1H := outSew1H
  s0Out.signSrc := fpSignSrc
  s0Out.rm := rm
  s0Out.hasSignInt := hasSignInt
  s0Out.trunSticky := trunSticky

  val int1HSrc = inSew1H(2, 0)
  val intMap = srcMap.map(int => int32Extend(int, hasSignInt && int.head(1).asBool))
  val intIn = Mux1H(int1HSrc, intMap)
  val intSignSrc = intIn.head(1).asBool
  val absIntSrc = Mux(intSignSrc, (~intIn.tail(1)).asUInt + 1.U, intIn.tail(1))
  val isZeroIntSrc = !absIntSrc.orR
  val intLeadZeros = Lzc(absIntSrc).data
  val intExpAdderIn0 = Wire(UInt(f32.expAdderWidth.W))
  val intExpAdderIn1 = Wire(UInt(f32.expAdderWidth.W))
  val intMinuxExp = extend((~(false.B ## intLeadZeros)).asUInt, f32.expAdderWidth).asUInt
  intExpAdderIn0 := Mux1H(float1HOut, fpParam.fp16AndFp32Formats.map(fp => (fp.bias + 31).U))
  intExpAdderIn1 := intMinuxExp
  val intExp = Wire(UInt(f32.expAdderWidth.W))
  intExp := intExpAdderIn0 + intExpAdderIn1

  s0Out.intSignSrc := intSignSrc
  s0Out.isZeroIntSrc := isZeroIntSrc
  s0Out.intExp := intExp
  s0Out.absIntSrc := absIntSrc
  s0Out.intLeadZeros := intLeadZeros

  val decodeFloatSrcRec = Mux1H(float1HSrc,
    fpParam.fp16AndFp32Formats.map(fp => expSrc(fp.expWidth - 1, 0)).zip(fpParam.fp16AndFp32Formats.map(fp => fp.expWidth)).map { case (exp, expWidth) =>
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

  val estExpNormaled = Mux(isSubnormalSrc, leadZeros(0), expSrc(0))

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

class CVT32NarrowConvert(width: Int = 32) extends Module {
  val io = IO(new Bundle {
    val in = Input(new VCVT.In(width))
    val out = Output(new VCVT.Out(width))
  })
  val (fire, src, opType, rm, inSew1H, outSew1H) = (
    io.in.fire, io.in.data.src, io.in.ctrl.opType, io.in.ctrl.rm, io.in.ctrl.inSew1H, io.in.ctrl.outSew1H
  )
  val fireS1 = GatedValidRegNext(fire)
  val s0 = Module(new CVT32NarrowModuleS0(width))
  s0.io.s0In.src := src
  s0.io.s0In.opType := opType
  s0.io.s0In.rm := rm
  s0.io.s0In.inSew1H := inSew1H
  s0.io.s0In.outSew1H := outSew1H
  val s1 = Module(new CVT32NarrowModuleS1(width))
  CommonConnect(s1.io.s1In, s0.io.s0Out, fire)
  val fpNarrow = Module(new CVT32Narrow(width))
  fpNarrow.io.exp := s1.io.s1Out.narrow.exp
  fpNarrow.io.cout := s1.io.s1Out.narrow.cout
  fpNarrow.io.nxRounded := s1.io.s1Out.narrow.nxRounded
  fpNarrow.io.expRounded := s1.io.s1Out.narrow.expRounded
  fpNarrow.io.fracRounded := s1.io.s1Out.narrow.fracRounded
  fpNarrow.io.inRounder := s1.io.s1Out.narrow.inRounder
  fpNarrow.io.sticky := s1.io.s1Out.narrow.sticky
  fpNarrow.io.trunSticky := s1.io.s1Out.narrow.trunSticky
  fpNarrow.io.signSrc := s1.io.s1Out.narrow.signSrc
  fpNarrow.io.signNonNan := s1.io.s1Out.narrow.signNonNan
  fpNarrow.io.rm := s1.io.s1Out.narrow.rm
  fpNarrow.io.expIsOnes := s1.io.s1Out.narrow.expIsOnes
  fpNarrow.io.fracNotZero := s1.io.s1Out.narrow.fracNotZero
  fpNarrow.io.isSNaN := s1.io.s1Out.narrow.isSNaN
  val fpNarrowResEx2 = RegEnable(fpNarrow.io.result, 0.U(width.W), fireS1)
  val fpNarrowFflagsEx2 = RegEnable(fpNarrow.io.fflags, 0.U.asTypeOf(Fflags()), fireS1)
  val intNarrowResEx2 = RegEnable(s1.io.s1Out.result, 0.U(width.W), fireS1)
  val intNarrowFflagsEx2 = RegEnable(s1.io.s1Out.fflags, 0.U.asTypeOf(Fflags()), fireS1)
  val isFpNarrowEx1 = s1.io.s1In.isFpNarrow
  val isFpNarrowEx2 = RegEnable(isFpNarrowEx1, false.B, fireS1)
  io.out.ex1.res := Mux(isFpNarrowEx1, fpNarrow.io.result, s1.io.s1Out.result)
  io.out.ex1.fflags := Mux(isFpNarrowEx1, fpNarrow.io.fflags, s1.io.s1Out.fflags)
  io.out.ex2.res := Mux(isFpNarrowEx2, fpNarrowResEx2, intNarrowResEx2)
  io.out.ex2.fflags := Mux(isFpNarrowEx2, fpNarrowFflagsEx2, intNarrowFflagsEx2)
}

class CVT32NarrowModuleS1(width: Int = 32) extends Module {
  val io = IO(new CVT32NarrowBundleS1(width))
  val s1In = io.s1In
  val special = s1In.special
  val expIsOnes = special.expIsOnes
  val fracNotZero = special.fracNotZero
  val isZero = special.isZero
  val isInf = special.isInf
  val isNaN = special.isNaN
  val isSNaN = special.isSNaN
  val expInS0 = s1In.exp
  val expPlus1Enable = s1In.expPlus1Enable
  val outSew1H = s1In.outSew1H
  val fracSrcLeft = s1In.fracSrcLeft
  val signSrc = s1In.signSrc
  val rm = s1In.rm
  val hasSignInt = s1In.hasSignInt
  val trunSticky = s1In.trunSticky
  val isFp2Int = s1In.isFp2Int
  val float1HOut = outSew1H(2, 1)
  val int1HOut = outSew1H(2, 0)
  val intParamMap = (0 to 2).map(i => (1 << i) * 8)
  val inRounder = s1In.inRounder
  val sticky = s1In.sticky
  val rounderMap = fpParam.fp16AndFp32Formats.map(fp => Seq(
    fracSrcLeft.head(fp.fracWidth),
    fracSrcLeft.tail(fp.fracWidth).head(1),
    fracSrcLeft.tail(fp.fracWidth + 1).orR,
    fracSrcLeft.head(fp.fracWidth).andR
  )).transpose
  val (rounderInputMap, rounderInMap, rounderStickyMap, isOnesRounderInputMap) = {
    (rounderMap(0), rounderMap(1), rounderMap(2), rounderMap(3))
  }
  val rounderInput = Mux(isFp2Int, inRounder.head(32), Mux1H(float1HOut, rounderInputMap))
  val rounder = Module(new RoundingUnit(width))
  rounder.io.in := rounderInput
  rounder.io.roundIn := Mux(isFp2Int, inRounder(0), Mux1H(float1HOut, rounderInMap))
  rounder.io.stickyIn := Mux(isFp2Int, sticky, Mux1H(float1HOut, rounderStickyMap))
  rounder.io.signIn := signSrc
  rounder.io.rm := rm
  val nxRounded = rounder.io.inexact
  val upRounded = rounder.io.r_up
  val exp = expInS0 + Mux(expPlus1Enable, 1.U, 0.U)
  val expIncrease = expInS0 + Mux(expPlus1Enable, 2.U, 1.U)
  val rounderInputIncrease = rounderInput + 1.U
  val cout = upRounded && Mux1H(float1HOut, isOnesRounderInputMap).asBool
  val expRounded = Mux(cout, expIncrease, exp)
  val fracRounded = Mux(upRounded, rounderInputIncrease, rounderInput)
  val signNonNan = signSrc && !isNaN
  io.s1Out.narrow.exp := exp
  io.s1Out.narrow.cout := cout
  io.s1Out.narrow.nxRounded := nxRounded
  io.s1Out.narrow.expRounded := expRounded
  io.s1Out.narrow.fracRounded := fracRounded
  io.s1Out.narrow.inRounder := inRounder
  io.s1Out.narrow.sticky := sticky
  io.s1Out.narrow.trunSticky := trunSticky
  io.s1Out.narrow.signSrc := signSrc
  io.s1Out.narrow.signNonNan := signNonNan
  io.s1Out.narrow.rm := rm
  io.s1Out.narrow.expIsOnes := expIsOnes
  io.s1Out.narrow.fracNotZero := fracNotZero
  io.s1Out.narrow.isSNaN := isSNaN
  val resultRounded = fracRounded
  val isZeroRounded = !resultRounded.orR
  val normalResult = Mux(signSrc && resultRounded.orR, (~resultRounded).asUInt + 1.U, resultRounded)
  val hasSignInt1HOut = int1HOut.asBools.map(oh => Seq(oh && !hasSignInt, oh && hasSignInt)).flatten
  val isOnesRounderInputMapFp2Int =
    intParamMap.map(intType => Seq(intType, intType - 1)).flatten.map(intType => rounderInput.tail(32 - intType).andR)
  val fp2IntCout = upRounded && Mux1H(hasSignInt1HOut, isOnesRounderInputMapFp2Int).asBool
  val ofExpRounded = !exp.head(1) && Mux1H(int1HOut,
    (3 to 5).map(i =>
      Mux1H(UIntToOH(hasSignInt ## fp2IntCout), VecInit((0 to 3).map {
        case 0 => exp(exp.getWidth - 2, i).orR
        case 1 => exp(exp.getWidth - 2, i).orR || exp(i-1, 0).andR
        case 2 => exp(exp.getWidth - 2, i).orR || exp(i-1, 0).andR
        case 3 => exp(exp.getWidth - 2, i).orR || exp(i-1, 1).andR
      }))
    )
  )
  val excludeFrac = Mux1H(int1HOut,
    intParamMap.map(intType => resultRounded(intType - 1) && !resultRounded(intType - 2, 0).orR))
  val excludeExp = Mux1H(int1HOut,
    (3 to 5).map(i => !exp.head(exp.getWidth - i).orR &&
      Mux(fp2IntCout, exp(i-1, 1).andR && !exp(0), exp(i-1, 0).andR)
    )
  )
  val toUnv = ofExpRounded || expIsOnes || signSrc && !(isZero || isZeroRounded && !ofExpRounded)
  val toUnx = !toUnv && nxRounded
  val toInv = ofExpRounded && !(signSrc && excludeExp && excludeFrac) || expIsOnes
  val toInx = !toInv && nxRounded
  val fp2IntNv = Mux(hasSignInt, toInv, toUnv)
  val fp2IntNx = Mux(hasSignInt, toInx, toUnx)
  val fp2IntResult = Mux1H(Seq(
    ((!hasSignInt && !toUnv) || (hasSignInt && !toInv)) -> normalResult,
    (!hasSignInt && toUnv && (isNaN || !signSrc && (isInf || ofExpRounded))) -> (~0.U(32.W)).asUInt,
    (!hasSignInt && toUnv && signSrc && !isNaN) -> 0.U(32.W),
    (hasSignInt && toInv) -> Mux1H(int1HOut, intParamMap.map(intType => signNonNan ## Fill(intType - 1, !signNonNan)))
  ))
  io.s1Out.result := fp2IntResult
  io.s1Out.fflags := Cat(fp2IntNv, false.B, false.B, false.B, fp2IntNx)
}