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

class CVT64BundleNarrowInput(width: Int = 64) extends Bundle {
  val exp = UInt(f64.expAdderWidth.W)
  val cout = Bool()
  val nxRounded = Bool()
  val expRounded = UInt(f64.expWidth.W)
  val fracRounded = UInt(width.W)
  val inRounder = UInt((width + 1).W)
  val sticky = Bool()
  val trunSticky = Bool()
  val signSrc = Bool()
  val signNonNan = Bool()
  val rm = Frm()
  val float1HOut = UInt(3.W)
  val expIsOnesSrc = Bool()
  val fracNotZeroSrc = Bool()
  val isSNaNSrc = Bool()
  val s1FpCanonicalNAN = Bool()
}

class CVT64Narrow(width: Int = 64) extends Module {
  val io = IO(new Bundle {
    val exp = Input(UInt(f64.expAdderWidth.W))
    val cout = Input(Bool())
    val nxRounded = Input(Bool())
    val expRounded = Input(UInt(f64.expWidth.W))
    val fracRounded = Input(UInt(width.W))
    val inRounder = Input(UInt((width + 1).W))
    val sticky = Input(Bool())
    val trunSticky = Input(Bool())
    val signSrc = Input(Bool())
    val signNonNan = Input(Bool())
    val rm = Input(Frm())
    val float1HOut = Input(UInt(3.W))
    val expIsOnesSrc = Input(Bool())
    val fracNotZeroSrc = Input(Bool())
    val isSNaNSrc = Input(Bool())
    val s1FpCanonicalNAN = Input(Bool())
    val result = Output(UInt(width.W))
    val fflags = Output(Fflags())
  })

  val ofRounded = !io.exp.head(1).asBool && Mux1H(io.float1HOut,
    fpParam.fpMap.map(fp => Mux(io.cout,
      io.exp(fp.expWidth - 1, 1).andR || io.exp(io.exp.getWidth - 2, fp.expWidth).orR,
      io.exp(fp.expWidth - 1, 0).andR || io.exp(io.exp.getWidth - 2, fp.expWidth).orR)
    )
  )
  val ufExpRounded = Mux(io.cout, io.exp.head(1).asBool, io.exp.head(1).asBool || !io.exp.orR)
  val nxOfRounded = io.nxRounded || ofRounded
  val maybeSub = io.exp.head(1).asBool || !io.exp.orR
  val (subFrac, shiftSticky) = (io.inRounder, io.sticky)
  val subRounderMap =
    Seq(f16, f32).map(fp => Seq(
      subFrac.tail(1).head(fp.fracWidth),
      subFrac.tail(fp.fracWidth+1).head(1),
      io.trunSticky || shiftSticky || subFrac.tail(fp.fracWidth+2).orR,
      subFrac.tail(1).head(fp.fracWidth).andR
    )).transpose
  val (subRounderInputMap, subRounerInMap, subRounderStikyMap, subIsOnesRounderInputMap) = {
    (subRounderMap(0), subRounderMap(1), subRounderMap(2), subRounderMap(3))
  }
  val subRounder = Module(new RoundingUnit(f32.fracWidth))
  val subRounderInput = Mux1H(io.float1HOut.tail(1), subRounderInputMap)
  subRounder.io.in := subRounderInput
  subRounder.io.roundIn := Mux1H(io.float1HOut.tail(1), subRounerInMap)
  subRounder.io.stickyIn := Mux1H(io.float1HOut.tail(1), subRounderStikyMap)
  subRounder.io.signIn := io.signSrc
  subRounder.io.rm := io.rm

  val subNxRounded = subRounder.io.inexact
  val subUpRounded = subRounder.io.r_up
  val subFracRounded = Mux(subUpRounded, subRounderInput + 1.U, subRounderInput)
  val subCout = subUpRounded && Mux1H(io.float1HOut.tail(1), subIsOnesRounderInputMap).asBool
  val subExpRounded = Mux(subCout, 1.U(f32.expWidth.W), 0.U(f32.expWidth.W))
  val rmin = io.rm === RTZ || (io.signSrc && io.rm === RUP) || (!io.signSrc && io.rm === RDN)

  val nv = io.isSNaNSrc && !io.s1FpCanonicalNAN
  val dz = false.B
  val of = !io.expIsOnesSrc && ofRounded && !io.s1FpCanonicalNAN
  val uf = !io.expIsOnesSrc && maybeSub && ufExpRounded && subNxRounded && !io.s1FpCanonicalNAN
  val nx = !io.expIsOnesSrc && (
    (!maybeSub && nxOfRounded) ||
      (maybeSub && subNxRounded)
    ) && !io.s1FpCanonicalNAN

  def fpNarrowResultMapGen(fp: FloatFormat): UInt = {
    Mux1H(Seq(
      io.expIsOnesSrc -> (io.signNonNan ## ~0.U(fp.expWidth.W) ## io.fracNotZeroSrc ## 0.U((fp.fracWidth - 1).W)),
      (!io.expIsOnesSrc && !maybeSub && ofRounded && (rmin || (io.rm === ROD))) ->
        (io.signNonNan ## fp.maxExp.U(fp.expWidth.W) ## ~0.U(fp.fracWidth.W)),
      (!io.expIsOnesSrc && !maybeSub && ofRounded && !(rmin || (io.rm === ROD))) ->
        (io.signNonNan ## (fp.maxExp + 1).U(fp.expWidth.W) ## 0.U(fp.fracWidth.W)),
      (!io.expIsOnesSrc && !maybeSub && !ofRounded) ->
        (io.signNonNan ## io.expRounded(fp.expWidth - 1, 0) ## io.fracRounded(fp.fracWidth - 1, 0)),
      (!io.expIsOnesSrc && maybeSub) ->
        (io.signNonNan ## subExpRounded(fp.expWidth - 1, 0) ## subFracRounded(fp.fracWidth - 1, 0))
    ))
  }
  val fpNarrowResultMap: Seq[UInt] = Seq(f16, f32).map(fpNarrowResultMapGen)
  val fpCanonicalNanResultMap: Seq[UInt] = Seq(f16, f32).map(fp =>
    ~0.U((fp.expWidth + 1).W) ## 0.U((fp.fracWidth - 1).W)
  )

  io.result := Mux(
    io.s1FpCanonicalNAN,
    Mux1H(io.float1HOut.tail(1), fpCanonicalNanResultMap),
    Mux1H(io.float1HOut.tail(1), fpNarrowResultMap)
  )
  io.fflags := Cat(nv, dz, of, uf, nx)
}

class CVT64NarrowConvert(width: Int = 64) extends Module {
  val io = IO(new Bundle {
    val in = Input(new VCVT.In(width))
    val out = Output(new VCVT.Out(width))
  })
  val (fire, src, opType, rmNext, inSew1H, outSew1H, isScalarFpInst) =
    (io.in.fire, io.in.data.src, io.in.ctrl.opType, io.in.ctrl.rm, io.in.ctrl.inSew1H, io.in.ctrl.outSew1H, io.in.ctrl.isScalarFpInst)
  val fireReg = GatedValidRegNext(fire)
  val float1HSrcNext = inSew1H(3, 1)
  val float1HOutNext = outSew1H(3, 1)
  val outIsIntNext = FCvtOpcode.outIsInt(opType)
  val hasSignIntNext = FCvtOpcode.isSignInt(opType)
  val inIs16 = FCvtOpcode.inIs16(opType)
  val inIs32 = FCvtOpcode.inIs32(opType)
  val outIsFp16Next = float1HOutNext(0)
  val outIsFp32Next = float1HOutNext(1)
  val fpInputWidthNext = FCvtOpcode.getInputDataWidth(opType)
  val fpOutputWidthNext = FCvtOpcode.getOutputDataWidth(opType)
  val isFpNarrowNext = FCvtOpcode.isF2F(opType) && fpOutputWidthNext < fpInputWidthNext
  val isFp2IntNext = FCvtOpcode.isF2I(opType)
  val isFp64To16Next = FCvtOpcode.isF2F(opType) && FCvtOpcode.inIs64(opType) && FCvtOpcode.outIs16(opType)
  val srcMap = (0 to 3).map(i => src((1 << i) * 8 - 1, 0))
  val floatMap = srcMap.zipWithIndex.map{ case (float, i) => floatExtend(float, i) }.drop(1)
  val input = Mux1H(float1HSrcNext, floatMap)
  val expSrcNext = input.tail(1).head(f64.expWidth)
  val fracSrc = input.tail(f64.expWidth + 1).head(f64.fracWidth)
  val signSrcNext = input.head(1).asBool
  val decodeFloatSrc = Mux1H(float1HSrcNext, fpParam.fpMap.map(fp =>
    VecInit(expSrcNext(fp.expWidth - 1, 0).orR, expSrcNext(fp.expWidth - 1, 0).andR, fracSrc.head(fp.fracWidth).orR).asUInt
  ))
  val (expNotZeroSrcNext, expIsOnesSrcNext, fracNotZeroSrcNext) = (decodeFloatSrc(0), decodeFloatSrc(1), decodeFloatSrc(2))
  val isNaNSrcNext = expIsOnesSrcNext && fracNotZeroSrcNext
  val isZeroSrcNext = !expNotZeroSrcNext && !fracNotZeroSrcNext
  val isInfSrcNext = expIsOnesSrcNext && !fracNotZeroSrcNext
  val isSubnormalSrcNext = !expNotZeroSrcNext && fracNotZeroSrcNext
  val isSNaNSrcNext = isNaNSrcNext && !fracSrc.head(1)
  val expNotZeroSrc = RegEnable(expNotZeroSrcNext, false.B, fire)
  val expIsOnesSrc = RegEnable(expIsOnesSrcNext, false.B, fire)
  val fracNotZeroSrc = RegEnable(fracNotZeroSrcNext, false.B, fire)
  val isZeroSrc = RegEnable(isZeroSrcNext, false.B, fire)
  val isInfSrc = RegEnable(isInfSrcNext, false.B, fire)
  val isNaNSrc = RegEnable(isNaNSrcNext, false.B, fire)
  val isSNaNSrc = RegEnable(isSNaNSrcNext, false.B, fire)
  val isFpNarrow = RegEnable(isFpNarrowNext, false.B, fire)
  val signSrc = RegEnable(signSrcNext, false.B, fire)
  val rm = RegEnable(rmNext, 0.U(3.W), fire)
  val hasSignInt = RegEnable(hasSignIntNext, false.B, fire)
  val signNonNan = !isNaNSrc && signSrc
  val outSew1HReg = RegEnable(outSew1H, 0.U(4.W), fire)
  val float1HOut = outSew1HReg.head(3)
  val int1HOut = outSew1HReg
  val intParamMap = (0 to 3).map(i => (1 << i) * 8)
  val expAdderIn0Next = Wire(UInt(f64.expAdderWidth.W))
  val expAdderIn1Next = Wire(UInt(f64.expAdderWidth.W))
  val expNext = expAdderIn0Next + expAdderIn1Next
  val expReg = RegEnable(expNext, fire)
  expAdderIn0Next := Mux1H(Seq(
    isFpNarrowNext -> Mux(isSubnormalSrcNext, false.B ## 1.U, false.B ## expSrcNext),
    isFp2IntNext -> Mux(isSubnormalSrcNext, false.B ## 1.U, false.B ## expSrcNext)
  ))
  val bias = Mux1H(float1HSrcNext, fpParam.fpMap.map(fp => fp.bias.U))
  val minusExp = extend((~(false.B ## Mux1H(Seq(
    isFpNarrowNext -> Mux(
      isFp64To16Next,
      fpParam.fp16To64BiasDelta.U,
      Mux1H(Seq(
        outIsFp16Next -> fpParam.fp16To32BiasDelta.U,
        outIsFp32Next -> fpParam.fp32To64BiasDelta.U
      ))
    ),
    isFp2IntNext -> bias
  )))).asUInt, f64.expAdderWidth).asUInt
  val expPlus1EnableNext = isFpNarrowNext || isFp2IntNext
  val expPlus1Enable = RegEnable(expPlus1EnableNext, fire)
  expAdderIn1Next := Mux(expPlus1EnableNext, minusExp, expSrcNext)
  val fracSrcLeftNext = fracSrc << (64 - f64.fracWidth)
  val fracSrcLeft = RegEnable(fracSrcLeftNext, 0.U(64.W), fire)
  val fracValueSrc = (expNotZeroSrcNext && !expIsOnesSrcNext) ## fracSrc
  val shamtInNext = fracValueSrc ## 0.U(11.W) ## false.B
  val shamtWidth = Mux(outIsIntNext, Mux1H(float1HSrcNext, fpParam.fpMap.map(fp => (63 + fp.bias).U)),
    Mux(
      isFp64To16Next,
      (fpParam.fp16To64BiasDelta + 1).U,
      Mux1H(Seq(
        outIsFp16Next -> (fpParam.fp16To32BiasDelta + 1).U,
        outIsFp32Next -> (fpParam.fp32To64BiasDelta + 1).U
      ))
    )
  ) + (~expSrcNext).asUInt
  val shamtWidthPlus1 = shamtWidth + 1.U
  val shamtNext = Mux(shamtWidth.andR, 0.U, Mux(shamtWidth(10, 6).orR, 65.U, shamtWidthPlus1))
  val (inRounderTmp, stickyTmp) = ShiftRightJam(shamtInNext, shamtNext)
  val inRounder = RegEnable(inRounderTmp, 0.U(65.W), fire)
  val sticky = RegEnable(stickyTmp, false.B, fire)
  val trunSticky = RegEnable(fracSrc.tail(f32.fracWidth).orR, false.B, fire)
  val rounderMap = fpParam.fpMap.map(fp => Seq(
    fracSrcLeft.head(fp.fracWidth),
    fracSrcLeft.tail(fp.fracWidth).head(1),
    fracSrcLeft.tail(fp.fracWidth + 1).orR,
    fracSrcLeft.head(fp.fracWidth).andR
  )).transpose
  val (rounderInputMap, rounderInMap, rounderStickyMap, isOnesRounderInputMap) = {
    (rounderMap(0), rounderMap(1), rounderMap(2), rounderMap(3))
  }
  val rounderInput = Mux(isFpNarrow, Mux1H(float1HOut, rounderInputMap), inRounder.head(64))
  val rounder = Module(new RoundingUnit(64))
  rounder.io.in := rounderInput
  rounder.io.roundIn := Mux(isFpNarrow, Mux1H(float1HOut, rounderInMap), inRounder(0))
  rounder.io.stickyIn := Mux(isFpNarrow, Mux1H(float1HOut, rounderStickyMap), sticky)
  rounder.io.signIn := signSrc
  rounder.io.rm := rm
  val nxRounded = rounder.io.inexact
  val upRounded = rounder.io.r_up
  val exp = expReg + Mux(expPlus1Enable, 1.U, 0.U)
  val expIncrease = expReg + Mux(expPlus1Enable, 2.U, 1.U)
  val rounderInputIncrease = rounderInput + 1.U
  val hasSignInt1HOut = int1HOut.asBools.map(oh => Seq(oh && !hasSignInt, oh && hasSignInt)).flatten
  val isOnesRounderInputMapFp2Int =
    intParamMap.map(intType => Seq(intType, intType - 1)).flatten.map(intType => rounderInput.tail(64 - intType).andR)
  val cout = upRounded && Mux(isFpNarrow,
    Mux1H(float1HOut, isOnesRounderInputMap),
    Mux1H(hasSignInt1HOut, isOnesRounderInputMapFp2Int)
  ).asBool
  val expRounded = Mux(cout, expIncrease, exp)
  val fracRounded = Mux(upRounded, rounderInputIncrease, rounderInput)
  val s0FpCanonicalNan = isScalarFpInst & (inIs16 & !src(width - 1, 16).andR | inIs32 & !src(width - 1, 32).andR)
  val s1FpCanonicalNan = RegEnable(s0FpCanonicalNan, false.B, fire)
  val fpNarrowInput = Wire(new CVT64BundleNarrowInput(width))
  fpNarrowInput.exp := exp
  fpNarrowInput.cout := cout
  fpNarrowInput.nxRounded := nxRounded
  fpNarrowInput.expRounded := expRounded
  fpNarrowInput.fracRounded := fracRounded
  fpNarrowInput.inRounder := inRounder
  fpNarrowInput.sticky := sticky
  fpNarrowInput.trunSticky := trunSticky
  fpNarrowInput.signSrc := signSrc
  fpNarrowInput.signNonNan := signNonNan
  fpNarrowInput.rm := rm
  fpNarrowInput.float1HOut := float1HOut
  fpNarrowInput.expIsOnesSrc := expIsOnesSrc
  fpNarrowInput.fracNotZeroSrc := fracNotZeroSrc
  fpNarrowInput.isSNaNSrc := isSNaNSrc
  fpNarrowInput.s1FpCanonicalNAN := s1FpCanonicalNan
  val resultRounded = fracRounded
  val isZeroRounded = !resultRounded.orR
  val normalResult = Mux(signSrc && resultRounded.orR, (~resultRounded).asUInt + 1.U, resultRounded)
  val ofExpRounded = !exp.head(1) && Mux1H(int1HOut,
    (3 to 6).map(i =>
      Mux1H(UIntToOH(hasSignInt ## cout), VecInit((0 to 3).map {
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
    (3 to 6).map(i => !exp.head(exp.getWidth - i).orR &&
      Mux(cout, exp(i-1, 1).andR && !exp(0), exp(i-1, 0).andR)
    )
  )
  val toUnv = ofExpRounded || expIsOnesSrc || signSrc && !(isZeroSrc || isZeroRounded && !ofExpRounded)
  val toUnx = !toUnv && nxRounded
  val toInv = ofExpRounded && !(signSrc && excludeExp && excludeFrac) || expIsOnesSrc
  val toInx = !toInv && nxRounded
  val fp2IntNv = Mux(hasSignInt, toInv, toUnv)
  val fp2IntNx = Mux(hasSignInt, toInx, toUnx)
  val fp2IntResult = Mux1H(Seq(
    ((!hasSignInt && !toUnv) || (hasSignInt && !toInv)) -> normalResult,
    (!hasSignInt && toUnv && (isNaNSrc || !signSrc && (isInfSrc || ofExpRounded))) -> (~0.U(64.W)).asUInt,
    (!hasSignInt && toUnv && signSrc && !isNaNSrc) -> 0.U(64.W),
    (hasSignInt && toInv) -> Mux1H(int1HOut, intParamMap.map(intType => signNonNan ## Fill(intType - 1, !signNonNan)))
  ))
  val fp2IntFflags = Cat(fp2IntNv, false.B, false.B, false.B, fp2IntNx)
  val fpNarrow = Module(new CVT64Narrow(width))
  fpNarrow.io.exp := fpNarrowInput.exp
  fpNarrow.io.cout := fpNarrowInput.cout
  fpNarrow.io.nxRounded := fpNarrowInput.nxRounded
  fpNarrow.io.expRounded := fpNarrowInput.expRounded
  fpNarrow.io.fracRounded := fpNarrowInput.fracRounded
  fpNarrow.io.inRounder := fpNarrowInput.inRounder
  fpNarrow.io.sticky := fpNarrowInput.sticky
  fpNarrow.io.trunSticky := fpNarrowInput.trunSticky
  fpNarrow.io.signSrc := fpNarrowInput.signSrc
  fpNarrow.io.signNonNan := fpNarrowInput.signNonNan
  fpNarrow.io.rm := fpNarrowInput.rm
  fpNarrow.io.float1HOut := fpNarrowInput.float1HOut
  fpNarrow.io.expIsOnesSrc := fpNarrowInput.expIsOnesSrc
  fpNarrow.io.fracNotZeroSrc := fpNarrowInput.fracNotZeroSrc
  fpNarrow.io.isSNaNSrc := fpNarrowInput.isSNaNSrc
  fpNarrow.io.s1FpCanonicalNAN := fpNarrowInput.s1FpCanonicalNAN
  val fpNarrowResEx2 = RegEnable(fpNarrow.io.result, 0.U(width.W), fireReg)
  val fpNarrowFflagsEx2 = RegEnable(fpNarrow.io.fflags, 0.U.asTypeOf(Fflags()), fireReg)
  val intNarrowResEx2 = RegEnable(fp2IntResult, 0.U(width.W), fireReg)
  val intNarrowFflagsEx2 = RegEnable(fp2IntFflags, 0.U.asTypeOf(Fflags()), fireReg)
  val isFpNarrowEx2 = RegEnable(isFpNarrow, false.B, fireReg)
  io.out.ex1.res := Mux(isFpNarrow, fpNarrow.io.result, fp2IntResult)
  io.out.ex1.fflags := Mux(isFpNarrow, fpNarrow.io.fflags, fp2IntFflags)
  io.out.ex2.res := Mux(isFpNarrowEx2, fpNarrowResEx2, intNarrowResEx2)
  io.out.ex2.fflags := Mux(isFpNarrowEx2, fpNarrowFflagsEx2, intNarrowFflagsEx2)
}