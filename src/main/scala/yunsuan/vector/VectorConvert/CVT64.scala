package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import yunsuan.vector.VectorConvert.util._
import yunsuan.vector.VectorConvert.utils._
import yunsuan.vector.VectorConvert.RoundingModle._
import yunsuan.util._

class CVT64(width: Int = 64, isVectorCvt: Boolean) extends CVT(width){
  val (fire, src, sew, opType, rm, input1H, output1H, isFpToVecInst, isFround, isFcvtmod) =
    (io.fire, io.src, io.sew, io.opType, io.rm, io.input1H, io.output1H, io.isFpToVecInst, io.isFround, io.isFcvtmod)
  val fireReg = GatedValidRegNext(fire)

  val outIsFpNext = opType.tail(1).head(1).asBool
  val hasSignIntNext = opType(0).asBool
  val inIsFpNext = opType.head(1).asBool
  val isWiden = !opType(4) && opType(3)
  val isNarrow = opType(4) && !opType(3)
  val outIsF16 = outIsFpNext && output1H(1)
  val outIsF64 = outIsFpNext && output1H(3)
  val isCrossHigh = opType(4) && opType(3) && outIsF64
  val isCrossLow = opType(4) && opType(3) && outIsF16
  val isEstimate7Next = opType(5)

  val (isInt2FpNext, isFpWidenNext, isFpNarrowNext, isFp2IntNext, isFpCrossHighNext, isFpCrossLowNext) =
    (!inIsFpNext, inIsFpNext && outIsFpNext && isWiden, inIsFpNext && outIsFpNext && isNarrow,
      !outIsFpNext, inIsFpNext && outIsFpNext && isCrossHigh, inIsFpNext && outIsFpNext && isCrossLow)

  val isFroundOrFroundnxNext = isFround.orR
  val isFpWiden = RegEnable(isFpWidenNext, false.B, fire)
  val isFpNarrow = RegEnable(isFpNarrowNext, false.B, fire)
  val isFp2Int = RegEnable(isFp2IntNext, false.B, fire)
  val isFpCrossHigh = RegEnable(isFpCrossHighNext, false.B, fire)
  val isFpCrossLow = RegEnable(isFpCrossLowNext, false.B, fire)
  val isFroundReg = RegEnable(isFroundOrFroundnxNext, false.B, fire)
  val isFcvtmodReg = RegEnable(isFcvtmod, false.B, fire)

  val s0_outIsF64 =  outIsFpNext && output1H(3)
  val s0_outIsF32 =  outIsFpNext && output1H(2)
  val s0_outIsF16 =  outIsFpNext && output1H(1)
  val s0_outIsU32 = !outIsFpNext && output1H(2) && !hasSignIntNext
  val s0_outIsS32 = !outIsFpNext && output1H(2) && hasSignIntNext
  val s0_outIsU64 = !outIsFpNext && output1H(3) && !hasSignIntNext
  val s0_outIsS64 = !outIsFpNext && output1H(3) && hasSignIntNext
  val s0_fpCanonicalNAN = isFpToVecInst & inIsFpNext & (input1H(1) & !src.head(48).andR | input1H(2) & !src.head(32).andR)

  val s1_isInt2Fp = RegEnable(isInt2FpNext, false.B, fire)
  val s1_isEstimate7 = RegEnable(isEstimate7Next, false.B, fire)
  val s1_isFPsrc = isFpWiden || isFpNarrow || isFpCrossHigh || isFpCrossLow || isFp2Int || isFroundReg || isFcvtmodReg
  val s1_outIsFP = RegEnable(outIsFpNext, fire)
  val s1_outIsF64 = RegEnable(s0_outIsF64, fire)
  val s1_outIsF32 = RegEnable(s0_outIsF32, fire)
  val s1_outIsF16 = RegEnable(s0_outIsF16, fire)
  val s1_outIsU32 = RegEnable(s0_outIsU32, fire)
  val s1_outIsS32 = RegEnable(s0_outIsS32, fire)
  val s1_outIsU64 = RegEnable(s0_outIsU64, fire)
  val s1_outIsS64 = RegEnable(s0_outIsS64, fire)
  val s1_fpCanonicalNAN = RegEnable(s0_fpCanonicalNAN, fire)

  val s2_outIsF64 = RegEnable(s1_outIsF64, fireReg)
  val s2_outIsFP = RegEnable(s1_outIsFP, fireReg)
  val s2_fpCanonicalNAN = RegEnable(s1_fpCanonicalNAN, fireReg)
  val s2_isInt2Fp = RegEnable(s1_isInt2Fp, fireReg)
  val s2_isEstimate7 = RegEnable(s1_isEstimate7, fireReg)
  val s2_isFPsrc = RegEnable(s1_isFPsrc, fireReg)
  val s2_isFround = RegEnable(isFroundReg, fireReg)
  //inst FPTOINT and FPTOFP module
  val fpcvt = Module(new FP_INCVT(width))
  fpcvt.io.fire := fire
  fpcvt.io.src := src
  fpcvt.io.rm := rm
  fpcvt.io.opType := opType
  fpcvt.io.input1H := input1H
  fpcvt.io.output1H := output1H
  fpcvt.io.isFpToVecInst := isFpToVecInst
  fpcvt.io.isFround := isFround
  fpcvt.io.isFcvtmod := isFcvtmod

  val s1_resultForfpCanonicalNAN = Mux1H(
    Seq(s1_outIsF64, s1_outIsF32, s1_outIsF16, s1_outIsU32 || s1_outIsU64, s1_outIsS32, s1_outIsS64),
    Seq(~0.U((f64.expWidth+1).W) ## 0.U((f64.fracWidth-1).W),
      ~0.U((f32.expWidth+1).W) ## 0.U((f32.fracWidth-1).W),
      ~0.U((f16.expWidth+1).W) ## 0.U((f16.fracWidth-1).W),
      ~0.U(64.W),
      ~0.U(31.W),
      ~0.U(63.W))
  )
  val s2_resultForfpCanonicalNAN = RegEnable(s1_resultForfpCanonicalNAN, fireReg)
  if(isVectorCvt){//inst INTTOFP and ESTMATE module
    val int2fp = Module(new INT2FP(width))
    int2fp.io.fire := fire
    int2fp.io.src := src
    int2fp.io.rm := rm
    int2fp.io.opType := opType
    int2fp.io.input1H := input1H
    int2fp.io.output1H := output1H
    val estmate7 = Module(new Estimate7(width))
    estmate7.io.fire := fire
    estmate7.io.src := src
    estmate7.io.rm := rm
    estmate7.io.opType := opType
    estmate7.io.input1H := input1H
    estmate7.io.output1H := output1H
    //result
    val result = Mux1H(Seq(
      s2_isInt2Fp -> int2fp.io.result,
      s2_isFPsrc -> fpcvt.io.result,
      s2_isEstimate7 -> estmate7.io.result
    ))
    val fflags = Mux1H(Seq(
      s2_isInt2Fp -> int2fp.io.fflags,
      s2_isFPsrc -> fpcvt.io.fflags,
      s2_isEstimate7 -> estmate7.io.fflags
    ))
    io.result := Mux(s2_fpCanonicalNAN, s2_resultForfpCanonicalNAN, result)
    io.fflags := Mux(s2_fpCanonicalNAN && !s2_outIsFP, "b10000".U, fflags)
  }else{
    val result = fpcvt.io.result
    val fflags = fpcvt.io.fflags
    io.result := Mux(s2_fpCanonicalNAN, s2_resultForfpCanonicalNAN, result)
    io.fflags := Mux(s2_fpCanonicalNAN && !s2_outIsFP, "b10000".U, Mux(s2_fpCanonicalNAN && s2_isFround, 0.U, fflags))
  }
}
class CVT_IO(width: Int) extends Bundle{
  val fire = Input(Bool())
  val src = Input(UInt(width.W))
  val opType = Input(UInt(8.W))
  val rm = Input(UInt(3.W))
  val input1H = Input(UInt(4.W))
  val output1H = Input(UInt(4.W))
  val isFpToVecInst = Input(Bool())
  val isFround = Input(UInt(2.W))
  val isFcvtmod = Input(Bool())
  val result = Output(UInt(width.W))
  val fflags = Output(UInt(5.W))
}
class INTCVT_IO(width: Int) extends Bundle{
  val fire = Input(Bool())
  val src = Input(UInt(width.W))
  val opType = Input(UInt(8.W))
  val rm = Input(UInt(3.W))
  val input1H = Input(UInt(4.W))
  val output1H = Input(UInt(4.W))
  val result = Output(UInt(width.W))
  val fflags = Output(UInt(5.W))
}
class FP_INCVT(width: Int) extends Module {
  val io = IO(new CVT_IO(width: Int))
  val intParamMap = (0 to 3).map(i => (1 << i) * 8)
  val widthExpAdder = 13 // 13bits is enough
  //input
  val (fire, src, opType, rmNext, input1H, output1H, isFpToVecInst, isFround, isFcvtmod) =
    (io.fire, io.src, io.opType, io.rm, io.input1H, io.output1H, io.isFpToVecInst, io.isFround, io.isFcvtmod)
  val fireReg = GatedValidRegNext(fire)

  val isWiden = !opType(4) && opType(3)
  val isNarrow = opType(4) && !opType(3)
  val outIsFpNext = opType.tail(1).head(1).asBool
  val outIsF16 = outIsFpNext && output1H(1)
  val outIsF64 = outIsFpNext && output1H(3)
  val isCrossHigh = opType(4) && opType(3) && outIsF64
  val isCrossLow = opType(4) && opType(3) && outIsF16
  val hasSignIntNext = opType(0).asBool
  val float1HSrcNext = input1H.head(3)//exclude f8
  val float1HOutNext = output1H.head(3)//exclude f8

  val isFroundOrFroundnxNext = isFround.orR
  val isFroundnxNext = isFround(1)

  //fp input extend
  val srcMap = (0 to 3).map(i => src((1 << i) * 8 - 1, 0))
  val floatMap = srcMap.zipWithIndex.map{case (float,i) => floatExtend(float, i)}.drop(1)
  val input = Mux1H(float1HSrcNext, floatMap)
  val expSrcNext = input.tail(1).head(f64.expWidth)
  val fracSrc = input.tail(f64.expWidth+1).head(f64.fracWidth)
  val signSrcNext = input.head(1).asBool
  val decodeFloatSrc = Mux1H(float1HSrcNext, fpParam.fpMap.map(fp =>
    VecInit(expSrcNext(fp.expWidth-1,0).orR, expSrcNext(fp.expWidth-1,0).andR, fracSrc.head(fp.fracWidth).orR).asUInt
  )
  )
  val (expNotZeroSrcNext, expIsOnesSrcNext, fracNotZeroSrcNext) = (decodeFloatSrc(0), decodeFloatSrc(1), decodeFloatSrc(2))
  val isNaNSrcNext = expIsOnesSrcNext && fracNotZeroSrcNext
  val isZeroSrcNext = !expNotZeroSrcNext && !fracNotZeroSrcNext
  val isSubnormalSrcNext = !expNotZeroSrcNext && fracNotZeroSrcNext
  val isnormalSrcNext = !expIsOnesSrcNext && expNotZeroSrcNext
  val isInfSrcNext = expIsOnesSrcNext && !fracNotZeroSrcNext
  val isSNaNSrcNext = isNaNSrcNext && !fracSrc.head(1)

  val (isFpWidenNext, isFpNarrowNext, isFp2IntNext, isFpCrossHighNext, isFpCrossLowNext) =
    (outIsFpNext && isWiden, outIsFpNext && isNarrow, !outIsFpNext,
      outIsFpNext && isCrossHigh, outIsFpNext && isCrossLow)

  val froundOrFroundnxIsZeroOrInfNext = isFroundOrFroundnxNext && (isZeroSrcNext || isInfSrcNext)

  //s1
  val expNotZeroSrc = RegEnable(expNotZeroSrcNext, false.B, fire)
  val expIsOnesSrc = RegEnable(expIsOnesSrcNext, false.B, fire)
  val fracNotZeroSrc = RegEnable(fracNotZeroSrcNext, false.B, fire)
  val isInfSrc = RegEnable(isInfSrcNext, false.B, fire)
  val isZeroSrc = RegEnable(isZeroSrcNext, false.B, fire)
  val isSubnormalSrc = RegEnable(isSubnormalSrcNext, false.B, fire)
  val isnormalSrc = RegEnable(isnormalSrcNext, false.B, fire)
  val isSNaNSrc = RegEnable(isSNaNSrcNext, false.B, fire)

  val isFpWiden = RegEnable(isFpWidenNext, false.B, fire)
  val isFpNarrow = RegEnable(isFpNarrowNext, false.B, fire)
  val isFp2Int = RegEnable(isFp2IntNext, false.B, fire)
  val isFpCrossHigh = RegEnable(isFpCrossHighNext, false.B, fire)
  val isFpCrossLow = RegEnable(isFpCrossLowNext, false.B, fire)
  val isNaNSrc = RegEnable(isNaNSrcNext, false.B, fire)
  val s0_fpCanonicalNAN = isFpToVecInst & (input1H(1) & !src.head(48).andR | input1H(2) & !src.head(32).andR)
  val s1_fpCanonicalNAN = RegEnable(s0_fpCanonicalNAN, fire)

  val isFroundnxReg = RegEnable(isFroundnxNext, false.B, fire)
  val isFroundOrFroundnxReg = RegEnable(isFroundOrFroundnxNext, false.B, fire)
  val isFcvtmodReg = RegEnable(isFcvtmod, false.B, fire)
  val froundOrFroundnxIsZeroOrInf = RegEnable(froundOrFroundnxIsZeroOrInfNext, false.B, fire)

  // for fpnarrow sub
  val trunSticky = RegEnable(fracSrc.tail(f32.fracWidth).orR, false.B, fire)
  val signSrc = RegEnable(signSrcNext, false.B, fire)
  val rm = RegEnable(rmNext, 0.U(3.W), fire)
  val hasSignInt = RegEnable(hasSignIntNext, false.B, fire)
  val signNonNan = !isNaNSrc && signSrc

  val output1HReg = RegEnable(output1H, 0.U(4.W), fire)
  val float1HOut = Wire(UInt(3.W))
  float1HOut := output1HReg.head(3)
  val int1HOut = Wire(UInt(4.W))
  int1HOut := output1HReg

  //output
  val nv, dz, of, uf, nx = Wire(Bool()) //cycle1
  val fflagsNext = Wire(UInt(5.W))
  val fflags = RegEnable(fflagsNext, 0.U(5.W), fireReg)
  val resultNext = Wire(UInt(width.W))
  val result = RegEnable(resultNext, 0.U(width.W), fireReg)

  //exp
  val expAdderIn0Next = Wire(UInt(widthExpAdder.W)) //13bits is enough
  val expAdderIn1Next = Wire(UInt(widthExpAdder.W))
  val expAdderIn0 = RegEnable(expAdderIn0Next, fire)
  val expAdderIn1 = RegEnable(expAdderIn1Next, fire)
  val exp = Wire(UInt(widthExpAdder.W))
  exp := expAdderIn0 + expAdderIn1

  val leadZerosNext = CLZ((fracSrc<<(64 - f64.fracWidth)).asUInt)
  expAdderIn0Next := Mux1H(Seq(
    isFpWidenNext -> Mux1H(float1HOutNext.head(2), fpParam.biasDeltaMap.take(2).map(delta => delta.U)),
    isFpCrossHighNext -> fpParam.biasDeltaMap(2).U,
    (isFpNarrowNext || isFpCrossLowNext) -> Mux(isSubnormalSrcNext, false.B ## 1.U, false.B ## expSrcNext),
    isFp2IntNext -> Mux(isSubnormalSrcNext, false.B ## 1.U, false.B ## expSrcNext)
  ))
  val biasDelta = Mux1H(float1HOutNext.tail(1), fpParam.biasDeltaMap.take(2).map(delta => delta.U))
  val bias =  Mux1H(float1HSrcNext, fpParam.fpMap.map(fp => fp.bias.U))
  val minusExp = extend((~(false.B ## Mux1H(
    Seq(
      (isFpWidenNext || isFpCrossHighNext) -> leadZerosNext,
      isFpNarrowNext -> biasDelta,
      isFpCrossLowNext -> fpParam.biasDeltaMap(2).U,
      isFp2IntNext -> bias
    )))).asUInt
    + 1.U, widthExpAdder).asUInt
  expAdderIn1Next := Mux1H(
    Cat(isFpNarrowNext || isFp2IntNext || isFpCrossLowNext, isFpWidenNext || isFpCrossHighNext).asBools.reverse,
    Seq(
      minusExp,
      Mux(isSubnormalSrcNext, minusExp, expSrcNext),
    )
  )
  //frac
  val fracSrcLeftNext = Wire(UInt(64.W))
  fracSrcLeftNext := fracSrc << (64 - f64.fracWidth)
  val leadZeros = RegEnable(leadZerosNext, fire)
  val fracSrcLeft = RegEnable(fracSrcLeftNext, 0.U(64.W), fire)
  val shiftLeft = Wire(UInt(64.W))
  shiftLeft := (fracSrcLeft.asUInt << 1) << leadZeros //cycle1
  val fracNormaled =  Wire(UInt(64.W))
  fracNormaled := Mux(isSubnormalSrc, shiftLeft, fracSrcLeft) //cycle1

  /** shift right
   * for: fp->fp Narrow, fp->int
   * cycle: 1
   *
   */

  // common
  val fracValueSrc = (expNotZeroSrcNext && !expIsOnesSrcNext) ## fracSrc
  val shamtInNext = fracValueSrc ## 0.U(11.W) ## false.B  //fp Narrow & fp->int
  val shamtWidth = Mux(!outIsFpNext || isFroundOrFroundnxNext, Mux1H(float1HSrcNext, fpParam.fpMap.map(fp => (63+fp.bias).U)),
    Mux(isCrossLow, (fpParam.biasDeltaMap(2) + 1).U, Mux1H(float1HOutNext.tail(1), fpParam.biasDeltaMap.take(2).map(delta => (delta + 1).U)))
  ) - expSrcNext
  val shamtNext = Mux(shamtWidth >= 65.U, 65.U, shamtWidth)

  val shamtIn = RegEnable(shamtInNext, fire)
  val shamt = RegEnable(shamtNext, fire)
  val (inRounderTmp, stickyTmp) = ShiftRightJam(shamtIn, shamt)
  val inRounder = Wire(UInt(65.W))
  val sticky = Wire(Bool())
  inRounder := inRounderTmp
  sticky := stickyTmp

  /**
   * fround
   * frac
   * cycle: 0
   */

  val froundMaxExpNext = Mux1H(float1HOutNext, fpParam.fpMap.map(fp => fp.froundMaxExp.U))
  val froundExpGreaterThanMaxExpNext = expSrcNext >= froundMaxExpNext

  val fracShiftMaskNext = Mux1H(float1HOutNext, fpParam.fpMap.map(fp => fp.froundShiftMask.U)) - expSrcNext

  val froundExpLessThanBiasNext = Mux1H(float1HOutNext, fpParam.fpMap.map(fp => !expSrcNext(fp.expWidth-1) && !expSrcNext(fp.expWidth-2, 0).andR))

  val froundExpLessThanBias = RegEnable(froundExpLessThanBiasNext, false.B, fire)
  val froundExpGreaterThanMaxExp = RegEnable(froundExpGreaterThanMaxExpNext, false.B, fire)
  val fracShiftMask = RegEnable(fracShiftMaskNext, 0.U, fire)
  val froundOldExp = RegEnable(expSrcNext, 0.U, fire)
  val froundOldFrac = RegEnable(fracSrc, 0.U, fire)

  // cycle1
  val froundShiftMask = Wire(UInt(64.W))
  val froundUpShiftMask = Wire(UInt(64.W))
  val froundOldInput = Wire(UInt(64.W))
  val froundUpInput = Wire(UInt(64.W))

  froundShiftMask := Mux1H(
    (0 until (1 << 6)).map(i =>
      (i.U === fracShiftMask) -> Cat(~0.U((64-i).W), 0.U(i.W)))
  )
  froundUpShiftMask := Mux1H(
    (0 until (1 << 6)).map(i =>
      (i.U === fracShiftMask) -> Cat(1.U((64-i).W), 0.U(i.W)))
  )
  froundOldInput := Cat(signSrc, froundOldExp, froundOldFrac) & froundShiftMask
  froundUpInput := froundOldInput + froundUpShiftMask


  /**
   * fcvtmod
   * cycle: 0
   */
  val fcvtmodExpGreatThanShiftMask = expSrcNext >= f64.froundShiftMask.U

  val fcvtmodNx         = WireInit(false.B)
  val fcvtmodFrac       = Wire(UInt(64.W))
  val fcvtmodTrueExp    = Wire(UInt(f64.expWidth.W))
  val fcvtmodShiftDelta = Wire(UInt(f64.expWidth.W))
  val fcvtmodShift      = Wire(UInt(f64.expWidth.W))
  val fcvtmodShiftLeftMask = Wire(UInt(f64.expWidth.W))

  val fcvtmodFracTmp = 0.U(11.W) ## fracValueSrc

  val fcvtmodExpLessThanBiasNext = !expSrcNext(f64.expWidth-1) && !expSrcNext(f64.expWidth-2, 0).andR

  fcvtmodTrueExp    := expSrcNext - f64.bias.U
  fcvtmodShiftDelta := expSrcNext - f64.froundShiftMask.U
  fcvtmodShift      := Mux(fcvtmodExpGreatThanShiftMask, fcvtmodShiftDelta, (~fcvtmodShiftDelta).asUInt + 1.U)
  fcvtmodShiftLeftMask := expSrcNext - 1011.U // expSrcNext - f64.froundShiftMask.U + 64.U

  when (fcvtmodExpGreatThanShiftMask && fcvtmodShift.head(5).orR) {
    fcvtmodFrac := 0.U
  }.elsewhen(fcvtmodExpGreatThanShiftMask && !fcvtmodShift.head(5).orR) {
    fcvtmodFrac := Mux1H(
      (0 until (1 << 6)).map(i =>
        (i.U === fcvtmodShift) -> Cat(fcvtmodFracTmp(63 - i, 0), 0.U(i.W))
      )
    )
  }.elsewhen(!fcvtmodExpGreatThanShiftMask && !fcvtmodShift.head(5).orR) {
    fcvtmodFrac := fcvtmodFracTmp >> fcvtmodShift
    fcvtmodNx := Mux1H(
      (0 until (1 << 6)).map(i =>
        (i.U === fcvtmodShiftLeftMask) -> Cat(fcvtmodFracTmp(63 - i, 0), 0.U(i.W))
      )
    ).orR
  }.otherwise {
    fcvtmodFrac := 0.U
    fcvtmodNx := true.B
  }

  val fcvtmodFracReg    = RegEnable(fcvtmodFrac, 0.U, fire)
  val fcvtmodTrueExpReg = RegEnable(fcvtmodTrueExp, 0.U, fire)
  val fcvtmodNxReg      = RegEnable(fcvtmodNx, false.B, fire)
  val fcvtmodExpLessThanBias = RegEnable(fcvtmodExpLessThanBiasNext, false.B, fire)

  /** rounder
   * for: int->fp, fp-fp Narrow, fp->int
   * cycle: 1
   */
  val rounderMapIn = Wire(UInt(64.W))
  rounderMapIn := Mux(isFpNarrow || isFpCrossLow, fracSrcLeft, shiftLeft)

  val rounderMap =
    fpParam.fpMap.map(fp => Seq(
      rounderMapIn.head(fp.fracWidth),
      rounderMapIn.tail(fp.fracWidth).head(1),
      rounderMapIn.tail(fp.fracWidth + 1).orR,
      rounderMapIn.head(fp.fracWidth).andR
    )
    ).transpose

  val (rounderInputMap, rounerInMap, rounderStikyMap, isOnesRounderInputMap) = {
    (rounderMap(0), rounderMap(1), rounderMap(2), rounderMap(3))
  }

  val selectInRounder = isFp2Int || isFroundOrFroundnxReg
  val rounderInput = Mux(selectInRounder, inRounder.head(64),  Mux1H(float1HOut, rounderInputMap))
  val rounderRoundIn = Mux(selectInRounder, inRounder(0), Mux1H(float1HOut, rounerInMap))
  val rounderStickyIn = Mux(selectInRounder, sticky, Mux1H(float1HOut, rounderStikyMap))

  val rounder = Module(new RoundingUnit(64))
  rounder.io.in := rounderInput
  rounder.io.roundIn := rounderRoundIn
  rounder.io.stickyIn := rounderStickyIn
  rounder.io.signIn := signSrc
  rounder.io.rm := rm

  // from rounder
  val nxRounded = rounder.io.inexact
  val upRounded = rounder.io.r_up
  val expIncrease = exp + 1.U
  val rounderInputIncrease = rounderInput + 1.U
  // for fp2int
  // 8bit: => u64, i64, u32, i32, u16, i16, u8, i8
  val hasSignInt1HOut = int1HOut.asBools.map(oh => Seq(oh && !hasSignInt, oh && hasSignInt)).flatten
  val isOnesRounderInputMapFp2Int =
    intParamMap.map(intType => Seq(intType, intType - 1)).flatten.map(intType => rounderInput.tail(64 - intType).andR)
  val cout = upRounded && Mux(isFp2Int,
    Mux1H(hasSignInt1HOut, isOnesRounderInputMapFp2Int),
    Mux1H(float1HOut, isOnesRounderInputMap)
  ).asBool
  val expRounded = Wire(UInt(f64.expWidth.W))
  expRounded := Mux(cout, expIncrease, exp)
  val fracRounded = Mux(upRounded, rounderInputIncrease, rounderInput)
  val rmin =
    rm === RTZ || (signSrc && rm === RUP) || (!signSrc && rm === RDN) //cycle1
  when(isFpWiden || isFpCrossHigh){
    /** fp -> fp widen/ fp16 -> fp64 cross high
     */
    def fpWidenResultMapGen(fp: FloatFormat): Seq[UInt] = {
      VecInit((0 to 2).map {
        case 0 => signNonNan ## ~0.U(fp.expWidth.W) ## fracNotZeroSrc ## 0.U((fp.fracWidth - 1).W) // INF or NaN->QNAN
        case 1 => signNonNan ## 0.U((fp.width - 1).W) // 0
        case 2 => signNonNan ## exp(fp.expWidth - 1, 0) ## fracNormaled.head(fp.fracWidth)
      })
    }
    val result1H = Cat(
      expIsOnesSrc,
      isZeroSrc,
      isSubnormalSrc || isnormalSrc
    )
    nv := isSNaNSrc && !s1_fpCanonicalNAN
    dz := false.B
    of := false.B
    uf := false.B
    nx := false.B

    val fpwidenResultMap: Seq[UInt] = Seq(f32, f64).map(fp => Mux1H(result1H.asBools.reverse, fpWidenResultMapGen(fp)))
    resultNext := Mux1H(float1HOut.head(2), fpwidenResultMap)
  }.elsewhen(isFpNarrow || isFpCrossLow){
    /** fp -> fp Narrow / fp64 -> fp16 cross low
     * note: IEEE754 uf：exp in (-b^emin, b^emin), after rounding(RiscV!!!)
     * note: IEEE754 uf：exp in (-b^emin, b^emin), before rounding(other)
     */
    // Mux(cout, exp > FP.maxExp -1, exp > FP.maxExp)
    val ofRounded = !exp.head(1).asBool && Mux1H(float1HOut,
      fpParam.fpMap.map(fp => Mux(cout,
        exp(fp.expWidth - 1, 1).andR || exp(exp.getWidth - 2, fp.expWidth).orR,
        exp(fp.expWidth - 1, 0).andR || exp(exp.getWidth - 2, fp.expWidth).orR)
      )
    )
    //val ufExpRounded = Mux(cout, interExp < 0.S, interExp < 1.S)
    val ufExpRounded = Mux(cout, exp.head(1).asBool, exp.head(1).asBool || !exp.orR)
    val nxOfRounded = nxRounded || ofRounded
    /** dest is Subnormal
     * dest: 1-toBias, src: srcExp - srcBias
     * src->dest :exp = srcExp - srcBias + toBias
     */
    //val maybeSub = exp < 1.S
    val maybeSub = exp.head(1).asBool || !exp.orR
    val subFracRounded = Wire(UInt(f32.fracWidth.W))
    val subExpRounded = Wire(UInt(f32.expWidth.W))

    val (subFrac, shiftSticky) = (inRounder, sticky)
    val subRounderMap =
      Seq(f16, f32).map(fp => Seq(
        subFrac.tail(1).head(fp.fracWidth),
        subFrac.tail(fp.fracWidth+1).head(1),  //1+toFracWidth +1 => drop head & tail
        trunSticky || shiftSticky || subFrac.tail(fp.fracWidth+2).orR,
        subFrac.tail(1).head(fp.fracWidth).andR
      )
      ).transpose

    val (subRounderInputMap, subRounerInMap, subRounderStikyMap, subIsOnesRounderInputMap) = {
      (subRounderMap(0), subRounderMap(1), subRounderMap(2), subRounderMap(3))
    }

    val subRounder = Module(new RoundingUnit(f32.fracWidth))
    val subRounderInput = Mux1H(float1HOut.tail(1), subRounderInputMap)
    subRounder.io.in := subRounderInput
    subRounder.io.roundIn := Mux1H(float1HOut.tail(1), subRounerInMap)
    subRounder.io.stickyIn := Mux1H(float1HOut.tail(1), subRounderStikyMap)
    subRounder.io.signIn := signSrc
    subRounder.io.rm := rm
    // from roundingUnit
    val subNxRounded = subRounder.io.inexact
    val subUpRounded = subRounder.io.r_up
    // out of roundingUint
    subFracRounded := Mux(subUpRounded, subRounderInput + 1.U, subRounderInput)
    val subCout = subUpRounded && Mux1H(float1HOut.tail(1), subIsOnesRounderInputMap).asBool
    subExpRounded := Mux(subCout, 1.U, 0.U)

    nv := isSNaNSrc && !s1_fpCanonicalNAN
    dz := false.B
    of := !expIsOnesSrc && ofRounded && !s1_fpCanonicalNAN
    uf := !expIsOnesSrc && maybeSub && ufExpRounded && subNxRounded && !s1_fpCanonicalNAN
    nx := !expIsOnesSrc && (
      (!maybeSub && nxOfRounded) ||
        (maybeSub && subNxRounded)
      ) && !s1_fpCanonicalNAN

    val result1H = Cat(
      expIsOnesSrc,
      !expIsOnesSrc && !maybeSub && ofRounded && (rmin || (rm === RTO)),
      !expIsOnesSrc && !maybeSub && ofRounded && !(rmin || (rm === RTO)),
      !expIsOnesSrc && !maybeSub && !ofRounded,
      !expIsOnesSrc && maybeSub
    )
    def fpNarrowResultMapGen(fp: FloatFormat): Seq[UInt] ={
      VecInit((0 to 4).map {
        case 0 => signNonNan ## ~0.U(fp.expWidth.W) ## fracNotZeroSrc ## 0.U((fp.fracWidth - 1).W)  // INF or NaN->QNAN
        case 1 => signNonNan ## fp.maxExp.U(fp.expWidth.W) ## ~0.U(fp.fracWidth.W)                  // of => GNF
        case 2 => signNonNan ## (fp.maxExp + 1).U(fp.expWidth.W) ## 0.U(fp.fracWidth.W)             // of => INF
        case 3 => signNonNan ## expRounded(fp.expWidth - 1, 0) ## fracRounded(fp.fracWidth - 1, 0)  // normal
        case 4 => signNonNan ## subExpRounded(fp.expWidth - 1, 0) ## subFracRounded(fp.fracWidth - 1, 0) //sub or uf
      })
    }
    val fpNarrowResultMap: Seq[UInt] = Seq(f16, f32).map(fp => Mux1H(result1H.asBools.reverse, fpNarrowResultMapGen(fp)))
    resultNext := Mux1H(float1HOut.tail(1), fpNarrowResultMap)
  }.elsewhen(isFroundOrFroundnxReg) {
    val oldInputReg = Mux1H(float1HOut, fpParam.fpMap.map(fp => signSrc ## froundOldExp(fp.expWidth - 1, 0) ## froundOldFrac.head(fp.fracWidth)))

    nv := isSNaNSrc
    dz := false.B
    of := false.B
    uf := false.B
    nx := isFroundnxReg && nxRounded && !froundOrFroundnxIsZeroOrInf && !froundExpGreaterThanMaxExp

    val result1H = Cat(
      froundOrFroundnxIsZeroOrInf || froundExpGreaterThanMaxExp && !isNaNSrc,
      isNaNSrc,
      froundExpLessThanBias && !froundOrFroundnxIsZeroOrInf,
      !froundExpLessThanBias && !froundExpGreaterThanMaxExp,
    )

    def froundResultMapGen(fp: FloatFormat): Seq[UInt] = {
      VecInit((0 to 3).map {
        case 0 => oldInputReg
        case 1 => 0.U ## ~0.U(fp.expWidth.W) ## 1.U ## 0.U((fp.fracWidth - 1).W)
        case 2 => signSrc ## Mux(upRounded, 0.U ## Fill(fp.expWidth - 1, 1.U(1.W)), 0.U(fp.expWidth.W)) ## 0.U(fp.fracWidth.W)
        case 3 => Mux(upRounded,
          froundUpInput.head(1) ## froundUpInput.tail(1).head(f64.expWidth)(fp.expWidth - 1, 0) ## froundUpInput.tail(1 + f64.expWidth).head(fp.fracWidth),
          froundOldInput.head(1) ## froundOldInput.tail(1).head(f64.expWidth)(fp.expWidth - 1, 0) ## froundOldInput.tail(1 + f64.expWidth).head(fp.fracWidth))
      })
    }

    val froundResultMap: Seq[UInt] = fpParam.fpMap.map(fp => Mux1H(result1H.asBools.reverse, froundResultMapGen(fp)))
    resultNext := Mux1H(float1HOut, froundResultMap)
  }.elsewhen(isFcvtmodReg) {
    val expIsOf  = !fcvtmodExpLessThanBias && fcvtmodTrueExpReg.head(f64.expWidth-5).orR
    val fracIsOf = Mux(signSrc, fcvtmodFracReg.head(32).orR || !fcvtmodFracReg.head(32).orR && fcvtmodFracReg.tail(32).head(1).asBool && fcvtmodFracReg.tail(33).orR, fcvtmodFracReg.head(33).orR)
    val fcvtmodIsOf = expIsOf || fracIsOf

    val fcvtmodNx1H = Seq(
      !expNotZeroSrc,
      expIsOnesSrc,
      expNotZeroSrc && !expIsOnesSrc
    )

    val fcvtmodNxOut1H = Seq(
      fracNotZeroSrc,
      false.B,
      !fcvtmodIsOf && fcvtmodNxReg
    )

    nv := expIsOnesSrc || fcvtmodIsOf
    dz := false.B
    of := false.B
    uf := false.B
    nx := Mux1H(fcvtmodNx1H, fcvtmodNxOut1H)

    val fcvtmodResult = Mux(signSrc, (~fcvtmodFracReg).asUInt + 1.U, fcvtmodFracReg)

    resultNext := Cat(Fill(32, fcvtmodResult.tail(32).head(1).asBool), fcvtmodResult.tail(32))
  }.otherwise{
    /** out is int, any fp->any int/uint
     * drop the shift left!
     */
    val resultRounded = fracRounded
    val isZeroRounded = !resultRounded.orR
    val normalResult = Mux(signSrc && resultRounded.orR, (~resultRounded).asUInt + 1.U, resultRounded) //exclude 0
    // i=log2(intType)
    val ofExpRounded = !exp.head(1) && Mux1H(int1HOut,
      (3 to 6).map(i =>
        Mux1H(UIntToOH(hasSignInt ## cout), VecInit((0 to 3).map {
          case 0 => exp(exp.getWidth-2, i).orR                        //>=intType   unsign & non cout
          case 1 => exp(exp.getWidth-2, i).orR || exp(i-1, 0).andR    //>=intType-1 unsign & cout
          case 2 => exp(exp.getWidth-2, i).orR || exp(i-1, 0).andR    //>=intType-1 sign   & non cout
          case 3 => exp(exp.getWidth-2, i).orR || exp(i-1, 1).andR    //>=intType-2 sign   & cout
        })
        )
      )
    )
    val excludeFrac = Mux1H(int1HOut,
      intParamMap.map(intType => resultRounded(intType - 1) && !resultRounded(intType - 2, 0).orR)) // 10000***000
    // i=log2(intType)
    val excludeExp = Mux1H(int1HOut,
      (3 to 6).map(i => !exp.head(exp.getWidth - i).orR &&
        Mux(cout,
          exp(i-1, 1).andR && !exp(0), // ==inType-2
          exp(i-1, 0).andR             // ==inType-1
        )
      )
    )
    val toUnv = ofExpRounded || expIsOnesSrc || signSrc &&
      !(isZeroSrc || isZeroRounded && !ofExpRounded) //exclude 0 & -0 after rounding
    val toUnx = !toUnv && nxRounded
    val toInv = ofExpRounded && !(signSrc && excludeExp && excludeFrac) || expIsOnesSrc //nv has included inf & nan
    val toInx = !toInv && nxRounded

    nv := Mux(hasSignInt, toInv, toUnv)
    dz := false.B
    of := false.B
    uf := false.B
    nx := Mux(hasSignInt, toInx, toUnx)
    val result1H = Cat(
      (!hasSignInt && !toUnv) || (hasSignInt && !toInv), //toUnv include nan & inf
      !hasSignInt && toUnv && (isNaNSrc || !signSrc && (isInfSrc || ofExpRounded)),
      !hasSignInt && toUnv && signSrc && !isNaNSrc,
      hasSignInt && toInv,
    )
    resultNext := Mux1H(result1H.asBools.reverse, Seq(
      normalResult,
      (~0.U(64.W)).asUInt,
      0.U(64.W),
      Mux1H(int1HOut, intParamMap.map(intType => signNonNan ## Fill(intType - 1, !signNonNan)))
    )
    )
  }
  fflagsNext := Cat(nv, dz, of, uf, nx)
  io.result := result
  io.fflags := fflags

}
class INT2FP(width: Int) extends Module{
  val io = IO(new INTCVT_IO(width: Int))
  val widthExpAdder = 13 // 13bits is enough
  //input
  val (fire, src, opType, rmNext, input1H, output1H) =
    (io.fire, io.src, io.opType, io.rm, io.input1H, io.output1H)
  val fireReg = GatedValidRegNext(fire)
  val hasSignIntNext = opType(0).asBool
  val int1HSrcNext = input1H
  val float1HOutNext = output1H.head(3)//exclude f8
  val output1HReg = RegEnable(output1H, 0.U(4.W), fire)
  val float1HOut = Wire(UInt(3.W))
  float1HOut := output1HReg.head(3)
  val srcMap = (0 to 3).map(i => src((1 << i) * 8 - 1, 0))
  val intMap = srcMap.map(int => intExtend(int, hasSignIntNext && int.head(1).asBool))
  val input = Mux1H(int1HSrcNext, intMap)
  val signSrcNext = input.head(1).asBool
  val signSrc = RegEnable(signSrcNext, false.B, fire)
  val rm = RegEnable(rmNext, 0.U(3.W), fire)
  // src is int
  val absIntSrcNext = Wire(UInt(64.W)) //cycle0
  absIntSrcNext := Mux(signSrcNext, (~input.tail(1)).asUInt + 1.U, input.tail(1))
  val isZeroIntSrcNext = !absIntSrcNext.orR
  val isZeroIntSrc = RegEnable(isZeroIntSrcNext, false.B, fire)
  //CLZ
  val clzIn = absIntSrcNext.asUInt
  val leadZerosNext = CLZ(clzIn)
  //exp
  val expAdderIn0Next = Wire(UInt(widthExpAdder.W)) //13bits is enough
  val expAdderIn1Next = Wire(UInt(widthExpAdder.W))
  val expAdderIn0 = RegEnable(expAdderIn0Next, fire)
  val expAdderIn1 = RegEnable(expAdderIn1Next, fire)
  val minusExp = extend((~(false.B ## leadZerosNext)).asUInt
    + 1.U, widthExpAdder).asUInt
  expAdderIn0Next := Mux1H(float1HOutNext, fpParam.fpMap.map(fp => (fp.bias + 63).U))
  expAdderIn1Next := minusExp
  val exp = Wire(UInt(widthExpAdder.W))
  exp := expAdderIn0 + expAdderIn1
  //frac
  val absIntSrc = RegEnable(absIntSrcNext, fire)
  val leadZeros = RegEnable(leadZerosNext, fire)
  val shiftLeft = Wire(UInt(64.W))
  shiftLeft := (absIntSrc.asUInt << 1) << leadZeros //cycle1
  //round
  val rounderMapIn = Wire(UInt(64.W))
  rounderMapIn := shiftLeft
  val rounderMap =
    fpParam.fpMap.map(fp => Seq(
      rounderMapIn.head(fp.fracWidth),
      rounderMapIn.tail(fp.fracWidth).head(1),
      rounderMapIn.tail(fp.fracWidth + 1).orR,
      rounderMapIn.head(fp.fracWidth).andR
    )
    ).transpose
  val (rounderInputMap, rounerInMap, rounderStikyMap, isOnesRounderInputMap) = {
    (rounderMap(0), rounderMap(1), rounderMap(2), rounderMap(3))
  }
  val rounderInput = Mux1H(float1HOut, rounderInputMap)
  val rounder = Module(new RoundingUnit(64))
  rounder.io.in := rounderInput
  rounder.io.roundIn := Mux1H(float1HOut, rounerInMap)
  rounder.io.stickyIn := Mux1H(float1HOut, rounderStikyMap)
  rounder.io.signIn := signSrc
  rounder.io.rm := rm
  val expIncrease = exp + 1.U
  val rounderInputIncrease = rounderInput + 1.U
  // from rounder
  val nxRounded = rounder.io.inexact
  val upRounded = rounder.io.r_up
  val cout = upRounded && Mux1H(float1HOut, isOnesRounderInputMap).asBool
  val expRounded = Wire(UInt(f64.expWidth.W))
  expRounded := Mux(cout, expIncrease, exp)
  val fracRounded = Mux(upRounded, rounderInputIncrease, rounderInput)
  val rmin =
    rm === RTZ || (signSrc && rm === RUP) || (!signSrc && rm === RDN) //cycle1
  /** int->fp   any int/uint-> any fp
   */
  // Mux(cout, exp > FP.maxExp -1, exp > FP.maxExp)
  val ofRounded = !exp.head(1).asBool && Mux1H(float1HOut,
    fpParam.fpMap.map(fp => Mux(cout,
      exp(fp.expWidth - 1, 1).andR || exp(exp.getWidth - 2, fp.expWidth).orR,
      exp(fp.expWidth - 1, 0).andR || exp(exp.getWidth - 2, fp.expWidth).orR)
    )
  )
  val nv, dz, of, uf, nx = Wire(Bool()) //cycle1
  val fflagsNext = Wire(UInt(5.W))
  val fflags = RegEnable(fflagsNext, 0.U(5.W), fireReg)
  val resultNext = Wire(UInt(64.W))
  val result = RegEnable(resultNext, 0.U(64.W), fireReg)
  nv := false.B
  dz := false.B
  of := ofRounded
  uf := false.B
  nx := ofRounded || nxRounded

  val result1H = Cat(
    ofRounded && rmin,
    ofRounded && !rmin,
    isZeroIntSrc,
    !ofRounded && !isZeroIntSrc
  )
  def int2FpResultMapGen(fp: FloatFormat): Seq[UInt] = {
    VecInit((0 to 3).map {
      case 0 => signSrc ## fp.maxExp.U(fp.expWidth.W) ## ~0.U(fp.fracWidth.W) //GNF
      case 1 => signSrc ## ~0.U(fp.expWidth.W) ## 0.U(fp.fracWidth.W) // INF
      case 2 => signSrc ## 0.U((fp.width - 1).W) // 0
      case 3 => signSrc ## expRounded(fp.expWidth-1, 0) ## fracRounded(fp.fracWidth-1, 0) // normal
    })
  }
  val int2FpResultMap: Seq[UInt] = fpParam.fpMap.map(fp => Mux1H(result1H.asBools.reverse, int2FpResultMapGen(fp)))
  resultNext := Mux1H(float1HOut, int2FpResultMap)
  //output
  fflagsNext := Cat(nv, dz, of, uf, nx)
  io.result := result
  io.fflags := fflags
}
class Estimate7(width: Int) extends Module{
  /** Estimate7: sqrt7 & rec7
 */
  val io = IO(new INTCVT_IO(width: Int))
  val widthExpAdder = 13 // 13bits is enough
  //input
  val (fire, src, opType, rmNext, input1H, output1H) =
    (io.fire, io.src, io.opType, io.rm, io.input1H, io.output1H)
  val fireReg = GatedValidRegNext(fire)
  val int1HSrcNext = input1H
  val float1HSrcNext = input1H.head(3)//exclude f8
  val int1HOutNext = output1H
  val float1HOutNext = output1H.head(3)//exclude f8
  val srcMap = (0 to 3).map(i => src((1 << i) * 8 - 1, 0))
  val floatMap = srcMap.zipWithIndex.map{case (float,i) => floatExtend(float, i)}.drop(1)
  val input = Mux1H(float1HSrcNext, floatMap)
  val signSrcNext = input.head(1).asBool
  val isEstimate7Next = opType(5)
  val isRecNext = opType(5) && opType(0)
  val rm = RegEnable(rmNext, 0.U(3.W), fire)
  val expSrcNext = input.tail(1).head(f64.expWidth)
  val fracSrc = input.tail(f64.expWidth+1).head(f64.fracWidth)
  val decodeFloatSrc = Mux1H(float1HSrcNext, fpParam.fpMap.map(fp =>
    VecInit(expSrcNext(fp.expWidth-1,0).orR, expSrcNext(fp.expWidth-1,0).andR, fracSrc.head(fp.fracWidth).orR).asUInt
  )
  )
  val (expNotZeroSrcNext, expIsOnesSrcNext, fracNotZeroSrcNext) = (decodeFloatSrc(0), decodeFloatSrc(1), decodeFloatSrc(2))
  val isSubnormalSrcNext = !expNotZeroSrcNext && fracNotZeroSrcNext
  val isZeroSrcNext = !expNotZeroSrcNext && !fracNotZeroSrcNext
  val isInfSrcNext = expIsOnesSrcNext && !fracNotZeroSrcNext
  val isNaNSrcNext = expIsOnesSrcNext && fracNotZeroSrcNext
  val isSNaNSrcNext = isNaNSrcNext && !fracSrc.head(1)
  val isQNaNSrcNext = isNaNSrcNext && fracSrc.head(1).asBool
  val isSubnormalRec2Next = isSubnormalSrcNext && !fracSrc.head(2).orR

  val expIsOnesSrc = RegEnable(expIsOnesSrcNext, false.B, fire)
  val isSubnormalSrc = RegEnable(isSubnormalSrcNext, false.B, fire)
  val isRec = RegEnable(isRecNext, false.B, fire)
  val isSNaNSrc = RegEnable(isSNaNSrcNext, false.B, fire)
  val signSrc = RegEnable(signSrcNext, false.B, fire)
  val isZeroSrc = RegEnable(isZeroSrcNext, false.B, fire)
  val isQNaNSrc = RegEnable(isQNaNSrcNext, false.B, fire)
  val isSubnormalRec2 = RegEnable(isSubnormalRec2Next, false.B, fire)
  val isInfSrc = RegEnable(isInfSrcNext, false.B, fire)
  val isNaNSrc = RegEnable(isNaNSrcNext, false.B, fire)

  val decodeFloatSrcRec = Mux1H(float1HSrcNext,
    fpParam.fpMap.map(fp => expSrcNext(fp.expWidth - 1, 0)).zip(fpParam.fpMap.map(fp => fp.expWidth)).map { case (exp, expWidth) =>
      VecInit(
        exp.head(expWidth-1).andR && !exp(0),
        exp.head(expWidth-2).andR && !exp(1) && exp(0)
      ).asUInt
    }
  )
  val (isNormalRec0Next, isNormalRec1Next) = (decodeFloatSrcRec(0), decodeFloatSrcRec(1))
  val isNormalRec2Next = expNotZeroSrcNext && !expIsOnesSrcNext && !isNormalRec0Next && !isNormalRec1Next
  val isSubnormalRec0Next = isSubnormalSrcNext && fracSrc.head(1).asBool
  val isSubnormalRec1Next = isSubnormalSrcNext && !fracSrc.head(1) && fracSrc.tail(1).head(1).asBool

  val isNormalRec0 = RegEnable(isNormalRec0Next, false.B, fire)
  val isNormalRec1 = RegEnable(isNormalRec1Next, false.B, fire)
  val isNormalRec2 = RegEnable(isNormalRec2Next, false.B, fire)
  val isSubnormalRec0 = RegEnable(isSubnormalRec0Next, false.B, fire)
  val isSubnormalRec1 = RegEnable(isSubnormalRec1Next, false.B, fire)

  val output1HReg = RegEnable(output1H, 0.U(4.W), fire)
  val float1HOut = Wire(UInt(3.W))
  float1HOut := output1HReg.head(3)
  val nv, dz, of, uf, nx = Wire(Bool()) //cycle1
  val fflagsNext = Wire(UInt(5.W))
  val fflags = RegEnable(fflagsNext, 0.U(5.W), fireReg)
  val resultNext = Wire(UInt(64.W))
  val result = RegEnable(resultNext, 0.U(64.W), fireReg)

  val clzIn = (fracSrc<<(64 - f64.fracWidth)).asUInt
  val leadZerosNext = CLZ(clzIn)
  val rmin =
    rm === RTZ || (signSrc && rm === RUP) || (!signSrc && rm === RDN) //cycle1
  //exp
  val expAdderIn0Next = Wire(UInt(widthExpAdder.W)) //13bits is enough
  val expAdderIn1Next = Wire(UInt(widthExpAdder.W))
  val expAdderIn0 = RegEnable(expAdderIn0Next, fire)
  val expAdderIn1 = RegEnable(expAdderIn1Next, fire)
  val minusExp = extend((~(false.B ## expSrcNext)).asUInt + 1.U, widthExpAdder).asUInt
  expAdderIn0Next := Mux1H(float1HOutNext, fpParam.fpMap.map(fp => Mux(isRecNext, (2 * fp.bias - 1).U, (3 * fp.bias - 1).U)))
  expAdderIn1Next := Mux(isSubnormalSrcNext, leadZerosNext, minusExp)
  val exp = Wire(UInt(widthExpAdder.W))
  exp := expAdderIn0 + expAdderIn1

  val expNormaled = Mux(isSubnormalSrcNext, leadZerosNext(0), expSrcNext(0)) //only the last bit is needed
  val expNormaled0 = RegEnable(expNormaled(0), false.B, fire)
  val fracSrcLeftNext = Wire(UInt(64.W))
  fracSrcLeftNext := fracSrc << (64 - f64.fracWidth)
  val fracSrcLeft = RegEnable(fracSrcLeftNext, 0.U(64.W), fire)
  val leadZeros = RegEnable(leadZerosNext, fire)

  val shiftLeft = Wire(UInt(64.W))
  shiftLeft := (fracSrcLeft.asUInt << 1) << leadZeros //cycle1
  val fracNormaled =  Wire(UInt(64.W))
  fracNormaled := Mux(isSubnormalSrc, shiftLeft, fracSrcLeft) //cycle1
  val rsqrt7Table = Module(new Rsqrt7Table)
  rsqrt7Table.src := expNormaled0 ## fracNormaled.head(6)
  val rec7Table = Module(new Rec7Table)
  rec7Table.src := fracNormaled.head(7)
  val fracEstimate = Mux(isRec, rec7Table.out, rsqrt7Table.out)

  nv := Mux(isRec, isSNaNSrc, (signSrc && !isZeroSrc && !isQNaNSrc) | isSNaNSrc)
  dz := isZeroSrc
  of := isRec && isSubnormalRec2
  uf := false.B
  nx := of
  def recResultMapGen(fp: FloatFormat): Seq[UInt] = {
    VecInit((0 to 6).map {
      case 0 => false.B ## ~0.U(fp.expWidth.W) ## true.B ## 0.U((fp.fracWidth - 1).W) //can
      case 1 => signSrc ## 0.U((fp.width - 1).W) //0
      case 2 => signSrc ## ~0.U(fp.expWidth.W) ## 0.U(fp.fracWidth.W) //INF
      case 3 => signSrc ## 0.U(fp.expWidth.W) ## 1.U(2.W) ## fracEstimate ## 0.U((fp.fracWidth - 2 - 7).W)
      case 4 => signSrc ## 0.U(fp.expWidth.W) ## 1.U(1.W) ## fracEstimate ## 0.U((fp.fracWidth - 1 - 7).W)
      case 5 => signSrc ## exp(fp.expWidth - 1, 0) ## fracEstimate ## 0.U((fp.fracWidth - 7).W)
      case 6 => signSrc ## fp.maxExp.U(fp.expWidth.W) ## ~0.U(fp.fracWidth.W) //GNF
    })
  }
  val recResult1H = Cat(
    isNaNSrc,
    isInfSrc,
    isZeroSrc || isSubnormalRec2 && !rmin,
    isNormalRec0,
    isNormalRec1,
    isNormalRec2 || isSubnormalRec0 || isSubnormalRec1,
    isSubnormalRec2 && rmin
  )
  val recResultMap: Seq[UInt] = fpParam.fpMap.map(fp => Mux1H(recResult1H.asBools.reverse, recResultMapGen(fp)))
  def sqrtResultMapGen(fp: FloatFormat): Seq[UInt] = {
    VecInit((0 to 3).map {
      case 0 => false.B ## ~0.U(fp.expWidth.W) ## true.B ## 0.U((fp.fracWidth - 1).W)
      case 1 => signSrc ## ~0.U(fp.expWidth.W) ## 0.U(fp.fracWidth.W)
      case 2 => signSrc ## exp(fp.expWidth, 1) ## fracEstimate ## 0.U((fp.fracWidth - 7).W) // exp/2 => >>1
      case 3 => 0.U(fp.width.W)
    })
  }
  val sqrtResult1H = Cat(
    signSrc & !isZeroSrc | isNaNSrc,
    isZeroSrc,
    !signSrc & !isZeroSrc & !expIsOnesSrc,
    !signSrc & isInfSrc,
  )
  val sqrtResultMap: Seq[UInt] = fpParam.fpMap.map(fp => Mux1H(sqrtResult1H.asBools.reverse, sqrtResultMapGen(fp)))
  resultNext := Mux(isRec, Mux1H(float1HOut, recResultMap), Mux1H(float1HOut, sqrtResultMap))

  fflagsNext := Cat(nv, dz, of, uf, nx)
  io.result := result
  io.fflags := fflags
}



