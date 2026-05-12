package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import yunsuan.vector.VectorConvert.util._
import yunsuan.vector.VectorConvert.utils._
import yunsuan.vector.VectorConvert.RoundingModle._
import yunsuan.util._
import yunsuan.encoding.Opcode.Opcodes.FCvtOpcode
import yunsuan.vector.Common._

class CVT64(width: Int = 64, isVectorCvt: Boolean, isI2F: Boolean = false) extends Module{
  val io = IO(new Bundle {
    val in = Input(new VCVT.In(width))
    val out = Output(new VCVT.Out(width))
  })
  val (fire, src, opType, rm, inSew1H, outSew1H, isScalarFpInst) =
    (io.in.fire, io.in.data.src, io.in.ctrl.opType, io.in.ctrl.rm, io.in.ctrl.inSew1H, io.in.ctrl.outSew1H, io.in.ctrl.isScalarFpInst)
  val fireReg = GatedValidRegNext(fire)

  val inIsFpNext       = FCvtOpcode.inIsFp(opType)
  val outIsFpNext      = FCvtOpcode.outIsFp(opType)
  val outIsIntNext     = FCvtOpcode.outIsInt(opType)
  val hasSignIntNext   = FCvtOpcode.isSignInt(opType)
  val hasUnSignIntNext = FCvtOpcode.isUnSignInt(opType)
  val isEstimate7Next  = FCvtOpcode.isEstimate7(opType)
  val isFround         = Cat(FCvtOpcode.isFroundNx(opType), FCvtOpcode.isFround(opType))
  val isFcvtmod        = FCvtOpcode.isFcvtMod(opType)

  val isInt2FpNext      = FCvtOpcode.isI2F(opType)
  val fpInputWidthNext  = FCvtOpcode.getInputDataWidth(opType)
  val fpOutputWidthNext = FCvtOpcode.getOutputDataWidth(opType)
  val isFpWidenNext     = FCvtOpcode.isF2F(opType) && fpOutputWidthNext > fpInputWidthNext
  val isFp2IntNext      = FCvtOpcode.isF2I(opType)

  val isFroundOrFroundnxNext = isFround.orR
  val isFpWiden = RegEnable(isFpWidenNext, false.B, fire)
  val isFp2Int = RegEnable(isFp2IntNext, false.B, fire)
  val isFroundReg = RegEnable(isFroundOrFroundnxNext, false.B, fire)
  val isFcvtmodReg = RegEnable(isFcvtmod, false.B, fire)

  val outIs64 = FCvtOpcode.outIs64(opType)
  val outIs32 = FCvtOpcode.outIs32(opType)
  val outIs16 = FCvtOpcode.outIs16(opType)
  val inIs16  = FCvtOpcode.inIs16(opType)
  val inIs32  = FCvtOpcode.inIs32(opType)

  val s0_outIsF64 = outIsFpNext && outIs64
  val s0_outIsF32 = outIsFpNext && outIs32
  val s0_outIsF16 = outIsFpNext && outIs16
  val s0_outIsU32 = outIsIntNext && outIs32 && hasUnSignIntNext
  val s0_outIsS32 = outIsIntNext && outIs32 && hasSignIntNext
  val s0_outIsU64 = outIsIntNext && outIs64 && hasUnSignIntNext
  val s0_outIsS64 = outIsIntNext && outIs64 && hasSignIntNext
  val s0_fpCanonicalNAN = isScalarFpInst & (inIsFpNext & (inIs16 & !src(63, 16).andR | inIs32 & !src(63, 32).andR))

  val s1_isInt2Fp = RegEnable(isInt2FpNext, false.B, fire)
  val s1_isEstimate7 = RegEnable(isEstimate7Next, false.B, fire)
  val s1_isFPsrc = isFpWiden || isFp2Int || isFroundReg || isFcvtmodReg
  val s1_outIsFP = RegEnable(outIsFpNext, fire)
  val s1_outIsF64 = RegEnable(s0_outIsF64, fire)
  val s1_outIsF32 = RegEnable(s0_outIsF32, fire)
  val s1_outIsF16 = RegEnable(s0_outIsF16, fire)
  val s1_outIsU32 = RegEnable(s0_outIsU32, fire)
  val s1_outIsS32 = RegEnable(s0_outIsS32, fire)
  val s1_outIsU64 = RegEnable(s0_outIsU64, fire)
  val s1_outIsS64 = RegEnable(s0_outIsS64, fire)
  val s1_fpCanonicalNAN = RegEnable(s0_fpCanonicalNAN, fire)

  val s2_outIsFP = RegEnable(s1_outIsFP, fireReg)
  val s2_fpCanonicalNAN = RegEnable(s1_fpCanonicalNAN, fireReg)
  val s2_isInt2Fp = RegEnable(s1_isInt2Fp, fireReg)
  val s2_isEstimate7 = RegEnable(s1_isEstimate7, fireReg)
  val s2_isFPsrc = RegEnable(s1_isFPsrc, fireReg)
  val s2_isFround = RegEnable(isFroundReg, fireReg)

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
  if(isVectorCvt){ // vector
    //inst FPTOINT and FPTOFP module
    val fpcvt = Module(new FP_INCVT(width))
    fpcvt.io.fire := fire
    fpcvt.io.src := src
    fpcvt.io.rm := rm
    fpcvt.io.opType := opType
    fpcvt.io.inSew1H := inSew1H
    fpcvt.io.outSew1H := outSew1H
    fpcvt.io.isScalarFpInst := isScalarFpInst
    fpcvt.io.isFround := isFround
    fpcvt.io.isFcvtmod := isFcvtmod
    //inst INTTOFP and ESTMATE module
    val int2fp = Module(new INT2FP(width))
    int2fp.io.fire := fire
    int2fp.io.src := src
    int2fp.io.rm := rm
    int2fp.io.opType := opType
    int2fp.io.inSew1H := inSew1H
    int2fp.io.outSew1H := outSew1H
    val estmate7 = Module(new Estimate7(width))
    estmate7.io.fire := fire
    estmate7.io.src := src
    estmate7.io.rm := rm
    estmate7.io.opType := opType
    estmate7.io.inSew1H := inSew1H
    estmate7.io.outSew1H := outSew1H
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
    val resultEx1 = Mux1H(Seq(
      s1_isInt2Fp -> int2fp.io.resultEx1,
      s1_isFPsrc -> fpcvt.io.resultEx1,
      s1_isEstimate7 -> estmate7.io.resultEx1,
    ))
    val fflagsEx1 = Mux1H(Seq(
      s1_isInt2Fp -> int2fp.io.fflagsEx1,
      s1_isFPsrc -> fpcvt.io.fflagsEx1,
      s1_isEstimate7 -> estmate7.io.fflagsEx1,
    ))
    io.out.ex2.res := result
    io.out.ex2.fflags := fflags
    io.out.ex1.res := resultEx1
    io.out.ex1.fflags := fflagsEx1
  }else if(!isI2F){ // scalar fp2int & fp2fp
    //inst FPTOINT and FPTOFP module
    val fpcvt = Module(new FP_INCVT(width))
    fpcvt.io.fire := fire
    fpcvt.io.src := src
    fpcvt.io.rm := rm
    fpcvt.io.opType := opType
    fpcvt.io.inSew1H := inSew1H
    fpcvt.io.outSew1H := outSew1H
    fpcvt.io.isScalarFpInst := isScalarFpInst
    fpcvt.io.isFround := isFround
    fpcvt.io.isFcvtmod := isFcvtmod
    val result = fpcvt.io.result
    val fflags = fpcvt.io.fflags
    val resultEx1 = fpcvt.io.resultEx1
    val fflagsEx1 = fpcvt.io.fflagsEx1
    io.out.ex2.res := Mux(s2_fpCanonicalNAN, s2_resultForfpCanonicalNAN, result)
    io.out.ex2.fflags := Mux(s2_fpCanonicalNAN && !s2_outIsFP, "b10000".U, Mux(s2_fpCanonicalNAN && s2_isFround, 0.U, fflags))
    io.out.ex1.res := resultEx1
    io.out.ex1.fflags := fflagsEx1
  }else { // scalar int2fp
    val int2fp = Module(new INT2FP(width))
    int2fp.io.fire := fire
    int2fp.io.src := src
    int2fp.io.rm := rm
    int2fp.io.opType := opType
    int2fp.io.inSew1H := inSew1H
    int2fp.io.outSew1H := outSew1H
    val result = int2fp.io.result
    val fflags = int2fp.io.fflags
    io.out.ex2.res := result
    io.out.ex2.fflags := fflags
    io.out.ex1.res := int2fp.io.resultEx1
    io.out.ex1.fflags := int2fp.io.fflagsEx1
  }
}
class CVT_IO(width: Int) extends Bundle{
  val fire     = Input(Bool())
  val src      = Input(UInt(width.W))
  val opType   = Input(FCvtOpcode())
  val rm       = Input(Frm())
  val inSew1H  = Input(Sew())
  val outSew1H = Input(Sew())
  val isScalarFpInst = Input(Bool())
  val isFround  = Input(UInt(2.W))
  val isFcvtmod = Input(Bool())
  val resultEx1 = Output(UInt(width.W))
  val fflagsEx1 = Output(Fflags())
  val result    = Output(UInt(width.W))
  val fflags    = Output(Fflags())
}

class INTCVT_IO(width: Int) extends Bundle{
  val fire     = Input(Bool())
  val src      = Input(UInt(width.W))
  val opType   = Input(FCvtOpcode())
  val rm       = Input(Frm())
  val inSew1H  = Input(Sew())
  val outSew1H = Input(Sew())
  val resultEx1 = Output(UInt(width.W))
  val fflagsEx1 = Output(Fflags())
  val result   = Output(UInt(width.W))
  val fflags   = Output(Fflags())
}
class FP_INCVT(width: Int) extends Module {
  val io = IO(new CVT_IO(width: Int))
  val intParamMap = (0 to 3).map(i => (1 << i) * 8)
  //input
  val (fire, src, opType, rmNext, inSew1H, outSew1H, isScalarFpInst, isFround, isFcvtmod) =
    (io.fire, io.src, io.opType, io.rm, io.inSew1H, io.outSew1H, io.isScalarFpInst, io.isFround, io.isFcvtmod)
  val fireReg = GatedValidRegNext(fire)

  val outIsIntNext     = FCvtOpcode.outIsInt(opType)
  val hasSignIntNext   = FCvtOpcode.isSignInt(opType)
  val float1HSrcNext = inSew1H(3, 1)  //exclude f8
  val float1HOutNext = outSew1H(3, 1) //exclude f8
  val outIsFp32Next = float1HOutNext(1)
  val outIsFp64Next = float1HOutNext(2)

  val isFroundOrFroundnxNext = isFround.orR
  val isFroundnxNext = isFround(1)

  //fp input extend
  val srcMap = (0 to 3).map(i => src((1 << i) * 8 - 1, 0))
  val floatMap = srcMap.zipWithIndex.map{case (float,i) => floatExtend(float, i)}.drop(1)
  val expSrcNextMap = floatMap.map(_.tail(1).head(f64.expWidth))
  val fracSrcMap = floatMap.map(_.tail(f64.expWidth+1).head(f64.fracWidth))
  val signSrcNextMap = floatMap.map(_.head(1).asBool)
  val expSrcNext = Mux1H(float1HSrcNext, expSrcNextMap)
  val fracSrc = Mux1H(float1HSrcNext, fracSrcMap)
  val signSrcNext = Mux1H(float1HSrcNext, signSrcNextMap)
  val decodeFloatSrcMap = fpParam.fpMap.zip(expSrcNextMap).zip(fracSrcMap).map { case ((fp, exp), frac) =>
    VecInit(exp(fp.expWidth-1,0).orR, exp(fp.expWidth-1,0).andR, frac.head(fp.fracWidth).orR).asUInt
  }
  val decodeFloatSrc = Mux1H(float1HSrcNext, decodeFloatSrcMap)
  val (expNotZeroSrcNext, expIsOnesSrcNext, fracNotZeroSrcNext) = (decodeFloatSrc(0), decodeFloatSrc(1), decodeFloatSrc(2))
  val isNaNSrcNext = expIsOnesSrcNext && fracNotZeroSrcNext
  val isZeroSrcNext = !expNotZeroSrcNext && !fracNotZeroSrcNext
  val isSubnormalSrcNext = !expNotZeroSrcNext && fracNotZeroSrcNext
  val isnormalSrcNext = !expIsOnesSrcNext && expNotZeroSrcNext
  val isInfSrcNext = expIsOnesSrcNext && !fracNotZeroSrcNext
  val isSNaNSrcNext = isNaNSrcNext && !fracSrc.head(1)

  val fpInputWidthNext    = FCvtOpcode.getInputDataWidth(opType)
  val fpOutputWidthNext   = FCvtOpcode.getOutputDataWidth(opType)
  val isFpWidenNext       = FCvtOpcode.isF2F(opType) && fpOutputWidthNext > fpInputWidthNext
  val isFp16To64Next      = FCvtOpcode.isF2F(opType) && FCvtOpcode.inIs16(opType) && FCvtOpcode.outIs64(opType)
  val isFp2IntNext        = FCvtOpcode.isF2I(opType)

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
  val isFp2Int = RegEnable(isFp2IntNext, false.B, fire)
  val isNaNSrc = RegEnable(isNaNSrcNext, false.B, fire)
  val s0_fpCanonicalNAN = isScalarFpInst & (inSew1H(1) & !src.head(48).andR | inSew1H(2) & !src.head(32).andR)
  val s1_fpCanonicalNAN = RegEnable(s0_fpCanonicalNAN, fire)

  val isFroundnxReg = RegEnable(isFroundnxNext, false.B, fire)
  val isFroundOrFroundnxReg = RegEnable(isFroundOrFroundnxNext, false.B, fire)
  val isFcvtmodReg = RegEnable(isFcvtmod, false.B, fire)
  val froundOrFroundnxIsZeroOrInf = RegEnable(froundOrFroundnxIsZeroOrInfNext, false.B, fire)

  val signSrc = RegEnable(signSrcNext, false.B, fire)
  val rm = RegEnable(rmNext, 0.U(3.W), fire)
  val hasSignInt = RegEnable(hasSignIntNext, false.B, fire)
  val signNonNan = !isNaNSrc && signSrc

  val outSew1HReg = RegEnable(outSew1H, 0.U(4.W), fire)
  val float1HOut = Wire(UInt(3.W))
  float1HOut := outSew1HReg.head(3)
  val int1HOut = Wire(UInt(4.W))
  int1HOut := outSew1HReg

  //output
  val nv, dz, of, uf, nx = Wire(Bool()) //cycle1
  val fflagsNext = Wire(UInt(5.W))
  val fflags = RegEnable(fflagsNext, 0.U(5.W), fireReg)
  val resultNext = Wire(UInt(width.W))
  val result = RegEnable(resultNext, 0.U(width.W), fireReg)

  //exp
  val expAdderIn0Next = Wire(UInt(f64.expAdderWidth.W)) //13bits is enough
  val expAdderIn1Next = Wire(UInt(f64.expAdderWidth.W))
  val expNext = Wire(UInt(f64.expAdderWidth.W))
  expNext := expAdderIn0Next + expAdderIn1Next
  val expReg = RegEnable(expNext, fire)

  val leadZerosNextMap = fracSrcMap.map(frac => Lzc((frac << (64 - f64.fracWidth)).asUInt).data)
  val leadZerosNext = Mux1H(float1HSrcNext, leadZerosNextMap)
  expAdderIn0Next := Mux1H(Seq(
    isFpWidenNext -> Mux(
      isFp16To64Next,
      fpParam.fp16To64BiasDelta.U,
      Mux1H(Seq(
        outIsFp32Next -> fpParam.fp16To32BiasDelta.U,
        outIsFp64Next -> fpParam.fp32To64BiasDelta.U
      ))
    ),
    isFp2IntNext -> Mux(isSubnormalSrcNext, false.B ## 1.U, false.B ## expSrcNext)
  ))
  val bias =  Mux1H(float1HSrcNext, fpParam.fpMap.map(fp => fp.bias.U))
  val minusExp = extend((~(false.B ## Mux1H(
    Seq(
      isFpWidenNext   -> leadZerosNext,
      isFp2IntNext    -> bias
    )))).asUInt, f64.expAdderWidth).asUInt

  val expPlus1EnableNext = isFp2IntNext || (isFpWidenNext && isSubnormalSrcNext)
  val expPlus1Enable = RegEnable(expPlus1EnableNext, fire)
  expAdderIn1Next := Mux(
    expPlus1EnableNext,
    minusExp,
    expSrcNext
  )
  //frac
  val fracSrcLeftNextMap = fracSrcMap.map(frac => frac << (64 - f64.fracWidth))
  val fracSrcLeftNext = Mux1H(float1HSrcNext, fracSrcLeftNextMap)
  val shiftLeftNextMap = fracSrcLeftNextMap.zip(leadZerosNextMap).map { case (fracLeft, leadZeros) =>
    (fracLeft.asUInt << 1) << leadZeros
  }
  val shiftLeftNext = Mux1H(float1HSrcNext, shiftLeftNextMap)
  val shiftLeft = RegEnable(shiftLeftNext, 0.U(64.W), fire)
  val fracSrcLeft = RegEnable(fracSrcLeftNext, 0.U(64.W), fire)
  val fracNormaled =  Wire(UInt(64.W))
  fracNormaled := Mux(isSubnormalSrc, shiftLeft, fracSrcLeft) //cycle1

  // common
  val shamtInNextMap = decodeFloatSrcMap.zip(fracSrcMap).map { case (decode, frac) =>
    val expNotZero = decode(0)
    val expIsOnes = decode(1)
    val fracValue = (expNotZero && !expIsOnes) ## frac
    fracValue ## 0.U(11.W) ## false.B
  }
  val fracValueSrc = (expNotZeroSrcNext && !expIsOnesSrcNext) ## fracSrc
  val shamtNextMap = fpParam.fpMap.zip(expSrcNextMap).map { case (fp, exp) =>
    val shamtWidth = (63 + fp.bias).U + (~exp).asUInt
    val shamtWidthPlus1 = shamtWidth + 1.U
    Mux(shamtWidth.andR, 0.U, Mux(shamtWidth(10, 6).orR, 65.U, shamtWidthPlus1))
  }
  val inRounderStickyMap = shamtInNextMap.zip(shamtNextMap).map { case (shamtIn, shamt) =>
    ShiftRightJam(shamtIn, shamt)
  }
  val inRounderTmp = Mux1H(float1HSrcNext, inRounderStickyMap.map(_._1))
  val stickyTmp = Mux1H(float1HSrcNext, inRounderStickyMap.map(_._2))

  val inRounder = RegEnable(inRounderTmp, 0.U(65.W), fire)
  val sticky = RegEnable(stickyTmp, false.B, fire)
  /**
   * fround
   * frac
   * cycle: 0
   */

  val froundMaxExpNext = Mux1H(float1HOutNext, fpParam.fpMap.map(fp => fp.froundMaxExp.U))
  val froundExpGreaterThanMaxExpNext = expSrcNext >= froundMaxExpNext

  val fracShiftMaskNext = Mux1H(float1HOutNext, fpParam.fpMap.map(fp => fp.froundShiftMask.U)) - expSrcNext
  val froundShiftMaskNext = Mux1H(
    (0 until (1 << 6)).map(i =>
      (i.U === fracShiftMaskNext) -> Cat(~0.U((64-i).W), 0.U(i.W)))
  )
  val froundUpShiftMaskNext = Mux1H(
    (0 until (1 << 6)).map(i =>
      (i.U === fracShiftMaskNext) -> Cat(1.U((64-i).W), 0.U(i.W)))
  )

  val froundExpLessThanBiasNext = Mux1H(float1HOutNext, fpParam.fpMap.map(fp => !expSrcNext(fp.expWidth-1) && !expSrcNext(fp.expWidth-2, 0).andR))

  val froundExpLessThanBias = RegEnable(froundExpLessThanBiasNext, false.B, fire)
  val froundExpGreaterThanMaxExp = RegEnable(froundExpGreaterThanMaxExpNext, false.B, fire)
  val froundShiftMask = RegEnable(froundShiftMaskNext, 0.U, fire)
  val froundUpShiftMask = RegEnable(froundUpShiftMaskNext, 0.U, fire)
  val froundOldExp = RegEnable(expSrcNext, 0.U, fire)
  val froundOldFrac = RegEnable(fracSrc, 0.U, fire)

  // cycle1
  val froundOldInput = Wire(UInt(64.W))
  val froundUpInput = Wire(UInt(64.W))

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

  val rounderInput = inRounder.head(64)
  val rounderRoundIn = inRounder(0)
  val rounderStickyIn = sticky

  val rounder = Module(new RoundingUnit(64))
  rounder.io.in := rounderInput
  rounder.io.roundIn := rounderRoundIn
  rounder.io.stickyIn := rounderStickyIn
  rounder.io.signIn := signSrc
  rounder.io.rm := rm

  // from rounder
  val nxRounded = rounder.io.inexact
  val upRounded = rounder.io.r_up
  val exp = expReg + Mux(expPlus1Enable, 1.U, 0.U)
  val expIncrease = expReg + Mux(expPlus1Enable, 2.U, 1.U)
  val rounderInputIncrease = rounderInput + 1.U
  // for fp2int
  // 8bit: => u64, i64, u32, i32, u16, i16, u8, i8
  val hasSignInt1HOut = int1HOut.asBools.map(oh => Seq(oh && !hasSignInt, oh && hasSignInt)).flatten
  val isOnesRounderInputMapFp2Int =
    intParamMap.map(intType => Seq(intType, intType - 1)).flatten.map(intType => rounderInput.tail(64 - intType).andR)
  val cout = upRounded && Mux1H(hasSignInt1HOut, isOnesRounderInputMapFp2Int).asBool
  val expRounded = Wire(UInt(f64.expWidth.W))
  expRounded := Mux(cout, expIncrease, exp)
  val fracRounded = Mux(upRounded, rounderInputIncrease, rounderInput)
  val rmin =
    rm === RTZ || (signSrc && rm === RUP) || (!signSrc && rm === RDN) //cycle1
  when(isFpWiden){
    /** fp -> fp wider
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
  io.resultEx1 := resultNext
  io.fflagsEx1 := fflagsNext
  io.result := result
  io.fflags := fflags

}
class INT2FP(width: Int) extends Module{
  val io = IO(new INTCVT_IO(width: Int))
  //input
  val (fire, src, opType, rmNext, inSew1H, outSew1H) =
    (io.fire, io.src, io.opType, io.rm, io.inSew1H, io.outSew1H)
  val fireReg = GatedValidRegNext(fire)
  val hasSignIntNext = FCvtOpcode.isSignInt(opType)
  val int1HSrcNext = inSew1H
  val float1HOutNext = outSew1H.head(3)//exclude f8
  val outSew1HReg = RegEnable(outSew1H, 0.U(4.W), fire)
  val float1HOut = Wire(UInt(3.W))
  float1HOut := outSew1HReg.head(3)
  val intWidthMap = Seq(8, 16, 32, 64)
  val srcMap = intWidthMap.map(width => src(width - 1, 0))
  val signSrcNextMap = srcMap.map(int => hasSignIntNext && int.head(1).asBool)
  val signSrcNext = Mux1H(int1HSrcNext, signSrcNextMap)
  val signSrc = RegEnable(signSrcNext, false.B, fire)
  val rm = RegEnable(rmNext, 0.U(3.W), fire)
  // src is int
  val absIntSrcRawNextMap = srcMap.zip(signSrcNextMap).map { case (int, sign) =>
    val neg = (~int).asUInt + 1.U
    Mux(sign, neg(int.getWidth - 1, 0), int)
  }
  val absIntSrcNextMap = absIntSrcRawNextMap.zip(intWidthMap).map { case (absInt, intWidth) =>
    if (intWidth == 64) absInt else 0.U((64 - intWidth).W) ## absInt
  }
  val absIntSrcNext = Mux1H(int1HSrcNext, absIntSrcNextMap)
  val isZeroIntSrcNext = Mux1H(int1HSrcNext, absIntSrcRawNextMap.map(absInt => !absInt.orR))
  val isZeroIntSrc = RegEnable(isZeroIntSrcNext, false.B, fire)
  //CLZ
  val leadZerosNextMap = absIntSrcRawNextMap.zip(intWidthMap).map { case (absInt, intWidth) =>
    if (intWidth == 64) {
      Lzc(absInt).data
    } else {
      ((64 - intWidth).U(6.W) + Lzc(absInt).data)(5, 0)
    }
  }
  val leadZerosNext = Mux1H(int1HSrcNext, leadZerosNextMap)
  //exp
  val expAdderIn0Next = Wire(UInt(f64.expAdderWidth.W)) //13bits is enough
  val expAdderIn1Next = Wire(UInt(f64.expAdderWidth.W))
  val minusExp = extend((~(false.B ## leadZerosNext)).asUInt, f64.expAdderWidth).asUInt
  expAdderIn0Next := Mux1H(float1HOutNext, fpParam.fpMap.map(fp => (fp.bias + 63).U))
  expAdderIn1Next := minusExp
  val expNext = Wire(UInt(f64.expAdderWidth.W))
  expNext := expAdderIn0Next + expAdderIn1Next
  val expReg = RegEnable(expNext, fire)
  //frac
  val absIntSrc = RegEnable(absIntSrcNext, fire)
  val leadZeros = RegEnable(leadZerosNext, fire)
  val shiftLeft = Wire(UInt(64.W))
  shiftLeft := (absIntSrc.asUInt << 1) << leadZeros //cycle1
  //round
  val rounderMapIn = Wire(UInt(64.W))
  rounderMapIn := shiftLeft
  val rmin =
    rm === RTZ || (signSrc && rm === RUP) || (!signSrc && rm === RDN) //cycle1
  /** int->fp   any int/uint-> any fp
   */
  val nv, dz, of, uf, nx = Wire(Bool()) //cycle1
  val fflagsNext = Wire(UInt(5.W))
  val fflags = RegEnable(fflagsNext, 0.U(5.W), fireReg)
  val resultNext = Wire(UInt(64.W))
  val result = RegEnable(resultNext, 0.U(64.W), fireReg)
  def int2FpResultMapGen(fp: FloatFormat, expRounded: UInt, fracRounded: UInt): Seq[UInt] = {
    VecInit((0 to 3).map {
      case 0 => signSrc ## fp.maxExp.U(fp.expWidth.W) ## ~0.U(fp.fracWidth.W) //GNF
      case 1 => signSrc ## ~0.U(fp.expWidth.W) ## 0.U(fp.fracWidth.W) // INF
      case 2 => signSrc ## 0.U((fp.width - 1).W) // 0
      case 3 => signSrc ## expRounded(fp.expWidth-1, 0) ## fracRounded(fp.fracWidth-1, 0) // normal
    })
  }

  val exp = expReg + 1.U
  val expIncrease = expReg + 2.U
  val int2FpByFp = fpParam.fpMap.map { fp =>
    val rounderInput = rounderMapIn.head(fp.fracWidth)
    val rounderIn = rounderMapIn.tail(fp.fracWidth).head(1).asBool
    val rounderSticky = rounderMapIn.tail(fp.fracWidth + 1).orR
    val rounder = Module(new RoundingUnit(fp.fracWidth))
    rounder.io.in := rounderInput
    rounder.io.roundIn := rounderIn
    rounder.io.stickyIn := rounderSticky
    rounder.io.signIn := signSrc
    rounder.io.rm := rm
    val nxRounded = rounder.io.inexact
    val upRounded = rounder.io.r_up
    val cout = upRounded && rounderInput.andR
    val expRounded = Wire(UInt(f64.expWidth.W))
    expRounded := Mux(cout, expIncrease, exp)
    val fracRounded = Mux(upRounded, rounderInput + 1.U, rounderInput)
    // Mux(cout, exp > FP.maxExp -1, exp > FP.maxExp)
    val ofRounded = if (fp.expWidth == f16.expWidth) {
      !exp.head(1).asBool && Mux(cout,
        exp(fp.expWidth - 1, 1).andR || exp(exp.getWidth - 2, fp.expWidth).orR,
        exp(fp.expWidth - 1, 0).andR || exp(exp.getWidth - 2, fp.expWidth).orR
      )
    } else {
      false.B
    }
    val normalResult = signSrc ## expRounded(fp.expWidth-1, 0) ## fracRounded(fp.fracWidth-1, 0)
    val result = if (fp.expWidth != f16.expWidth) {
      Mux(isZeroIntSrc, signSrc ## 0.U((fp.width - 1).W), normalResult)
    } else {
      val result1H = Cat(
        ofRounded && rmin,
        ofRounded && !rmin,
        isZeroIntSrc,
        !ofRounded && !isZeroIntSrc
      )
      Mux1H(result1H.asBools.reverse, int2FpResultMapGen(fp, expRounded, fracRounded))
    }
    (result, ofRounded, nxRounded)
  }
  val int2FpResultMap: Seq[UInt] = int2FpByFp.map(_._1)
  val ofRounded = Mux1H(float1HOut, int2FpByFp.map(_._2)).asBool
  val nxRounded = Mux1H(float1HOut, int2FpByFp.map(_._3)).asBool
  nv := false.B
  dz := false.B
  of := ofRounded
  uf := false.B
  nx := ofRounded || nxRounded

  resultNext := Mux1H(float1HOut, int2FpResultMap)
  //output
  fflagsNext := Cat(nv, dz, of, uf, nx)
  io.resultEx1 := resultNext
  io.fflagsEx1 := fflagsNext
  io.result := result
  io.fflags := fflags
}
class Estimate7(width: Int) extends Module{
  /** Estimate7: sqrt7 & rec7
 */
  val io = IO(new INTCVT_IO(width: Int))
  //input
  val (fire, src, opType, rmNext, inSew1H, outSew1H) =
    (io.fire, io.src, io.opType, io.rm, io.inSew1H, io.outSew1H)
  val fireReg = GatedValidRegNext(fire)
  val int1HSrcNext = inSew1H
  val float1HSrcNext = inSew1H.head(3)//exclude f8
  val int1HOutNext = outSew1H
  val float1HOutNext = outSew1H.head(3)//exclude f8
  val srcMap = (0 to 3).map(i => src((1 << i) * 8 - 1, 0))
  val floatMap = srcMap.zipWithIndex.map{case (float,i) => floatExtend(float, i)}.drop(1)
  val input = Mux1H(float1HSrcNext, floatMap)
  val signSrcNext = input.head(1).asBool
  val isEstimate7Next = FCvtOpcode.isEstimate7(opType)
  val isRecNext = FCvtOpcode.isRec(opType)
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

  val outSew1HReg = RegEnable(outSew1H, 0.U(4.W), fire)
  val float1HOut = Wire(UInt(3.W))
  float1HOut := outSew1HReg(3, 1)
  val nv, dz, of, uf, nx = Wire(Bool()) //cycle1
  val fflagsNext = Wire(Fflags())
  val fflags = RegEnable(fflagsNext, 0.U(5.W), fireReg)
  val resultNext = Wire(UInt(width.W))
  val result = RegEnable(resultNext, 0.U(64.W), fireReg)

  val clzIn = (fracSrc<<(64 - f64.fracWidth)).asUInt
  val leadZerosNext = Lzc(clzIn).data
  val rmin =
    rm === RTZ || (signSrc && rm === RUP) || (!signSrc && rm === RDN) //cycle1
  //exp
  val expAdderIn0Next = Wire(UInt(f64.expAdderWidth.W)) //13bits is enough
  val expAdderIn1Next = Wire(UInt(f64.expAdderWidth.W))
  val expAdderIn0 = RegEnable(expAdderIn0Next, fire)
  val expAdderIn1 = RegEnable(expAdderIn1Next, fire)
  val minusExp = extend((~(false.B ## expSrcNext)).asUInt + 1.U, f64.expAdderWidth).asUInt
  expAdderIn0Next := Mux1H(float1HOutNext, fpParam.fpMap.map(fp => Mux(isRecNext, (2 * fp.bias - 1).U, (3 * fp.bias - 1).U)))
  expAdderIn1Next := Mux(isSubnormalSrcNext, leadZerosNext, minusExp)
  val exp = Wire(UInt(f64.expAdderWidth.W))
  exp := expAdderIn0 + expAdderIn1

  val expNormaled = Mux(isSubnormalSrcNext, leadZerosNext(0), expSrcNext(0)) //only the last bit is needed
  val expNormaled0 = RegEnable(expNormaled(0), false.B, fire)
  val fracSrcLeftNext = fracSrc << (64 - f64.fracWidth)
  val fracNormaledNext = Mux(isSubnormalSrcNext, (fracSrcLeftNext.asUInt << 1) << leadZerosNext, fracSrcLeftNext)
  val fracNormaled = RegEnable(fracNormaledNext, 0.U(64.W), fire)
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
  io.resultEx1 := resultNext
  io.fflagsEx1 := fflagsNext
  io.result := result
  io.fflags := fflags
}