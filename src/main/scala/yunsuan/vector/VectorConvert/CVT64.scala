package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import yunsuan.vector.VectorConvert.util._
import yunsuan.vector.VectorConvert.utils._
import yunsuan.vector.VectorConvert.RoundingModle._
import yunsuan.util._

class CVT64(width: Int = 64) extends CVT(width){

  //parameter
  val fpParamMap = Seq(f16, f32, f64)
  val biasDeltaMap = Seq(f32.bias - f16.bias, f64.bias - f32.bias)
  val intParamMap = (0 to 3).map(i => (1 << i) * 8)
  val widthExpAdder = 13 // 13bits is enough

  // input
  val (fire, src, sew, opType, rmNext, input1H, output1H) =
      (io.fire, io.src, io.sew, io.opType, io.rm, io.input1H, io.output1H)
  val fireReg = GatedValidRegNext(fire)

  // control for cycle 0
  val isWiden = !opType(4) && opType(3)
  val isNarrow = opType(4) && !opType(3)
  val inIsFp = opType.head(1).asBool
  val outIsFp = opType.tail(1).head(1).asBool
  val hasSignIntNext = opType(0).asBool

  val int1HSrcNext = input1H
  val float1HSrcNext = input1H.head(3)//exclude f8

  val int1HOutNext = output1H
  val float1HOutNext = output1H.head(3)//exclude f8

  val srcMap = (0 to 3).map(i => src((1 << i) * 8 - 1, 0))
  val intMap = srcMap.map(int => intExtend(int, hasSignIntNext && int.head(1).asBool))

  val floatMap = srcMap.zipWithIndex.map{case (float,i) => floatExtend(float, i)}.drop(1)
  val input = Mux(inIsFp,
    Mux1H(float1HSrcNext, floatMap),
    Mux1H(int1HSrcNext, intMap)
  )

  val signSrcNext = input.head(1).asBool

  // src is int
  val absIntSrc = Wire(UInt(64.W)) //cycle0
  absIntSrc := Mux(signSrcNext, (~input.tail(1)).asUInt + 1.U, input.tail(1))
  val isZeroIntSrcNext = !absIntSrc.orR

  /** src is float contral path
   * special: +/- INF, NaN, qNaN, SNaN, 0, Great NF, canonical NaN
   */
  val expSrc = input.tail(1).head(f64.expWidth)
  val fracSrc = input.tail(f64.expWidth+1).head(f64.fracWidth)
  val decodeFloatSrc = Mux1H(float1HSrcNext, fpParamMap.map(fp =>
      VecInit(expSrc(fp.expWidth-1,0).orR, expSrc(fp.expWidth-1,0).andR, fracSrc.head(fp.fracWidth).orR).asUInt
    )
  )

  val (expNotZeroSrcNext, expIsOnesSrcNext, fracNotZeroSrcNext) = (decodeFloatSrc(0), decodeFloatSrc(1), decodeFloatSrc(2))
  val expIsZeroSrcNext = !expNotZeroSrcNext
  val fracIsZeroSrcNext = !fracNotZeroSrcNext
  val isSubnormalSrcNext = expIsZeroSrcNext && fracNotZeroSrcNext
  val isnormalSrcNext = !expIsOnesSrcNext && !expIsZeroSrcNext
  val isInfSrcNext = expIsOnesSrcNext && fracIsZeroSrcNext
  val isZeroSrcNext = expIsZeroSrcNext && fracIsZeroSrcNext
  val isNaNSrcNext = expIsOnesSrcNext && fracNotZeroSrcNext
  val isSNaNSrcNext = isNaNSrcNext && !fracSrc.head(1)
  val isQNaNSrcNext = isNaNSrcNext && fracSrc.head(1).asBool

  // for sqrt7/rec7
  val isEstimate7Next = opType(5)
  val isRecNext = opType(5) && opType(0)

  val decodeFloatSrcRec = Mux1H(float1HSrcNext,
      fpParamMap.map(fp => expSrc(fp.expWidth - 1, 0)).zip(fpParamMap.map(fp => fp.expWidth)).map { case (exp, expWidth) =>
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
  val isSubnormalRec2Next = isSubnormalSrcNext && !fracSrc.head(2).orR

  // type int->fp, fp->fp widen, fp->fp Narrow, fp->int
  val (isInt2FpNext, isFpWidenNext, isFpNarrowNext, isFp2IntNext) =
    (!inIsFp, inIsFp && outIsFp && isWiden, inIsFp && outIsFp && isNarrow, !outIsFp)

  //contral sign to cycle1
  val expNotZeroSrc = RegEnable(expNotZeroSrcNext, false.B, fire)
  val expIsOnesSrc = RegEnable(expIsOnesSrcNext, false.B, fire)
  val fracNotZeroSrc = RegEnable(fracNotZeroSrcNext, false.B, fire)
  val expIsZeroSrc = RegEnable(expIsZeroSrcNext, false.B, fire)
  val fracIsZeroSrc = RegEnable(fracIsZeroSrcNext, false.B, fire)
  val isSubnormalSrc = RegEnable(isSubnormalSrcNext, false.B, fire)
  val isnormalSrc = RegEnable(isnormalSrcNext, false.B, fire)
  val isInfSrc = RegEnable(isInfSrcNext, false.B, fire)
  val isZeroSrc = RegEnable(isZeroSrcNext, false.B, fire)
  val isNaNSrc = RegEnable(isNaNSrcNext, false.B, fire)
  val isSNaNSrc = RegEnable(isSNaNSrcNext, false.B, fire)
  val isQNaNSrc = RegEnable(isQNaNSrcNext, false.B, fire)
  val isNormalRec0 = RegEnable(isNormalRec0Next, false.B, fire)
  val isNormalRec1 = RegEnable(isNormalRec1Next, false.B, fire)
  val isNormalRec2 = RegEnable(isNormalRec2Next, false.B, fire)
  val isSubnormalRec0 = RegEnable(isSubnormalRec0Next, false.B, fire)
  val isSubnormalRec1 = RegEnable(isSubnormalRec1Next, false.B, fire)
  val isSubnormalRec2 = RegEnable(isSubnormalRec2Next, false.B, fire)

  val isRec = RegEnable(isRecNext, false.B, fire)

  val isInt2Fp = RegEnable(isInt2FpNext, false.B, fire)
  val isFpWiden = RegEnable(isFpWidenNext, false.B, fire)
  val isFpNarrow = RegEnable(isFpNarrowNext, false.B, fire)
  val isEstimate7 = RegEnable(isEstimate7Next, false.B, fire)
  val isFp2Int = RegEnable(isFp2IntNext, false.B, fire)

  // for fpnarrow sub
  val trunSticky = RegEnable(fracSrc.tail(f32.fracWidth).orR, false.B, fire)

  val signSrc = RegEnable(signSrcNext, false.B, fire)
  val rm = RegEnable(rmNext, 0.U(3.W), fire)

  val hasSignInt = RegEnable(hasSignIntNext, false.B, fire)
  val isZeroIntSrc = RegEnable(isZeroIntSrcNext, false.B, fire)
  val signNonNan = !isNaNSrc && signSrc


  /** critical path
   * share:
   *    1.count leading zero Max is 64
   *    2.adder: exp
   *    3.shift left/right(UInt)
   *    4.rounding module: +1(exp & roundInput)
   *    5.Mux/Mux1H
   *
   * general step:
   *    step1: clz + adder -> compute really exp
   *    step2: shift left/right -> put the first one to the correct position
   *    step3: rounding
   *    step4: select result and fflags by mux1H
   *
   * pipe:
   *    cycle0: adder:64bits -> 13bits adder -> sl 6bits/rl 7bits
   *    cycle1: adder:64bits -> adder: 64bits -> Mux/Mux1H
   *    cycle2: result/fflags
   *                                                  |                                          |
   *    int->fp:       abs(adder) -> exp adder  -> sl | ->  rounding(adder) -> Mux/Mux1H         |
   *    fpwiden:       fpdecode   -> exp adder        | -> Mux/Mux1H                             |
   *    fpNarrow(nor): fpdecode   -> exp adder  -> sl | ->  rounding(adder) --\                  |
   *    fpNarrow(sub): fpdecode   -> exp adder2 -> sr | ->  rounding(adder) -> Mux/Mux1H         |
   *    estimate7:     fpdecode   -> exp adder        | ->  decoder  -> Mux                      |
   *    fp-> int:      fpdecode   -> exp adder2 -> sr | ->  rounding(adder) -> ~+1 -> Mux/Mux1H  |
   *                                                  |                                          | -> result & fflags
   */

  // for cycle1
  val output1HReg = RegEnable(output1H, 0.U(4.W), fire)
  val float1HOut = Wire(UInt(3.W))
  float1HOut := output1HReg.head(3)
  val int1HOut = Wire(UInt(4.W))
  int1HOut := output1HReg

  val expNext = Wire(UInt(widthExpAdder.W))
  val expReg = RegEnable(expNext, 0.U(widthExpAdder.W), fire)
  val exp = Wire(UInt(widthExpAdder.W))
  exp := expReg

  val fracNormaledNext =  Wire(UInt(64.W))
  val fracNormaled = RegEnable(fracNormaledNext, 0.U(64.W), fire)

  val rounderMapInNext = Wire(UInt(64.W))
  val rounderMapInReg = RegEnable(rounderMapInNext, 0.U(64.W), fire)
  val rounderMapIn = Wire(UInt(64.W))
  rounderMapIn := rounderMapInReg

  //for cycle2 -> output
  val nv, dz, of, uf, nx = Wire(Bool()) //cycle1
  val fflagsNext = Wire(UInt(5.W))
  val fflags = RegEnable(fflagsNext, 0.U(5.W), fireReg)
  val resultNext = Wire(UInt(64.W))
  val result = RegEnable(resultNext, 0.U(64.W), fireReg)

  /** clz
   * for: int->fp, fp->fp widen, estimate7,  reuse clz according to fracSrc << (64 - f64.fracWidth)
   * cycle: 0
   */
  val clzIn = Mux(inIsFp, fracSrc<<(64 - f64.fracWidth), absIntSrc).asUInt
  val leadZeros = CLZ(clzIn)

  /** exp adder
   * for: all exp compute
   * cycle: 0
   */

  val type1H = Cat(isInt2FpNext, isFpWidenNext, isFpNarrowNext, isEstimate7Next, isFp2IntNext).asBools.reverse
  val expAdderIn0 = Wire(UInt(widthExpAdder.W)) //13bits is enough
  val expAdderIn1 = Wire(UInt(widthExpAdder.W))

  val biasDelta = Mux1H(float1HOutNext.tail(1), biasDeltaMap.map(delta => delta.U))
  val bias =  Mux1H(float1HSrcNext, fpParamMap.map(fp => fp.bias.U))
  val minusExp = extend((~(false.B ## Mux1H(
    Cat(isInt2FpNext || isFpWidenNext, isFpNarrowNext, isEstimate7Next, isFp2IntNext).asBools.reverse,
    Seq(
      leadZeros,
      biasDelta,
      expSrc,
      bias
    )))).asUInt
    + 1.U, widthExpAdder).asUInt

  expAdderIn0 := Mux1H(type1H, Seq(
      Mux1H(float1HOutNext, fpParamMap.map(fp => (fp.bias + 63).U)),
      Mux1H(float1HOutNext.head(2), biasDeltaMap.map(delta => delta.U)),
      Mux(isSubnormalSrcNext, false.B ## 1.U, false.B ## expSrc),
      Mux1H(float1HOutNext, fpParamMap.map(fp => Mux(isRecNext, (2 * fp.bias - 1).U, (3 * fp.bias - 1).U))),
      Mux(isSubnormalSrcNext, false.B ## 1.U, false.B ## expSrc)
    )
  )

  expAdderIn1 := Mux1H(
    Cat(isInt2FpNext || isFpNarrowNext || isFp2IntNext, isFpWidenNext, isEstimate7Next).asBools.reverse,
    Seq(
      minusExp,
      Mux(isSubnormalSrcNext, minusExp, expSrc),
      Mux(isSubnormalSrcNext, leadZeros, minusExp),
    )
  )
  expNext := expAdderIn0 + expAdderIn1

  // for estimate7
  val expNormaled = Mux(isSubnormalSrcNext, leadZeros(0), expSrc(0)) //only the last bit is needed
  val expNormaled0 = RegEnable(expNormaled(0), false.B, fire)

  /** shift left
   * for: int->fp, fp->fp widen, estimate7, reuse shift left according to fracSrc << (64 - f64.fracWidth)
   * cycle: 0
   *
   */
  val shiftLeft = Wire(UInt(64.W))
  shiftLeft := (Mux(inIsFp, fracSrc << (64 - f64.fracWidth), absIntSrc).asUInt << 1) << leadZeros //cycle0
  // for estimate7 & fp->fp widen
  fracNormaledNext := Mux(isSubnormalSrcNext, shiftLeft, fracSrc << (64 - f64.fracWidth)) //cycle0


  /** shift right
   * for: fp->fp Narrow, fp->int
   * cycle: 0
   *
   */

  // common
  val fracValueSrc = (expNotZeroSrcNext && !expIsOnesSrcNext) ## fracSrc
  val shamtIn = fracValueSrc ## 0.U(11.W) ## false.B  //fp Narrow & fp->int
  val shamtWidth = Mux(!outIsFp, Mux1H(float1HSrcNext, fpParamMap.map(fp => (63+fp.bias).U)),
    Mux1H(float1HOutNext.tail(1), biasDeltaMap.map(delta => (delta + 1).U))
    ) - expSrc
  val shamt = Mux(shamtWidth >= 65.U, 65.U, shamtWidth)
  val (inRounderNext, stickyNext) = ShiftRightJam(shamtIn, shamt)
  val inRounder = RegEnable(inRounderNext, 0.U(65.W), fire)
  val sticky = RegEnable(stickyNext, false.B, fire)


  /** rounder
   * for: int->fp, fp-fp Narrow, fp->int
   * cycle: 1
   */
  rounderMapInNext := Mux(isFpNarrowNext, fracSrc << (64 - f64.fracWidth), shiftLeft)

  val rounderMap =
    fpParamMap.map(fp => Seq(
      rounderMapIn.head(fp.fracWidth),
      rounderMapIn.tail(fp.fracWidth).head(1),
      rounderMapIn.tail(fp.fracWidth + 1).orR,
      rounderMapIn.head(fp.fracWidth).andR
    )
    ).transpose

  val (rounderInputMap, rounerInMap, rounderStikyMap, isOnesRounderInputMap) = {
    (rounderMap(0), rounderMap(1), rounderMap(2), rounderMap(3))
  }

  val rounderInput = Mux(isFp2Int, inRounder.head(64),  Mux1H(float1HOut, rounderInputMap))


  val rounder = Module(new RoundingUnit(64))
  rounder.io.in := rounderInput
  rounder.io.roundIn := Mux(isFp2Int, inRounder(0), Mux1H(float1HOut, rounerInMap))
  rounder.io.stickyIn := Mux(isFp2Int, sticky, Mux1H(float1HOut, rounderStikyMap))
  rounder.io.signIn := signSrc
  rounder.io.rm := rm

  // from rounder
  val nxRounded = rounder.io.inexact
  val upRounded = rounder.io.r_up

  /** after rounding
   *  for all exclude estimate7 & fp->fp widen
   *  cycle: 1
   */
  val expIncrease = exp + 1.U
  val rounderInputIncrease = rounderInput + 1.U

  // for fp2int
  // 8bit: => u64, i64, u32, i32, u16, i16, u8, i8
  val hasSignInt1HOut = int1HOut.asBools.map(oh => Seq(oh && !hasSignInt, oh && hasSignInt)).flatten
  val isOnesRounderInputMapFp2Int =
    intParamMap.map(intType => Seq(intType, intType - 1)).flatten.map(intType => rounderInput.tail(64 - intType).andR)

  // for all
  val cout = upRounded && Mux(isFp2Int,
    Mux1H(hasSignInt1HOut, isOnesRounderInputMapFp2Int),
    Mux1H(float1HOut, isOnesRounderInputMap)
  ).asBool
  val expRounded = Wire(UInt(f64.expWidth.W))
  expRounded := Mux(cout, expIncrease, exp)
  val fracRounded = Mux(upRounded, rounderInputIncrease, rounderInput)

  val rmin =
    rm === RTZ || (signSrc && rm === RUP) || (!signSrc && rm === RDN) //cycle1


  /** Mux/Mux1H
   * cycle: 1
   */
  when(isInt2Fp){
    /** int->fp   any int/uint-> any fp
     */
    // Mux(cout, exp > FP.maxExp -1, exp > FP.maxExp)
    val ofRounded = !exp.head(1).asBool && Mux1H(float1HOut,
      fpParamMap.map(fp => Mux(cout,
        exp(fp.expWidth - 1, 1).andR || exp(exp.getWidth - 2, fp.expWidth).orR,
        exp(fp.expWidth - 1, 0).andR || exp(exp.getWidth - 2, fp.expWidth).orR)
      )
    )

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

    val int2FpResultMap: Seq[UInt] = fpParamMap.map(fp => Mux1H(result1H.asBools.reverse, int2FpResultMapGen(fp)))
    resultNext := Mux1H(float1HOut, int2FpResultMap)

  }.elsewhen(isFpWiden){
    /** fp -> fp widen
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

    nv := isSNaNSrc
    dz := false.B
    of := false.B
    uf := false.B
    nx := false.B

    val fpwidenResultMap: Seq[UInt] = Seq(f32, f64).map(fp => Mux1H(result1H.asBools.reverse, fpWidenResultMapGen(fp)))
    resultNext := Mux1H(float1HOut.head(2), fpwidenResultMap)

  }.elsewhen(isFpNarrow){
    /** fp -> fp Narrow
     * note: IEEE754 uf：exp in (-b^emin, b^emin), after rounding(RiscV!!!)
     * note: IEEE754 uf：exp in (-b^emin, b^emin), before rounding(other)
     */

    /**dest is normal
     */
    // Mux(cout, exp > FP.maxExp -1, exp > FP.maxExp)
    val ofRounded = !exp.head(1).asBool && Mux1H(float1HOut,
      fpParamMap.map(fp => Mux(cout,
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


    nv := isSNaNSrc
    dz := false.B
    of := !expIsOnesSrc && ofRounded
    uf := !expIsOnesSrc && maybeSub && ufExpRounded && subNxRounded
    nx := !expIsOnesSrc && (
      (!maybeSub && nxOfRounded) ||
        (maybeSub && subNxRounded)
      )

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
  }.elsewhen(isEstimate7) {
    /** Estimate7: sqrt7 & rec7
     */

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
    val recResultMap: Seq[UInt] = fpParamMap.map(fp => Mux1H(recResult1H.asBools.reverse, recResultMapGen(fp)))

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
    val sqrtResultMap: Seq[UInt] = fpParamMap.map(fp => Mux1H(sqrtResult1H.asBools.reverse, sqrtResultMapGen(fp)))
    resultNext := Mux(isRec, Mux1H(float1HOut, recResultMap), Mux1H(float1HOut, sqrtResultMap))

  }.otherwise {//5
    // !outIsFp
    /** out is int, any fp->any int/uint
     * drop the shift left!
     * todo: detail refactor exclude
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
      hasSignInt && toInv
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

