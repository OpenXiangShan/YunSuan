package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import yunsuan.vector.VectorConvert.util._
import yunsuan.vector.VectorConvert.utils._
import yunsuan.vector.VectorConvert.RoundingModle._

class CVT64(width: Int = 64) extends CVT(width){

  //parameter
  val fpParamMap = Seq(f16, f32, f64)
  val biasDeltaMap = Seq(f32.bias - f16.bias, f64.bias - f32.bias)
  val intParamMap = (0 to 3).map(i => (1 << i) * 8)
  val widthExpAdder = 14 // 13bits is enough

  // input
  val (src, sew, opType, rm, input1H, output1H) =
      (io.src, io.sew, io.opType, io.rm, io.input1H, io.output1H)

  dontTouch(input1H)
  dontTouch(output1H)

  // output
  val nv, dz, of, uf, nx = Wire(Bool())
  val fflags = Wire(UInt(5.W))
  val result = Wire(UInt(64.W))

  // control
  val widen = opType(4,3) // 0->single 1->widen 2->narrow => width of result
  val isSingle = !opType(4) && !opType(3)
  val isWiden = !opType(4) && opType(3)
  val isnarrow = opType(4) && !opType(3)

  val inIsFp = opType.head(1).asBool
  val outIsFp = opType.tail(1).head(1).asBool

  dontTouch(inIsFp)
  dontTouch(outIsFp)

  val hasSignInt = opType(0).asBool

  val int1HSrc = VecInit((0 to 3).map(i => input1H(2*i))).asUInt
  val float1HSrc = VecInit((1 to 3).map(i => input1H(2*i+1))).asUInt //exclude f8

  val int1HOut = VecInit((0 to 3).map(i => output1H(2 * i))).asUInt
  val float1HOut = VecInit((1 to 3).map(i => output1H(2 * i + 1))).asUInt //exclude f8

  val srcMap = (0 to 3).map(i => src((1 << i) * 8 - 1, 0))
  val intMap = srcMap.map(int => intExtend(int, hasSignInt && int.head(1).asBool))

  val floatMap = srcMap.zipWithIndex.map{case (float,i) => floatExtend(float, i)}.drop(1)
  val input = Mux(inIsFp,
    Mux1H(float1HSrc, floatMap),
    Mux1H(int1HSrc, intMap)
  )


  // data
  /** src common
   * init
   */
  val signSrc = input.head(1).asBool

  /** src is int
   * init
   */
  val absIntSrc = Wire(UInt(64.W))
  absIntSrc := Mux(signSrc, (~input.tail(1)).asUInt + 1.U, input.tail(1))
  val isZeroIntSrc = !absIntSrc.orR

  /** src is float
   * init
   * special: INF, NaN, qNaN, SNaN, 0, Great NF均有正负, canonical NaN
   */
  val expSrc = input.tail(1).head(f64.expWidth)
  val fracSrc = input.tail(f64.expWidth+1).head(f64.fracWidth)

  val decodeFloatSrc = Mux1H(float1HSrc, fpParamMap.map(fp =>
      VecInit(expSrc(fp.expWidth-1,0).orR, expSrc(fp.expWidth-1,0).andR, fracSrc.head(fp.fracWidth).orR).asUInt
    )
  )

  val (expNotZeroSrc, expIsOnesSrc, fracNotZeroSrc) = (decodeFloatSrc(0), decodeFloatSrc(1), decodeFloatSrc(2))
  val expIsZeroSrc = !expNotZeroSrc
  val fracIsZeroSrc = !fracNotZeroSrc
  val isSubnormalSrc = expIsZeroSrc && fracNotZeroSrc
  val isnormalSrc = !expIsOnesSrc && !expIsZeroSrc
  val isInfSrc = expIsOnesSrc && fracIsZeroSrc
  val isZeroSrc = expIsZeroSrc && fracIsZeroSrc
  val isNaNSrc = expIsOnesSrc && fracNotZeroSrc
  val isSNaNSrc = isNaNSrc && !fracSrc.head(1)
  val isQNaNSrc = isNaNSrc && fracSrc.head(1).asBool


  /**
   * todo：未来要砍的，但是砍它和对应的elseWhen时：一定要注意Mux(isRec,,)和Mux(isF16toF32,,)！！！
   */
  // for sqrt7/rec7
  val isEstimate7 = opType(5)
  val isSqrt = opType(5) && !opType(0)
  val isRec = opType(5) && opType(0)

  val decodeFloatSrcRec = Mux1H(float1HSrc,
      fpParamMap.map(fp => expSrc(fp.expWidth - 1, 0)).zip(fpParamMap.map(fp => fp.expWidth)).map { case (exp, expWidth) =>
        VecInit(
          exp.head(expWidth-1).andR && !exp(0),
          exp.head(expWidth-2).andR && !exp(1) && exp(0)
        ).asUInt
      }
  )

  val (isNormalRec0, isNormalRec1) = (decodeFloatSrcRec(0), decodeFloatSrcRec(1))
  val isNormalRec2 = expNotZeroSrc && !expIsOnesSrc && !isNormalRec0 && !isNormalRec1
  val isSubnormalRec0 = isSubnormalSrc && fracSrc.head(1).asBool
  val isSubnormalRec1 = isSubnormalSrc && !fracSrc.head(1) && fracSrc.tail(1).head(1).asBool
  val isSubnormalRec2 = isSubnormalSrc && !fracSrc.head(2).orR

  val signNonNan = !isNaNSrc && signSrc


  /** dest common
   * init
   */
  val rmin =
    rm === RTZ || (signSrc && rm === RUP) || (!signSrc && rm === RDN)
  val fracValueSrc = (expNotZeroSrc && !expIsOnesSrc) ## fracSrc

  /** critical circuit & share
   * 1.count leading zero Max is 64
   * 2.adder: exp
   * 3.shift left/right(UInt)
   * 4.rounding module: +1
   *
   * step1: clz + adder -> compute really exp
   * step2: shift left/right -> put the first one to the correct position
   * step3: rounding
   * step4: select result and fflags by mux1H
   *
   * cycle1: clz
   * cycle1: clz -> exp adder
   * cycle1: clz -> shift left
   *
   * cycle : biasDeltaMap.map(delta => (delta + 1).U)) - expSrc -> shift right(fp->fp narrow)
   * cycle : exp -> 63.S-exp -> shift right(fp->int)
   *
   */
  // type int->fp, fp->fp widen, fp->fp narrow, estimate7, fp->int
  val (isInt2Fp, isFp2FpWiden, isFp2FpNarrow, isFp2Int) =
          (!inIsFp, inIsFp && outIsFp && isWiden, inIsFp && outIsFp && isnarrow, !outIsFp)

  val type1H = Cat(isInt2Fp, isFp2FpWiden, isFp2FpNarrow, isEstimate7, isFp2Int).asBools.reverse

  /** share
   * clz
   * for: int->fp, fp->fp widen, estimate7,  reuse clz according to fracSrc << (64 - f64.fracWidth), fp尾数高位对齐到64bit absint
   * pipe num: 1
   */
  val clzIn = Mux(inIsFp, fracSrc<<(64 - f64.fracWidth), absIntSrc).asUInt
  val leadZeros = CLZ(clzIn)

  /** share
   * exp adder
   * for: all exp compute
   * pipe num: 1
   */

  val expAdderIn0 = Wire(UInt(widthExpAdder.W)) //13bits is enough, 多给1bit
  val expAdderIn1 = Wire(UInt(widthExpAdder.W))
  val exp = Wire(UInt(widthExpAdder.W))

  // todo: refactor
//  val minusLeadZeros = extend((~(false.B ## leadZeros)).asUInt + 1.U, widthExpAdder).asUInt
//  val minusBiasDelta = extend((~(false.B ## Mux1H(float1HOut.tail(1), biasDeltaMap.map(delta => delta.U)))).asUInt + 1.U, widthExpAdder).asUInt
//  val minusExpSrc    = extend((~(false.B ## expSrc)).asUInt + 1.U, widthExpAdder).asUInt
//  val minusBias      = extend((~(false.B ## Mux1H(float1HSrc, fpParamMap.map(fp => fp.bias.U)))).asUInt + 1.U, widthExpAdder).asUInt
  val biasDelta = Mux1H(float1HOut.tail(1), biasDeltaMap.map(delta => delta.U))
  val bias =  Mux1H(float1HSrc, fpParamMap.map(fp => fp.bias.U))
  val minusExp = extend((~(false.B ## Mux1H(Cat(isInt2Fp || isFp2FpWiden, isFp2FpNarrow, isEstimate7, isFp2Int).asBools.reverse,
    Seq(
      leadZeros,
      biasDelta,
      expSrc,
      bias
    )
  ))).asUInt + 1.U, widthExpAdder).asUInt

  expAdderIn0 := Mux1H(type1H, Seq( //全是正的，不用管
      Mux1H(float1HOut, fpParamMap.map(fp => (fp.bias + 63).U)),
      Mux1H(float1HOut.head(2), biasDeltaMap.map(delta => delta.U)),
      Mux(isSubnormalSrc, false.B ## 1.U, false.B ## expSrc),
      Mux1H(float1HOut, fpParamMap.map(fp => Mux(isRec, (2 * fp.bias - 1).U, (3 * fp.bias - 1).U))),
      Mux(isSubnormalSrc, false.B ## 1.U, false.B ## expSrc)
    )
  )

//  expAdderIn1 := Mux1H(type1H, Seq(
//      minusExp,
//      Mux(isSubnormalSrc, minusExp, expSrc),
//      minusExp,
//      Mux(isSubnormalSrc, leadZeros, minusExp),
//      minusExp
//    )
//  )

  expAdderIn1 := Mux1H(Cat(isInt2Fp || isFp2FpNarrow || isFp2Int, isFp2FpWiden, isEstimate7).asBools.reverse, Seq(
      minusExp,
      Mux(isSubnormalSrc, minusExp, expSrc),
      Mux(isSubnormalSrc, leadZeros, minusExp),
    )
  )
  exp := expAdderIn0 + expAdderIn1


  /** share
   * shift left
   * for: int->fp, fp->fp widen, estimate7, reuse shift left according to fracSrc << (64 - f64.fracWidth), fp尾数高位对齐到64bit absint
   * pipe num: 1
   *
   * shiftLeft将int的abs和fp normal的fracSrc都统一到了靠近高位的64bit 对于转浮点有用，已经drop了隐含1
   * fracNormaled将fp规格化和非规格化的尾数统一到了一起（用在estimate7中） todo：看能不能和shift right需要的frac统一起来
   */
  val shiftLeft = Wire(UInt(64.W))
  shiftLeft := (Mux(inIsFp, fracSrc << (64 - f64.fracWidth), absIntSrc).asUInt << 1) << leadZeros
  // for estimate7 & fp->fp widen
  val fracNormaled = Wire(UInt(64.W))
  fracNormaled := Mux(isSubnormalSrc, shiftLeft, fracSrc << (64 - f64.fracWidth))

  /** share
   * shift right
   * for: fp->fp narrow, fp->int
   * in fp->fp narrow sub中: 右移之前shamt不需要等 exp的结果(但与normal应该同时进行: shamt= biasDetal +1 -expSrc
   * 而在fp->int中(63.S - expValue)需要等exp的结果；shamt =(63.S - expValue)=> 63 + bias - expSrc
   * 所以另外用一个加法器来计算这两种shamt
   */

  val shamtIn = fracValueSrc ## 0.U(11.W) ## false.B  //在这里 fp narrow和 fp->int是一致的
  val shamtWidth = Mux(!outIsFp, Mux1H(float1HSrc, fpParamMap.map(fp => (63+fp.bias).U)),
    Mux1H(float1HOut.tail(1), biasDeltaMap.map(delta => (delta + 1).U))
    ) - expSrc

  val (inRounder, sticky) = ShiftRightJam(shamtIn, shamtWidth) //todo:最多移64位，最好作下限制

  /** share
   * rounder adder
   * for: int->fp, fp-fp narrow, fp->int
   * ？？fp narrow中sub和normal是不是互斥的（不是，narrow的结果里需要sub和nor的结果，所以让其并行执行
   *
   * done：由于shiftLeft将int的abs和fp normal的fracSrc都统一到了靠近高位的64bit,前面这句话不正确！！！
   * 所以将 int->fp 和fp->fp的normal统一起来了，
   * 因为结果是根据result1HOut来选的，所以fp->fp norrow的rounderMap直接包含于int-> fp的，直接砍掉fp->fp norrow的rounderMap
   * 再Mux一下把fp->int的也砍掉
   */
  //todo:用力想想why这里要多这个，fp norrow src is normal 会出现 out is subnormal，且隐含1被干掉了
  val rounderMapIn = Mux(isFp2FpNarrow, fracSrc << (64 - f64.fracWidth), shiftLeft)
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
   */
  val expIncrease = exp + 1.U //todo：注意exp有无可能在为0或负时指数计算出错
  val rounderInputIncrease = rounderInput + 1.U

  // for fp2int
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
  //note:expNormaled is for estimate7 & fp->fp widen, 他们是不同的


  /** share
   * all
   * todo:
   * for: all
   */


  when(!inIsFp){//1
    /** int->fp   any int/uint-> any fp
     *
     * done: extend u/i to i65
     *
     */

//    val expRounded = Wire(UInt(f64.expWidth.W))
//    // out of roundingUint
//    val fracRounded = Mux(upRounded, rounderInputIncrease, rounderInput)
//    val cout = upRounded && Mux1H(float1HOut, isOnesRounderInputMap).asBool
//    expRounded := Mux(cout, expIncrease, exp)

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
        case 2 => signSrc ## 0.U((fp.width - 1).W) // 0 是不是包含在下面一种情况下,不能因为在rounder时没有排除这种情况,全零进rounder出来的1##全零是非零
        case 3 => signSrc ## expRounded(fp.expWidth-1, 0) ## fracRounded(fp.fracWidth-1, 0) // normal
      })
    }

    val int2FpResultMap: Seq[UInt] = fpParamMap.map(fp => Mux1H(result1H.asBools.reverse, int2FpResultMapGen(fp)))
    result := Mux1H(float1HOut, int2FpResultMap)

  }.elsewhen(inIsFp && outIsFp && isWiden){//2 todo: 砍elsewhen时一定要注意 f16->f32->f64的定义
    /** fp -> fp widen
     *
     * done: share inner
     *
     */
    val expNormaled = Wire(UInt(f64.expWidth.W))
    expNormaled := exp

    def fpWidenResultMapGen(fp: FloatFormat): Seq[UInt] = {
      VecInit((0 to 2).map {
        case 0 => signNonNan ## ~0.U(fp.expWidth.W) ## fracNotZeroSrc ## 0.U((fp.fracWidth - 1).W) // INF or NaN->QNAN
        case 1 => signNonNan ## 0.U((fp.width - 1).W) // 0
        case 2 => signNonNan ## expNormaled(fp.expWidth - 1, 0) ## fracNormaled.head(fp.fracWidth)
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
    result := Mux1H(float1HOut.head(2), fpwidenResultMap)

  }.elsewhen(inIsFp && outIsFp && isnarrow){//3
    /** fp -> fp narrow
     *
     * note:下溢：非零结果看作无限指数范围计算，舍入之后非零结果严格处于(-b^emin, b^emin)区间，或者
     * note: 非零结果看作无限指数范围和无限精度计算，舍入之前非零结果严格处于(-b^emin, b^emin)区间。
     * done: share exp adder & rounder inner
     *
     */

//    val interExp = exp.asSInt
    /**
     * ！！！dest is normal todo：它什么都不用等，sub要等指数计算和右移的结果
     */

    // out of roundingUint
//    val fracRounded = Mux(upRounded, rounderInputIncrease, rounderInput)
//    val cout = upRounded && Mux1H(float1HOut, isOnesRounderInputMap).asBool
//    val expRounded = Mux(cout, expIncrease, exp)

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

    /**
     * ！！！dest is Subnormal
     */
    //目的格式原点前一位的实际指数为1-toBias，差值为1-toBias - (srcExp - srcBias)
    //或者，按照注释的理解在浮点格式中（加偏置之后），原点的前一位指数为1，src->to之后为down_exp = srcExp - srcBias + toBias
    //val maybeSub = interExp < 1.S
    val maybeSub = exp.head(1).asBool || !exp.orR
    val subFracRounded = Wire(UInt(f32.fracWidth.W))
    val subExpRounded = Wire(UInt(f32.expWidth.W))

    val (subFrac, shiftSticky) = (inRounder, sticky)
    val subRounderMap =
      Seq(f16, f32).map(fp => Seq(
        subFrac.tail(1).head(fp.fracWidth),
        subFrac.tail(fp.fracWidth+1).head(1),  //去掉1+toFracWidth +1的头1位和尾1位
        fracSrc.tail(f32.fracWidth).orR || shiftSticky || subFrac.tail(fp.fracWidth+2).orR, //???
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
    subRounder.io.stickyIn := Mux1H(float1HOut.tail(1), subRounderStikyMap) //todo 找到准确的那几位来进行orR
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
      !expIsOnesSrc && !maybeSub && ofRounded && (rmin || (io.rm === RTO)),
      !expIsOnesSrc && !maybeSub && ofRounded && !(rmin || (io.rm === RTO)),
      !expIsOnesSrc && !maybeSub && !ofRounded,
      !expIsOnesSrc && maybeSub
    )

    def fpnarrowResultMapGen(fp: FloatFormat): Seq[UInt] ={
      VecInit((0 to 4).map {
        case 0 => signNonNan ## ~0.U(fp.expWidth.W) ## fracNotZeroSrc ## 0.U((fp.fracWidth - 1).W)  // INF or NaN->QNAN
        case 1 => signNonNan ## fp.maxExp.U(fp.expWidth.W) ## ~0.U(fp.fracWidth.W)                  // of => great FN
        case 2 => signNonNan ## (fp.maxExp + 1).U(fp.expWidth.W) ## 0.U(fp.fracWidth.W)             // of => great FN
        case 3 => signNonNan ## expRounded(fp.expWidth - 1, 0) ## fracRounded(fp.fracWidth - 1, 0)  // normal
        case 4 => signNonNan ## subExpRounded(fp.expWidth - 1, 0) ## subFracRounded(fp.fracWidth - 1, 0) //sub or uf
      })
    }

    val fpnarrowResultMap: Seq[UInt] = Seq(f16, f32).map(fp => Mux1H(result1H.asBools.reverse, fpnarrowResultMapGen(fp)))
    result := Mux1H(float1HOut.tail(1), fpnarrowResultMap)
  }.elsewhen(isEstimate7) {//4 todo: 砍elsewhen时一定要注意 isSqrt/isRec 的定义！！！
    /** Estimate7: sqrt7 & rec7
     *
     * done: share adder inner
     *
     */

    val expNormaled = Mux(isSubnormalSrc, -leadZeros, expSrc)
    val expEstimate = exp

    val rsqrt7Table = Module(new Rsqrt7Table)
    rsqrt7Table.src := expNormaled(0) ## fracNormaled.head(6)
    val rec7Table = Module(new Rec7Table)
    rec7Table.src := fracNormaled.head(7)
    val fracEstimate = Mux(opType(0), rec7Table.out, rsqrt7Table.out)

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
        case 5 => signSrc ## expEstimate(fp.expWidth - 1, 0) ## fracEstimate ## 0.U((fp.fracWidth - 7).W)
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
        case 2 => signSrc ## expEstimate(fp.expWidth, 1) ## fracEstimate ## 0.U((fp.fracWidth - 7).W) // exp/2 => >>1
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
    result := Mux(isRec, Mux1H(float1HOut, recResultMap), Mux1H(float1HOut, sqrtResultMap))

  }.elsewhen(!outIsFp) {//5
    /** out is int, any fp->any int/uint
     *
     * done: deal the width to 64bits, drop the shift left!
     * todo: detail refactor 包括特殊情况的排除和排除条件的生成
     *
     */

//    // 8bit: => u64, i64, u32, i32, u16, i16, u8, i8
//    val hasSignInt1HOut = int1HOut.asBools.map(oh => Seq(oh && !hasSignInt, oh && hasSignInt)).flatten
//    val isOnesRounderInputMap =
//      intParamMap.map(intType =>Seq(intType, intType-1)).flatten.map(intType => rounderInput.tail(64 - intType).andR)

    // out of roundingUint
//    val resultRounded = Mux(upRounded, rounderInputIncrease, rounderInput)
//    val cout = upRounded && Mux1H(hasSignInt1HOut, isOnesRounderInputMap)
//    val expRounded = Mux(cout, expIncrease, exp).asSInt
//    val isZeroRounded = !resultRounded.orR

    val resultRounded = fracRounded
    val isZeroRounded = !resultRounded.orR

    val normalResult = Mux(signSrc && resultRounded.orR, (~resultRounded).asUInt + 1.U, resultRounded) //排除0     补码

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

    val toInv = ofExpRounded && !(signSrc && excludeExp && excludeFrac) || expIsOnesSrc //nv has included inf & nan, 排除负的最大指数
    val toInx = !toInv && nxRounded

    nv := Mux(hasSignInt, toInv, toUnv)
    dz := false.B
    of := false.B
    uf := false.B
    nx := Mux(hasSignInt, toInx, toUnx)


    val result1H = Cat(
      (!hasSignInt && !toUnv) || (hasSignInt && !toInv), //toUnv包含nan和inf
      !hasSignInt && toUnv && (isNaNSrc || !signSrc && (isInfSrc || ofExpRounded)),
      !hasSignInt && toUnv && signSrc && !isNaNSrc,
      hasSignInt && toInv,
    )

//    def toIntResulutMapGen(intType: Int): Seq[UInt]={ //todo : refactor
//      VecInit((0 to 3).map {
//        case 0 => normalResult
//        case 1 => (~0.U(intType.W)).asUInt
//        case 2 => 0.U(intType.W)
//        case 3 => signNonNan ## Fill(intType-1, !signNonNan)
//      })
//    }
//
//    val intResultMap: Seq[UInt] = intParamMap.map(intType => Mux1H(result1H.asBools.reverse, toIntResulutMapGen(intType)))
//    result := Mux1H(int1HOut, intResultMap)

    result := Mux1H(result1H.asBools.reverse, Seq(
        normalResult,
        (~0.U(64.W)).asUInt,
        0.U(64.W),
        Mux1H(int1HOut, intParamMap.map(intType => signNonNan ## Fill(intType - 1, !signNonNan)))
      )
    )

  }.otherwise{
    nv := false.B
    dz := false.B
    of := false.B
    uf := false.B
    nx := false.B
    result := 0.U
  }

  fflags := Cat(nv, dz, of, uf, nx)
  io.result := result
  io.fflags := fflags
}

