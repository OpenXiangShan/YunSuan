package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import yunsuan.vector.VectorConvert.util._
import yunsuan.vector.VectorConvert.utils._
import yunsuan.vector.VectorConvert.RoundingModle._

class CVT64(width: Int = 64) extends CVT(width){

  // input
  val src = io.src
  val sew = io.sew
  val opType = io.opType
  val rm = io.rm
  val input1H = io.input1H
  val output1H = io.output1H

  dontTouch(input1H)
  dontTouch(output1H)

  val widen = opType(4,3) // 0->single 1->widen 2->norrow => width of result

  val isSingle = !opType(4) && !opType(3)
  val isWiden = !opType(4) && opType(3)
  val isNorrow = opType(4) && !opType(3)

  val inIsFp = opType.head(1).asBool
  val outIsFp = opType.tail(1).head(1).asBool

  dontTouch(inIsFp)
  dontTouch(outIsFp)

  val hasSignInt = opType(0).asBool

  // output
  val fflags = Wire(UInt(5.W))
  val result = Wire(UInt(64.W))

  val int1HSrc = VecInit((0 to 3).map(i => input1H(2*i))).asUInt
  val float1HSrc = VecInit((1 to 3).map(i => input1H(2*i+1))).asUInt //exclude f8

  val int1HOut = VecInit((0 to 3).map(i => output1H(2 * i))).asUInt
  val float1HOut = VecInit((1 to 3).map(i => output1H(2 * i + 1))).asUInt //exclude f8

  val fpMap = Seq(f16, f32, f64)

  val srcMap = (0 to 3).map(i => src((1 << i) * 8 - 1, 0))
  val intMap = srcMap.map(int => intExtend(int, hasSignInt && int.head(1).asBool))

  val floatMap = srcMap.zipWithIndex.map{case (float,i) => floatExtend(float, i)}.drop(1)
  val input = Mux(inIsFp,
    Mux1H(float1HSrc, floatMap),
    Mux1H(int1HSrc, intMap)
  )

  /** src common
   * init
   */
  val signSrc = input.head(1).asBool
  //  commom
  //  val firstOneSrc = Mux(inIsFp, Mux(expIsZeroSrc, 1.U,expSrc) - biasSrc, leftFirstOne(absIntSrc))

  /** src is int
   * init
   */
  val absIntSrc = Wire(UInt(64.W))
  absIntSrc := Mux(signSrc, (~input.tail(1)).asUInt + 1.U, input.tail(1))
  val isZeroIntSrc = !absIntSrc.orR


  /** src is float
   * init
   */
  val expSrc = input.tail(1).head(f64.expWidth)
  val fracSrc = input.tail(f64.expWidth+1).head(f64.fracWidth)
  val fracValueSrc = true.B ## fracSrc

  val expWidthSrc = Mux1H(float1HSrc, Seq(f16.expWidth.U, f32.expWidth.U, f64.expWidth.U))
  val fracWidthSrc = Mux1H(float1HSrc, Seq(f16.fracWidth.U, f32.fracWidth.U, f64.fracWidth.U))
  val biasSrc = Mux1H(float1HSrc, Seq(f16.bias.U, f32.bias.U, f64.bias.U))

  val expf16Src = expSrc(f16.expWidth - 1, 0)
  val expf32Src = expSrc(f32.expWidth - 1, 0)
  val expf64Src = expSrc(f64.expWidth - 1, 0)
  val fracf16Src = fracSrc.head(f16.fracWidth)
  val fracf32Src = fracSrc.head(f32.fracWidth)
  val fracf64Src = fracSrc.head(f64.fracWidth)

  val decodeFloatSrc = Mux1H(float1HSrc, Seq( // todo: refactor
    VecInit(expf16Src.orR, expf16Src.andR, fracf16Src.orR).asUInt, //f16
    VecInit(expf32Src.orR, expf32Src.andR, fracf32Src.orR).asUInt, //f32
    VecInit(expf64Src.orR, expf64Src.andR, fracf64Src.orR).asUInt  //f64
  )).asBools

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
  // special
  // INF, NaN, qNaN, SNaN, 0, Great NF均有正负
  // canonical NaN

  /**
   * todo：未来要砍的，但是砍它和对应的elseWhen时：一定要注意Mux(isRec,,)和Mux(isF16toF32,,)！！！
   */
  // for sqrt7/rec7
  val isEstimate7 = opType(5)
  val isSqrt = opType(5) && !opType(0)
  val isRec = opType(5) && opType(0)
  val isF16toF32 = input1H(3) && output1H(5)
  val isF32toF64 = input1H(5) && output1H(7)

  // for rec7
  //  val isNormalRec0 = expSrc >= (2*b).U && expSrc < (2*b + 1).U
  //  val isNormalRec1 = expSrc >= (2*b - 1).U && expSrc < (2*b).U
  //  val isNormalRec2 = expSrc >= 1.U && expSrc < (2*b - 1).U
  val decodeFloatSrcRec = Mux1H(float1HSrc, Seq(  // todo: refactor
    VecInit(expf16Src.head(f16.expWidth - 1).andR && !expf16Src(0),
      expf16Src.head(f16.expWidth - 2).andR && !expf16Src(1) && expf16Src(0)).asUInt, //f16
    VecInit(expf32Src.head(f32.expWidth - 1).andR && !expf32Src(0),
      expf32Src.head(f32.expWidth - 2).andR && !expf32Src(1) && expf32Src(0)).asUInt, //f32
    VecInit(expf64Src.head(f64.expWidth - 1).andR && !expf64Src(0),
      expf64Src.head(f64.expWidth - 2).andR && !expf64Src(1) && expf64Src(0)).asUInt //f64
  )).asBools

  val (isNormalRec0, isNormalRec1) = (decodeFloatSrcRec(0), decodeFloatSrcRec(1))
  val isNormalRec2 = expNotZeroSrc && !expIsOnesSrc && !isNormalRec0 && !isNormalRec1
  val isSubnormalRec0 = isSubnormalSrc && fracSrc.head(1).asBool
  val isSubnormalRec1 = isSubnormalSrc && !fracSrc.head(1) && fracSrc.tail(1).head(1).asBool
  val isSubnormalRec2 = isSubnormalSrc && !fracSrc.head(2).orR

  val signNonNan = !isNaNSrc && signSrc


  /** dest is float
   * init
   */
  val expWidthOut = Mux1H(float1HOut, fpMap.map(fp => fp.expWidth.U))
  val fracWidthOut = Mux1H(float1HOut, fpMap.map(fp => fp.fracWidth.U))
  val biasOut = Mux1H(float1HOut, fpMap.map(fp => fp.bias.U))


  /** dest is int
   * init
   */
  val maxInt = (0 to 3).map(i => !hasSignInt ## ~0.U(((1 << i) * 8 - 1).W))
  val minInt = (0 to 3).map(i => hasSignInt ## 0.U(((1 << i) * 8 - 1).W))

//  val toWidthValue = if (toFormat.hasSign) toWidth - 1 else toWidth

  /** dest common
   * init
   */
  val rmin =
    rm === RTZ || (signSrc && rm === RUP) || (!signSrc && rm === RDN)

  //    val resultOverflow = Mux(rmin, //todo：要不要拆这个, 看到时候能不能和后面统一起来
  //      f64.maxExp.U(f64.expWidth.W) ## ~0.U(f64.fracWidth.W), // great FN
  //      (f64.maxExp + 1).U(f64.expWidth.W) ## 0.U(f64.fracWidth.W) // INF
  //    )

  val nv = Wire(Bool())
  val dz = Wire(Bool())
  val of = Wire(Bool())
  val uf = Wire(Bool())
  val nx = Wire(Bool())


  /** critical circuit
   * 1.count leading zero Max is 64
   * 2.adder:
   * f->f  : expSrc - biasSrc.U
   * f->i  : expSrc - biasSrc.U
   * i->f  : firstOne + biasOut.U
   * sqrt7 :
   * rec7  :
   * 3.shift left/right(UInt)
   * 4.rounding module
   *
   * step1: clz + adder -> compute really exp
   * step2: shift -> put the first one to the correct position
   * step3: rounding
   * step4: select result and fflags by mux1H
   *
   */

  // clz
  val clzIn = Mux(inIsFp, fracSrc<<(64 - f64.fracWidth), absIntSrc).asUInt
  val leadZeros = CLZ(clzIn)

  // adder todo: 干掉所有的减号 => 自己按位取反 和 比较符号 => 逻辑运算

  // shift left
  val absShiftLeft = Wire(UInt(64.W))
  absShiftLeft :=  (absIntSrc << 1) << leadZeros //abs 64bit todo: replace
  val fracShiftLeft = (fracSrc << 1) << leadZeros   //frac 53bit todo : replace

  // shift right

  // rounder todo: 抽那个+1的加法

  // mux


  when(!inIsFp && outIsFp){//1
    /** any Int-> any fp
     */
    // todo: circuit need to reuse, done. Maxbias is 1023, MaxfirstOne is 64(Int), Maxexp is 2048, so the 13bits for adder is enough.
    val exp = Wire(UInt(f64.expWidth.W))
    val expRounded = Wire(UInt(f64.expWidth.W))
    exp := Mux1H(float1HOut, fpMap.map(fp => (fp.bias + 63).U)) - leadZeros //todo 尝试去和estimate7的加法器重用  adder1

    val rounderMap =
      fpMap.map(fp => Seq(
        absShiftLeft.head(fp.fracWidth),
        absShiftLeft.tail(fp.fracWidth).head(1),
        absShiftLeft.tail(fp.fracWidth + 1).orR,
        absShiftLeft.head(fp.fracWidth).andR
      )
    ).transpose

    val (rounderInputMap, rounerInMap, rounderStikyMap, isOnesRounderInputMap) = {
      (rounderMap(0), rounderMap(1), rounderMap(2), rounderMap(3))
    }

    val rounder = Module(new RoundingUnit(f64.fracWidth)) //todo: circuit is abled to reuse
    val rounderInput = Mux1H(float1HOut, rounderInputMap)
    rounder.io.in := rounderInput // todo:拿不到正确的，这里没有问题
    rounder.io.roundIn := Mux1H(float1HOut, rounerInMap)
    rounder.io.stickyIn :=  Mux1H(float1HOut, rounderStikyMap) //todo 找到准确的那几位来进行orR
    rounder.io.signIn := signSrc
    rounder.io.rm := rm

    // from roundingUnit
    val nxRounded = rounder.io.inexact
    val upRounded = rounder.io.r_up

    // out of roundingUint
    val fracRounded = Mux(upRounded, rounderInput + 1.U, rounderInput) //todo：加法重用 adder2！！！
    val cout = upRounded && Mux1H(float1HOut, isOnesRounderInputMap).asBool

    expRounded := Mux(cout, exp + 1.U, exp) // todo: adder3
    val ofRounded = Mux(cout,
        exp > Mux1H(float1HOut, fpMap.map(fp => (fp.maxExp-1).U)),
        exp > Mux1H(float1HOut, fpMap.map(fp => fp.maxExp.U))
    ) // todo: adder4

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

    val int2FpResultMap: Seq[UInt] = fpMap.map(fp => Mux1H(result1H.asBools.reverse, int2FpResultMapGen(fp)))
    result := Mux1H(float1HOut, int2FpResultMap)

  }.elsewhen(inIsFp && outIsFp && isWiden){//2 todo: 砍elsewhen时一定要注意 f16->f32->f64的定义
    /** fp -> fp widen
     * 1.count leading zero    :done
     * 2.compute exp: adder    :todo
     * 3.compute frac: shift   :done
     */

    val fracNormaled = Wire(UInt(f64.fracWidth.W))
    fracNormaled := Mux(isSubnormalSrc, (fracSrc << 1) << leadZeros, fracSrc)

    //  f16->f32->f64
    //  val expNormaled = Mux(isSubnormalSrc, -leadZeros, expSrc)
    //  f16tof32 := (f32.bias - f16.bias).U - expNormaled)
    //  f32tof64 := (f64.bias - f32.bias).U - expNormaled
    //  todo: 去和 estimate7 重用adder
    val biasDelta =  Wire(UInt(f64.expWidth.W)) //todo:注意位宽
    biasDelta := Mux1H(float1HOut.head(2), Seq((f32.bias - f16.bias).U, (f64.bias - f32.bias).U))
    val minusExp = Mux(isSubnormalSrc,
                        extend((~(false.B ## leadZeros)).asUInt + 1.U, biasDelta.getWidth),
                        expSrc).asUInt
    val expWiden = biasDelta + minusExp

    val expNormaled = Wire(UInt(f64.expWidth.W)) //todo:重用的时候需要去调整这里的位宽，取高一点，否则在estimate7时会出现高位被截断
    expNormaled := expWiden

    def fpWidenResultMapGen(fp: FloatFormat): Seq[UInt] = {
      VecInit((0 to 2).map {
        case 0 => signNonNan ## ~0.U(fp.expWidth.W) ## fracNotZeroSrc ## 0.U((fp.fracWidth - 1).W) // INF or NaN->QNAN
        case 1 => signNonNan ## 0.U((fp.width - 1).W) // 0
        case 2 => signNonNan ## expNormaled(fp.expWidth - 1, 0) ## fracNormaled(f64.fracWidth - 1, f64.fracWidth - fp.fracWidth)
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
  }.elsewhen(inIsFp && outIsFp && isNorrow){//3
    /** fp -> fp norrow
     * 下溢：非零结果看作无限指数范围计算，舍入之后非零结果严格处于(-b^emin, b^emin)区间，或者
     * 非零结果看作无限指数范围和无限精度计算，舍入之前非零结果严格处于(-b^emin, b^emin)区间。
     */

    val interExp = Mux(isSubnormalSrc, false.B ## 1.U, false.B ## expSrc).asSInt -
      Mux1H(float1HOut.tail(1), Seq((f32.bias - f16.bias).S,(f64.bias - f32.bias).S)) //todo: have to think of sign

    /**
     * normal
     */
    val rounderMap =
      Seq(f16, f32).map(fp => Seq(
        fracSrc.head(fp.fracWidth),
        fracSrc.tail(fp.fracWidth).head(1),
        fracSrc.tail(fp.fracWidth + 1).orR,
        fracSrc.head(fp.fracWidth).andR
        )
      ).transpose

    val (rounderInputMap, rounerInMap, rounderStikyMap, isOnesRounderInputMap) = {
      (rounderMap(0), rounderMap(1), rounderMap(2), rounderMap(3))
    }

    val rounder = Module(new RoundingUnit(f32.fracWidth)) //todo: circuit is abled to reuse
    val rounderInput = Mux1H(float1HOut.tail(1), rounderInputMap)
    rounder.io.in := rounderInput // todo:拿不到正确的，这里没有问题
    rounder.io.roundIn := Mux1H(float1HOut.tail(1), rounerInMap)
    rounder.io.stickyIn := Mux1H(float1HOut.tail(1), rounderStikyMap) //todo 找到准确的那几位来进行orR
    rounder.io.signIn := signSrc
    rounder.io.rm := rm

    // from roundingUnit
    val nxRounded = rounder.io.inexact
    val upRounded = rounder.io.r_up

    // out of roundingUint
    val fracRounded = Mux(upRounded, rounderInput + 1.U, rounderInput) //todo：加法重用 adder2！！！
    val cout = upRounded && Mux1H(float1HOut.tail(1), isOnesRounderInputMap).asBool

    val expRounded = Mux(cout, interExp + 1.S, interExp) // todo: adder3
    val ofRounded = Mux(cout,
      interExp > Mux1H(float1HOut.tail(1), Seq(f16, f32).map(fp => (fp.maxExp - 1).S)),
      interExp > Mux1H(float1HOut.tail(1), Seq(f16, f32).map(fp => fp.maxExp.S))
    ) // todo: adder4

    val ufExpRounded = Mux(cout, interExp < 0.S, interExp < 1.S) // todo: adder4
    val nxOfRounded = nxRounded || ofRounded

    /**
     * Subnormal
     */
    //目的格式原点前一位的实际指数为1-toBias，差值为1-toBias - (srcExp - srcBias)
    //或者，按照注释的理解在浮点格式中（加偏置之后），原点的前一位指数为1，src->to之后为down_exp = srcExp - srcBias + toBias
    val maybeSub = interExp < 1.S  // todo: adder5
    val subFracRounded = Wire(UInt(f32.fracWidth.W))
    val subExpRounded = Wire(UInt(f32.expWidth.W))

    val shamt = Mux1H(float1HOut.tail(1), Seq((f32.bias - f16.bias + 1).U,(f64.bias - f32.bias + 1).U)) - expSrc //todo: adder6
    val (subFrac, shiftSticky) = ShiftRightJam(     // todo: reuse
      Cat(expNotZeroSrc, fracSrc.head(f32.fracWidth + 1)), //src是否是规格化的，输入的位宽为1+toFracWidth +1
      shamt
    )

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
    subRounder.io.in := subRounderInput // todo:拿不到正确的，这里没有问题
    subRounder.io.roundIn := Mux1H(float1HOut.tail(1), subRounerInMap)
    subRounder.io.stickyIn := Mux1H(float1HOut.tail(1), subRounderStikyMap) //todo 找到准确的那几位来进行orR
    subRounder.io.signIn := signSrc
    subRounder.io.rm := rm

    // from roundingUnit
    val subNxRounded = subRounder.io.inexact
    val subUpRounded = subRounder.io.r_up

    // out of roundingUint
    subFracRounded := Mux(subUpRounded, subRounderInput + 1.U, subRounderInput) //todo: adder7
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

    def fpNorrowResultMapGen(fp: FloatFormat): Seq[UInt] ={
      VecInit((0 to 4).map {
        case 0 => signNonNan ## ~0.U(fp.expWidth.W) ## fracNotZeroSrc ## 0.U((fp.fracWidth - 1).W)  // INF or NaN->QNAN
        case 1 => signNonNan ## fp.maxExp.U(fp.expWidth.W) ## ~0.U(fp.fracWidth.W)                  // of => great FN
        case 2 => signNonNan ## (fp.maxExp + 1).U(fp.expWidth.W) ## 0.U(fp.fracWidth.W)             // of => great FN
        case 3 => signNonNan ## expRounded(fp.expWidth - 1, 0) ## fracRounded(fp.fracWidth - 1, 0)  // normal
        case 4 => signNonNan ## subExpRounded(fp.expWidth - 1, 0) ## subFracRounded(fp.fracWidth - 1, 0) //sub or uf
      })
    }

    val fpNorrowResultMap: Seq[UInt] = Seq(f16, f32).map(fp => Mux1H(result1H.asBools.reverse, fpNorrowResultMapGen(fp)))
    result := Mux1H(float1HOut.tail(1), fpNorrowResultMap)
  }.elsewhen(isEstimate7) {//5 todo: 砍elsewhen时一定要注意 isSqrt/isRec 的定义！！！
    /** Estimate7 opType(5)
     * 1.count leading zero    :done
     * 2.compute exp: adder    :todo
     * 3.compute frac: shift left   :done
     */

    //-leadZeros： -Uint => 位宽不变，按位取反加1，但这里只用到了最低位，所以没问题
    val expNormaled = Mux(isSubnormalSrc, -leadZeros, expSrc)
    val fracNormaled = Wire(UInt(f64.fracWidth.W))
    fracNormaled := Mux(isSubnormalSrc, (fracSrc << 1) << leadZeros, fracSrc)

    //  expSqrt := ((3 * f64.bias - 1).U - expNormaled) >> 1
    //  expRec := (2 * f64.bias - 1).U - expNormaled
    val biasEstimate = Mux(isRec, Mux1H(float1HOut, fpMap.map(fp => (2 * fp.bias - 1).U)),
                                  Mux1H(float1HOut, fpMap.map(fp => (3 * fp.bias - 1).U)))
    val minusExp = Mux(isSubnormalSrc, leadZeros,
                                        extend( (~(false.B ## expSrc)).asUInt + 1.U, biasEstimate.getWidth)).asUInt
    val expEstimate = biasEstimate + minusExp // todo: reuse 和fp2fp widen

    val rsqrt7Table = Module(new Rsqrt7Table)
    rsqrt7Table.src := expNormaled(0) ## fracNormaled.head(6)
    val rec7Table = Module(new Rec7Table)
    rec7Table.src := fracNormaled.head(7)
    val fracEstimate = Mux(opType(0), rec7Table.out, rsqrt7Table.out)

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

    def sqrtResultMapGen(fp: FloatFormat): Seq[UInt] = {
      VecInit((0 to 3).map {
        case 0 => false.B ## ~0.U(fp.expWidth.W) ## true.B ## 0.U((fp.fracWidth - 1).W)
        case 1 => signSrc ## ~0.U(fp.expWidth.W) ## 0.U(fp.fracWidth.W)
        case 2 => signSrc ## expEstimate(fp.expWidth, 1) ## fracEstimate ## 0.U((fp.fracWidth - 7).W) // exp/2 => >>1
        case 3 => 0.U(fp.width.W)
      })
    }

    when(opType(0)){//5
      /**rec7: opType(5) & opType(0)
       */
      nv := isSNaNSrc
      dz := isZeroSrc
      of := isSubnormalRec2
      uf := false.B
      nx := isSubnormalRec2

      val result1H = Cat(
        isNaNSrc,
        isInfSrc,
        isZeroSrc || isSubnormalRec2 && !rmin,
        isNormalRec0,
        isNormalRec1,
        isNormalRec2 || isSubnormalRec0 || isSubnormalRec1,
        isSubnormalRec2 && rmin
      )

      val recResultMap: Seq[UInt] = fpMap.map(fp => Mux1H(result1H.asBools.reverse, recResultMapGen(fp)))
      result := Mux1H(float1HOut, recResultMap)
    }.otherwise{//6
      /** rsqrt7: opType(5) & !opType(0)
       */
      nv := (signSrc && !isZeroSrc && !isQNaNSrc) | isSNaNSrc
      dz := isZeroSrc
      of := false.B
      uf := false.B
      nx := false.B

      val result1H = Cat(
        signSrc & !isZeroSrc | isNaNSrc,
        isZeroSrc,
        !signSrc & !isZeroSrc & !expIsOnesSrc,
        !signSrc & isInfSrc,
      )

      val sqrtResultMap: Seq[UInt] = fpMap.map(fp => Mux1H(result1H.asBools.reverse, sqrtResultMapGen(fp)))
      result := Mux1H(float1HOut, sqrtResultMap)
    }

  }.elsewhen(input1H(7) && output1H(6) && !opType(0)) {//7
    /**
     * out is Uint
     */
    /** f64->i64 input1H(7) && output1H(6)
     *  ->anyUInt fix f64->f16/f32
     *
     *  deal the width to 64bits, drop the shift left!!!
     *
     */

    val fracFp2In = fracValueSrc ## 0.U(11.W)
    val expValue = Mux(isSubnormalSrc, 1.U, false.B ## expSrc).asSInt - f64.bias.S

    val (inRounder, sticky) = ShiftRightJam(fracFp2In ## false.B, (63.S - expValue).asUInt)
    val rounder = Module(new RoundingUnit(64))
    rounder.io.in := inRounder.head(64) //drop least bit
    rounder.io.roundIn := inRounder(0)
    rounder.io.stickyIn := sticky
    rounder.io.signIn := signSrc
    rounder.io.rm := rm

    val resultRounded = rounder.io.out
    val nxRounded = rounder.io.inexact
    val cout = rounder.io.cout //如果有进位说明原来>towidth个1
    val expRounded = Mux(cout, expValue + 1.S, expValue)

//    val normalResult = Mux(signSrc, (~resultRounded).asUInt + 1.U, resultRounded) // todo: fix
    val normalResult = resultRounded // todo: fix

    nv := expRounded >= 64.S || signSrc  //todo: fix
    dz := false.B
    of := false.B
    uf := false.B
    nx := !nv && nxRounded && !signSrc

    val result1H = Cat(
      !isNaNSrc && !nv,
      !signSrc && (isInfSrc || (expRounded >= 64.S)) || isNaNSrc,
      signSrc && !isNaNSrc
    )

    val resultMap = VecInit((0 to 2).map {
      case 0 => normalResult
      case 1 => ~0.U(64.W)
      case 2 => 0.U(64.W)
    })

    result := Mux1H(result1H.asBools.reverse, resultMap)
  }.elsewhen (input1H(7) && output1H(6) && opType(0)) {//8
    /** f64->i64 input1H(7) && output1H(6)
     * out is Int
     * deal the width to 63bits, drop the shift left
     */

    val fracFp2In = fracValueSrc ## 0.U(10.W) //todo:
    val expValue = Mux(isSubnormalSrc, false.B ## 1.U, false.B ## expSrc).asSInt - f64.bias.S

    val (inRounder, sticky) = ShiftRightJam(fracFp2In ## false.B, (62.S - expValue).asUInt)
    val rounder = Module(new RoundingUnit(63))
    rounder.io.in := inRounder.head(63) //drop least bit
    rounder.io.roundIn := inRounder(0)
    rounder.io.stickyIn := sticky
    rounder.io.signIn := signSrc
    rounder.io.rm := rm

    val resultRounded = rounder.io.out
    val nxRounded = rounder.io.inexact
    val cout = rounder.io.cout //如果有进位说明原来>towidth个1
    val expRounded = Mux(cout, expValue+ 1.S, expValue)

    val normalResult = Mux(signSrc, (~resultRounded).asUInt + 1.U, resultRounded) // todo: fix

    nv := (!signSrc && expRounded >= 63.S) || (signSrc && expRounded >= 63.S && !cout) //nv has included inf & nan
    dz := false.B
    of := false.B
    uf := false.B
    nx := !nv && nxRounded

    result := Mux(nv,
      signNonNan ## Fill(63, !signNonNan),
      normalResult
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

