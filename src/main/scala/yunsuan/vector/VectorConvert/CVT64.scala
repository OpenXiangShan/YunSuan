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


  /** dest is float
   * init
   */
  val expWidthOut = Mux1H(float1HOut, Seq(f16.expWidth.U, f32.expWidth.U, f64.expWidth.U))
  val fracWidthOut = Mux1H(float1HOut, Seq(f16.fracWidth.U, f32.fracWidth.U, f64.fracWidth.U))
  val biasOut = Mux1H(float1HOut, Seq(f16.bias.U, f32.bias.U, f64.bias.U))


  /** dest is int
   * init
   */
  val maxInt = (0 to 3).map(i => !hasSignInt ## ~0.U(((1 << i) * 8 - 1).W))
  val minInt = (0 to 3).map(i => hasSignInt ## 0.U(((1 << i) * 8 - 1).W))

//  val toWidthValue = if (toFormat.hasSign) toWidth - 1 else toWidth

  /** dest common
   * init
   */


  //  val nv = Wire(Bool())
  //  val dz = Wire(Bool())
  //  val of = Wire(Bool())
  //  val uf = Wire(Bool())
  //  val nx = Wire(Bool())

  //  通过Mux1H选出哪些是要进rounding module的
  //  val rounderInput = Mux(inIsFp,
  //  Mux1H(float1HSrc, Seq(
  //
  //  )),
  //  Mux1H(int1HSrc, Seq(
  //
  //  ))
  //)


  /** critical circuit
   * 1.count leading zero Max is 64
   * 2.adder:
   *      f->f  : expSrc - biasSrc.U
   *      f->i  : expSrc - biasSrc.U
   *      i->f  : firstOne + biasOut.U
   *      sqrt7 :
   *      rec7  :
   * 3.shift left/right(UInt)
   * 4.rounding module
   *
   * step1: clz + adder -> compute really exp
   * step2: shift -> put the first one to the correct position
   * step3: rounding
   * step4: select result and fflags by mux1H
   *
   */

  /**
   * out is float
   */
  when(input1H(6) && output1H(7)){
    /** i64->f64 input1H(6) && output1H(7)
     * CLZ
     * Int -> Float: f64 -> f16/f32
     */
    val leadZeros = CLZ(absIntSrc)
    // todo: circuit need to reuse, done. Maxbias is 1023, MaxfirstOne is 64(Int), Maxexp is 2048, so the 13bits for adder is enough.
    val exp = Wire(UInt(f64.expWidth.W))
    val expRounded = Wire(UInt(f64.expWidth.W))

    exp := (63 + f64.bias).U - leadZeros
    val rounderInput = Wire(UInt(64.W)) // 无论是浮点的尾数还是整数的有效位数，都从这里给进rounder
    rounderInput := (absIntSrc << 1) << leadZeros //add 1 to drop the highest bit todo: sub of chisel

    val rounder = Module(new RoundingUnit(f64.fracWidth)) //todo: circuit is abled to reuse
    rounder.io.in := rounderInput.head(f64.fracWidth)
    rounder.io.roundIn := rounderInput.tail(f64.fracWidth).head(1)
    rounder.io.stickyIn := rounderInput.tail(f64.fracWidth + 1).orR //todo 找到准确的那几位来进行orR
    rounder.io.signIn := signSrc
    rounder.io.rm := rm
    val fracRounded = rounder.io.out
    val nxRounded = rounder.io.inexact
    val cout = rounder.io.cout
    expRounded := Mux(cout, exp + 1.U, exp)
    val overflowRounded = Mux(cout, exp > (f64.maxExp - 1).U, exp > f64.maxExp.U) // todo

    val rmin =
      rm === RTZ || (rm === RDN && !signSrc) || (rm === RUP && signSrc)

    val resultOverflow = Mux(rmin,
      f64.maxExp.U(f64.expWidth.W) ## ~0.U(f64.fracWidth.W), // great FN
      (f64.maxExp + 1).U(f64.expWidth.W) ## 0.U(f64.fracWidth.W) // INF
    )

    val nv = false.B
    val dz = false.B
    val of = overflowRounded
    val uf = false.B
    val nx = overflowRounded || nxRounded

    val result1H = Wire(UInt(3.W))
    result1H := overflowRounded ## isZeroIntSrc ## (!overflowRounded && !isZeroIntSrc)

    val resultMap = VecInit((-1 to 1).map {
      case -1 => signSrc ## resultOverflow // outofRange
      case 0 => signSrc ## 0.U((f64.width - 1).W) // 0 是不是包含在下面一种情况下,不能因为在rounder时没有排除这种情况,全零进rounder出来的1##全零是非零
      case 1 => signSrc ## expRounded ## fracRounded // normal
    })

    result := Mux1H(result1H.asBools.reverse, resultMap)
    fflags := Cat(nv, dz, of, uf, nx)

  }.elsewhen(input1H(5) && output1H(7)){
    /** f32->f64 input1H(5) && output1H(7)
     * 1.count leading zero
     * 2.compute exp: adder
     * 3.compute frac: shift
     */
    val result1H = Wire(UInt(4.W))
    result1H := expIsOnesSrc ## isZeroSrc ## isSubnormalSrc ## isnormalSrc
    val toExp = Wire(UInt(f64.expWidth.W))
    val toFrac = Wire(UInt(f64.fracWidth.W))
    val toSubNormalExp = Wire(UInt(f64.expWidth.W))
    val toSubNormalFrac = Wire(UInt(f64.fracWidth.W))

    toExp := (expSrc + (f64.bias - f32.bias).U)
    toFrac := fracSrc.head(f64.fracWidth)
    val sign = !isNaNSrc && signSrc

    val leadZeros = CLZ(fracSrc)
    toSubNormalExp := (-f32.bias + f64.bias).U - leadZeros
    toSubNormalFrac := (fracSrc << 1) << leadZeros

    val resultMap = VecInit((0 to 3).map {
      case 0 => sign ## ~0.U(f64.expWidth.W) ## fracNotZeroSrc ## 0.U((f64.fracWidth - 1).W) // INF or NaN->QNAN
      case 1 => sign ## 0.U((f64.width - 1).W) // 0
      case 2 => sign ## toSubNormalExp ## toSubNormalFrac
      case 3 => sign ## toExp ## toFrac
    })

    fflags := isSNaNSrc ## 0.U(4.W)
    result := Mux1H(result1H.asBools.reverse, resultMap)
  }.elsewhen( input1H(3) && output1H(5) ){
    /** f16->f32 input1H(3) && output1H(5)
     * 1.count leading zero
     */
    val result1H = Wire(UInt(4.W))
    result1H := expIsOnesSrc ## isZeroSrc ## isSubnormalSrc ## isnormalSrc
    val toExp = Wire(UInt(f32.expWidth.W))
    val toFrac = Wire(UInt(f32.fracWidth.W))
    val toSubNormalExp = Wire(UInt(f32.expWidth.W))
    val toSubNormalFrac = Wire(UInt(f64.fracWidth.W))

    toExp := expSrc + (f32.bias - f16.bias).U
    toFrac := fracSrc.head(f32.fracWidth)
    val sign = !isNaNSrc && signSrc

    val leadZeros = CLZ(fracSrc)
    toSubNormalExp := (-f16.bias + f32.bias).U - leadZeros
    toSubNormalFrac := ((fracSrc << 1) << leadZeros)

    val resultMap = VecInit((0 to 3).map {
      case 0 => sign ## ~0.U(f32.expWidth.W) ## fracNotZeroSrc ## 0.U((f32.fracWidth - 1).W) // INF or NaN->QNAN
      case 1 => sign ## 0.U((f32.width - 1).W) // 0
      case 2 => sign ## toSubNormalExp ## toSubNormalFrac(f64.fracWidth-1, f64.fracWidth-f32.fracWidth)
      case 3 => sign ## toExp ## toFrac
    })

    fflags := isSNaNSrc ## 0.U(4.W)
    result := Mux1H(result1H.asBools.reverse, resultMap)

  }.elsewhen(input1H(7) && output1H(5)){
    /** f64->f32 input1H(7) && output1H(5)
     *
     */
    val interExp = (false.B ## expSrc).asSInt - (f64.bias - f32.bias).S //todo: have to think of sign
    val sign = !isNaNSrc && signSrc


    val rounder = RoundingUnit(fracSrc, io.rm, signSrc, f32.fracWidth)
    val inexactRounded = rounder.io.inexact
    val cout = rounder.io.cout
    val fracUp = rounder.io.r_up
    val fracRounded = rounder.io.out
    val expRounded = Mux(cout, interExp + 1.S, interExp)
    val overflowRounded = Mux(cout, interExp > (f32.maxExp - 1).S, interExp > f32.maxExp.S)
    val underflowExpRounded = Mux(cout, interExp < 0.S, interExp < 1.S)
    val inexactOverflow = inexactRounded || overflowRounded


    /*
        Subnormal path
     */

    /*
        if down_exp < 1, the input will be a subnormal in output's form,
        so we need to right shift the `sig`.
        shamt = 1 - down_exp
              = 1 - (fp_in.exp - exp_delta)
              = (1 + exp_delta) - fp_in.exp
     */
    //以小数点为原点，输入的实际指数为srcExp - srcBias，
    //目的格式原点前一位的实际指数为1-toBias，差值为1-toBias - (srcExp - srcBias)
    //或者，按照注释的理解在浮点格式中（加偏置之后），原点的前一位指数为1，src->to之后为down_exp = srcExp - srcBias + toBias

    //  非零结果看作无限指数范围计算，舍入之后非零结果严格处于(-b^emin, b^emin)区间，或者
    //  非零结果看作无限指数范围和无限精度计算，舍入之前非零结果严格处于(-b^emin, b^emin)区间。
    //  val normal_stickyBit = fp_in.sig.tail(outPrecision).orR()
    //  val subnormal_sitckyBit = shift_sticky | normal_stickyBi
    val shamt = (f64.bias - f32.bias + 1).U(f32.expWidth.W) - expSrc
    val (subnormal_sig, shift_sticky) = ShiftRightJam(
      Cat(!expIsZeroSrc, fracSrc.head(f32.fracWidth + 1)), //src是否是规格化的，输入的位宽为1+toFracWidth +1
      shamt
    )
    val subnormal_sitckyBit = shift_sticky | fracSrc.tail(f32.fracWidth).orR //被移出去的那几位的stiky和及其后面剩余位的sticky
    val subnormal_rounder = Module(new RoundingUnit(f32.fracWidth))
    subnormal_rounder.io.in := subnormal_sig.tail(1).head(f32.fracWidth) //去掉1+toFracWidth +1的头1位和尾1位
    subnormal_rounder.io.roundIn := subnormal_sig(0) //前面多加一位的作用
    subnormal_rounder.io.stickyIn := subnormal_sitckyBit

    subnormal_rounder.io.signIn := signSrc
    subnormal_rounder.io.rm := io.rm
    val subnormal_sig_rounded = subnormal_rounder.io.out
    val subnormal_exp_rounded = Mux(subnormal_rounder.io.cout, 1.U, 0.U)
    val subnormal_ix = subnormal_rounder.io.inexact

    val may_be_subnormal = interExp < 1.S

    val rmin =
      io.rm === RTZ || (io.rm === RDN && !signSrc) || (io.rm === RUP && signSrc)

    val resultOverflow = Mux(rmin,
      f32.maxExp.U(f32.expWidth.W) ## ~0.U(f32.fracWidth.W), // great FN
      (f32.maxExp + 1).U(f32.expWidth.W) ## 0.U(f32.fracWidth.W) // INF
    )


    val nv = isSNaNSrc
    val dz = false.B
    val of = !expIsOnesSrc && overflowRounded
    val uf = !expIsOnesSrc && may_be_subnormal && underflowExpRounded && subnormal_ix //todo
    val nx = !expIsOnesSrc && (
      (!may_be_subnormal && inexactOverflow) ||
        (may_be_subnormal && subnormal_ix)
      )

    val result1H = Cat(expIsOnesSrc,
      !may_be_subnormal && overflowRounded,
      !may_be_subnormal && !overflowRounded,
      may_be_subnormal
    )

    val resultMap = VecInit((0 to 3).map {
      case 0 => sign ## ~0.U(f32.expWidth.W) ## fracNotZeroSrc ## 0.U((f32.fracWidth - 1).W) // INF or NaN->QNAN todo
      case 1 => sign ## resultOverflow // of
      case 2 => sign ## expRounded(f32.expWidth - 1, 0) ## fracRounded // normal
      case 3 => sign ## subnormal_exp_rounded ## subnormal_sig_rounded //sub or uf
    })


    result := Mux1H(result1H.asBools.reverse, resultMap)
    fflags := Cat(nv, dz, of, uf, nx)
  }.elsewhen(input1H(5) && output1H(3)) {
    /** f32->f64 input1H(5) && output1H(3)
     *
     */


    result := 0.U
    fflags := 0.U


  }.elsewhen(opType(5) & !opType(0)) {
    /** f64 rsqrt7: opType(5) & !opType(0)
     * clz
     * shift left
     * leadingZero +1.U //maxWidth is 8
     * (3*f64.bias-1).U - expNormaled // maxwidth is 14, so 16bit adder is enough
     */
    val leadingZeros = CLZ(fracSrc)
    val expNormaled = Mux(isSubnormalSrc, -leadingZeros, expSrc) // max is 9bits
    val sigNormaled = Wire(UInt(f64.fracWidth.W))
    sigNormaled := Mux(isSubnormalSrc, (fracSrc << 1) << leadingZeros, fracSrc)
    val expSqrt = Wire(UInt(f64.expWidth.W))
    expSqrt := Mux(isSubnormalSrc,
      (3 * f64.bias - 1).U + leadingZeros,
      (3 * f64.bias - 1).U - expSrc
    ) >> 1
    //  expSqrt := ((3 * f64.bias - 1).U - expNormaled) >> 1
    val rsqrt7Table = Module(new Rsqrt7Table)
    rsqrt7Table.src := expNormaled(0) ## sigNormaled.head(6)
    val fracSqrt = rsqrt7Table.out

  val result1H = Cat(
    signSrc & !isZeroSrc | isNaNSrc,
    isZeroSrc,
    !signSrc & !isZeroSrc & !expIsOnesSrc,
    !signSrc & isInfSrc,
  )

  val resultMap = VecInit((0 to 3).map {
    case 0 => false.B ## ~0.U(f64.expWidth.W) ## true.B ## 0.U((f64.fracWidth-1).W)
    case 1 => signSrc ## ~0.U(f64.expWidth.W) ## 0.U(f64.fracWidth.W)
    case 2 => signSrc ## expSqrt ## fracSqrt ## 0.U((f64.fracWidth -7).W)
    case 3 => 0.U(f64.width.W)
  })

  val nv = signSrc | isSNaNSrc
  val dz = isZeroSrc
  fflags := nv ## dz ## 0.U(3.W)
  result := Mux1H(result1H.asBools.reverse, resultMap)
}.elsewhen(opType(5) & opType(0)) {
    /** f64 rrec7: opType(5) & opType(0)
     * (2 * f64.bias - 1).U - expNormaled // maxwidth is 13
     */
    val leadingZeros = CLZ(fracSrc)
    val expNormaled = Mux(isSubnormalSrc, -leadingZeros, expSrc) // max is 9bits
    val sigNormaled = Wire(UInt(f64.fracWidth.W))
    sigNormaled := Mux(isSubnormalSrc, (fracSrc << 1) << leadingZeros, fracSrc)
    val expRec = Wire(UInt(f64.expWidth.W))
    expRec := Mux(isSubnormalSrc,
      (2 * f64.bias - 1).U + leadingZeros,
      (2 * f64.bias - 1).U - expSrc
    )
//    expRec := (2 * f64.bias - 1).U - expNormaled
    val rec7Table = Module(new Rec7Table)
    rec7Table.src := sigNormaled.head(7)
    val fracRec = rec7Table.out

    val result1H = Cat(
      isInfSrc,
      isZeroSrc || isSubnormalRec2 && (rm === RUP || rm === RNE || rm === RMM),
      isNaNSrc,
      isNormalRec0,
      isNormalRec1,
      isNormalRec2 || isSubnormalRec0 || isSubnormalRec1,
      isSubnormalRec2 && (rm === RDN || rm === RTZ)
    )

    val resultMap = VecInit((0 to 6).map {
      case 0 => signSrc ## 0.U((f64.width-1).W)
      case 1 => signSrc ## ~0.U(f64.expWidth.W) ## 0.U(f64.fracWidth.W)
      case 2 => false.B ## ~0.U(f64.expWidth.W) ## true.B ## 0.U((f64.fracWidth-1).W)
      case 3 => signSrc ## 0.U(f64.expWidth.W) ## 1.U(2.W) ## fracRec ## 0.U((f64.fracWidth-2-7).W)
      case 4 => signSrc ## 0.U(f64.expWidth.W) ## 1.U(1.W) ## fracRec ## 0.U((f64.fracWidth-1-7).W)
      case 5 => signSrc ## expRec ## fracRec ## 0.U((f64.fracWidth-7).W)
      case 6 => signSrc ## f64.maxExp.U(f64.expWidth.W) ## ~0.U(f64.fracWidth.W)
    })

    val nv = isSNaNSrc
    val dz = isZeroSrc
    val of = isSubnormalRec2
    val nx = isSubnormalRec2

    fflags := nv ## dz ## of ## false.B ## nx
    result := Mux1H(result1H.asBools.reverse, resultMap)
  }.elsewhen(input1H(7) && output1H(6)) {
    /**
     * out is int
     */
    /** f64->i64 input1H(7) && output1H(6)
     *
     */
      
    // expValue >= 0.S && expValue < toWidth.S
    val expValue = (false.B ## expSrc).asSInt - f64.bias.S //这里已经排除了0/负数的情况 todo：小数的情况
    //实际需要的位宽就是 expValue + 1.U ,浮点有效数字是fracWidth +1
    //    val widthFloatFrac = f64.fracWidth + 1
    //    val widthEffect = expValue + 1.S
    //  shift left: widthFloatFrac <= widthEffect
    val resultShiftLeft = fracValueSrc << (false.B ## (expValue - f64.fracWidth.S)).asUInt //todo：确定它的位宽，这样的位宽是不确定的,不需要吧，在结果中会截断前面的

    //shift right & rounded: widthFloatFrac > widthEffect
    val (inRounder, sticky) = ShiftRightJam(fracValueSrc, ((f64.fracWidth - 1).S - expValue).asUInt)
    val rounder = Module(new RoundingUnit(f64.fracWidth - 1))
    rounder.io.in := inRounder.head(f64.fracWidth + 1) //drop least bit
    rounder.io.roundIn := inRounder(0)
    rounder.io.stickyIn := sticky
    rounder.io.signIn := signSrc
    rounder.io.rm := rm
    //让有效位全部进rounder，然后根据输出确定位宽， 后续打拍的话也在进rounder和出rounder这里打
    val resultRounded = rounder.io.out
    val nxRounded = rounder.io.inexact
    val cout = rounder.io.cout //如果有进位说明原来>towidth个1
    val expRounded = Mux(cout, expValue, expValue + 1.S)

    val normal = Wire(UInt((f64.fracWidth - 1).W))
    val normalResult = Wire(UInt((f64.fracWidth - 1).W))
    normal := Mux(f64.fracWidth.S <= expValue, resultShiftLeft, resultRounded)
    normalResult := Mux(signSrc, (~normal).asUInt + 1.U, normal)

    val nv = expIsOnesSrc || expValue >= (f64.fracWidth - 1).S || (!signSrc && cout && expValue < (f64.fracWidth - 1).S)
    val dz = false.B
    val of = false.B
    val uf = false.B
    val nx = !nv && f64.fracWidth.S <= expValue && nxRounded

    val result1H = Wire(UInt(3.W))
    result1H(0) := isZeroSrc
    result1H(1) := expValue < (f64.fracWidth - 1).S && !cout
    result1H(2) := isNaNSrc || isInfSrc || expValue >= (f64.fracWidth - 1).S || (cout && expValue < (f64.fracWidth - 1).S)

    val resultMap = VecInit((-1 to 1).map {
      case -1 => 0.U((f64.fracWidth -1).W)
      case 0 => signSrc ## normalResult
      case 1 => signSrc ## Fill(f64.fracWidth -1, !signSrc)
    })
    result := Mux1H(result1H.asBools.reverse, resultMap)
    fflags := Cat(nv, dz, of, uf, nx)

  }.otherwise{
    fflags := 0.U
    result := 0.U
  }

  io.result := result
  io.fflags := fflags
}

