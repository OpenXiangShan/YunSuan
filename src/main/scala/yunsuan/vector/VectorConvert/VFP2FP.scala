//package yunsuan.vector.VectorConvert
//
// import chisel3._
// import chisel3.util._
// import yunsuan.vector.VectorConvert.utils.leftFirstOne
// import yunsuan.vector.VectorConvert.RoundingModle._
// import yunsuan.vector.VectorConvert.utils._
//
//
// // any float -> float
// // sew -> 2sew
// // todo: frac -> significand
//
// class FP2FPIO(srcFloatFormat: FloatFormat, toFloatFormat: FloatFormat) extends Bundle{
//   val src = Input(UInt(srcFloatFormat.width.W))
//   val rm = Input(UInt(3.W))
//   val result = Output(UInt(toFloatFormat.width.W))
//   val fflags = Output(UInt(5.W))
// }
//
// abstract class FP2FP(srcFloatFormat: FloatFormat, toFloatFormat: FloatFormat) extends Module{
//   val io = IO(new FP2FPIO(srcFloatFormat, toFloatFormat))
// }
//
//
// // 2sew -> sew 需要考虑NV DZ OF UF NZ（无效，除零，上溢，下溢，不精确
// class Double2Float(srcFloatFormat: FloatFormat, toFloatFormat: FloatFormat) extends FP2FP(srcFloatFormat, toFloatFormat) {
//   val srcBias = srcFloatFormat.bias
//   val srcExpWidth = srcFloatFormat.expWidth
//   val srcFracWidth = srcFloatFormat.fracWidth
//   val srcWidth = srcFloatFormat.width
//   val input = io.src
//
//   val toBias = toFloatFormat.bias
//   val toExpWidth = toFloatFormat.expWidth
//   val toFracWidth = toFloatFormat.fracWidth
//   val toWidth = toFloatFormat.width
//
//   val srcExp = input(srcWidth - 2, srcWidth - 1 - srcExpWidth)
//   val srcFrac = input.tail(srcFracWidth)
//
//   val toExp = Wire(UInt(toExpWidth.W))
//   val toFrac = Wire(UInt(toFracWidth.W))
//   val toSubNormalExp = Wire(UInt(toExpWidth.W))
//   val toSubNormalFrac = Wire(UInt(toFracWidth.W))
//
//   val toSign = input.head(1).asBool
//   val result = Wire(UInt(toWidth.W))
//   val result1H = Wire(UInt(4.W))
//   val fflags = Wire(UInt(5.W))
//
//
//   val expIsOnes = srcExp.andR
//   val expIsZeros = !srcExp.orR
//   val fracIsZero = !srcFrac.orR
//   val fracNonZero = srcFrac.orR
//
//   val isINF = expIsOnes && fracIsZero
//   val isNaN = expIsOnes && fracNonZero
//   val isSNaN = isNaN && !srcFrac.head(1).asBool
//   val isQNaN = isNaN && srcFrac.head(1).asBool
//   val isZero = expIsZeros && fracIsZero
//   val isSubNormal = expIsZeros && fracNonZero
//   val Normal = !expIsZeros && !expIsOnes
//
//   val interExp = (false.B ## srcExp).asSInt - (srcBias - toBias).S //todo: have to think of sign
//   val sign = !isNaN && toSign
//
//
//   val rounder = RoundingUnit(srcFrac, io.rm, toSign, toExpWidth)
//   val inexactRounded = rounder.io.inexact
//   val cout = rounder.io.cout
//   val fracUp = rounder.io.r_up
//   val fracRounded = rounder.io.out
//   val expRounded = Mux(cout, interExp + 1.S, interExp)
//   val overflowRounded = Mux(cout, interExp > (toFloatFormat.maxExp-1).S, interExp > toFloatFormat.maxExp.S )
//   val underflowExpRounded = Mux(cout, interExp < 0.S, interExp < 1.S)
//   val inexactOverflow = inexactRounded || overflowRounded
//
//
//   /*
//       Subnormal path
//    */
//
//   /*
//       if down_exp < 1, the input will be a subnormal in output's form,
//       so we need to right shift the `sig`.
//       shamt = 1 - down_exp
//             = 1 - (fp_in.exp - exp_delta)
//             = (1 + exp_delta) - fp_in.exp
//    */
//   //以小数点为原点，输入的实际指数为srcExp - srcBias，
//   //目的格式原点前一位的实际指数为1-toBias，差值为1-toBias - (srcExp - srcBias)
//   //或者，按照注释的理解在浮点格式中（加偏置之后），原点的前一位指数为1，src->to之后为down_exp = srcExp - srcBias + toBias
//
//   //  非零结果看作无限指数范围计算，舍入之后非零结果严格处于(-b^emin, b^emin)区间，或者
//   //  非零结果看作无限指数范围和无限精度计算，舍入之前非零结果严格处于(-b^emin, b^emin)区间。
//   //  val normal_stickyBit = fp_in.sig.tail(outPrecision).orR()
//   //  val subnormal_sitckyBit = shift_sticky | normal_stickyBi
//   val shamt = (srcBias - toBias + 1).U(srcExpWidth.W) - srcExp
//   val (subnormal_sig, shift_sticky) = ShiftRightJam(
//     Cat(!expIsZeros, srcFrac.head(toFracWidth + 1)),  //src是否是规格化的，输入的位宽为1+toFracWidth +1
//     shamt
//   )
//   val subnormal_sitckyBit = shift_sticky | srcFrac.tail(toFracWidth).orR //被移出去的那几位的stiky和及其后面剩余位的sticky
//   val subnormal_rounder = Module(new RoundingUnit(toFracWidth))
//   subnormal_rounder.io.in := subnormal_sig.tail(1).head(toFracWidth) //去掉1+toFracWidth +1的头1位和尾1位
//   subnormal_rounder.io.roundIn := subnormal_sig(0) //前面多加一位的作用
//   subnormal_rounder.io.stickyIn := subnormal_sitckyBit
//
//
//   subnormal_rounder.io.signIn := toSign
//   subnormal_rounder.io.rm := io.rm
//   val subnormal_sig_rounded = subnormal_rounder.io.out
//   val subnormal_exp_rounded = Mux(subnormal_rounder.io.cout, 1.U, 0.U)
//   val subnormal_ix = subnormal_rounder.io.inexact
//
//   val may_be_subnormal = interExp < 1.S
//
//   val rmin =
//     io.rm === RTZ || (io.rm === RDN && !toSign) || (io.rm === RUP && toSign)
//
//   val resultOverflow = Mux(rmin,
//     toFloatFormat.maxExp.U(toExpWidth.W) ## ~0.U(toFracWidth.W), // great FN
//     (toFloatFormat.maxExp+1).U(toExpWidth.W) ## 0.U(toFracWidth.W)   // INF
//   )
//
//
//   val nv = isSNaN
//   val dz = false.B
//   val of = !expIsOnes && overflowRounded
//   val uf = !expIsOnes && may_be_subnormal && underflowExpRounded && subnormal_ix //todo
//   val nx = !expIsOnes && (
//     (!may_be_subnormal && inexactOverflow) ||
//       (may_be_subnormal && subnormal_ix)
//     )
//
//   result1H := Cat(expIsOnes,
//     !may_be_subnormal && overflowRounded,
//     !may_be_subnormal && !overflowRounded,
//     may_be_subnormal
//   )
//
//   val resultMap = VecInit((0 to 3).map {
//     case 0 => sign ## ~0.U(toExpWidth.W) ## fracNonZero ## 0.U((toFracWidth - 1).W) // INF or NaN->QNAN todo
//     case 1 => sign ## resultOverflow // of
//     case 2 => sign ## expRounded(toExpWidth - 1, 0) ## fracRounded // normal
//     case 3 => sign ## subnormal_exp_rounded ## subnormal_sig_rounded //sub or uf
//   })
//
//
//   result := Mux1H(result1H.asBools.reverse, resultMap)
//   io.result := result
//   io.fflags := Cat(nv, dz, of, uf, nx)
// }
//
// class Float2Double(srcFloatFormat: FloatFormat, toFloatFormat: FloatFormat) extends FP2FP(srcFloatFormat, toFloatFormat) {
//   val srcBias = srcFloatFormat.bias
//   val srcExpWidth = srcFloatFormat.expWidth
//   val srcFracWidth = srcFloatFormat.fracWidth
//   val srcWidth = srcFloatFormat.width
//   val input = io.src
//
//   val toBias = toFloatFormat.bias
//   val toExpWidth = toFloatFormat.expWidth
//   val toFracWidth = toFloatFormat.fracWidth
//   val toWidth = toFloatFormat.width
//
//   val srcExp = input(srcWidth - 2, srcWidth - 1 - srcExpWidth)
//   val srcFrac = input.tail(srcWidth - srcFracWidth)
//
//   val toExp = Wire(UInt(toExpWidth.W))
//   val toFrac = Wire(UInt(toFracWidth.W))
//   val toSubNormalExp = Wire(UInt(toExpWidth.W))
//   val toSubNormalFrac = Wire(UInt(toFracWidth.W))
//
//   val toSign = input.head(1).asBool
//   val result = Wire(UInt(toWidth.W))
//   val result1H = Wire(UInt(4.W))
//   val fflags = Wire(UInt(5.W))
//
//   //todo: 规格化判断
//   val expIsOnes = srcExp.andR
//   val expIsZeros = !srcExp.orR
//   val fracIsZero = !srcFrac.orR
//   val fracNonZero = srcFrac.orR
//
//   val isINF = expIsOnes && fracIsZero
//   val isNaN = expIsOnes && fracNonZero
//   val isSNaN = isNaN && !srcFrac.head(1).asBool
//   val isQNaN = isNaN && srcFrac.head(1).asBool
//   val isZero = expIsZeros && fracIsZero
//   val isSubNormal = expIsZeros && fracNonZero
//   val Normal = !expIsZeros && !expIsOnes
//
//   result1H := expIsOnes ## isZero ## isSubNormal ## Normal
//   assert(PopCount(result1H) === 1.U, "input of src is not valid")
//
//   toExp := srcExp + (toBias - srcBias).U // todo: sign of exp? no, beacuse srcExp >= 0 这里一定是>0 所以不考虑
//   toFrac := input.tail(srcWidth - srcFracWidth) ## 0.U((toFracWidth - srcFracWidth).W)
//   val sign = !isNaN && toSign
//
//   val value = input.tail(srcWidth - srcFracWidth)
//   val firstOne = leftFirstOne(value)
//   //  toSubNormalExp := (1-srcBias).U - (srcFracWidth.U - firstOne) + toBias.U //计算出1的实际指数然后+toBias
//   toSubNormalExp := (-srcBias + toBias - srcExpWidth + 1).U + firstOne
//   toSubNormalFrac := value << (toFracWidth.U - firstOne)
//
//   val resultMap = VecInit((0 to 3).map {
//     case 0 => sign ## ~0.U(toExpWidth.W) ## fracNonZero ## 0.U((toFracWidth - 1).W) // INF or NaN->QNAN
//     case 1 => sign ## 0.U(toWidth - 1) // 0
//     case 2 => sign ## toSubNormalExp ## toSubNormalFrac // 规格化
//     case 3 => sign ## toExp ## toFrac
//   })
//
//   fflags := isNaN ## 0.U(4.W)
//   io.result := Mux1H(result1H.asBools.reverse, resultMap) //todo:要不要reverse
//   io.fflags := fflags
// }
//
// class VFP2FP(srcFloatFormat: FloatFormat,
//              toFloatFormat: FloatFormat
//             ) extends Module{
//   val io = IO(new FP2FPIO(srcFloatFormat, toFloatFormat))
//   val m = if(srcFloatFormat.width < toFloatFormat.width) {Module(new Double2Float(srcFloatFormat, toFloatFormat)) }else {Module(new Float2Double(srcFloatFormat, toFloatFormat))}
//   io <> m.io
// }
//
// object VFP2FP {
//   def apply(srcFloatFormat: FloatFormat,
//             toFloatFormat: FloatFormat,
//            )(input:   UInt,
//              rm: UInt
//            ): (UInt, UInt) = {
//     val m = Module(new VFP2FP(srcFloatFormat, toFloatFormat))
//     m.io.src := input
//     m.io.rm := rm
//     (m.io.result, m.io.fflags)
//   }
// }