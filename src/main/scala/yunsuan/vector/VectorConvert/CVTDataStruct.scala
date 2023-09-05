//package yunsuan.vector.VectorConvert
//
//import chisel3._
//import chisel3.util._
//import scala.math._
//import yunsuan.vector.VectorConvert.utils._
//
////
////todo：舍入规则，格式判断（是不是INF、NaN, 开始之前需要看src和to各自所有可能的范围，弄个excel把他弄出来)
////todo：目前合在一起实现的都未采取舍入规则，均已截断，后面需要添加，主要看这个过程是否需要截断，在截断时需要采取舍入规则
//// any Int/UInt -> any Float
//// rm
//def int2Float(
//    input:            UInt,
//    hasSign:          Boolean,
//    toFloatFormat:    FloatFormat
//           ): UInt = {
//  val bias = toFloatFormat.bias
//  val expWidth = toFloatFormat.expWidth
//  val fracWidth = toFloatFormat.fracWidth
//
//  val srcWidth = input.getWidth
//  val toWidth = toFloatFormat.width
//  val widthEffect = max(toWidth, srcWidth)
//  val fracInter = Wire(UInt(widthEffect.W))
//
//  val exp = Wire(UInt(expWidth.W))
//  val frac = Wire(UInt(fracWidth.W))
//
//  val sign = if(hasSign) input.head(1).asBool else false.B
//  val value = if(hasSign) Mux(sign, (~input.tail(srcWidth-1)).asUInt + 1.U, input.tail(srcWidth-1)) else input //note：注意这里有无问题，todo：前面Int7位，后面UInt8位
//  val firstOne = leftFirstOne(value)
//
//  exp := firstOne + bias.U
//  fracInter := value << (widthEffect.U - firstOne) //add 1 to drop the highest bit
//  frac := fracInter.head(fracWidth) //todo:有截断时需要考虑舍入
//  sign ## exp ## frac
//}
//
//
//// any float -> UInt
//def float2UInt(
//               input: UInt,
//               srcFloatFormat: FloatFormat = f16,
//               toHasSign: Boolean = false,
//               toWidth: Int = 16
//             ): UInt = {
//  val bias = srcFloatFormat.bias
//  val expWidth = srcFloatFormat.expWidth
//  val fracWidth = srcFloatFormat.fracWidth
//  val fWidth = input.getWidth
//  val exp = input(fWidth - 2, fWidth - 1 - expWidth)
//  val frac = true.B ## input(fWidth - 1 - expWidth - 1, 0)
//
//  // toUInt
//  val result = Wire(UInt(toWidth.W))
//  val result1H = Wire(UInt(3.W))
//
//  val isINF = exp.andR && !(input(fWidth - 1 - expWidth - 1, 0).andR)
//  val isNaN = exp.andR && input(fWidth - 1 - expWidth - 1, 0).orR
//  val isNonStand = !exp.andR
//
//  val sign = input.head(1).asBool
//  val isFraction = exp < bias.U // include NonStand
//  val isOutOfRange = (exp - bias.U) >= toWidth.U
//
//
//  //todo：规格化判断
//  result1H(0) := sign
//  result1H(2) := (!sign && isFraction) || isOutOfRange || (!sign && isINF) || isNaN
//  result1H(1) := !result(0) && !result(2)
//  //    Assert()
//  val widthValid = exp - bias.U
//  result := frac >> (frac.getWidth.U - widthValid) //todo: round
//
//  val resultMap = VecInit((-1 to 1).map {
//    case -1 => 0.U //
//    case 0 => result // rangle
//    case 1 => Fill(toWidth, true.B) // +NAN
//  })
//  Mux1H(result1H, resultMap)
//}
//
//
//def float2Int(
//              input: UInt,
//              srcFloatFormat: FloatFormat = f16,
//              toHasSign: Boolean = false,
//              toWidth: Int = 16
//            ): UInt = {
//  val bias = srcFloatFormat.bias
//  val expWidth = srcFloatFormat.expWidth
//  val fracWidth = srcFloatFormat.fracWidth
//  val fWidth = input.getWidth
//  val exp = input(fWidth - 2, fWidth - 1 - expWidth)
//  val frac = true.B ## input(fWidth - 1 - expWidth - 1, 0)
//
//
//  // toInt note: range of int8 is [-128, 127]
//  val sign = input.head(1).asBool
//  val result = Wire(UInt(toWidth.W))
//  val result1H = Wire(UInt(3.W))
//
//  val isINF = exp.andR && !(input(fWidth - 1 - expWidth - 1, 0).andR)
//  val isNaN = exp.andR && input(fWidth - 1 - expWidth - 1, 0).orR
//  val isNonStand = !exp.andR
//
//  val isFraction = exp < bias.U
//  // 指数大于位宽 或者等于位宽时
//  val isNegOutOfRange = sign && (((exp - bias.U) > (toWidth - 1).U) || ((exp - bias.U) === (toWidth - 1).U && PopCount(frac) > 0.U)) //exp is out-of-Range exclude 1000 ... 0000(eg. for int8, -128)
//  val isPosOutOfRange = !sign && (exp - bias.U) >= (toWidth - 1).U
//
//  result1H(0) := (sign && isFraction) || isNegOutOfRange || (sign && isINF)
//  result1H(2) := (!sign && isFraction) || isPosOutOfRange || (!sign && isINF) || isNaN
//  result1H(1) := !result(0) && !result(2)
//  assert(PopCount(result1H) === 1.U, "input of src is not valid")
//
//  val widthValid = exp - bias.U
//  result.head(1) := sign
//  result.tail(toWidth - 1) := (~(frac >> (frac.getWidth.U - widthValid))).asUInt + 1.U // todo：round
//
//  val resultMap = VecInit((-1 to 1).map {
//    case -1 => true.B ## Fill(toWidth - 1, false.B)
//    case 0 => result
//    case 1 => false.B ## Fill(toWidth - 1, true.B)
//  })
//  Mux1H(result1H, resultMap)
//}
//
//
//// any float -> float
//// sew -> 2sew
//def float2double(
//                 input:    UInt,
//                 srcFloatFormat   : FloatFormat,
//                 toFloatFormat   : FloatFormat
//               ): UInt = {
//  val srcBias = srcFloatFormat.bias
//  val srcExpWidth = srcFloatFormat.expWidth
//  val srcFracWidth = srcFloatFormat.fracWidth
//  val srcWidth = srcFloatFormat.width
//
//  val toBias = toFloatFormat.bias
//  val toExpWidth = toFloatFormat.expWidth
//  val toFracWidth = toFloatFormat.fracWidth
//  val toWidth = toFloatFormat.width
//
//  val srcExp = input(srcWidth - 2, srcWidth - 1 - srcExpWidth)
//  val srcFrac = true.B ## input.tail(srcFracWidth)
//
//  val toExp = Wire(UInt(toExpWidth.W))
//  val toFrac = Wire(UInt(toFracWidth.W))
//  val toSubNormalExp = Wire(UInt(toExpWidth.W))
//  val toSubNormalFrac = Wire(UInt(toFracWidth.W))
//
//  val toSign = input.head(1).asBool
//  val result = Wire(UInt(toWidth.W))
//  val result1H = Wire(UInt(4.W))
//
//  //todo: 规格化判断
//  val expIsOnes = srcExp.andR
//  val expIsZeros = !srcExp.andR
//  val fracIsZero = !srcFrac.tail(srcFracWidth).andR
//  val fracNonZero = srcFrac.tail(srcFracWidth).orR
//
//  val isINF = expIsOnes && fracIsZero
//  val isNaN = expIsOnes && fracNonZero
//  val isSNaN = isNaN && !srcFrac.tail(srcFracWidth).head(1).asBool
//  val isQNaN = isNaN && srcFrac.tail(srcFracWidth).head(1).asBool
//  val isZero = expIsZeros && fracIsZero
//  val isSubNormal = expIsZeros && fracNonZero
//  val Normal = !expIsZeros && !expIsOnes
//
//  result1H := expIsOnes ## isZero ## isSubNormal ## Normal
//  assert(PopCount(result1H) === 1.U, "input of src is not valid")
//
//  toExp := srcExp + (toBias - srcBias).U
//  toFrac := input.tail(srcFracWidth) ## 0.U((toFracWidth - srcFracWidth).W)
//  val sign = !isNaN && toSign
//
//  val value = input.tail(srcFracWidth)
//  val firstOne = leftFirstOne(value)
////  toSubNormalExp := (1-srcBias).U - (srcFracWidth.U - firstOne) + toBias.U //计算出1的实际指数然后+toBias
//  toSubNormalExp := (-srcBias+toBias-srcExpWidth + 1).U + firstOne
//  toSubNormalFrac := value << (toFracWidth.U - firstOne)
//
//  val resultMap = VecInit((0 to 3).map {
//    case 0 => sign ## ~0.U(toExpWidth.W) ## fracNonZero ## 0.U((toFracWidth-1).W) // INF or NaN->QNAN todo:这里看手册为什么
//    case 1 => sign ## 0.U(toWidth - 1) // 0
//    case 2 => sign ## toSubNormalExp ## toSubNormalFrac// 规格化
//    case 3 => sign ## toExp ## toFrac
//  })
//  Mux1H(result1H, resultMap)
//}
//
//
//// 2sew -> sew 待删除
//def doubletoFloat(
//                  input:    UInt,
//                  srcFloatFormat   : FloatFormat,
//                  toFloatFormat   : FloatFormat
//                ): UInt = {
//  val srcBias = srcFloatFormat.bias
//  val srcExpWidth = srcFloatFormat.expWidth
//  val srcFracWidth = srcFloatFormat.fracWidth
//  val srcWidth = srcFloatFormat.width
//
//  val toBias = toFloatFormat.bias
//  val toExpWidth = toFloatFormat.expWidth
//  val toFracWidth = toFloatFormat.fracWidth
//  val toWidth = toFloatFormat.width
//
//  val srcExp = input(srcWidth - 2, srcWidth - 1 - srcExpWidth)
//  val srcFrac = true.B ## input.tail(srcFracWidth)
//
//  val toExp = Wire(UInt(toExpWidth.W))
//  val toFrac = Wire(UInt(toFracWidth.W))
//
//  val toSign = input.head(1)
//  val result = Wire(UInt(toWidth.W))
//  val result1H = Wire(UInt(5.W))
//
//  //todo: 规格化判断
//  val isINF = srcExp.andR && !(input.tail(srcFracWidth).andR)
//  val isNaN = srcExp.andR && input.tail(srcFracWidth).orR
//  val isNonStand = !srcExp.andR
//  val isOutOfRange =
//  val stand = !isINF && !isNaN && !isNonStand
//
//  result1H := isINF ## isNaN ## isNonStand ## isOutOfRange ## stand
//  assert(PopCount(result1H) === 1.U, "input of src is not valid")
//
//  toExp := srcExp + toBias.U - srcBias.U
//
//  val resultMap = VecInit((0 to 3).map {
//    case 0 => toSign ## Fill(toExpWidth, true.B) ## Fill(toFracWidth, false.B)
//    case 1 => toSign ## Fill(toExpWidth, true.B) ## input.tail(srcFracWidth).head(toFracWidth)//todo：如何保证目的float的尾数不全为零
//    case 2 => toSign ## Fill(toWidth-1, false.B)// todo: 非规格化的数 2sew->sew的话 其粒度过于小, 先让其为0(具体需要看手册是如何规定的
//    case 3 => false.B ## Fill(toWidth - 1, true.B) // todo: outofRange 看下手册都有哪些情况，上溢大于最大值-> +INF，下溢小于最小值 -INF，
//    case 4 => toSign ## toExp ## input.tail(srcFracWidth).head(toFracWidth)//todo rounding modle
//  })
//
//  Mux1H(result1H, resultMap)
//}
//
//
//def double2Float(
//                  input:    UInt,
//                  srcFloatFormat   : FloatFormat,
//                  toFloatFormat   : FloatFormat
//                ): UInt = {
//  val srcBias = srcFloatFormat.bias
//  val srcExpWidth = srcFloatFormat.expWidth
//  val srcFracWidth = srcFloatFormat.fracWidth
//  val srcWidth = srcFloatFormat.width
//
//  val toBias = toFloatFormat.bias
//  val toExpWidth = toFloatFormat.expWidth
//  val toFracWidth = toFloatFormat.fracWidth
//  val toWidth = toFloatFormat.width
//
//  val srcExp = input(srcWidth - 2, srcWidth - 1 - srcExpWidth)
//  val srcFrac = true.B ## input.tail(srcFracWidth)
//
//  val toExp = Wire(UInt(toExpWidth.W))
//  val toFrac = Wire(UInt(toFracWidth.W))
//  val toSubNormalExp = Wire(UInt(toExpWidth.W))
//  val toSubNormalFrac = Wire(UInt(toFracWidth.W))
//
//  val toSign = input.head(1).asBool
//  val result = Wire(UInt(toWidth.W))
//  val result1H = Wire(UInt(4.W))
//
//  //todo: 规格化判断
//  val expIsOnes = srcExp.andR
//  val expIsZeros = !srcExp.andR
//  val fracIsZero = !srcFrac.tail(srcFracWidth).andR
//  val fracNonZero = srcFrac.tail(srcFracWidth).orR
//
//  val isINF = expIsOnes && fracIsZero
//  val isNaN = expIsOnes && fracNonZero
//  val isSNaN = isNaN && !srcFrac.tail(srcFracWidth).head(1).asBool
//  val isQNaN = isNaN && srcFrac.tail(srcFracWidth).head(1).asBool
//  val isZero = expIsZeros && fracIsZero
//  val isSubNormal = expIsZeros && fracNonZero
//  val Normal = !expIsZeros && !expIsOnes
//
//  result1H := expIsOnes ## isZero ## isSubNormal ## Normal
//  assert(PopCount(result1H) === 1.U, "input of src is not valid")
//
//  toExp := srcExp + (toBias - srcBias).U
//  toFrac := input.tail(srcFracWidth) ## 0.U((toFracWidth - srcFracWidth).W)
//  val sign = !isNaN && toSign
//
//  val value = input.tail(srcFracWidth)
//  val firstOne = leftFirstOne(value)
//  //  toSubNormalExp := (1-srcBias).U - (srcFracWidth.U - firstOne) + toBias.U //计算出1的实际指数然后+toBias
//  toSubNormalExp := (-srcBias+toBias-srcExpWidth + 1).U + firstOne
//  toSubNormalFrac := value << (toFracWidth.U - firstOne)
//
//  val resultMap = VecInit((0 to 3).map {
//    case 0 => sign ## ~0.U(toExpWidth.W) ## fracNonZero ## 0.U((toFracWidth-1).W) // INF or NaN->QNAN
//    case 1 => sign ## 0.U(toWidth - 1) // 0
//    case 2 => sign ## 0.U(toWidth - 1) // 0
//    case 3 => sign ## toExp ## toFrac
//  })
//  Mux1H(result1H, resultMap)
//}
//
//
//def fp2Fp(
//     input:    UInt,
//     isWiden: Boolean,
//     srcFloatFormat   : FloatFormat,
//     toFloatFormat   : FloatFormat
//   ): UInt = {
//      if(isWiden) float2double(input, srcFloatFormat, toFloatFormat) else double2Float(input, srcFloatFormat, toFloatFormat)
//   }
//
//
//
//
////2
//class Element8 extends Module{
//  val input = Wire(UInt(8.W))
//
//  def ui8toF16: UInt = int2Float(input, false, f16)
//  def i8toF16: UInt = int2Float(input, true, f16)
//}
//
////11
//class Element16 extends Module{
//  val input = Wire(UInt(16.W))
//
//  //  ui16toF16
//  //  i16toF16
//  //   F16toUI16
//  //   F16toI16
//
//
//
//
//  def ui16toF32: UInt = int2Float(input, false, f32)
//  def i16toF32: UInt = int2Float(input, true, f32)
//
//  //   F16toUI32
//  //   F16toI32
//  def f16toF32: UInt = float2double(input, f16, f32)
//
//  //   F16toUI8
//  //   F16toI8
//}
//
//// 14
//class Element32 extends Module{
//  val input = Wire(UInt(32.W))
//
//
//  //  def ui32toF32
//  //  def i32toF32
//  //   F32toUI32
//  //   F32toI32
//
//  def ui32toF64: UInt = int2Float(input, false, f32)
//  def i32toF64: UInt = int2Float(input, true, f32)
//
//  //   F32toUI64
//  //   F32toI64
//  def f32toF64: UInt = float2double(input, f32, f64)
//
//  //  def ui32toF16
////  def i32toF16
//
//  //   F32toUI16
//  //   F32toI16
//  //   F32toF16
//
//
//}
//// 9
//class Element64 extends Module{
//  val input = Wire(UInt(64.W))
//
////  def ui64toF64
////  def i64toF64
//
//  //   F64toUI64
//  //   F64toI64
//
////  def ui64toF32
////  def i64toF32
//
//  //   F64toUI32
//  //   F64toI32
//  //   F64toF32
//
//}
//
//
//
//
//
//
