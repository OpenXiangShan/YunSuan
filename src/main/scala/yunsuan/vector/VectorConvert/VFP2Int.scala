//package yunsuan.vector.VectorConvert
//
//import chisel3._
//import chisel3.util._
//import scala.math._
//import yunsuan.vector.VectorConvert.utils._
//
//// 必须是精确的，否则置fflags
//
////需要注意的是，当源操作数为NaN、无穷或者其他转换后超出目标格式表示范围的有穷浮点数时，在不能用其它方式指示的情况下应该产生无效操作异常信号；
////当转换操作的结果值与其操作数值不同，但又可以用目标格式表示时，convertToInteger相关操作不会产生不精确异常，而convertToIntegerExact相关操作会产生不精确异常。
//
//class FPtoIntIO(srcFormat: FloatFormat, toFormat: IntFormat) extends Bundle {
//  val src = Input(UInt(srcFormat.width.W))
//  val rm = Input(UInt(3.W))
//  val result = Output(UInt(toFormat.width.W))
//  val fflags = Output(UInt(5.W))
//}
//
//abstract class FPtoINT(srcFormat: FloatFormat, toFormat: IntFormat) extends Module {
//  val io = IO(new FPtoIntIO(srcFormat, toFormat))
//}
//
//// todo: fflags 和 result 必须是精确的，否则置fflags
//class FP2UInt(srcFormat: FloatFormat, toFormat: IntFormat) extends FPtoINT(srcFormat, toFormat) {
//  val src = io.src
//  val rm = io.rm
//
//  val bias = srcFormat.bias
//  val expWidth = srcFormat.expWidth
//  val fracWidth = srcFormat.fracWidth
//
//  val hasSign = toFormat.hasSign
//  val toWidth = toFormat.width
//
//  val srcSign = src.head(1).asBool
//  val srcExp = src.tail(1).head(expWidth)
//  val srcFrac = src.tail(1 + expWidth)
//
//
//  val fflags = Wire(UInt(3.W))
//  val result = Wire(UInt(toWidth.W))
//  val result1H = Wire(UInt(3.W))
//
//  //todo: 规格化判断
//  val expIsOnes = srcExp.andR
//  val expIsZeros = !srcExp.orR
//  val fracIsZero = !srcFrac.orR
//  val fracNonZero = srcFrac.orR
//
//  val isINF = expIsOnes && fracIsZero
//  val isNaN = expIsOnes && fracNonZero
//  val isSNaN = isNaN && !srcFrac.head(1).asBool
//  val isQNaN = isNaN && srcFrac.head(1).asBool
//  val isZero = expIsZeros && fracIsZero
//  val isSubNormal = expIsZeros && fracNonZero
//  val Normal = !expIsZeros && !expIsOnes
//
//
//  // expValue >= 0.S && expValue < toWidth.S
//  val expValue = (false.B ## srcExp).asSInt - bias.S //这里已经排除了0/负数的情况 todo：小数的情况
//  val fracValue = true.B ## srcFrac
//  //实际需要的位宽就是 expValue + 1.U ,浮点有效数字是fracWidth +1
//  val widthFloatFrac = fracWidth + 1
//  val widthEffect = expValue + 1.S
//  //  shift left: widthFloatFrac <= widthEffect
//  val resultShiftLeft = fracValue << (expValue - fracWidth.S) //todo：确定它的位宽，这样的位宽是不确定的,不需要吧，在结果中会截断前面的
//
//  //shift right & rounded: widthFloatFrac > widthEffect
//  val (inRounder ,sticky) = ShiftRightJam(fracValue, ((fracWidth-1).S - expValue).asUInt)
//  val rounder = Module(new RoundingUnit(toWidth))
//  rounder.io.in := inRounder(inRounder.getWidth, 1) //drop least bit
//  rounder.io.roundIn := inRounder(0)
//  rounder.io.stickyIn := sticky
//  rounder.io.signIn := srcSign
//  rounder.io.rm := rm
//  //让有效位全部进rounder，然后根据输出确定位宽， 后续打拍的话也在进rounder和出rounder这里打
//  val resultRounded = rounder.io.out
//  val nxRounded = rounder.io.inexact
//  val cout = rounder.io.cout //如果有进位说明原来>towidth个1
//  val expRounded = Mux(cout, expValue, expValue + 1.S)
//
//  val normalResult = Mux(fracWidth.S <= expValue, resultShiftLeft,  resultRounded)
//
//
//  val nv = srcSign || expIsOnes || expValue >= toWidth.S || (cout && expValue < toWidth.S)
//  val dz = false.B
//  val of = false.B
//  val uf = false.B
//  val nx = !nv && nxRounded
//
//  result1H(0) := srcSign || isZero
//  result1H(1) := !srcSign && expValue < toWidth.S && !cout
//  result1H(2) := isNaN || !srcSign && ((expIsOnes && fracIsZero) || expValue >= toWidth.S || expValue < toWidth.S)
//
//  val resultMap = VecInit((-1 to 1).map {
//    case -1 => 0.U(toWidth.W)
//    case 0 => normalResult
//    case 1 => ~0.U(toWidth.W)
//  })
//  result := Mux1H(result1H, resultMap)
//  fflags := Cat(nv, dz, of, uf, nx)
//  io.result := result
//  io.fflags := fflags
//}
//
//class FP2Int(srcFormat: FloatFormat, toFormat: IntFormat) extends FPtoINT(srcFormat, toFormat) {
//  val src = io.src
//  val rm = io.rm
//
//  val bias = srcFormat.bias
//  val expWidth = srcFormat.expWidth
//  val fracWidth = srcFormat.fracWidth
//
//  val hasSign = toFormat.hasSign
//  val toWidth = toFormat.width
//  val toWidthValue = if (toFormat.hasSign) toWidth - 1 else toWidth
//
//  val srcSign = src.head(1).asBool
//  val srcExp = src.tail(1).head(expWidth)
//  val srcFrac = src.tail(1 + expWidth)
//
//
//  val fflags = Wire(UInt(3.W))
//  val result = Wire(UInt(toWidth.W))
//  val result1H = Wire(UInt(3.W))
//
//  //todo: 规格化判断
//  val expIsOnes = srcExp.andR
//  val expIsZeros = !srcExp.orR
//  val fracIsZero = !srcFrac.orR
//  val fracNonZero = srcFrac.orR
//
//  val isINF = expIsOnes && fracIsZero
//  val isNaN = expIsOnes && fracNonZero
//  val isSNaN = isNaN && !srcFrac.head(1).asBool
//  val isQNaN = isNaN && srcFrac.head(1).asBool
//  val isZero = expIsZeros && fracIsZero
//  val isSubNormal = expIsZeros && fracNonZero
//  val Normal = !expIsZeros && !expIsOnes
//
//
//  // expValue >= 0.S && expValue < toWidth.S
//  val expValue = (false.B ## srcExp).asSInt - bias.S //这里已经排除了0/负数的情况 todo：小数的情况
//  val fracValue = true.B ## srcFrac
//  //实际需要的位宽就是 expValue + 1.U ,浮点有效数字是fracWidth +1
//  val widthFloatFrac = fracWidth + 1
//  val widthEffect = expValue + 1.S
//  //  shift left: widthFloatFrac <= widthEffect
//  val resultShiftLeft = fracValue << (expValue - fracWidth.S) //todo：确定它的位宽，这样的位宽是不确定的,不需要吧，在结果中会截断前面的
//
//  //shift right & rounded: widthFloatFrac > widthEffect
//  val (inRounder, sticky) = ShiftRightJam(fracValue, ((fracWidth - 1).S - expValue).asUInt)
//  val rounder = Module(new RoundingUnit(toWidthValue))
//  rounder.io.in := inRounder.head(fracWidth+1) //drop least bit
//  rounder.io.roundIn := inRounder(0)
//  rounder.io.stickyIn := sticky
//  rounder.io.signIn := srcSign
//  rounder.io.rm := rm
//  //让有效位全部进rounder，然后根据输出确定位宽， 后续打拍的话也在进rounder和出rounder这里打
//  val resultRounded = rounder.io.out
//  val nxRounded = rounder.io.inexact
//  val cout = rounder.io.cout //如果有进位说明原来>towidth个1
//  val expRounded = Mux(cout, expValue, expValue + 1.S)
//
//  val normal = Wire(UInt(toWidthValue.W))
//  val normalResult = Wire(UInt(toWidthValue.W))
//  normal := Mux(fracWidth.S <= expValue, resultShiftLeft, resultRounded)
//  normalResult := Mux(srcSign, (~normal).asUInt + 1.U, normal)
//
//  val nv = expIsOnes || expValue >= toWidthValue.S || (!srcSign && cout && expValue < toWidthValue.S)
//  val dz = false.B
//  val of = false.B
//  val uf = false.B
//  val nx = !nv && fracWidth.S <= expValue && nxRounded
//
//  result1H(0) := isZero
//  result1H(1) := expValue < toWidthValue.S && !cout
//  result1H(2) := isNaN || isINF || expValue >= toWidthValue.S || (cout && expValue < toWidthValue.S)
//
//  val resultMap = VecInit((-1 to 1).map {
//    case -1 => 0.U((toWidthValue).W)
//    case 0 => srcSign ## normalResult
//    case 1 => srcSign ## Fill(toWidthValue, !srcSign)
//  })
//  result := Mux1H(result1H.asBools.reverse, resultMap)
//  fflags := Cat(nv, dz, of, uf, nx)
//  io.result := result
//  io.fflags := fflags
//}
//
// class VFP2Int(srcFormat: FloatFormat, toFormat: IntFormat) extends Module{
//   val io = IO(new FPtoIntIO(srcFormat, toFormat))
//   val m = if(toFormat.hasSign) {Module(new FP2Int(srcFormat, toFormat))} else {Module(new FP2UInt(srcFormat, toFormat))}
//   io <> m.io
// }
//
// object VFP2Int{
//   def apply(srcFormat: FloatFormat,
//             toFormat: IntFormat
//            )(input: UInt,
//              rm: UInt): Unit = {
//     val m = Module(new VFP2Int(srcFormat, toFormat))
//     m.io.src := input
//     m.io.rm := rm
//     (m.io.result, m.io.fflags)
//   }
// }
