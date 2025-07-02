package yunsuan.fpu
import chisel3._
import chisel3.util._
import yunsuan.{FcmpOpCode, VectorElementFormat}
class FloatCompare() extends Module {
  val io = IO(new Bundle() {
    val src0, src1 = Input(UInt(64.W))
    val fpFormat   = Input(VectorElementFormat()) // result format b01->fp16,b10->fp32,b11->fp64
    val opCode    = Input(UInt(FcmpOpCode.width.W))
    val result     = Output(UInt(64.W))
    val fflags     = Output(UInt(5.W))
  })
  val isF16 = io.fpFormat === VectorElementFormat.h
  val isF32 = io.fpFormat === VectorElementFormat.w
  val isF64 = io.fpFormat === VectorElementFormat.d
  val isFeq = FcmpOpCode.isFeq(io.opCode)
  val isFlt = FcmpOpCode.isFlt(io.opCode)
  val isFle = FcmpOpCode.isFle(io.opCode)
  val isQuiet = FcmpOpCode.isQuiet(io.opCode)
  val isFclass = FcmpOpCode.isFclass(io.opCode)
  val src0 = io.src0
  val src1 = io.src1
  val src0F16Sign = src0(15)
  val src0F32Sign = src0(31)
  val src0F64Sign = src0(63)
  val src0Sign = Mux1H(Seq(isF16, isF32, isF64), Seq(src0F16Sign, src0F32Sign, src0F64Sign))
  val src1F16Sign = src1(15)
  val src1F32Sign = src1(31)
  val src1F64Sign = src1(63)
  val src1Sign = Mux1H(Seq(isF16, isF32, isF64), Seq(src1F16Sign, src1F32Sign, src1F64Sign))
  val src0Abs = Cat(src0(62, 32), isF32 || src0F32Sign, src0(30, 16), isF16 || src0F16Sign, src0(14, 0))
  val src1Abs = Cat(src1(62, 32), isF32 || src1F32Sign, src1(30, 16), isF16 || src1F16Sign, src1(14, 0))
  val src0AbsLt = src0Abs < src1Abs
  val src0AbsEq = src0Abs === src1Abs
  val src0AbsIsZero = Mux1H(Seq(isF16, isF32, isF64), Seq(src0Abs(14, 0) === 0.U, src0Abs(30, 0) === 0.U, src0Abs === 0.U))
  val src0src1AllZero = src0AbsIsZero && src0AbsEq
  val resultNormalEq = (src0Sign === src1Sign) && src0AbsEq || src0src1AllZero
  val resultNormalLt = Mux1H(
    Seq(
      !src0Sign && !src1Sign,
      !src0Sign && src1Sign,
      src0Sign && !src1Sign,
      src0Sign && src1Sign,
    ),
    Seq(
      src0AbsLt,
      false.B,
      true.B && !src0src1AllZero,
      !src0AbsLt && !src0AbsEq
    )
  )
  val resultNormalLe = resultNormalLt || resultNormalEq
  val src0F16IsNan = src0(14, 10).andR && src0(9, 0).orR
  val src0F32IsNan = src0(30, 23).andR && src0(22, 0).orR
  val src0F64IsNan = src0(62, 52).andR && src0(51, 0).orR
  val src0IsFpCanonicalNAN = Mux1H(Seq(isF16, isF32), Seq(!src0.head(48).andR, !src0.head(32).andR))
  val src0IsNan = Mux1H(Seq(isF16, isF32, isF64), Seq(src0F16IsNan, src0F32IsNan, src0F64IsNan)) || src0IsFpCanonicalNAN
  val src0IsSNan = src0IsNan && Mux1H(Seq(isF16, isF32, isF64), Seq(!src0(9), !src0(22), !src0(51))) && !src0IsFpCanonicalNAN
  val src1F16IsNan = src1(14, 10).andR && src1(9, 0).orR
  val src1F32IsNan = src1(30, 23).andR && src1(22, 0).orR
  val src1F64IsNan = src1(62, 52).andR && src1(51, 0).orR
  val src1IsFpCanonicalNAN = Mux1H(Seq(isF16, isF32), Seq(!src1.head(48).andR, !src1.head(32).andR))
  val src1IsNan = Mux1H(Seq(isF16, isF32, isF64), Seq(src1F16IsNan, src1F32IsNan, src1F64IsNan)) || src1IsFpCanonicalNAN
  val src1IsSNan = src1IsNan && Mux1H(Seq(isF16, isF32, isF64), Seq(!src1(9), !src1(22), !src1(51))) && !src1IsFpCanonicalNAN
  val fflagsNV = Mux(isQuiet || isFeq, src0IsSNan || src1IsSNan, src0IsNan || src1IsNan)
  val fflagsFcmp = Cat(fflagsNV, 0.U(4.W))
  val resultNormal = Mux1H(Seq(isFeq, isFlt, isFle), Seq(resultNormalEq, resultNormalLt, resultNormalLe))
  val resultFcmp = Cat(0.U(63.W), Mux(src0IsNan || src1IsNan, 0.U, resultNormal))

  // for fclass
  val src0IsQNan = src0IsNan && Mux1H(Seq(isF16, isF32, isF64), Seq(src0(9), src0(22), src0(51)))
  val src0IsInf = Mux1H(
    Seq(isF16, isF32, isF64),
    Seq(
      src0(14, 10).andR && !src0(9, 0).orR,
      src0(30, 23).andR && !src0(22, 0).orR,
      src0(62, 52).andR && !src0(51, 0).orR
    )
  )
  val src0IsNormalNumber = Mux1H(
    Seq(isF16, isF32, isF64),
    Seq(
      !src0(14, 10).andR && src0(14, 10).orR,
      !src0(30, 23).andR && src0(30, 23).orR,
      !src0(62, 52).andR && src0(62, 52).orR
    )
  )
  val src0IsSubnormalNumber = Mux1H(
    Seq(isF16, isF32, isF64),
    Seq(
      !src0(14, 10).orR && src0(9, 0).orR,
      !src0(30, 23).orR && src0(22, 0).orR,
      !src0(62, 52).orR && src0(51, 0).orR
    )
  )
  val resultQNan = (1 << 9).U
  val resultFclass = Mux(src0IsFpCanonicalNAN, resultQNan, Reverse(Cat(
    src0Sign  && src0IsInf,
    src0Sign  && src0IsNormalNumber,
    src0Sign  && src0IsSubnormalNumber,
    src0Sign  && src0AbsIsZero,
    !src0Sign && src0AbsIsZero,
    !src0Sign && src0IsSubnormalNumber,
    !src0Sign && src0IsNormalNumber,
    !src0Sign && src0IsInf,
    src0IsSNan,
    src0IsQNan
  )))
  io.result := Mux(isFclass, resultFclass, resultFcmp)
  io.fflags := Mux(isFclass, 0.U, fflagsFcmp)
}