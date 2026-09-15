package yunsuan.vector.vfalu

import chisel3._
import chisel3.util._
import yunsuan.encoding.Opcode.Opcodes.VFMiscOpcode
import yunsuan.vector.vfmul.utils.{VFAlgoUtils, VFBundle, VFModule}
import yunsuan.util.{SignExt, ZeroExt}

class VectorFloatCmpInput extends VFBundle {
  val src0   = UInt(fpMaxWidth.W)
  val src1   = UInt(fpMaxWidth.W)
  val isfp16 = Bool()
  val isfp32 = Bool()
  val isfp64 = Bool()
  val op     = VFMiscOpcode()
}

class VectorFloatCmpOutput extends VFBundle {
  val result    = UInt(fpMaxWidth.W)
  val fflags    = Vec(fpMaxWidth / byte, UInt(flagsWidth.W))
}

class VectorFloatCmp extends VFModule {
  val io = IO(new Bundle {
    val in  = Input(new VectorFloatCmpInput)
    val out = Output(new VectorFloatCmpOutput)
  })

  private implicit val op: UInt = io.in.op
  private val isFeq    = VFMiscOpcode.isFeq
  private val isFne    = VFMiscOpcode.isFne
  private val isFlt    = VFMiscOpcode.isFlt
  private val isFle    = VFMiscOpcode.isFle
  private val isFgt    = VFMiscOpcode.isFgt
  private val isFge    = VFMiscOpcode.isFge
  private val isCompare = isFeq || isFne || isFlt || isFle || isFgt || isFge

  private def compareElement(src0: UInt, src1: UInt, fpType: String): (Bool, Bool) = {
    val src0IsNaN = getNanOfFp(src0)(fpType)
    val src1IsNaN = getNanOfFp(src1)(fpType)
    val anyNaN = src0IsNaN || src1IsNaN
    // signaling NaN: an NaN whose quite bit (highest bits in fraction) is 1
    val src0IsSNaN = src0IsNaN && !src0(quiteBit(fpType))
    val src1IsSNaN = src1IsNaN && !src1(quiteBit(fpType))

    val src0Sign = getSign(src0)(fpType)
    val src1Sign = getSign(src1)(fpType)
    val src0Abs = getAbs(src0)(fpType)
    val src1Abs = getAbs(src1)(fpType)

    val src0AbsLt = src0Abs < src1Abs
    val src0AbsEq = src0Abs === src1Abs
    val bothZero = !src0Abs.orR && src0AbsEq
    val equal = !anyNaN && ((src0Sign === src1Sign) && src0AbsEq || bothZero)
    val less = !anyNaN && Mux1H(
      Seq(
        !src0Sign && !src1Sign,
        !src0Sign && src1Sign,
        src0Sign && !src1Sign,
        src0Sign && src1Sign,
      ),
      Seq(
        src0AbsLt,
        false.B,
        !bothZero, // src0 less than src1 if they are not all zeros
        !src0AbsLt && !src0AbsEq
      )
    )
    val greater = !anyNaN && !equal && !less

    val result = Mux1H(Seq(
      isFeq -> equal,
      isFne -> !equal,
      isFlt -> less,
      isFle -> (less || equal),
      isFgt -> greater,
      isFge -> (greater || equal)
    ))
    val invalid = MuxCase(false.B, Seq(
      (isFeq || isFne) -> (src0IsSNaN || src1IsSNaN),
      (isFlt || isFle || isFgt || isFge) -> anyNaN
    ))
    (result, invalid)
  }

  private val src0Fp16 = VFAlgoUtils.split2Vec(io.in.src0, elemNum("fp16"))
  private val src1Fp16 = VFAlgoUtils.split2Vec(io.in.src1, elemNum("fp16"))
  private val src0Fp32 = VFAlgoUtils.split2Vec(io.in.src0, elemNum("fp32"))
  private val src1Fp32 = VFAlgoUtils.split2Vec(io.in.src1, elemNum("fp32"))

  private val fp16Compare = src0Fp16.zip(src1Fp16).map { case (src0, src1) => compareElement(src0, src1, "fp16") }
  private val fp32Compare = src0Fp32.zip(src1Fp32).map { case (src0, src1) => compareElement(src0, src1, "fp32") }
  private val fp64Compare = compareElement(io.in.src0, io.in.src1, "fp64")
  private val fp16CmpBits = VecInit(fp16Compare.map(_._1))
  private val fp32CmpBits = VecInit(fp32Compare.map(_._1))
  private val fp64CmpBits = fp64Compare._1
  private val fp16CmpInvs = VecInit(fp16Compare.map(_._2))
  private val fp32CmpInvs = VecInit(fp32Compare.map(_._2))
  private val fp64CmpInvs = fp64Compare._2

  private val fp16CmpResult = Cat(fp16CmpBits.map(ZeroExt(_, 16)).reverse)
  private val fp32CmpResult = Cat(fp32CmpBits.map(ZeroExt(_, 32)).reverse)
  private val fp64CmpResult = Cat(0.U(63.W), fp64CmpBits)
  io.out.result := Mux1H(Seq(
    io.in.isfp16 -> fp16CmpResult,
    io.in.isfp32 -> fp32CmpResult,
    io.in.isfp64 -> fp64CmpResult
  ))

  private val fp16Flags = fp16CmpInvs.map { Cat(_, 0.U(4.W)) }
  private val fp32Flags = fp32CmpInvs.map { Cat(_, 0.U(4.W)) }
  private val fp64Flags = Cat(fp64CmpInvs, 0.U(4.W))
  io.out.fflags := VFAlgoUtils.expandFflags(
    fp16Flags,
    fp32Flags,
    fp64Flags,
    io.in.isfp16,
    io.in.isfp32,
    io.in.isfp64
  )
}
