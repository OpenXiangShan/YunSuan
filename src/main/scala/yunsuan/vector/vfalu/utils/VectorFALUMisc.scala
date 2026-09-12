package yunsuan.vector.vfalu.utils

import chisel3._
import chisel3.util._
import yunsuan.encoding.Opcode.Opcodes.FMacOpcode
import yunsuan.vector.vfmul.utils.{VFAlgoUtils, VFBundle, VFModule}
import yunsuan.util.{SignExt, ZeroExt}

class VectorFALUMiscInput extends VFBundle {
  val src0     = UInt(fpMaxWidth.W)
  val src1     = UInt(fpMaxWidth.W)
  val isfp16   = Bool()
  val isfp32   = Bool()
  val isfp64   = Bool()
  val op       = FMacOpcode()
}

class VectorFALUMiscOutput extends VFBundle {
  val result = Output(UInt(fpMaxWidth.W))
  val fflags = Output(Vec(fpMaxWidth / byte, UInt(flagsWidth.W)))
}

class VectorFALUMiscIO extends Bundle {
  val in  = Input(new VectorFALUMiscInput())
  val out = Output(new VectorFALUMiscOutput())
}

class VectorFALUMisc extends VFModule {
  val io = IO(new VectorFALUMiscIO())
  
  private val src0   = io.in.src0
  private val src1   = io.in.src1
  private val isFp16 = io.in.isfp16
  private val isFp32 = io.in.isfp32
  private val isFp64 = io.in.isfp64
  private implicit val op: UInt = io.in.op

  private val isFmax   = FMacOpcode.isFmax
  private val isFmin   = FMacOpcode.isFmin
  private val isFsgnj  = FMacOpcode.isFsgnj
  private val isFsgnjn = FMacOpcode.isFsgnjn
  private val isFsgnjx = FMacOpcode.isFsgnjx

  private val isFsgn = isFsgnj || isFsgnjn || isFsgnjx

  private val src0F16Vec = VFAlgoUtils.split2Vec(src0, elemNum("fp16"))
  private val src0F32Vec = VFAlgoUtils.split2Vec(src0, elemNum("fp32"))
  private val src1F16Vec = VFAlgoUtils.split2Vec(src1, elemNum("fp16"))
  private val src1F32Vec = VFAlgoUtils.split2Vec(src1, elemNum("fp32"))

  private val src0F16SignSeq = src0F16Vec.map(getSign(_)("fp16"))
  private val src0F32SignSeq = src0F32Vec.map(getSign(_)("fp32"))
  private val src0F64Sign = getSign(src0)("fp64")
  private val src1F16SignSeq = src1F16Vec.map(getSign(_)("fp16"))
  private val src1F32SignSeq = src1F32Vec.map(getSign(_)("fp32"))
  private val src1F64Sign = getSign(src1)("fp64")

  private val src0F16IsNanSeq = src0F16Vec.map(getNanOfFp(_)("fp16"))
  private val src0F32IsNanSeq = src0F32Vec.map(getNanOfFp(_)("fp32"))
  private val src0F64IsNan = getNanOfFp(src0)("fp64")
  private val src1F16IsNanSeq = src1F16Vec.map(getNanOfFp(_)("fp16"))
  private val src1F32IsNanSeq = src1F32Vec.map(getNanOfFp(_)("fp32"))
  private val src1F64IsNan = getNanOfFp(src1)("fp64")

  /**
   * For Fp16, Seq(0, 1, 2, 3) -> (src0[15:0], src0[31:16], src0[47:32], src0[63:48])
   * For Fp32, Seq(0, 1, 2, 3) -> (src0[31:0] * 2, src0[63:32] * 2)
   * For Fp64, Seq(0, 1, 2, 3) -> (src0[63:0] * 4)
   */
  private val src0SignSeq: Seq[Bool] = Seq.tabulate(4)(i => Mux1H(Seq(
    isFp16 ->             src0F16SignSeq(i),
    isFp32 -> (if (i < 2) src0F32SignSeq(i) else true.B),
    isFp64 -> (if (i < 1) src0F64Sign       else true.B)
  )))
  private val src1SignSeq: Seq[Bool] = Seq.tabulate(4)(i => Mux1H(Seq(
    isFp16 ->             src1F16SignSeq(i),
    isFp32 -> (if (i < 2) src1F32SignSeq(i) else true.B),
    isFp64 -> (if (i < 1) src1F64Sign       else true.B)
  )))
  private val src0IsNanSeq: Seq[Bool] = Seq.tabulate(4)(i => Mux1H(Seq(
    isFp16 ->             src0F16IsNanSeq(i),
    isFp32 -> (if (i < 2) src0F32IsNanSeq(i) else true.B),
    isFp64 -> (if (i < 1) src0F64IsNan       else true.B)
  )))
  private val src1IsNanSeq: Seq[Bool] = Seq.tabulate(4)(i => Mux1H(Seq(
    isFp16 ->             src1F16IsNanSeq(i),
    isFp32 -> (if (i < 2) src1F32IsNanSeq(i) else true.B),
    isFp64 -> (if (i < 1) src1F64IsNan       else true.B)
  )))
  private val src0IsSNanSeq: Seq[Bool] = Seq.tabulate(4)(i => Mux1H(Seq(
    isFp16 ->             !src0F16Vec(i)(quiteBit("fp16")),
    isFp32 -> (if (i < 2) !src0F32Vec(i)(quiteBit("fp32")) else true.B),
    isFp64 -> (if (i < 1) !src0(quiteBit("fp64"))          else true.B)
  )) && src0IsNanSeq(i))
  private val src1IsSNanSeq: Seq[Bool] = Seq.tabulate(4)(i => Mux1H(Seq(
    isFp16 ->             !src1F16Vec(i)(quiteBit("fp16")),
    isFp32 -> (if (i < 2) !src1F32Vec(i)(quiteBit("fp32")) else true.B),
    isFp64 -> (if (i < 1) !src1(quiteBit("fp64"))          else true.B)
  )) && src1IsNanSeq(i))
  private val isSNanSeq = src0IsSNanSeq.zip(src1IsSNanSeq).map { case (src0IsSNan, src1IsSNan) => src0IsSNan || src1IsSNan }
  private val fflagsEach16bit = isSNanSeq.map(isSNan => Mux(isFsgn, 0.U(5.W), Cat(isSNan, 0.U(4.W))))

  private val src0AbsVec = Wire(Vec(4, UInt((fpMaxWidth - 1).W)))
  src0AbsVec.zipWithIndex.foreach { case (src, i) => src := Mux1H(Seq(
    isFp16 ->             ZeroExt(getAbs(src0F16Vec(i))("fp16"), fpMaxWidth - 1),
    isFp32 -> (if (i < 2) ZeroExt(getAbs(src0F32Vec(i))("fp32"), fpMaxWidth - 1) else Fill(fpMaxWidth - 1, 1.U)),
    isFp64 -> (if (i < 1) src0(fpMaxWidth - 2, 0)                                else Fill(fpMaxWidth - 1, 1.U))
  )) }
  private val src1AbsVec = Wire(Vec(4, UInt((fpMaxWidth - 1).W)))
  src1AbsVec.zipWithIndex.foreach { case (src, i) => src := Mux1H(Seq(
    isFp16 ->             ZeroExt(getAbs(src1F16Vec(i))("fp16"), fpMaxWidth - 1),
    isFp32 -> (if (i < 2) ZeroExt(getAbs(src1F32Vec(i))("fp32"), fpMaxWidth - 1) else Fill(fpMaxWidth - 1, 1.U)),
    isFp64 -> (if (i < 1) src1(fpMaxWidth - 2, 0)                                else Fill(fpMaxWidth - 1, 1.U))
  )) }
  private val src0AbsLtSeq = src0AbsVec.zip(src1AbsVec).map { case (src0, src1) => src0 < src1 }
  private val src0AbsEqSeq = src0AbsVec.zip(src1AbsVec).map { case (src0, src1) => src0 === src1 }
  
  // fmax/fmin/fmaxm/fminm
  private val selSrc0ForMinSeq = Seq.tabulate(4)(i => Mux1H(
    Seq(
    !src0SignSeq(i) && !src1SignSeq(i),
    !src0SignSeq(i) &&  src1SignSeq(i),
     src0SignSeq(i) && !src1SignSeq(i),
     src0SignSeq(i) &&  src1SignSeq(i),
    ),
    Seq(
      src0AbsLtSeq(i) || src0AbsEqSeq(i),
      false.B,
      true.B,
      !src0AbsLtSeq(i)
    )
  ))

  private val src0IsNan1H        = Cat(src0IsNanSeq.reverse)
  private val src1IsNan1H        = Cat(src1IsNanSeq.reverse)
  private val selSrc0ForMin1H    = Cat(selSrc0ForMinSeq.reverse)
  private val src0IsNan1HInv     = (~src0IsNan1H).asUInt
  private val src1IsNan1HInv     = (~src1IsNan1H).asUInt
  private val selSrc0ForMin1HInv = (~selSrc0ForMin1H).asUInt

  private val isFmaxMask = Fill(4, isFmax)
  private val isFminMask = Fill(4, isFmin)
  private val bothSrcNan1H    = src0IsNan1H    & src1IsNan1H
  private val nonSrcNan1H     = src0IsNan1HInv & src1IsNan1HInv
  private val onlySrc0IsNan1H = src0IsNan1H    & src1IsNan1HInv
  private val onlySrc1IsNan1H = src0IsNan1HInv & src1IsNan1H

  // private val resultIsSrc01H = (nonSrcNan1H & ((isFmaxMask & selSrc0ForMin1HInv) | (isFminMask & selSrc0ForMin1H   ))) | onlySrc1IsNan1H
  // private val resultIsSrc11H = (nonSrcNan1H & ((isFmaxMask & selSrc0ForMin1H   ) | (isFminMask & selSrc0ForMin1HInv))) | onlySrc0IsNan1H
  private val resultIsCanonicalNAN1H = bothSrcNan1H
  private val resultIsSrc01H = nonSrcNan1H & ((isFmaxMask & selSrc0ForMin1HInv) | (isFminMask & selSrc0ForMin1H   )) | onlySrc1IsNan1H
  private val resultIsSrc11H = nonSrcNan1H & ((isFmaxMask & selSrc0ForMin1H   ) | (isFminMask & selSrc0ForMin1HInv)) | onlySrc0IsNan1H
  private val resultMaxMinF16Seq = Seq.tabulate(4)(i => Mux1H(Seq(
    resultIsCanonicalNAN1H(i) -> canonicalNan("fp16"),
    resultIsSrc01H(i)         -> src0F16Vec(i),
    resultIsSrc11H(i)         -> src1F16Vec(i)
  )))

  private val resultMaxMinF32Seq = Seq.tabulate(2)(i => Mux1H(Seq(
    resultIsCanonicalNAN1H(i) -> canonicalNan("fp32"),
    resultIsSrc01H(i)         -> src0F32Vec(i),
    resultIsSrc11H(i)         -> src1F32Vec(i)
  )))
  private val resultMaxMinF64 = Mux1H(Seq(
    resultIsCanonicalNAN1H(0) -> canonicalNan("fp64"),
    resultIsSrc01H(0)         -> src0,
    resultIsSrc11H(0)         -> src1
  ))
  private val resultMaxMin = Mux1H(Seq(
    isFp16 -> Cat(resultMaxMinF16Seq.reverse),
    isFp32 -> Cat(resultMaxMinF32Seq.reverse),
    isFp64 -> resultMaxMinF64
  ))

  // fsgnj/fsgnjn/fsgnjx
  private val fsgnSignSeq = (src0SignSeq zip src1SignSeq).map { case (src0Sign, src1Sign) => Mux1H(Seq(
    isFsgnj  -> src1Sign,
    isFsgnjn -> !src1Sign,
    isFsgnjx -> (src0Sign ^ src1Sign)
  )) }
  private val resultFsgn = Mux1H(Seq(
    isFp16 -> Cat(src0F16Vec.zipWithIndex.map { case (src, i) => Cat(fsgnSignSeq(i), getAbs(src)("fp16")) }.reverse ),
    isFp32 -> Cat(src0F32Vec.zipWithIndex.map { case (src, i) => Cat(fsgnSignSeq(i), getAbs(src)("fp32")) }.reverse ),
    isFp64 -> Cat(fsgnSignSeq.head, getAbs(src0)("fp64"))
  ))

  io.out.result := Mux(isFsgn, resultFsgn, resultMaxMin)
  io.out.fflags := VFAlgoUtils.expandFflags(fflagsEach16bit, isFp16, isFp32, isFp64)
}