package yunsuan.encoding.Opcode

import chisel3._
import chisel3.util._
import sourcecode.{Name => SourceName}
import yunsuan.encoding.Opcode.OpcodeTraits._
import yunsuan.util.ChiselExt._

import scala.beans.BeanProperty
import scala.collection.mutable

abstract class Opcode(val factory: Opcodes) extends Cloneable {
  val encode: BitPat
  @BeanProperty
  var name: String = _

  var traits: Set[OpcodeTrait] = Set[OpcodeTrait]()

  protected var latency: Option[Int] = None

  def setTraits(OpcodeTraits: OpcodeTrait*): this.type = {
    this.traits = OpcodeTraits.toSet
    this
  }

  def addTraits(OpcodeTraits: OpcodeTrait*): this.type = {
    this.traits |= OpcodeTraits.toSet
    this
  }

  def removeTraits(OpcodeTraits: OpcodeTrait*): this.type = {
    this.traits &~= OpcodeTraits.toSet
    this
  }

  def +(OpcodeTrait: OpcodeTrait): this.type = {
    this.traits += OpcodeTrait
    this
  }

  def -(OpcodeTrait: OpcodeTrait): this.type = {
    this.traits -= OpcodeTrait
    this
  }

  def getTraits: Set[OpcodeTrait] = this.traits

  def setLatency(lat: Int): this.type = {
    this.latency = Some(lat)
    this
  }

  def S1v: this.type = {
    checkSrc1En()
    this.traits += Src1Vp
    this
  }

  def S2x: this.type = {
    checkSrc1En()
    this.traits += Src1Gp
    this
  }

  def S1f: this.type = {
    checkSrc1En()
    this.traits += Src1Fp
    this
  }

  def S1i(immType: UInt): this.type = {
    checkSrc1En()
    this.traits += Src1Imm(immType)
    this
  }

  def S2xRemove: this.type = {
    val copied = this.copy()
    copied.traits -= Src2Gp
    copied.name = this.name + "_S2xRemove"
    copied
  }

  def S1xS2xRemove: this.type = {
    val copied = this.copy()
    copied.traits -= Src1Gp
    copied.traits -= Src2Gp
    copied.name = this.name + "_S1xS2xRemove"
    copied
  }

  private def checkSrc1En(): Unit = {
    require(this.traits.contains(Src1En), s"${this} uop should add Src1En")
  }

  def rev: this.type = {
    this.traits += Src12Rev
    this
  }

  def getLat: Int = {
    if (this.latency.isEmpty) {
      this.setLatency(this.factory.getLat(this))
    }

    this.latency.get
  }

  override def toString: String = {
    val bitString = s"${encode.rawString}".padTo(encode.getWidth, '0')
    f"${getName()}%20s (BitPat<${encode.getWidth}>(b$bitString)) @ ${factory} with traits(${traits.mkString(",")})"
  }

  override def clone(): AnyRef = {
    val cloned = super.clone().asInstanceOf[Opcode]
    cloned.traits = this.traits
    cloned.name = this.name
    cloned
  }

  def copy(): this.type = {
    this.clone().asInstanceOf[this.type]
  }
}

object Opcode {
  def apply() = UInt(Opcodes.getWidth.W)
}

object Latency {
  def apply(): UInt = UInt(log2Up(width + 2).W)

  // use all 1s to specify uncertain latency like div
  def uncertain(): UInt = Fill(width, 1.U)

  lazy val width: Int = log2Up(Opcodes.getMaxFixLat + 2)
}

abstract class Opcodes {
  class Type(val encode: BitPat) extends Opcode(this) {
    override def clone(): AnyRef = {
      val cloned = super.clone().asInstanceOf[Opcode]
      cloned
    }
  }

  private var width: Option[Int] = None

  private val records: mutable.Set[Type] = collection.mutable.Set.empty

  def all: Seq[Type] = records.toSeq

  def allBitPats: Seq[BitPat] = records.map(_.encode).toSeq

  def getWidth: Int = width.get

  private def updateWidth(w: Int)(name: String): Unit = {
    if (this.width.isEmpty)
      this.width = Option(w)
    else
      require(this.width.get == w, s"The width of opcode should be ${this.width.get}, but get ${w} in $name.")
  }

  def apply(): UInt = UInt(Opcodes.this.getWidth.W)

  lazy val maxLat = getMaxLat

  def getMaxLat: Int = this.all.map(_.getLat).max

  // Todo: override it in every child class
  def getLat(opcode: Opcode): Int = {
    println(s"[Warning] please rewrite getLat method in class $getClass")
    0
  }

  /**
   * Create opcode (in the class Type) with given bit patterns and automatically fill the name.
   * @param bp1 the first bit pattern of the opcode, which is required
   * @param bp2 the rest bit patterns of the opcode, which is optional
   * @param name the name of the opcode
   * @return
   */
  def Value(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    ValueImpl((bp1 +: bp2).reduce(_ ## _))
  }

  private def ValueImpl(bitpat: BitPat)(implicit name: SourceName): Type = {
    val res = new Type(bitpat)
    res.setName(name.value)
    records.addOne(res)
    Opcodes.this.updateWidth(bitpat.getWidth)(name.value)
    Opcodes.updateWidth(bitpat.getWidth)

    res
  }

  def DvSvlS1(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name) + VpWen + VlRen + Src1En
  }

  def DvSvlS2vS1(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name) + VpWen + VlRen + Src2Vp + Src1En
  }

  def DvSvlS2vS1v(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name) + VpWen + VlRen + Src2Vp + Src1Vp
  }

  def DvSvlS2vS1S3v(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name) + VpWen + VlRen + Src2Vp + Src1En + Src3Vp
  }

  def DmSvlS2vS1(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name) + VmWen + VlRen + Src2Vp + Src1En
  }

  def DvSvlS2v(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name) + VpWen + VlRen + Src2Vp
  }

  def DvS2v(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name) + VpWen + Src2Vp
  }

  def DaS2s(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name) + Src2Vs
  }

  def DsS1a(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name) + Src1Gp + VsWen
  }

  def DsSvlS2vS1s(subOp: BitPat, opb: BitPat, res: BitPat, dtype: BitPat)(implicit name: SourceName): Type = {
    Value(dtype, opb, res, subOp)(name) + VsWen + VlRen + Src2Vp + Src1En + Src1Vs
  }

  def DwsSvlS2vS1ws(subOp: BitPat, opb: BitPat, res: BitPat, dtype: BitPat)(implicit name: SourceName): Type = {
    Value(dtype, opb, res, subOp)(name) + VwsWen + VlRen + Src2Vp + Src1En + Src1Vws
  }
  /**
   * opcode5            func3/5 -> (14, 12)/(31, 27)
   * 00000 (LOAD)                 => IntLoadInstPattern (I-Type)
   * 00001 (LOAD_FP)      "000" | "101" | "110" | "111" => [Vector]
   *                      "001" | "010" | "011"         => FpITypeLoadInstPattern (fp-i)
   * 00010 (CUSTOM_0)
   * 00011 (MSIC_MEM)     "000"   => FenceInstPattern (I-Type)
   *                      "001"   => FenceiInstPattern (I-Type)
   *                      "010"   => CboInstPattern (I-Type)
   * 00100 (OP_IMM)               => IntImmInstPattern (I-Type)
   * 00101 (AUIPC)                => IntUTypePattern
   * 00110 (OP_IMM_32)            => IntImmInstPattern (I-Type)
   *
   * 01000 (STORE)                => IntStoreInstPattern (S-Type)
   * 01001 (STORE_FP)     "000" | "101" | "110" | "111" => [Vector]
   *                      "001" | "010" | "011"         => FpSTypeInstPattern (fp-i)
   * 01010 (CUSTOM_1)
   * 01011 (AMO)         "00010"  => AmoLrInstPattern (I-Type)
   *                     others   => AmoInstPattern (R-Type)
   * 01100 (OP)                   => IntRTypePattern
   * 01101 (LUI)                  => IntUTypePattern
   * 01110 (OP_32)                => IntRTypePattern
   * 01111 (INST64b)
   *
   * 10000 (MADD)                 => FpR4TypeInstPattern
   * 10001 (MSUB)                 => FpR4TypeInstPattern
   * 10010 (NMSUB)                => FpR4TypeInstPattern
   * 10011 (NMADD)                => FpR4TypeInstPattern
   * 10100 (OP_FP)  (31:30): "00" => FpRTypeFpDestInstPattern
   *                         "01" => FpITypeF2fInstPattern
   *                         "10" => FpRTypeIntDestInstPattern
   *                         "11" => {(29,28):}
   *                           "00" => FpITypeF2iInstPattern
   *                           "01" => FpITypeI2fInstPattern
   *                           "10" => FpITypeF2iInstPattern
   *                           "11" => {rs2:}
   *                             "00001" => FpITypeImmInstPattern
   *                             others => FpITypeI2fInstPattern
   * 10101 (OP_V)                 => [Vector]
   * 10110 (CUSTOM_2)
   * 10111 (INST48b_1)
   *
   * 11000 (BRANCH)               => IntBTypePattern
   * 11001 (JALR)                 => JalrPattern (I-Type)
   * 11010 (RESERVED)  {func3==0,imm12==0}  => CustomTrapPattern (I-Type)
   * 11011 (JAL)                  => IntJTypePattern
   * 11100 (SYSTEM)       "000"   => SystemInstPattern (I-Type)
   *                      "100"   => {func7(0):}
   *                          '0'   => HyperLoadInstPattern (I-Type)
   *                          '1'   => HyperStoreInstPattern (S-Type)
   *                      others  => CSRInstPattern (I-Type)
   * 11101 (OP_VE)                => [Vector]
   * 11110 (CUSTOM_3)
   * 11111 (INSTge80b)
   */

  /**
   * Following Seq[OpcodeTrait] are used to generate certain type of uop corresponding to simple instruction.
   * e.g. RTypeTrait for R-type instruction, ITypeTrait for I-type instruction, etc.
   */
  def IntRType(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name) + GpWen         + Src1Gp + Src2Gp
  }

  def IntIType(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name) + GpWen         + Src1Gp
  }

  def IntBSType(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name)                 + Src1Gp + Src2Gp
  }

  def IntUJType(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name) + GpWen
  }

  def FpITypeF2fInst(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name)         + FpWen + Src1Fp
  }

  def FpITypeF2iInst(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name) + GpWen +         Src1Fp
  }

  def FpITypeI2fOtherInst(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name)         + FpWen + Src1Gp
  }

  def FpITypeImmInst(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name)         + FpWen
  }

  def FpITypeLoadInst(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name)         + FpWen + Src1Gp
  }


  def FpRTypeIntDestInst(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name) + GpWen         + Src1Fp + Src2Fp
  }

  def FpRTypeFpDestInst(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name)         + FpWen + Src1Fp + Src2Fp
  }


  def FpR4TypeInst(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name)         + FpWen + Src1Fp + Src2Fp + Src3Fp
  }

  def FpSTypeInst(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    Value(bp1, bp2: _*)(name)                 + Src1Gp + Src2Fp
  }

  override def toString: String = {
    getClass.getSimpleName
  }
}

object Opcodes {
  def main(args: Array[String]): Unit = {
    val opcodes = Seq(
      VIAluOpcode,
      VMAluOpcode,
      VIMacOpcode,
      VIDivOpcode,
      VIRedOpcode,
      VIPermOpcode,
      FCvtOpcode,
      FMiscOpcode,
      FMacOpcode,
      VFMacOpcode,
      VFCvtOpcode,
      VFMiscOpcode,
    )

    for (opcodeCls <- opcodes) {
      for (opcode <- opcodeCls.all.sortBy(_.encode.value)) {
        println(s"${opcode}")
      }
    }
  }

  def apply(): UInt = UInt(width.W)

  private var width: Int = 0

  def getWidth: Int = width

  def updateWidth(w: Int): Unit = Opcodes.width = w.max(Opcodes.width)

  private var maxFixLat: Int = 0

  def getMaxFixLat: Int = maxFixLat

  def updateMaxFixLat(lat: Int): Unit = this.maxFixLat = lat.max(this.maxFixLat)

  trait FMacOpcode extends Opcodes with DataType {
    private val OP2 = bb"0"
    private val OP3 = bb"1"

    private val DV = bb"0"
    private val DW = bb"1"
    private val S2V = bb"0"
    private val S2W = bb"1"

    // bit(2): 0 -> vs1 * vd, 1 -> vs1 * vs2
    // bit(1): 0 -> add     , 1 -> sub
    // bit(0): 0 -> pos     , 1 -> neg
    private val FMADD  = bb"0000" // +((vs1[i] * vd[i]) + vs2[i])
    private val FNMADD = bb"0001" // -((vs1[i] * vd[i]) + vs2[i])
    private val FMSUB  = bb"0010" // +((vs1[i] * vd[i]) - vs2[i])
    private val FNMSUB = bb"0011" // -((vs1[i] * vd[i]) - vs2[i])
    private val FMACC  = bb"0100" // +((vs1[i] * vs2[i]) + vd[i])
    private val FNMACC = bb"0101" // -((vs1[i] * vs2[i]) + vd[i])
    private val FMSAC  = bb"0110" // +((vs1[i] * vs2[i]) - vd[i])
    private val FNMSAC = bb"0111" // -((vs1[i] * vs2[i]) - vd[i])
    private val FADD   = bb"0000"
    private val FSUB   = bb"0001"
    private val FMIN   = bb"0010"
    private val FMAX   = bb"0011"
    private val FMUL   = bb"0100"
    private val FSGNJ  = bb"0101"
    private val FSGNJN = bb"0110"
    private val FSGNJX = bb"0111"
    private val FMINM  = bb"1000"
    private val FMAXM  = bb"1001"
    // TODO
    private val FWADD4 = bb"1010"

    val fmadd_fp16   : Opcode = FpR4TypeInst(FMADD , OP3, S2V, DV, FP16, F)
    val fmadd_fp32   : Opcode = FpR4TypeInst(FMADD , OP3, S2V, DV, FP32, F)
    val fmadd_fp64   : Opcode = FpR4TypeInst(FMADD , OP3, S2V, DV, FP64, F)
    val fmsub_fp16   : Opcode = FpR4TypeInst(FMSUB , OP3, S2V, DV, FP16, F)
    val fmsub_fp32   : Opcode = FpR4TypeInst(FMSUB , OP3, S2V, DV, FP32, F)
    val fmsub_fp64   : Opcode = FpR4TypeInst(FMSUB , OP3, S2V, DV, FP64, F)
    val fnmsub_fp16  : Opcode = FpR4TypeInst(FNMSUB, OP3, S2V, DV, FP16, F)
    val fnmsub_fp32  : Opcode = FpR4TypeInst(FNMSUB, OP3, S2V, DV, FP32, F)
    val fnmsub_fp64  : Opcode = FpR4TypeInst(FNMSUB, OP3, S2V, DV, FP64, F)
    val fnmadd_fp16  : Opcode = FpR4TypeInst(FNMADD, OP3, S2V, DV, FP16, F)
    val fnmadd_fp32  : Opcode = FpR4TypeInst(FNMADD, OP3, S2V, DV, FP32, F)
    val fnmadd_fp64  : Opcode = FpR4TypeInst(FNMADD, OP3, S2V, DV, FP64, F)
    val fadd_fp16    : Opcode = FpRTypeFpDestInst(FADD  , OP2, S2V, DV, FP16, F)
    val fadd_fp32    : Opcode = FpRTypeFpDestInst(FADD  , OP2, S2V, DV, FP32, F)
    val fadd_fp64    : Opcode = FpRTypeFpDestInst(FADD  , OP2, S2V, DV, FP64, F)
    val fsub_fp16    : Opcode = FpRTypeFpDestInst(FSUB  , OP2, S2V, DV, FP16, F)
    val fsub_fp32    : Opcode = FpRTypeFpDestInst(FSUB  , OP2, S2V, DV, FP32, F)
    val fsub_fp64    : Opcode = FpRTypeFpDestInst(FSUB  , OP2, S2V, DV, FP64, F)
    val fmin_fp16    : Opcode = FpRTypeFpDestInst(FMIN  , OP2, S2V, DV, FP16, F)
    val fmax_fp16    : Opcode = FpRTypeFpDestInst(FMAX  , OP2, S2V, DV, FP16, F)
    val fminm_fp16   : Opcode = FpRTypeFpDestInst(FMINM , OP2, S2V, DV, FP16, F)
    val fmaxm_fp16   : Opcode = FpRTypeFpDestInst(FMAXM , OP2, S2V, DV, FP16, F)
    val fsgnj_fp16   : Opcode = FpRTypeFpDestInst(FSGNJ , OP2, S2V, DV, FP16, F)
    val fsgnjn_fp16  : Opcode = FpRTypeFpDestInst(FSGNJN, OP2, S2V, DV, FP16, F)
    val fsgnjx_fp16  : Opcode = FpRTypeFpDestInst(FSGNJX, OP2, S2V, DV, FP16, F)
    val fmin_fp32    : Opcode = FpRTypeFpDestInst(FMIN  , OP2, S2V, DV, FP32, F)
    val fmax_fp32    : Opcode = FpRTypeFpDestInst(FMAX  , OP2, S2V, DV, FP32, F)
    val fminm_fp32   : Opcode = FpRTypeFpDestInst(FMINM , OP2, S2V, DV, FP32, F)
    val fmaxm_fp32   : Opcode = FpRTypeFpDestInst(FMAXM , OP2, S2V, DV, FP32, F)
    val fsgnj_fp32   : Opcode = FpRTypeFpDestInst(FSGNJ , OP2, S2V, DV, FP32, F)
    val fsgnjn_fp32  : Opcode = FpRTypeFpDestInst(FSGNJN, OP2, S2V, DV, FP32, F)
    val fsgnjx_fp32  : Opcode = FpRTypeFpDestInst(FSGNJX, OP2, S2V, DV, FP32, F)
    val fmin_fp64    : Opcode = FpRTypeFpDestInst(FMIN  , OP2, S2V, DV, FP64, F)
    val fmax_fp64    : Opcode = FpRTypeFpDestInst(FMAX  , OP2, S2V, DV, FP64, F)
    val fminm_fp64   : Opcode = FpRTypeFpDestInst(FMINM , OP2, S2V, DV, FP64, F)
    val fmaxm_fp64   : Opcode = FpRTypeFpDestInst(FMAXM , OP2, S2V, DV, FP64, F)
    val fsgnj_fp64   : Opcode = FpRTypeFpDestInst(FSGNJ , OP2, S2V, DV, FP64, F)
    val fsgnjn_fp64  : Opcode = FpRTypeFpDestInst(FSGNJN, OP2, S2V, DV, FP64, F)
    val fsgnjx_fp64  : Opcode = FpRTypeFpDestInst(FSGNJX, OP2, S2V, DV, FP64, F)
    val fmul_fp16    : Opcode = FpRTypeFpDestInst(FMUL  , OP2, S2V, DV, FP16, F)
    val fmul_fp32    : Opcode = FpRTypeFpDestInst(FMUL  , OP2, S2V, DV, FP32, F)
    val fmul_fp64    : Opcode = FpRTypeFpDestInst(FMUL  , OP2, S2V, DV, FP64, F)
    val vfmul_fp16   : Opcode = DvSvlS2vS1(FMUL, OP2, S2V, DV, FP16, V)
    val vfmul_fp32   : Opcode = DvSvlS2vS1(FMUL, OP2, S2V, DV, FP32, V)
    val vfmul_fp64   : Opcode = DvSvlS2vS1(FMUL, OP2, S2V, DV, FP64, V)
    val vfwmul_fp16  : Opcode = DvSvlS2vS1(FMUL, OP2, S2V, DW, FP16, V)
    val vfwmul_fp32  : Opcode = DvSvlS2vS1(FMUL, OP2, S2V, DW, FP32, V)
    val vfmadd_fp16  : Opcode = DvSvlS2vS1S3v(FMADD , OP3, S2V, DV, FP16, V)
    val vfmadd_fp32  : Opcode = DvSvlS2vS1S3v(FMADD , OP3, S2V, DV, FP32, V)
    val vfmadd_fp64  : Opcode = DvSvlS2vS1S3v(FMADD , OP3, S2V, DV, FP64, V)
    val vfnmadd_fp16 : Opcode = DvSvlS2vS1S3v(FNMADD, OP3, S2V, DV, FP16, V)
    val vfnmadd_fp32 : Opcode = DvSvlS2vS1S3v(FNMADD, OP3, S2V, DV, FP32, V)
    val vfnmadd_fp64 : Opcode = DvSvlS2vS1S3v(FNMADD, OP3, S2V, DV, FP64, V)
    val vfmsub_fp16  : Opcode = DvSvlS2vS1S3v(FMSUB , OP3, S2V, DV, FP16, V)
    val vfmsub_fp32  : Opcode = DvSvlS2vS1S3v(FMSUB , OP3, S2V, DV, FP32, V)
    val vfmsub_fp64  : Opcode = DvSvlS2vS1S3v(FMSUB , OP3, S2V, DV, FP64, V)
    val vfnmsub_fp16 : Opcode = DvSvlS2vS1S3v(FNMSUB, OP3, S2V, DV, FP16, V)
    val vfnmsub_fp32 : Opcode = DvSvlS2vS1S3v(FNMSUB, OP3, S2V, DV, FP32, V)
    val vfnmsub_fp64 : Opcode = DvSvlS2vS1S3v(FNMSUB, OP3, S2V, DV, FP64, V)
    val vfmacc_fp16  : Opcode = DvSvlS2vS1S3v(FMACC , OP3, S2V, DV, FP16, V)
    val vfmacc_fp32  : Opcode = DvSvlS2vS1S3v(FMACC , OP3, S2V, DV, FP32, V)
    val vfmacc_fp64  : Opcode = DvSvlS2vS1S3v(FMACC , OP3, S2V, DV, FP64, V)
    val vfnmacc_fp16 : Opcode = DvSvlS2vS1S3v(FNMACC, OP3, S2V, DV, FP16, V)
    val vfnmacc_fp32 : Opcode = DvSvlS2vS1S3v(FNMACC, OP3, S2V, DV, FP32, V)
    val vfnmacc_fp64 : Opcode = DvSvlS2vS1S3v(FNMACC, OP3, S2V, DV, FP64, V)
    val vfmsac_fp16  : Opcode = DvSvlS2vS1S3v(FMSAC , OP3, S2V, DV, FP16, V)
    val vfmsac_fp32  : Opcode = DvSvlS2vS1S3v(FMSAC , OP3, S2V, DV, FP32, V)
    val vfmsac_fp64  : Opcode = DvSvlS2vS1S3v(FMSAC , OP3, S2V, DV, FP64, V)
    val vfnmsac_fp16 : Opcode = DvSvlS2vS1S3v(FNMSAC, OP3, S2V, DV, FP16, V)
    val vfnmsac_fp32 : Opcode = DvSvlS2vS1S3v(FNMSAC, OP3, S2V, DV, FP32, V)
    val vfnmsac_fp64 : Opcode = DvSvlS2vS1S3v(FNMSAC, OP3, S2V, DV, FP64, V)
    val vfwmacc_fp16 : Opcode = DvSvlS2vS1S3v(FMACC , OP3, S2V, DW, FP16, V)
    val vfwmacc_fp32 : Opcode = DvSvlS2vS1S3v(FMACC , OP3, S2V, DW, FP32, V)
    val vfwnmacc_fp16: Opcode = DvSvlS2vS1S3v(FNMACC, OP3, S2V, DW, FP16, V)
    val vfwnmacc_fp32: Opcode = DvSvlS2vS1S3v(FNMACC, OP3, S2V, DW, FP32, V)
    val vfwmsac_fp16 : Opcode = DvSvlS2vS1S3v(FMSAC , OP3, S2V, DW, FP16, V)
    val vfwmsac_fp32 : Opcode = DvSvlS2vS1S3v(FMSAC , OP3, S2V, DW, FP32, V)
    val vfwnmsac_fp16: Opcode = DvSvlS2vS1S3v(FNMSAC, OP3, S2V, DW, FP16, V)
    val vfwnmsac_fp32: Opcode = DvSvlS2vS1S3v(FNMSAC, OP3, S2V, DW, FP32, V)
    val vfadd_fp16   : Opcode = DvSvlS2vS1(FADD, OP2, S2V, DV, FP16, V)
    val vfadd_fp32   : Opcode = DvSvlS2vS1(FADD, OP2, S2V, DV, FP32, V)
    val vfadd_fp64   : Opcode = DvSvlS2vS1(FADD, OP2, S2V, DV, FP64, V)
    val vfsub_fp16   : Opcode = DvSvlS2vS1(FSUB, OP2, S2V, DV, FP16, V)
    val vfsub_fp32   : Opcode = DvSvlS2vS1(FSUB, OP2, S2V, DV, FP32, V)
    val vfsub_fp64   : Opcode = DvSvlS2vS1(FSUB, OP2, S2V, DV, FP64, V)
    val vfmin_fp16   : Opcode = DvSvlS2vS1(FMIN, OP2, S2V, DV, FP16, V)
    val vfmin_fp32   : Opcode = DvSvlS2vS1(FMIN, OP2, S2V, DV, FP32, V)
    val vfmin_fp64   : Opcode = DvSvlS2vS1(FMIN, OP2, S2V, DV, FP64, V)
    val vfmax_fp16   : Opcode = DvSvlS2vS1(FMAX, OP2, S2V, DV, FP16, V)
    val vfmax_fp32   : Opcode = DvSvlS2vS1(FMAX, OP2, S2V, DV, FP32, V)
    val vfmax_fp64   : Opcode = DvSvlS2vS1(FMAX, OP2, S2V, DV, FP64, V)
    val vfsgnj_fp16  : Opcode = DvSvlS2vS1(FSGNJ, OP2, S2V, DV, FP16, V)
    val vfsgnj_fp32  : Opcode = DvSvlS2vS1(FSGNJ, OP2, S2V, DV, FP32, V)
    val vfsgnj_fp64  : Opcode = DvSvlS2vS1(FSGNJ, OP2, S2V, DV, FP64, V)
    val vfsgnjn_fp16 : Opcode = DvSvlS2vS1(FSGNJN, OP2, S2V, DV, FP16, V)
    val vfsgnjn_fp32 : Opcode = DvSvlS2vS1(FSGNJN, OP2, S2V, DV, FP32, V)
    val vfsgnjn_fp64 : Opcode = DvSvlS2vS1(FSGNJN, OP2, S2V, DV, FP64, V)
    val vfsgnjx_fp16 : Opcode = DvSvlS2vS1(FSGNJX, OP2, S2V, DV, FP16, V)
    val vfsgnjx_fp32 : Opcode = DvSvlS2vS1(FSGNJX, OP2, S2V, DV, FP32, V)
    val vfsgnjx_fp64 : Opcode = DvSvlS2vS1(FSGNJX, OP2, S2V, DV, FP64, V)
    val vfwadd_fp16  : Opcode = DvSvlS2vS1(FADD, OP2, S2V, DW, FP16, V)
    val vfwadd_fp32  : Opcode = DvSvlS2vS1(FADD, OP2, S2V, DW, FP32, V)
    val vfwsub_fp16  : Opcode = DvSvlS2vS1(FSUB, OP2, S2V, DW, FP16, V)
    val vfwsub_fp32  : Opcode = DvSvlS2vS1(FSUB, OP2, S2V, DW, FP32, V)
    val vfwadd_w_fp16: Opcode = DvSvlS2vS1(FADD, OP2, S2W, DW, FP16, V)
    val vfwadd_w_fp32: Opcode = DvSvlS2vS1(FADD, OP2, S2W, DW, FP32, V)
    val vfwsub_w_fp16: Opcode = DvSvlS2vS1(FSUB, OP2, S2W, DW, FP16, V)
    val vfwsub_w_fp32: Opcode = DvSvlS2vS1(FSUB, OP2, S2W, DW, FP32, V)

    val vfwadd4_fp16 : Opcode = Value(FWADD4, OP2, S2V, DW, FP16, V)
    val vfwadd4_fp32 : Opcode = Value(FWADD4, OP2, S2V, DW, FP32, V)
    // There are no widen uops when FP64

    Opcodes.updateMaxFixLat(this.getMaxLat)

    def getOpNum(implicit op: UInt): Bool = op(5)

    def getSubOpcode(implicit op: UInt): UInt = op(9, 6)

    def getCtrlOpcode(implicit op: UInt): UInt = op(5, 0)

    def getVs2Format(implicit op: UInt): UInt = op(4)

    def getDestMode(implicit op: UInt): UInt = op(3)

    def getDataType(implicit op: UInt): UInt = op(2, 1)

    def getFuType(implicit op: UInt): UInt = op(0)

    def getFormat(implicit op: UInt): UInt = op(2, 1)

    def useADD(implicit op: UInt): Bool = { (
      getOpNum === OP3 ||
        getOpNum === OP2 && (getSubOpcode === FADD || getSubOpcode === FSUB)
      )
    }

    def useMUL(implicit op: UInt): Bool = { (
      getOpNum === OP3 ||
        getOpNum === OP2 && getSubOpcode === FMUL
      )
    }

    def isVfadd(implicit op: UInt): Bool = getVs2Format === S2V && getDestMode === DV && getSubOpcode === FADD
    def isVfsub(implicit op: UInt): Bool = getVs2Format === S2V && getDestMode === DV && getSubOpcode === FSUB
    def isVfmin(implicit op: UInt): Bool = getVs2Format === S2V && getDestMode === DV && getSubOpcode === FMIN
    def isVfmax(implicit op: UInt): Bool = getVs2Format === S2V && getDestMode === DV && getSubOpcode === FMAX
    def isVfsgnj(implicit op: UInt): Bool = getVs2Format === S2V && getDestMode === DV && getSubOpcode === FSGNJ
    def isVfsgnjn(implicit op: UInt): Bool = getVs2Format === S2V && getDestMode === DV && getSubOpcode === FSGNJN
    def isVfsgnjx(implicit op: UInt): Bool = getVs2Format === S2V && getDestMode === DV && getSubOpcode === FSGNJX
    def isVfminm(implicit op: UInt): Bool = getVs2Format === S2V && getDestMode === DV && getSubOpcode === FMINM
    def isVfmaxm(implicit op: UInt): Bool = getVs2Format === S2V && getDestMode === DV && getSubOpcode === FMAXM

    def isVfwFadd(implicit op: UInt): Bool = getSubOpcode === FADD && getDestMode === DW
    def isVfwFsub(implicit op: UInt): Bool = getSubOpcode === FSUB && getDestMode === DW
    def isVfWiden(implicit op: UInt): Bool = {
      getDestMode === DW && Mux1H(Seq(
        (getOpNum === OP2) -> getSubOpcode.isOneOf(FADD, FSUB, FMUL),
        (getOpNum === OP3) -> getSubOpcode.isOneOf(FMACC, FNMACC, FMSAC, FNMSAC),
      ))
    }
    def isOpbWiden(implicit op: UInt): Bool = isVfWiden && getVs2Format === S2W
    def isResWiden(implicit op: UInt): Bool = isVfWiden

    def isFadd(implicit op: UInt): Bool = getSubOpcode === FADD
    def isFsub(implicit op: UInt): Bool = getSubOpcode === FSUB
    def isFmin(implicit op: UInt): Bool = getSubOpcode === FMIN
    def isFmax(implicit op: UInt): Bool = getSubOpcode === FMAX
    def isFsgnj(implicit op: UInt): Bool = getSubOpcode === FSGNJ
    def isFsgnjn(implicit op: UInt): Bool = getSubOpcode === FSGNJ
    def isFsgnjx(implicit op: UInt): Bool = getSubOpcode === FSGNJX
    def isFminm(implicit op: UInt): Bool = getSubOpcode === FMINM
    def isFmaxm(implicit op: UInt): Bool = getSubOpcode === FMAXM

    def isFmul(implicit op: UInt): Bool = {
      getOpNum === OP2 && getSubOpcode === FMUL
    }

    def isOP3(implicit op: UInt): Bool = {
      getOpNum === OP3
    }

    def isFmadd(implicit op: UInt): Bool = {
      getOpNum === OP3 && getSubOpcode === FMADD
    }

    def isFnmadd(implicit op: UInt): Bool = {
      getOpNum === OP3 && getSubOpcode === FNMADD
    }

    def isFmsub(implicit op: UInt): Bool = {
      getOpNum === OP3 && getSubOpcode === FMSUB
    }

    def isFnmsub(implicit op: UInt): Bool = {
      getOpNum === OP3 && getSubOpcode === FNMSUB
    }

    def isFmacc(implicit op: UInt): Bool = {
      getOpNum === OP3 && getSubOpcode === FMACC
    }

    def isFnmacc(implicit op: UInt): Bool = {
      getOpNum === OP3 && getSubOpcode === FNMACC
    }

    def isFmsac(implicit op: UInt): Bool = {
      getOpNum === OP3 && getSubOpcode === FMSAC
    }

    def isFnmsac(implicit op: UInt): Bool = {
      getOpNum === OP3 && getSubOpcode === FNMSAC
    }

    object LitUtil {
      def getOp(implicit op: BitPat): BitPat = op.head(4)

      def isLat1(implicit op: BitPat): Boolean = getOp.isOneOf(FADD, FSUB, FMIN, FMAX, FSGNJ, FSGNJN, FSGNJX, FMINM, FMAXM)

      def getLat(implicit op: BitPat): Int = {
        if (isLat1) 1
        else 3
      }
    }
  }

  object FMacOpcode extends FMacOpcode {
    override def getLat(opcode: Opcode): Int = {
      require(this.all.contains(opcode))
      LitUtil.getLat(opcode.encode)
    }
  }
  object VFMacOpcode extends FMacOpcode {
    override def getLat(opcode: Opcode): Int = {
      require(this.all.contains(opcode))
      3
    }
  }

  trait FMiscOpcode extends Opcodes with DataType {
    private val S2V = bb"0"
    private val S2W = bb"1"

    private val FEQ    = bb"0000"
    private val FLE    = bb"0010"
    private val FLT    = bb"0110"
    private val FNE    = bb"1000"
    private val FGT    = bb"1010"
    private val FGE    = bb"1110"
    private val FLEQ   = bb"0011"
    private val FLTQ   = bb"0111"
    private val FCLASS = bb"0100"

    private val DM = bb"1"
    private val DV = bb"0"

    val feq_fp16    : Opcode = FpRTypeIntDestInst(FEQ , DM, FP16, F)
    val fle_fp16    : Opcode = FpRTypeIntDestInst(FLE , DM, FP16, F)
    val flt_fp16    : Opcode = FpRTypeIntDestInst(FLT , DM, FP16, F)
    val fleq_fp16   : Opcode = FpRTypeIntDestInst(FLEQ, DM, FP16, F)
    val fltq_fp16   : Opcode = FpRTypeIntDestInst(FLTQ, DM, FP16, F)
    val fclass_fp16 : Opcode = FpITypeF2iInst(FCLASS  , DV, FP16, F)
    val vmfeq_fp16  : Opcode = DmSvlS2vS1(FEQ , DM, FP16, V)
    val vmfle_fp16  : Opcode = DmSvlS2vS1(FLE , DM, FP16, V)
    val vmflt_fp16  : Opcode = DmSvlS2vS1(FLT , DM, FP16, V)
    val vmfne_fp16  : Opcode = DmSvlS2vS1(FNE , DM, FP16, V)
    val vmfgt_fp16  : Opcode = DmSvlS2vS1(FGT , DM, FP16, V)
    val vmfge_fp16  : Opcode = DmSvlS2vS1(FGE , DM, FP16, V)
    val vfclass_fp16: Opcode = DvSvlS2v(FCLASS, DV, FP16, V)
    val feq_fp32    : Opcode = FpRTypeIntDestInst(FEQ , DM, FP32, F)
    val fle_fp32    : Opcode = FpRTypeIntDestInst(FLE , DM, FP32, F)
    val flt_fp32    : Opcode = FpRTypeIntDestInst(FLT , DM, FP32, F)
    val fleq_fp32   : Opcode = FpRTypeIntDestInst(FLEQ, DM, FP32, F)
    val fltq_fp32   : Opcode = FpRTypeIntDestInst(FLTQ, DM, FP32, F)
    val fclass_fp32 : Opcode = FpITypeF2iInst(FCLASS  , DV, FP32, F)
    val vmfeq_fp32  : Opcode = DmSvlS2vS1(FEQ , DM, FP32, V)
    val vmfle_fp32  : Opcode = DmSvlS2vS1(FLE , DM, FP32, V)
    val vmflt_fp32  : Opcode = DmSvlS2vS1(FLT , DM, FP32, V)
    val vmfne_fp32  : Opcode = DmSvlS2vS1(FNE , DM, FP32, V)
    val vmfgt_fp32  : Opcode = DmSvlS2vS1(FGT , DM, FP32, V)
    val vmfge_fp32  : Opcode = DmSvlS2vS1(FGE , DM, FP32, V)
    val vfclass_fp32: Opcode = DvSvlS2v(FCLASS, DV, FP32, V)
    val feq_fp64    : Opcode = FpRTypeIntDestInst(FEQ , DM, FP64, F)
    val fle_fp64    : Opcode = FpRTypeIntDestInst(FLE , DM, FP64, F)
    val flt_fp64    : Opcode = FpRTypeIntDestInst(FLT , DM, FP64, F)
    val fleq_fp64   : Opcode = FpRTypeIntDestInst(FLEQ, DM, FP64, F)
    val fltq_fp64   : Opcode = FpRTypeIntDestInst(FLTQ, DM, FP64, F)
    val fclass_fp64 : Opcode = FpITypeF2iInst(FCLASS  , DV, FP64, F)
    val vmfeq_fp64  : Opcode = DmSvlS2vS1(FEQ , DM, FP64, V)
    val vmfle_fp64  : Opcode = DmSvlS2vS1(FLE , DM, FP64, V)
    val vmflt_fp64  : Opcode = DmSvlS2vS1(FLT , DM, FP64, V)
    val vmfne_fp64  : Opcode = DmSvlS2vS1(FNE , DM, FP64, V)
    val vmfgt_fp64  : Opcode = DmSvlS2vS1(FGT , DM, FP64, V)
    val vmfge_fp64  : Opcode = DmSvlS2vS1(FGE , DM, FP64, V)
    val vfclass_fp64: Opcode = DvSvlS2v(FCLASS, DV, FP64, V)

    def getOpcodes(implicit op: UInt): UInt  = op(7, 4)
    def getDestType(implicit op: UInt): UInt = op(3)
    def getFormat(implicit op: UInt): UInt   = op(2, 1)
    def getDataType(implicit op: UInt): UInt = op(0)

    def isFeq(implicit op: UInt): Bool    = getOpcodes === FEQ && getDestType === DM
    def isFlt(implicit op: UInt): Bool    = getOpcodes.isOneOf(FLT, FLTQ) && getDestType === DM
    def isFle(implicit op: UInt): Bool    = getOpcodes.isOneOf(FLE, FLEQ) && getDestType === DM
    def isQuiet(implicit op: UInt): Bool  = op(5, 4).andR && getDestType === DM
    def isFclass(implicit op: UInt): Bool = getOpcodes === FCLASS && getDestType === DV

    def isFne(implicit op: UInt): Bool = getOpcodes === FNE && getDestType === DM
    def isFgt(implicit op: UInt): Bool = getOpcodes === FGT && getDestType === DM
    def isFge(implicit op: UInt): Bool = getOpcodes === FGE && getDestType === DM
    def isFleq(implicit op: UInt): Bool = getOpcodes === FLEQ && getDestType === DM
    def isFltq(implicit op: UInt): Bool = getOpcodes === FLTQ && getDestType === DM
  }

  object FMiscOpcode extends FMiscOpcode
  object FAluOpcode extends FMacOpcode

  trait VFMiscOpcode extends Opcodes with DataType {
    private val VF_MISC = bb"11"
    private val VF = bb"1"

    private val FEQ    = bb"0000"
    private val FLE    = bb"0010"
    private val FLT    = bb"0110"
    private val FNE    = bb"1000"
    private val FGT    = bb"1010"
    private val FGE    = bb"1110"
    private val FCLASS = bb"0100"

    private val DM = bb"1"
    private val DV = bb"0"

    val vmfeq_fp16  : Opcode = DmSvlS2vS1(VF_MISC, FEQ   , DM, FP16, V)
    val vmfeq_fp32  : Opcode = DmSvlS2vS1(VF_MISC, FEQ   , DM, FP32, V)
    val vmfeq_fp64  : Opcode = DmSvlS2vS1(VF_MISC, FEQ   , DM, FP64, V)
    val vmfle_fp16  : Opcode = DmSvlS2vS1(VF_MISC, FLE   , DM, FP16, V)
    val vmfle_fp32  : Opcode = DmSvlS2vS1(VF_MISC, FLE   , DM, FP32, V)
    val vmfle_fp64  : Opcode = DmSvlS2vS1(VF_MISC, FLE   , DM, FP64, V)
    val vmflt_fp16  : Opcode = DmSvlS2vS1(VF_MISC, FLT   , DM, FP16, V)
    val vmflt_fp32  : Opcode = DmSvlS2vS1(VF_MISC, FLT   , DM, FP32, V)
    val vmflt_fp64  : Opcode = DmSvlS2vS1(VF_MISC, FLT   , DM, FP64, V)
    val vmfne_fp16  : Opcode = DmSvlS2vS1(VF_MISC, FNE   , DM, FP16, V)
    val vmfne_fp32  : Opcode = DmSvlS2vS1(VF_MISC, FNE   , DM, FP32, V)
    val vmfne_fp64  : Opcode = DmSvlS2vS1(VF_MISC, FNE   , DM, FP64, V)
    val vmfgt_fp16  : Opcode = DmSvlS2vS1(VF_MISC, FGT   , DM, FP16, V)
    val vmfgt_fp32  : Opcode = DmSvlS2vS1(VF_MISC, FGT   , DM, FP32, V)
    val vmfgt_fp64  : Opcode = DmSvlS2vS1(VF_MISC, FGT   , DM, FP64, V)
    val vmfge_fp16  : Opcode = DmSvlS2vS1(VF_MISC, FGE   , DM, FP16, V)
    val vmfge_fp32  : Opcode = DmSvlS2vS1(VF_MISC, FGE   , DM, FP32, V)
    val vmfge_fp64  : Opcode = DmSvlS2vS1(VF_MISC, FGE   , DM, FP64, V)
    val vfclass_fp16: Opcode = DvSvlS2v(VF_MISC, FCLASS, DV, FP16, V)
    val vfclass_fp32: Opcode = DvSvlS2v(VF_MISC, FCLASS, DV, FP32, V)
    val vfclass_fp64: Opcode = DvSvlS2v(VF_MISC, FCLASS, DV, FP64, V)

    Opcodes.updateMaxFixLat(this.getMaxLat)

    def getFamily(implicit op: UInt): UInt = op(9)
    def getOpcodes(implicit op: UInt): UInt = op(7, 4)
    def getDestType(implicit op: UInt): UInt = op(3)
    def getFormat(implicit op: UInt): UInt = op(2, 1)
    def getDataType(implicit op: UInt): UInt = op(0)

    private def isVectorMisc(implicit op: UInt): Bool = getFamily === VF

    def isFeq(implicit op: UInt): Bool = isVectorMisc && getOpcodes === FEQ && getDestType === DM
    def isFlt(implicit op: UInt): Bool = isVectorMisc && getOpcodes === FLT && getDestType === DM
    def isFle(implicit op: UInt): Bool = isVectorMisc && getOpcodes === FLE && getDestType === DM
    def isFne(implicit op: UInt): Bool = isVectorMisc && getOpcodes === FNE && getDestType === DM
    def isFgt(implicit op: UInt): Bool = isVectorMisc && getOpcodes === FGT && getDestType === DM
    def isFge(implicit op: UInt): Bool = isVectorMisc && getOpcodes === FGE && getDestType === DM
    def isFclass(implicit op: UInt): Bool = isVectorMisc && getOpcodes === FCLASS && getDestType === DV
    def isVfCompare(implicit op: UInt): Bool = isVectorMisc && getDestType === DM
    def isDstMask(implicit op: UInt): Bool = isVfCompare

    override def getLat(opcode: Opcode): Int = {
      require(this.all.contains(opcode))
      0
    }
  }

  object VFMiscOpcode extends VFMiscOpcode

  trait VFRedOpcode extends Opcodes with DataType {
    private val opbN = bb"0"
    private val resN = bb"0"
    private val resW = bb"1"

    private val FRED_USUM = bb"11010"
    private val FRED_MIN  = bb"10100"
    private val FRED_MAX  = bb"10101"
    private val FRED_OSUM = bb"10110"

    val vfredusum_fp16 : Opcode = DsSvlS2vS1s(FRED_USUM, opbN, resN, FP16)
    val vfredusum_fp32 : Opcode = DsSvlS2vS1s(FRED_USUM, opbN, resN, FP32)
    val vfredusum_fp64 : Opcode = DsSvlS2vS1s(FRED_USUM, opbN, resN, FP64)
    val vfredosum_fp16 : Opcode = DsSvlS2vS1s(FRED_OSUM, opbN, resN, FP16)
    val vfredosum_fp32 : Opcode = DsSvlS2vS1s(FRED_OSUM, opbN, resN, FP32)
    val vfredosum_fp64 : Opcode = DsSvlS2vS1s(FRED_OSUM, opbN, resN, FP64)
    val vfredmin_fp16  : Opcode = DsSvlS2vS1s(FRED_MIN,  opbN, resN, FP16)
    val vfredmin_fp32  : Opcode = DsSvlS2vS1s(FRED_MIN,  opbN, resN, FP32)
    val vfredmin_fp64  : Opcode = DsSvlS2vS1s(FRED_MIN,  opbN, resN, FP64)
    val vfredmax_fp16  : Opcode = DsSvlS2vS1s(FRED_MAX,  opbN, resN, FP16)
    val vfredmax_fp32  : Opcode = DsSvlS2vS1s(FRED_MAX,  opbN, resN, FP32)
    val vfredmax_fp64  : Opcode = DsSvlS2vS1s(FRED_MAX,  opbN, resN, FP64)
    val vfwredusum_fp16: Opcode = DwsSvlS2vS1ws(FRED_USUM, opbN, resW, FP16)
    val vfwredusum_fp32: Opcode = DwsSvlS2vS1ws(FRED_USUM, opbN, resW, FP32)
    val vfwredosum_fp16: Opcode = DwsSvlS2vS1ws(FRED_OSUM, opbN, resW, FP16)
    val vfwredosum_fp32: Opcode = DwsSvlS2vS1ws(FRED_OSUM, opbN, resW, FP32)

    // to do
    def isVfredusum(implicit op: UInt): Bool = false.B
    def isVfredmin(implicit op: UInt): Bool = false.B
    def isVfredmax(implicit op: UInt): Bool = false.B
    def isVfredosum(implicit op: UInt): Bool = false.B
    def isVfwredosum(implicit op: UInt): Bool = false.B
    def isVfRedUnordered(implicit op: UInt): Bool = isVfredusum || isVfredmin || isVfredmax
    def isVfRedUnComp(implicit op: UInt): Bool = isVfredmin || isVfredmax
    def isVfRedOrdered(implicit op: UInt): Bool = isVfredosum || isVfwredosum
    def isVfWRedOrdered(implicit op: UInt): Bool = isVfwredosum
    def isVfredosumNoWiden(implicit op: UInt): Bool = isVfredosum
    def isVfReduction(implicit op: UInt): Bool = isVfRedUnordered || isVfRedOrdered

    def vfredusum(implicit op: UInt): Bool = isVfredusum
    def vfredmin(implicit op: UInt): Bool = isVfredmin
    def vfredmax(implicit op: UInt): Bool = isVfredmax
    def vfredosum(implicit op: UInt): Bool = isVfredosum
    def vfwredosum(implicit op: UInt): Bool = isVfwredosum
  }

  object VFRedOpcode extends VFRedOpcode

  trait VFDivOpcode extends Opcodes with DataType {
    private val FDIV  = bb"0"
    private val FSQRT = bb"1"

    val vfdiv_fp16 : Opcode = DvSvlS2vS1(FDIV , FP16, V)
    val vfsqrt_fp16: Opcode = DvSvlS2vS1(FSQRT, FP16, V)
    val vfdiv_fp32 : Opcode = DvSvlS2vS1(FDIV , FP32, V)
    val vfsqrt_fp32: Opcode = DvSvlS2vS1(FSQRT, FP32, V)
    val vfdiv_fp64 : Opcode = DvSvlS2vS1(FDIV , FP64, V)
    val vfsqrt_fp64: Opcode = DvSvlS2vS1(FSQRT, FP64, V)

    private def getOp(implicit op: UInt): UInt = op(3)

    def isFDiv(implicit op: UInt): Bool = getOp === FDIV
    def isFSqrt(implicit op: UInt): Bool = getOp === FSQRT
  }

  object VFDivOpcode extends VFDivOpcode

  trait FCvtOpcode extends Opcodes with DataType {
    private val F2F = bb"00"
    private val F2I = bb"01"
    private val I2F = bb"10"
    private val OTHER = bb"11"

    /**
     * Three sub opcode of [[F2F]]
     * [[cvt]], [[rnd]], [[rndnx]]
     */

    private val cvt = bb"00"

    private val rnd = bb"10"

    private val rndnx = bb"11"

    /**
     * sub-opcode of [[F2I]]
     */

    private val F2S = bb"00"
    private val F2U = bb"01"
    private val F2SMOD = bb"10"

    /**
     * sub-opcode of [[I2F]]
     */

    private val S2F = bb"00"
    private val U2F = bb"01"
    private val FMVI2F = bb"11"

    /**
     * sub-opcode of [[OTHER]]
     */

    private val FREC7   = bb"1000"
    private val FRSQRT7 = bb"0101"
    private val FMVF2I  = bb"0000"

    val fcvt_fp32_fp16: Opcode = FpITypeF2fInst(FP32, FP16, cvt, F2F, F)
    val fcvt_fp64_fp16: Opcode = FpITypeF2fInst(FP64, FP16, cvt, F2F, F)
    val fcvt_fp16_fp32: Opcode = FpITypeF2fInst(FP16, FP32, cvt, F2F, F)
    val fcvt_fp64_fp32: Opcode = FpITypeF2fInst(FP64, FP32, cvt, F2F, F)
    val fcvt_fp16_fp64: Opcode = FpITypeF2fInst(FP16, FP64, cvt, F2F, F)
    val fcvt_fp32_fp64: Opcode = FpITypeF2fInst(FP32, FP64, cvt, F2F, F)

    val frnd_fp16: Opcode = FpITypeF2fInst(FP16, FP16, rnd, F2F, F)
    val frnd_fp32: Opcode = FpITypeF2fInst(FP32, FP32, rnd, F2F, F)
    val frnd_fp64: Opcode = FpITypeF2fInst(FP64, FP64, rnd, F2F, F)

    val frndnx_fp16: Opcode = FpITypeF2fInst(FP16, FP16, rndnx, F2F, F)
    val frndnx_fp32: Opcode = FpITypeF2fInst(FP32, FP32, rndnx, F2F, F)
    val frndnx_fp64: Opcode = FpITypeF2fInst(FP64, FP64, rndnx, F2F, F)

    // two narrow
    val vfcvt_fp16_fp32: Opcode = DvSvlS2v(FP16, FP32, cvt, F2F, V)
    val vfcvt_fp32_fp64: Opcode = DvSvlS2v(FP32, FP64, cvt, F2F, V)

    // two widen
    val vfcvt_fp32_fp16: Opcode = DvSvlS2v(FP32, FP16, cvt, F2F, V)
    val vfcvt_fp64_fp32: Opcode = DvSvlS2v(FP64, FP32, cvt, F2F, V)

    val fcvt_si32_fp16: Opcode = FpITypeF2iInst(I32, FP16, F2S, F2I, F)
    val fcvt_ui32_fp16: Opcode = FpITypeF2iInst(I32, FP16, F2U, F2I, F)
    val fcvt_si64_fp16: Opcode = FpITypeF2iInst(I64, FP16, F2S, F2I, F)
    val fcvt_ui64_fp16: Opcode = FpITypeF2iInst(I64, FP16, F2U, F2I, F)
    val fcvt_si32_fp32: Opcode = FpITypeF2iInst(I32, FP32, F2S, F2I, F)
    val fcvt_ui32_fp32: Opcode = FpITypeF2iInst(I32, FP32, F2U, F2I, F)
    val fcvt_si64_fp32: Opcode = FpITypeF2iInst(I64, FP32, F2S, F2I, F)
    val fcvt_ui64_fp32: Opcode = FpITypeF2iInst(I64, FP32, F2U, F2I, F)
    val fcvt_si32_fp64: Opcode = FpITypeF2iInst(I32, FP64, F2S, F2I, F)
    val fcvt_ui32_fp64: Opcode = FpITypeF2iInst(I32, FP64, F2U, F2I, F)
    val fcvt_si64_fp64: Opcode = FpITypeF2iInst(I64, FP64, F2S, F2I, F)
    val fcvt_ui64_fp64: Opcode = FpITypeF2iInst(I64, FP64, F2U, F2I, F)
    val fcvtmod_si32_fp64: Opcode = FpITypeF2iInst(I32, FP64, F2SMOD, F2I, F)

    val vfcvt_si8_fp16 : Opcode = DvSvlS2v(I8, FP16, F2S, F2I, V)
    val vfcvt_ui8_fp16 : Opcode = DvSvlS2v(I8, FP16, F2U, F2I, V)
    val vfcvt_si16_fp16: Opcode = DvSvlS2v(I16, FP16, F2S, F2I, V)
    val vfcvt_ui16_fp16: Opcode = DvSvlS2v(I16, FP16, F2U, F2I, V)
    val vfcvt_si32_fp16: Opcode = DvSvlS2v(I32, FP16, F2S, F2I, V)
    val vfcvt_ui32_fp16: Opcode = DvSvlS2v(I32, FP16, F2U, F2I, V)
    val vfcvt_si16_fp32: Opcode = DvSvlS2v(I16, FP32, F2S, F2I, V)
    val vfcvt_ui16_fp32: Opcode = DvSvlS2v(I16, FP32, F2U, F2I, V)
    val vfcvt_si32_fp32: Opcode = DvSvlS2v(I32, FP32, F2S, F2I, V)
    val vfcvt_ui32_fp32: Opcode = DvSvlS2v(I32, FP32, F2U, F2I, V)
    val vfcvt_si64_fp32: Opcode = DvSvlS2v(I64, FP32, F2S, F2I, V)
    val vfcvt_ui64_fp32: Opcode = DvSvlS2v(I64, FP32, F2U, F2I, V)
    val vfcvt_si32_fp64: Opcode = DvSvlS2v(I32, FP64, F2S, F2I, V)
    val vfcvt_ui32_fp64: Opcode = DvSvlS2v(I32, FP64, F2U, F2I, V)
    val vfcvt_si64_fp64: Opcode = DvSvlS2v(I64, FP64, F2S, F2I, V)
    val vfcvt_ui64_fp64: Opcode = DvSvlS2v(I64, FP64, F2U, F2I, V)

    val fcvt_fp16_si32: Opcode = FpITypeI2fOtherInst(FP16, I32,    S2F, I2F, F)
    val fcvt_fp16_ui32: Opcode = FpITypeI2fOtherInst(FP16, I32,    U2F, I2F, F)
    val fcvt_fp16_si64: Opcode = FpITypeI2fOtherInst(FP16, I64,    S2F, I2F, F)
    val fcvt_fp16_ui64: Opcode = FpITypeI2fOtherInst(FP16, I64,    U2F, I2F, F)
    val fcvt_fp32_si32: Opcode = FpITypeI2fOtherInst(FP32, I32,    S2F, I2F, F)
    val fcvt_fp32_ui32: Opcode = FpITypeI2fOtherInst(FP32, I32,    U2F, I2F, F)
    val fcvt_fp32_si64: Opcode = FpITypeI2fOtherInst(FP32, I64,    S2F, I2F, F)
    val fcvt_fp32_ui64: Opcode = FpITypeI2fOtherInst(FP32, I64,    U2F, I2F, F)
    val fcvt_fp64_si32: Opcode = FpITypeI2fOtherInst(FP64, I32,    S2F, I2F, F)
    val fcvt_fp64_ui32: Opcode = FpITypeI2fOtherInst(FP64, I32,    U2F, I2F, F)
    val fcvt_fp64_si64: Opcode = FpITypeI2fOtherInst(FP64, I64,    S2F, I2F, F)
    val fcvt_fp64_ui64: Opcode = FpITypeI2fOtherInst(FP64, I64,    U2F, I2F, F)
    val fmv_fp16_i    : Opcode = FpITypeI2fOtherInst(FP16, I64, FMVI2F, I2F, F)
    val fmv_fp32_i    : Opcode = FpITypeI2fOtherInst(FP32, I64, FMVI2F, I2F, F)
    val fmv_fp64_i    : Opcode = FpITypeI2fOtherInst(FP64, I64, FMVI2F, I2F, F)

    val vfcvt_fp16_si16: Opcode = DvSvlS2v(FP16, I16, S2F, I2F, V)
    val vfcvt_fp16_ui16: Opcode = DvSvlS2v(FP16, I16, U2F, I2F, V)
    val vfcvt_fp16_si32: Opcode = DvSvlS2v(FP16, I32, S2F, I2F, V)
    val vfcvt_fp16_ui32: Opcode = DvSvlS2v(FP16, I32, U2F, I2F, V)
    val vfcvt_fp32_si16: Opcode = DvSvlS2v(FP32, I16, S2F, I2F, V)
    val vfcvt_fp32_ui16: Opcode = DvSvlS2v(FP32, I16, U2F, I2F, V)
    val vfcvt_fp32_si32: Opcode = DvSvlS2v(FP32, I32, S2F, I2F, V)
    val vfcvt_fp32_ui32: Opcode = DvSvlS2v(FP32, I32, U2F, I2F, V)
    val vfcvt_fp32_si64: Opcode = DvSvlS2v(FP32, I64, S2F, I2F, V)
    val vfcvt_fp32_ui64: Opcode = DvSvlS2v(FP32, I64, U2F, I2F, V)
    val vfcvt_fp64_si32: Opcode = DvSvlS2v(FP64, I32, S2F, I2F, V)
    val vfcvt_fp64_ui32: Opcode = DvSvlS2v(FP64, I32, U2F, I2F, V)
    val vfcvt_fp64_si64: Opcode = DvSvlS2v(FP64, I64, S2F, I2F, V)
    val vfcvt_fp64_ui64: Opcode = DvSvlS2v(FP64, I64, U2F, I2F, V)

    val fmv_i_fp16 : Opcode = FpITypeF2iInst(FP16, FMVF2I, OTHER, F)
    val fmv_i_fp32 : Opcode = FpITypeF2iInst(FP32, FMVF2I, OTHER, F)
    val fmv_i_fp64 : Opcode = FpITypeF2iInst(FP64, FMVF2I, OTHER, F)

    val vfrec7_fp16  : Opcode = DvSvlS2v(FP16, FREC7, OTHER, V)
    val vfrec7_fp32  : Opcode = DvSvlS2v(FP32, FREC7, OTHER, V)
    val vfrec7_fp64  : Opcode = DvSvlS2v(FP64, FREC7, OTHER, V)
    val vfrsqrt7_fp16: Opcode = DvSvlS2v(FP16, FRSQRT7, OTHER, V)
    val vfrsqrt7_fp32: Opcode = DvSvlS2v(FP32, FRSQRT7, OTHER, V)
    val vfrsqrt7_fp64: Opcode = DvSvlS2v(FP64, FRSQRT7, OTHER, V)

    Opcodes.updateMaxFixLat(this.getMaxLat)

    def getOutputDataWidth(implicit op: UInt): UInt = op(8, 7)
    def getInputDataWidth(implicit op: UInt): UInt = Mux(isOther, getOutputDataWidth, op(6, 5))
    def getCvtSign(implicit op: UInt): UInt = op(4, 3)
    def getCvtType(implicit op: UInt): UInt = op(2, 1)
    def getDataType(implicit op: UInt): UInt = op(0)

    def isF2F(implicit op: UInt): Bool = getCvtType === F2F
    def isF2I(implicit op: UInt): Bool = getCvtType === F2I
    def isI2F(implicit op: UInt): Bool = getCvtType === I2F
    def isOther(implicit op: UInt): Bool = getCvtType === OTHER

    def isFround(implicit op: UInt): Bool    = isF2F && op(4, 3) === rnd
    def isFroundNx(implicit op: UInt): Bool  = isF2F && op(4, 3) === rndnx
    def isFcvtMod(implicit op: UInt): Bool   = isF2I && op(4, 3) === F2SMOD
    def isEstimate7(implicit op: UInt): Bool = isOther && op(6, 3).isOneOf(FREC7, FRSQRT7)
    def isRec(implicit op: UInt): Bool       = isOther && op(6, 3) === FREC7

    def inIsFp(implicit op: UInt): Bool      = !op(2)
    def outIsFp(implicit op: UInt): Bool     = !op(1)
    def outIsInt(implicit op: UInt): Bool    = op(2, 1) === F2I
    def isSignInt(implicit op: UInt): Bool   = !op(3)
    def isUnSignInt(implicit op: UInt): Bool = op(3)

    def isFmvF2I(implicit op: UInt): Bool    = isOther && op(6, 3) === FMVF2I

    def inIs64(implicit op: UInt): Bool = getInputDataWidth === FP64
    def inIs32(implicit op: UInt): Bool = getInputDataWidth === FP32
    def inIs16(implicit op: UInt): Bool = getInputDataWidth === FP16
    def outIs64(implicit op: UInt): Bool = getOutputDataWidth === FP64
    def outIs32(implicit op: UInt): Bool = getOutputDataWidth === FP32
    def outIs16(implicit op: UInt): Bool = getOutputDataWidth === FP16

    object LitUtil {
      def getOutputDataWidth(implicit op: BitPat): BitPat = op(8, 7)
      def getInputDataWidthRaw(implicit op: BitPat): BitPat = op(6, 5)
      def getCvtType(implicit op: BitPat): BitPat = op(2, 1)

      def isOther(implicit op: BitPat): Boolean = getCvtType.isOneOf(OTHER)

      def getVectorLat(implicit op: BitPat): Int = {
        if (getOutputDataWidth.isOneOf(FP64) || (!isOther && getInputDataWidthRaw.isOneOf(FP64))) 2
        else 1
      }
    }


  }

  object FCvtOpcode extends FCvtOpcode
  object VFCvtOpcode extends FCvtOpcode {
    override def getLat(opcode: Opcode): Int = {
      require(this.all.contains(opcode))
      LitUtil.getVectorLat(opcode.encode)
    }
  }

  trait VIAluOpcode extends Opcodes with DataType {

    private val ADDER  = bb"000"
    private val CMP    = bb"100"
    private val LOGIC  = bb"010"
    private val MOVE   = bb"110"
    private val CADDER = bb"001"
    private val BITOP  = bb"101"
    private val SHIFT  = bb"011"
    private val CSHIFT = bb"111"

    private val S2VDV  = bb"000"
    private val S2VDW  = bb"001"
    private val S2WDV  = bb"010"
    private val S2WDW  = bb"011"
    private val S2VDM  = bb"100"
    private val S2F8DV = bb"101"
    private val S2F4DV = bb"110"
    private val S2F2DV = bb"111"

    private val useSUB = bb"1"
    private val noSUB = bb"0"

    private val SGN = bb"1"
    private val UGN = bb"0"

    /**
     * sub opcode of [[ADDER]]
     */
    private val ADD  = noSUB  ## SGN ## bb"00"
    private val SUB  = useSUB ## SGN ## bb"00"
    private val ADC  = noSUB  ## UGN ## bb"01"
    private val SBC  = useSUB ## UGN ## bb"01"
    private val MINU = useSUB ## UGN ## bb"10"
    private val MIN  = useSUB ## SGN ## bb"10"
    private val MAXU = useSUB ## UGN ## bb"11"
    private val MAX  = useSUB ## SGN ## bb"11"

    /**
     * sub opcode of [[CMP]]
     */
    private val EQ  = useSUB ## UGN ## bb"00"
    private val NE  = useSUB ## SGN ## bb"00"
    private val LTU = useSUB ## UGN ## bb"01"
    private val LT  = useSUB ## SGN ## bb"01"
    private val LEU = useSUB ## UGN ## bb"10"
    private val LE  = useSUB ## SGN ## bb"10"
    private val GTU = useSUB ## UGN ## bb"11"
    private val GT  = useSUB ## SGN ## bb"11"

    /**
     * sub opcode of [[LOGIC]]
     */
    private val ANDN = bb"0000"
    private val AND  = bb"0001"
    private val OR   = bb"0010"
    private val XOR  = bb"0011"
    private val ORN  = bb"0100"
    private val NAND = bb"0101"
    private val NOR  = bb"0110"
    private val XNOR = bb"0111"

    /**
     * sub opcode of [[MOVE]]
     */
    private val ZEXT   = bb"0000"
    private val SEXT   = bb"0100"

    /**
     * sub opcode of [[CADDER]]
     * [[AADDU]] and [[WADDU]] will be distinguashed by [[S2VDV]] or [[S2VDW]]
     */
    private val AADDU = noSUB  ## UGN ## bb"00"
    private val AADD  = noSUB  ## SGN ## bb"00"
    private val ASUBU = useSUB ## UGN ## bb"01"
    private val ASUB  = useSUB ## SGN ## bb"01"
    private val SADDU = noSUB  ## UGN ## bb"10"
    private val SADD  = noSUB  ## SGN ## bb"10"
    private val SSUBU = useSUB ## UGN ## bb"11"
    private val SSUB  = useSUB ## SGN ## bb"11"
    private val WADDU = noSUB  ## UGN ## bb"00"
    private val WADD  = noSUB  ## SGN ## bb"00"
    private val WSUBU = useSUB ## UGN ## bb"01"
    private val WSUB  = useSUB ## SGN ## bb"01"

    /**
     * WADD4U:
     *   vd(i) = waddu( vs1(2i) + vs2(2i) ) + waddu( vs1(2i + 1) + vs2(2i + 1) )
     */
    private val WADD4U = noSUB ## UGN ## bb"10"

    /**
     * WADD4:
     *   vd(i) = wadd( vs1(2i) + vs2(2i) ) + wadd( vs1(2i + 1) + vs2(2i + 1) )
     */
    private val WADD4  = noSUB ## SGN ## bb"10"

    /**
     * sub opcode of [[BITOP]]
     */
    private val CPOP_M = bb"0000"
    private val CPOP_V = bb"0001"
    private val FIRST  = bb"0010"
    private val ID     = bb"0011"
    private val MSBF   = bb"0100"
    private val MSIF   = bb"0101"
    private val MSOF   = bb"0110"
    private val IOTA   = bb"0111"
    private val REV8   = bb"0000"

    /**
     * sub opcode of [[SHIFT]] and [[CSHIFT]]
     */
    private val SLL   = bb"0" ## UGN ## bb"00"
    private val SRL   = bb"0" ## UGN ## bb"01"
    private val SRA   = bb"0" ## SGN ## bb"01"
    private val ROR   = bb"0" ## UGN ## bb"10"
    private val ROL   = bb"0" ## UGN ## bb"11"
    private val CLIPU = bb"1" ## UGN ## bb"00"
    private val CLIP  = bb"1" ## SGN ## bb"00"

    val vadd_e8   = DvSvlS2vS1(ADD, ADDER, S2VDV, E8)
    val vadd_e16  = DvSvlS2vS1(ADD, ADDER, S2VDV, E16)
    val vadd_e32  = DvSvlS2vS1(ADD, ADDER, S2VDV, E32)
    val vadd_e64  = DvSvlS2vS1(ADD, ADDER, S2VDV, E64)
    val vsub_e8   = DvSvlS2vS1(SUB, ADDER, S2VDV, E8)
    val vsub_e16  = DvSvlS2vS1(SUB, ADDER, S2VDV, E16)
    val vsub_e32  = DvSvlS2vS1(SUB, ADDER, S2VDV, E32)
    val vsub_e64  = DvSvlS2vS1(SUB, ADDER, S2VDV, E64)
    val vadc_e8   = DvSvlS2vS1(ADC, ADDER, S2VDV, E8)  + V0RenAsSrc
    val vadc_e16  = DvSvlS2vS1(ADC, ADDER, S2VDV, E16) + V0RenAsSrc
    val vadc_e32  = DvSvlS2vS1(ADC, ADDER, S2VDV, E32) + V0RenAsSrc
    val vadc_e64  = DvSvlS2vS1(ADC, ADDER, S2VDV, E64) + V0RenAsSrc
    val vmadc_e8  = DmSvlS2vS1(ADC, ADDER, S2VDM, E8)  + V0RenAsSrc
    val vmadc_e16 = DmSvlS2vS1(ADC, ADDER, S2VDM, E16) + V0RenAsSrc
    val vmadc_e32 = DmSvlS2vS1(ADC, ADDER, S2VDM, E32) + V0RenAsSrc
    val vmadc_e64 = DmSvlS2vS1(ADC, ADDER, S2VDM, E64) + V0RenAsSrc
    val vsbc_e8   = DvSvlS2vS1(SBC, ADDER, S2VDV, E8)  + V0RenAsSrc
    val vsbc_e16  = DvSvlS2vS1(SBC, ADDER, S2VDV, E16) + V0RenAsSrc
    val vsbc_e32  = DvSvlS2vS1(SBC, ADDER, S2VDV, E32) + V0RenAsSrc
    val vsbc_e64  = DvSvlS2vS1(SBC, ADDER, S2VDV, E64) + V0RenAsSrc
    val vmsbc_e8  = DmSvlS2vS1(SBC, ADDER, S2VDM, E8)  + V0RenAsSrc
    val vmsbc_e16 = DmSvlS2vS1(SBC, ADDER, S2VDM, E16) + V0RenAsSrc
    val vmsbc_e32 = DmSvlS2vS1(SBC, ADDER, S2VDM, E32) + V0RenAsSrc
    val vmsbc_e64 = DmSvlS2vS1(SBC, ADDER, S2VDM, E64) + V0RenAsSrc

    val vminu_e8  = DvSvlS2vS1(MINU, ADDER, S2VDV, E8 )
    val vminu_e16 = DvSvlS2vS1(MINU, ADDER, S2VDV, E16)
    val vminu_e32 = DvSvlS2vS1(MINU, ADDER, S2VDV, E32)
    val vminu_e64 = DvSvlS2vS1(MINU, ADDER, S2VDV, E64)
    val vmin_e8   = DvSvlS2vS1(MIN,  ADDER, S2VDV, E8 )
    val vmin_e16  = DvSvlS2vS1(MIN,  ADDER, S2VDV, E16)
    val vmin_e32  = DvSvlS2vS1(MIN,  ADDER, S2VDV, E32)
    val vmin_e64  = DvSvlS2vS1(MIN,  ADDER, S2VDV, E64)
    val vmaxu_e8  = DvSvlS2vS1(MAXU, ADDER, S2VDV, E8 )
    val vmaxu_e16 = DvSvlS2vS1(MAXU, ADDER, S2VDV, E16)
    val vmaxu_e32 = DvSvlS2vS1(MAXU, ADDER, S2VDV, E32)
    val vmaxu_e64 = DvSvlS2vS1(MAXU, ADDER, S2VDV, E64)
    val vmax_e8   = DvSvlS2vS1(MAX ,  ADDER, S2VDV, E8 )
    val vmax_e16  = DvSvlS2vS1(MAX ,  ADDER, S2VDV, E16)
    val vmax_e32  = DvSvlS2vS1(MAX ,  ADDER, S2VDV, E32)
    val vmax_e64  = DvSvlS2vS1(MAX ,  ADDER, S2VDV, E64)

    val vmseq_e8   = DmSvlS2vS1(EQ , CMP, S2VDM, E8)
    val vmseq_e16  = DmSvlS2vS1(EQ , CMP, S2VDM, E16)
    val vmseq_e32  = DmSvlS2vS1(EQ , CMP, S2VDM, E32)
    val vmseq_e64  = DmSvlS2vS1(EQ , CMP, S2VDM, E64)
    val vmsne_e8   = DmSvlS2vS1(NE , CMP, S2VDM, E8)
    val vmsne_e16  = DmSvlS2vS1(NE , CMP, S2VDM, E16)
    val vmsne_e32  = DmSvlS2vS1(NE , CMP, S2VDM, E32)
    val vmsne_e64  = DmSvlS2vS1(NE , CMP, S2VDM, E64)
    val vmsltu_e8  = DmSvlS2vS1(LTU, CMP, S2VDM, E8)
    val vmsltu_e16 = DmSvlS2vS1(LTU, CMP, S2VDM, E16)
    val vmsltu_e32 = DmSvlS2vS1(LTU, CMP, S2VDM, E32)
    val vmsltu_e64 = DmSvlS2vS1(LTU, CMP, S2VDM, E64)
    val vmslt_e8   = DmSvlS2vS1(LT , CMP, S2VDM, E8)
    val vmslt_e16  = DmSvlS2vS1(LT , CMP, S2VDM, E16)
    val vmslt_e32  = DmSvlS2vS1(LT , CMP, S2VDM, E32)
    val vmslt_e64  = DmSvlS2vS1(LT , CMP, S2VDM, E64)
    val vmsleu_e8  = DmSvlS2vS1(LEU, CMP, S2VDM, E8)
    val vmsleu_e16 = DmSvlS2vS1(LEU, CMP, S2VDM, E16)
    val vmsleu_e32 = DmSvlS2vS1(LEU, CMP, S2VDM, E32)
    val vmsleu_e64 = DmSvlS2vS1(LEU, CMP, S2VDM, E64)
    val vmsle_e8   = DmSvlS2vS1(LE , CMP, S2VDM, E8)
    val vmsle_e16  = DmSvlS2vS1(LE , CMP, S2VDM, E16)
    val vmsle_e32  = DmSvlS2vS1(LE , CMP, S2VDM, E32)
    val vmsle_e64  = DmSvlS2vS1(LE , CMP, S2VDM, E64)
    val vmsgtu_e8  = DmSvlS2vS1(GTU, CMP, S2VDM, E8)
    val vmsgtu_e16 = DmSvlS2vS1(GTU, CMP, S2VDM, E16)
    val vmsgtu_e32 = DmSvlS2vS1(GTU, CMP, S2VDM, E32)
    val vmsgtu_e64 = DmSvlS2vS1(GTU, CMP, S2VDM, E64)
    val vmsgt_e8   = DmSvlS2vS1(GT , CMP, S2VDM, E8)
    val vmsgt_e16  = DmSvlS2vS1(GT , CMP, S2VDM, E16)
    val vmsgt_e32  = DmSvlS2vS1(GT , CMP, S2VDM, E32)
    val vmsgt_e64  = DmSvlS2vS1(GT , CMP, S2VDM, E64)

    val vandn_e8  = DvSvlS2vS1(ANDN, LOGIC, S2VDV, E8)
    val vandn_e16 = DvSvlS2vS1(ANDN, LOGIC, S2VDV, E16)
    val vandn_e32 = DvSvlS2vS1(ANDN, LOGIC, S2VDV, E32)
    val vandn_e64 = DvSvlS2vS1(ANDN, LOGIC, S2VDV, E64)
    val vand_e8   = DvSvlS2vS1(AND , LOGIC, S2VDV, E8)
    val vand_e16  = DvSvlS2vS1(AND , LOGIC, S2VDV, E16)
    val vand_e32  = DvSvlS2vS1(AND , LOGIC, S2VDV, E32)
    val vand_e64  = DvSvlS2vS1(AND , LOGIC, S2VDV, E64)
    val vor_e8    = DvSvlS2vS1(OR  , LOGIC, S2VDV, E8)
    val vor_e16   = DvSvlS2vS1(OR  , LOGIC, S2VDV, E16)
    val vor_e32   = DvSvlS2vS1(OR  , LOGIC, S2VDV, E32)
    val vor_e64   = DvSvlS2vS1(OR  , LOGIC, S2VDV, E64)
    val vxor_e8   = DvSvlS2vS1(XOR , LOGIC, S2VDV, E8)
    val vxor_e16  = DvSvlS2vS1(XOR , LOGIC, S2VDV, E16)
    val vxor_e32  = DvSvlS2vS1(XOR , LOGIC, S2VDV, E32)
    val vxor_e64  = DvSvlS2vS1(XOR , LOGIC, S2VDV, E64)

    val vmandn = DvSvlS2vS1v(ANDN, LOGIC, S2VDM, EX)
    val vmand  = DvSvlS2vS1v(AND , LOGIC, S2VDM, EX)
    val vmor   = DvSvlS2vS1v(OR  , LOGIC, S2VDM, EX)
    val vmxor  = DvSvlS2vS1v(XOR , LOGIC, S2VDM, EX)
    val vmorn  = DvSvlS2vS1v(ORN , LOGIC, S2VDM, EX)
    val vmnand = DvSvlS2vS1v(NAND, LOGIC, S2VDM, EX)
    val vmnor  = DvSvlS2vS1v(NOR , LOGIC, S2VDM, EX)
    val vmxnor = DvSvlS2vS1v(XNOR, LOGIC, S2VDM, EX)

    val vzext2_e8  = DvSvlS2v(ZEXT, MOVE, S2F2DV, E8 ) // vzext.vf2 when sew=e16
    val vzext2_e16 = DvSvlS2v(ZEXT, MOVE, S2F2DV, E16)
    val vzext2_e32 = DvSvlS2v(ZEXT, MOVE, S2F2DV, E32)
    val vzext4_e8  = DvSvlS2v(ZEXT, MOVE, S2F4DV, E8 )
    val vzext4_e16 = DvSvlS2v(ZEXT, MOVE, S2F4DV, E16)
    val vzext8_e8  = DvSvlS2v(ZEXT, MOVE, S2F8DV, E8 )
    val vsext2_e8  = DvSvlS2v(SEXT, MOVE, S2F2DV, E8 ) // vsext.vf2 when sew=e16
    val vsext2_e16 = DvSvlS2v(SEXT, MOVE, S2F2DV, E16)
    val vsext2_e32 = DvSvlS2v(SEXT, MOVE, S2F2DV, E32)
    val vsext4_e8  = DvSvlS2v(SEXT, MOVE, S2F4DV, E8 )
    val vsext4_e16 = DvSvlS2v(SEXT, MOVE, S2F4DV, E16)
    val vsext8_e8  = DvSvlS2v(SEXT, MOVE, S2F8DV, E8 )

    val vwaddu_e8    = DvSvlS2vS1(WADDU, CADDER, S2VDW, E8)
    val vwaddu_e16   = DvSvlS2vS1(WADDU, CADDER, S2VDW, E16)
    val vwaddu_e32   = DvSvlS2vS1(WADDU, CADDER, S2VDW, E32)
    val vwadd_e8     = DvSvlS2vS1(WADD , CADDER, S2VDW, E8)
    val vwadd_e16    = DvSvlS2vS1(WADD , CADDER, S2VDW, E16)
    val vwadd_e32    = DvSvlS2vS1(WADD , CADDER, S2VDW, E32)
    val vwsubu_e8    = DvSvlS2vS1(WSUBU, CADDER, S2VDW, E8)
    val vwsubu_e16   = DvSvlS2vS1(WSUBU, CADDER, S2VDW, E16)
    val vwsubu_e32   = DvSvlS2vS1(WSUBU, CADDER, S2VDW, E32)
    val vwsub_e8     = DvSvlS2vS1(WSUB , CADDER, S2VDW, E8)
    val vwsub_e16    = DvSvlS2vS1(WSUB , CADDER, S2VDW, E16)
    val vwsub_e32    = DvSvlS2vS1(WSUB , CADDER, S2VDW, E32)
    val vwaddu_w_e8  = DvSvlS2vS1(WADDU, CADDER, S2WDW, E8)
    val vwaddu_w_e16 = DvSvlS2vS1(WADDU, CADDER, S2WDW, E16)
    val vwaddu_w_e32 = DvSvlS2vS1(WADDU, CADDER, S2WDW, E32)
    val vwadd_w_e8   = DvSvlS2vS1(WADD , CADDER, S2WDW, E8)
    val vwadd_w_e16  = DvSvlS2vS1(WADD , CADDER, S2WDW, E16)
    val vwadd_w_e32  = DvSvlS2vS1(WADD , CADDER, S2WDW, E32)
    val vwsubu_w_e8  = DvSvlS2vS1(WSUBU, CADDER, S2WDW, E8)
    val vwsubu_w_e16 = DvSvlS2vS1(WSUBU, CADDER, S2WDW, E16)
    val vwsubu_w_e32 = DvSvlS2vS1(WSUBU, CADDER, S2WDW, E32)
    val vwsub_w_e8   = DvSvlS2vS1(WSUB , CADDER, S2WDW, E8)
    val vwsub_w_e16  = DvSvlS2vS1(WSUB , CADDER, S2WDW, E16)
    val vwsub_w_e32  = DvSvlS2vS1(WSUB , CADDER, S2WDW, E32)

    // internal uop to support reduction sum
    val vwadd4u_e8  = DvSvlS2vS1(WADD4U, CADDER, S2VDW, E8 )
    val vwadd4u_e16 = DvSvlS2vS1(WADD4U, CADDER, S2VDW, E16)
    val vwadd4u_e32 = DvSvlS2vS1(WADD4U, CADDER, S2VDW, E32)
    val vwadd4_e8   = DvSvlS2vS1(WADD4 , CADDER, S2VDW, E8 )
    val vwadd4_e16  = DvSvlS2vS1(WADD4 , CADDER, S2VDW, E16)
    val vwadd4_e32  = DvSvlS2vS1(WADD4 , CADDER, S2VDW, E32)

    val vaaddu_e8  = DvSvlS2vS1(AADDU, CADDER, S2VDV, E8)
    val vaaddu_e16 = DvSvlS2vS1(AADDU, CADDER, S2VDV, E16)
    val vaaddu_e32 = DvSvlS2vS1(AADDU, CADDER, S2VDV, E32)
    val vaaddu_e64 = DvSvlS2vS1(AADDU, CADDER, S2VDV, E64)
    val vaadd_e8   = DvSvlS2vS1(AADD , CADDER, S2VDV, E8)
    val vaadd_e16  = DvSvlS2vS1(AADD , CADDER, S2VDV, E16)
    val vaadd_e32  = DvSvlS2vS1(AADD , CADDER, S2VDV, E32)
    val vaadd_e64  = DvSvlS2vS1(AADD , CADDER, S2VDV, E64)
    val vasubu_e8  = DvSvlS2vS1(ASUBU, CADDER, S2VDV, E8)
    val vasubu_e16 = DvSvlS2vS1(ASUBU, CADDER, S2VDV, E16)
    val vasubu_e32 = DvSvlS2vS1(ASUBU, CADDER, S2VDV, E32)
    val vasubu_e64 = DvSvlS2vS1(ASUBU, CADDER, S2VDV, E64)
    val vasub_e8   = DvSvlS2vS1(ASUB , CADDER, S2VDV, E8)
    val vasub_e16  = DvSvlS2vS1(ASUB , CADDER, S2VDV, E16)
    val vasub_e32  = DvSvlS2vS1(ASUB , CADDER, S2VDV, E32)
    val vasub_e64  = DvSvlS2vS1(ASUB , CADDER, S2VDV, E64)
    val vsaddu_e8  = DvSvlS2vS1(SADDU, CADDER, S2VDV, E8 ) + VxsatWen
    val vsaddu_e16 = DvSvlS2vS1(SADDU, CADDER, S2VDV, E16) + VxsatWen
    val vsaddu_e32 = DvSvlS2vS1(SADDU, CADDER, S2VDV, E32) + VxsatWen
    val vsaddu_e64 = DvSvlS2vS1(SADDU, CADDER, S2VDV, E64) + VxsatWen
    val vsadd_e8   = DvSvlS2vS1(SADD , CADDER, S2VDV, E8 ) + VxsatWen
    val vsadd_e16  = DvSvlS2vS1(SADD , CADDER, S2VDV, E16) + VxsatWen
    val vsadd_e32  = DvSvlS2vS1(SADD , CADDER, S2VDV, E32) + VxsatWen
    val vsadd_e64  = DvSvlS2vS1(SADD , CADDER, S2VDV, E64) + VxsatWen
    val vssubu_e8  = DvSvlS2vS1(SSUBU, CADDER, S2VDV, E8)
    val vssubu_e16 = DvSvlS2vS1(SSUBU, CADDER, S2VDV, E16)
    val vssubu_e32 = DvSvlS2vS1(SSUBU, CADDER, S2VDV, E32)
    val vssubu_e64 = DvSvlS2vS1(SSUBU, CADDER, S2VDV, E64)
    val vssub_e8   = DvSvlS2vS1(SSUB , CADDER, S2VDV, E8)
    val vssub_e16  = DvSvlS2vS1(SSUB , CADDER, S2VDV, E16)
    val vssub_e32  = DvSvlS2vS1(SSUB , CADDER, S2VDV, E32)
    val vssub_e64  = DvSvlS2vS1(SSUB , CADDER, S2VDV, E64)

    val vsll_e8   = DvSvlS2vS1(SLL, SHIFT, S2VDV, E8)
    val vsll_e16  = DvSvlS2vS1(SLL, SHIFT, S2VDV, E16)
    val vsll_e32  = DvSvlS2vS1(SLL, SHIFT, S2VDV, E32)
    val vsll_e64  = DvSvlS2vS1(SLL, SHIFT, S2VDV, E64)
    val vsrl_e8   = DvSvlS2vS1(SRL, SHIFT, S2VDV, E8)
    val vsrl_e16  = DvSvlS2vS1(SRL, SHIFT, S2VDV, E16)
    val vsrl_e32  = DvSvlS2vS1(SRL, SHIFT, S2VDV, E32)
    val vsrl_e64  = DvSvlS2vS1(SRL, SHIFT, S2VDV, E64)
    val vsra_e8   = DvSvlS2vS1(SRA, SHIFT, S2VDV, E8)
    val vsra_e16  = DvSvlS2vS1(SRA, SHIFT, S2VDV, E16)
    val vsra_e32  = DvSvlS2vS1(SRA, SHIFT, S2VDV, E32)
    val vsra_e64  = DvSvlS2vS1(SRA, SHIFT, S2VDV, E64)
    val vror_e8   = DvSvlS2vS1(ROR, SHIFT, S2VDV, E8)
    val vror_e16  = DvSvlS2vS1(ROR, SHIFT, S2VDV, E16)
    val vror_e32  = DvSvlS2vS1(ROR, SHIFT, S2VDV, E32)
    val vror_e64  = DvSvlS2vS1(ROR, SHIFT, S2VDV, E64)
    val vrol_e8   = DvSvlS2vS1(ROL, SHIFT, S2VDV, E8)
    val vrol_e16  = DvSvlS2vS1(ROL, SHIFT, S2VDV, E16)
    val vrol_e32  = DvSvlS2vS1(ROL, SHIFT, S2VDV, E32)
    val vrol_e64  = DvSvlS2vS1(ROL, SHIFT, S2VDV, E64)

    val vssrl_e8    = DvSvlS2vS1(SRL  , CSHIFT, S2VDV, E8 ) + VxrmRen
    val vssrl_e16   = DvSvlS2vS1(SRL  , CSHIFT, S2VDV, E16) + VxrmRen
    val vssrl_e32   = DvSvlS2vS1(SRL  , CSHIFT, S2VDV, E32) + VxrmRen
    val vssrl_e64   = DvSvlS2vS1(SRL  , CSHIFT, S2VDV, E64) + VxrmRen
    val vssra_e8    = DvSvlS2vS1(SRA  , CSHIFT, S2VDV, E8 ) + VxrmRen
    val vssra_e16   = DvSvlS2vS1(SRA  , CSHIFT, S2VDV, E16) + VxrmRen
    val vssra_e32   = DvSvlS2vS1(SRA  , CSHIFT, S2VDV, E32) + VxrmRen
    val vssra_e64   = DvSvlS2vS1(SRA  , CSHIFT, S2VDV, E64) + VxrmRen
    val vwsll_e8    = DvSvlS2vS1(SLL  , CSHIFT, S2VDW, E8)
    val vwsll_e16   = DvSvlS2vS1(SLL  , CSHIFT, S2VDW, E16)
    val vwsll_e32   = DvSvlS2vS1(SLL  , CSHIFT, S2VDW, E32)
    val vnsrl_e8    = DvSvlS2vS1(SRL  , CSHIFT, S2WDV, E8)
    val vnsrl_e16   = DvSvlS2vS1(SRL  , CSHIFT, S2WDV, E16)
    val vnsrl_e32   = DvSvlS2vS1(SRL  , CSHIFT, S2WDV, E32)
    val vnsra_e8    = DvSvlS2vS1(SRA  , CSHIFT, S2WDV, E8)
    val vnsra_e16   = DvSvlS2vS1(SRA  , CSHIFT, S2WDV, E16)
    val vnsra_e32   = DvSvlS2vS1(SRA  , CSHIFT, S2WDV, E32)
    val vnclipu_e8  = DvSvlS2vS1(CLIPU, CSHIFT, S2WDV, E8)  + VxsatWen + VxrmRen
    val vnclipu_e16 = DvSvlS2vS1(CLIPU, CSHIFT, S2WDV, E16) + VxsatWen + VxrmRen
    val vnclipu_e32 = DvSvlS2vS1(CLIPU, CSHIFT, S2WDV, E32) + VxsatWen + VxrmRen
    val vnclip_e8   = DvSvlS2vS1(CLIP , CSHIFT, S2WDV, E8)  + VxsatWen + VxrmRen
    val vnclip_e16  = DvSvlS2vS1(CLIP , CSHIFT, S2WDV, E16) + VxsatWen + VxrmRen
    val vnclip_e32  = DvSvlS2vS1(CLIP , CSHIFT, S2WDV, E32) + VxsatWen + VxrmRen

    val vrev8_e8  = DvSvlS2v(REV8, BITOP, S2VDV, E8 )
    val vrev8_e16 = DvSvlS2v(REV8, BITOP, S2VDV, E16)
    val vrev8_e32 = DvSvlS2v(REV8, BITOP, S2VDV, E32)
    val vrev8_e64 = DvSvlS2v(REV8, BITOP, S2VDV, E64)

    Opcodes.updateMaxFixLat(this.getMaxLat)

    def getOp(implicit op: UInt): UInt = op.drop(8)
    def getOpClass(implicit op: UInt): UInt = op(7, 5)
    def getDataType(implicit op: UInt): UInt = op(4, 2)
    def getDataWidth(implicit op: UInt): UInt = op(1, 0)

    def isWidenVs2(implicit op: UInt): Bool = getDataType.isOneOf(S2VDW)
    def isWiden(implicit op: UInt): Bool = getDataType.isOneOf(S2VDW, S2WDW)
    def isNarrow(implicit op: UInt): Bool = getDataType.isOneOf(S2WDV)
    def isSourceE8(implicit op: UInt): Bool = getDataWidth === E8
    def isSourceE16(implicit op: UInt): Bool = getDataWidth === E16
    def isSourceE32(implicit op: UInt): Bool = getDataWidth === E32
    def isSourceE64(implicit op: UInt): Bool = getDataWidth === E64

    private def dataWidthAndTypeIsOneOf(seqs: (BitPat, Seq[BitPat])*)(implicit op: UInt) = {
      seqs
        .map { case (eWidth, typ) => getDataWidth === eWidth && getDataType.isOneOf(typ)}
        .reduce(_ || _)
    }

    def isDestE8(implicit op: UInt): Bool = dataWidthAndTypeIsOneOf(
      E8 -> Seq(S2VDV),
      E16 -> Seq(S2WDV),
    )

    def isDestE16(implicit op: UInt): Bool = dataWidthAndTypeIsOneOf(
      E8 -> Seq(S2VDW, S2WDW, S2F2DV),
      E16 -> Seq(S2VDV),
      E32 -> Seq(S2WDV),
    )

    def isDestE32(implicit op: UInt): Bool = dataWidthAndTypeIsOneOf(
      E8 -> Seq(S2F4DV),
      E16 -> Seq(S2VDW, S2WDW, S2F2DV),
      E32 -> Seq(S2VDV),
      E64 -> Seq(S2WDV),
    )

    def isDestE64(implicit op: UInt): Bool = dataWidthAndTypeIsOneOf(
      E8 -> Seq(S2F8DV),
      E16 -> Seq(S2F4DV),
      E32 -> Seq(S2VDW, S2WDW, S2F2DV),
      E64 -> Seq(S2VDV),
    )

    def isDestM(implicit op: UInt): Bool = getDataType.isOneOf(S2VDM)
    def isOpMask(implicit op: UInt): Bool = getDataType.isOneOf(S2VDM) && getOpClass === LOGIC

    def isSext2(implicit op: UInt): Bool = getOpClass === MOVE && getOp === SEXT && getDataType === S2F2DV
    def isSext4(implicit op: UInt): Bool = getOpClass === MOVE && getOp === SEXT && getDataType === S2F4DV
    def isSext8(implicit op: UInt): Bool = getOpClass === MOVE && getOp === SEXT && getDataType === S2F8DV
    def isZext2(implicit op: UInt): Bool = getOpClass === MOVE && getOp === ZEXT && getDataType === S2F2DV
    def isZext4(implicit op: UInt): Bool = getOpClass === MOVE && getOp === ZEXT && getDataType === S2F4DV
    def isZext8(implicit op: UInt): Bool = getOpClass === MOVE && getOp === ZEXT && getDataType === S2F8DV

    def isExt2(implicit op: UInt): Bool = getDataType === S2F2DV
    def isExt4(implicit op: UInt): Bool = getDataType === S2F4DV
    def isExt8(implicit op: UInt): Bool = getDataType === S2F8DV

    def isSigned(implicit op: UInt): Bool = getOp.apply(2)

    def isMisc(implicit op: UInt): Bool = isVcpop || isShift || isZvbbOthers ||  isExt2 || isExt4 || isExt8 || isBitLogic

    def isAddCarry(implicit op: UInt): Bool = getOpClass === ADDER && getOp.isOneOf(ADC, SBC)
    def isShift(implicit op: UInt): Bool = getOpClass.isOneOf(SHIFT, CSHIFT)

    def isSub(implicit op: UInt): Bool = getOp.apply(3)
    def isVmsbc(implicit op: UInt): Bool = getOp === SBC // Todo: check if it is only vmsbc
    def isBitLogic(implicit op: UInt): Bool = getOpClass === LOGIC
    def isLeftShiftLogic(implicit op: UInt): Bool = isShift && getOp.isOneOf(SLL, ROL)
    def isMaxMin(implicit op: UInt): Bool = getOpClass.isOneOf(ADDER) && getOp.isOneOf(MINU, MIN, MAXU, MAX)
    def isSat(implicit op: UInt): Bool = getOpClass.isOneOf(CADDER) && getOp.isOneOf(SADDU, SADD, SSUBU, SSUB)
    def isAvg(implicit op: UInt): Bool = getOpClass.isOneOf(CADDER) && getOp.isOneOf(AADDU, AADD, ASUBU, ASUB) && getDataType === S2VDV

    def isNClip(implicit op: UInt): Bool = getOpClass.isOneOf(CSHIFT) && getOp.isOneOf(CLIPU, CLIP)

    def isCmpEq(implicit op: UInt): Bool = getOp === EQ
    def isCmpLt(implicit op: UInt): Bool = getOp === LT || getOp === LTU
    def isCmpNe(implicit op: UInt): Bool = getOp === NE
    def isCmpLe(implicit op: UInt): Bool = getOp === LE || getOp === LEU
    def isCmpGt(implicit op: UInt): Bool = getOp === GT || getOp === GTU

    def isMax(implicit op: UInt): Bool = getOp.isOneOf(MAX, MAXU)

    def isScalVro(implicit op: UInt): Bool = getOpClass === CSHIFT || getOpClass === SHIFT && getOp.isOneOf(ROR, ROL)
    def isNotVro(implicit op: UInt): Bool = getOpClass === CSHIFT
    def isZvbbOthers(implicit op: UInt): Bool = isVrev8 // not supported yet
    def isVcpop(implicit op: UInt): Bool = false.B // not supported yet
    def isVbrev(implicit op: UInt): Bool = false.B // not supported yet
    def isVbrev8(implicit op: UInt): Bool = false.B // not supported yet
    def isVrev8(implicit op: UInt): Bool = getOpClass === BITOP && getOp === REV8 // not supported yet
    def isCountZero(implicit op: UInt): Bool = false.B // not supported yet
    def isCtz(implicit op: UInt): Bool = false.B // not supported yet
    def isPredicateAlwaysTrue(implicit op: UInt): Bool = getOpClass === ADDER && getOp.isOneOf(ADC, SBC)

    object Res {
      def isAnd(implicit op: UInt): Bool = getOpClass === LOGIC && getOp === AND
      def isOr(implicit op: UInt): Bool = getOpClass === LOGIC && getOp === OR
      def isXor(implicit op: UInt): Bool = getOpClass === LOGIC && getOp === XOR
      def isNand(implicit op: UInt): Bool = getOpClass === LOGIC && getOp === NAND
      def isAndn(implicit op: UInt): Bool = getOpClass === LOGIC && getOp === ANDN
      def isNor(implicit op: UInt): Bool = getOpClass === LOGIC && getOp === NOR
      def isOrn(implicit op: UInt): Bool = getOpClass === LOGIC && getOp === ORN
      def isXnor(implicit op: UInt): Bool = getOpClass === LOGIC && getOp === XNOR
      def isExt(implicit op: UInt): Bool = getOpClass === MOVE && getOp.isOneOf(ZEXT, SEXT) && getDataType.isOneOf(S2F2DV, S2F4DV, S2F8DV)
      def isScal(implicit op: UInt): Bool = getOpClass === CSHIFT && getOp.isOneOf(SRL, SRA) && getDataType === S2VDV
      def isVroShift(implicit op: UInt): Bool = getOpClass === SHIFT && getOp.isOneOf(ROR, ROL) && getDataType === S2VDV
      def isVwsll(implicit op: UInt): Bool = getOpClass === CSHIFT && getOp === SLL && getDataType === S2VDW
      def isLeftShift(implicit op: UInt): Bool = getOpClass === SHIFT && getOp === SLL && getDataType === S2VDV

      def isRightShift(implicit op: UInt): Bool = Mux1H(Seq(
        (getOpClass === SHIFT  && getDataType === S2VDV) -> getOp.isOneOf(SRL, SRA),
        (getOpClass === CSHIFT && getDataType === S2WDV) -> getOp.isOneOf(SRL, SRA),
        (getOpClass === CSHIFT && getDataType === S2WDV) -> getOp.isOneOf(CLIPU, CLIP),
      ))

      def isLeadZero(implicit op: UInt): Bool = false.B  // not supported yet
      def isBrev(implicit op: UInt): Bool = false.B  // not supported yet
      def isBrev8(implicit op: UInt): Bool = false.B  // not supported yet
      def isRev8(implicit op: UInt): Bool = getOpClass === BITOP && getDataType === S2VDV && getOp === REV8
      def isAvg(implicit op: UInt): Bool = getOpClass === CADDER && getDataType === S2VDV && getOp.isOneOf(AADDU, AADD, ASUBU, ASUB)
      def isAdder(implicit op: UInt): Bool = Mux1H(Seq(
        (getOpClass === ADDER  && getDataType === S2VDV)             -> getOp.isOneOf(ADD, SUB, ADC, SBC),
        (getOpClass === CADDER && getDataType.isOneOf(S2VDW, S2WDW)) -> getOp.isOneOf(WADDU, WADD, WSUBU, WSUB),
      ))
    }

    object LitUtil {
      def getOp(implicit op: BitPat): BitPat = op.drop(8)
      def getOpClass(implicit op: BitPat): BitPat = op(7, 5)
      def getDataType(implicit op: BitPat): BitPat = op(4, 2)
      def isMaxMin(implicit op: BitPat): Boolean = getOpClass.isOneOf(ADDER) && getOp.isOneOf(MINU, MIN, MAXU, MAX)
      def isSat(implicit op: BitPat): Boolean = getOpClass.isOneOf(CADDER) && getOp.isOneOf(SADDU, SADD, SSUBU, SSUB)
      def isNClip(implicit op: BitPat): Boolean = getOpClass.isOneOf(CSHIFT) && getOp.isOneOf(CLIPU, CLIP)

      def getLat(implicit op: BitPat): Int = {
        if (isSat || isMaxMin || isNClip) 1
        else 0
      }
    }

    override def getLat(opcode: Opcode): Int = {
      require(this.all.contains(opcode))
      LitUtil.getLat(opcode.encode)
    }
  }

  object VIAluOpcode extends VIAluOpcode

  trait VMAluOpcode extends Opcodes with DataType {
    private val DV = bb"00"
    private val DX = bb"01"
    private val DM = bb"11"

    private val CPOP_M = bb"000"
    private val CPOP_V = bb"001"
    private val FIRST = bb"010"
    private val ID    = bb"011"
    private val MSBF  = bb"100"
    private val MSIF  = bb"101"
    private val MSOF  = bb"110"
    private val IOTA  = bb"111"

    val vcpop_m = Value(CPOP_M, DX, EX) + GpWen
    val vfirst  = Value(FIRST , DX, EX) + GpWen
    val vmsbf   = Value(MSBF  , DM, EX)
    val vmsif   = Value(MSIF  , DM, EX)
    val vmsof   = Value(MSOF  , DM, EX)

    val vcpop_v_e8  = Value(CPOP_V, DV, E8 )
    val vcpop_v_e16 = Value(CPOP_V, DV, E16)
    val vcpop_v_e32 = Value(CPOP_V, DV, E32)
    val vcpop_v_e64 = Value(CPOP_V, DV, E64)
    val viota_e8    = Value(IOTA  , DV, E8 )
    val viota_e16   = Value(IOTA  , DV, E16)
    val viota_e32   = Value(IOTA  , DV, E32)
    val viota_e64   = Value(IOTA  , DV, E64)
    val vid_e8      = Value(ID    , DV, E8 )
    val vid_e16     = Value(ID    , DV, E16)
    val vid_e32     = Value(ID    , DV, E32)
    val vid_e64     = Value(ID    , DV, E64)
  }

  object VMAluOpcode extends VMAluOpcode

  trait VIMacOpcode extends Opcodes with DataType {
    private val S1U = bb"0"
    private val S1S = bb"1"
    private val S2U = bb"0"
    private val S2S = bb"1"

    private val DW = bb"1"
    private val DV = bb"0"

    private val OP2 = bb"0"
    private val OP3 = bb"1"

    private val MADD  = bb"000"
    private val NMSUB = bb"001"
    private val MACC  = bb"010"
    private val NMSAC = bb"011"

    private val MUL  = bb"000"
    private val MULH = bb"001"
    private val SMUL = bb"010"
    private val WADD4 = bb"011"

    val vmulhu_e8   = DvSvlS2vS1(S2U, S1U, MULH , OP2, DV, E8 )
    val vmulhu_e16  = DvSvlS2vS1(S2U, S1U, MULH , OP2, DV, E16)
    val vmulhu_e32  = DvSvlS2vS1(S2U, S1U, MULH , OP2, DV, E32)
    val vmulhu_e64  = DvSvlS2vS1(S2U, S1U, MULH , OP2, DV, E64)
    val vmul_e8     = DvSvlS2vS1(S2S, S1S, MUL  , OP2, DV, E8 )
    val vmul_e16    = DvSvlS2vS1(S2S, S1S, MUL  , OP2, DV, E16)
    val vmul_e32    = DvSvlS2vS1(S2S, S1S, MUL  , OP2, DV, E32)
    val vmul_e64    = DvSvlS2vS1(S2S, S1S, MUL  , OP2, DV, E64)
    val vmulhsu_e8  = DvSvlS2vS1(S2S, S1U, MULH , OP2, DV, E8 )
    val vmulhsu_e16 = DvSvlS2vS1(S2S, S1U, MULH , OP2, DV, E16)
    val vmulhsu_e32 = DvSvlS2vS1(S2S, S1U, MULH , OP2, DV, E32)
    val vmulhsu_e64 = DvSvlS2vS1(S2S, S1U, MULH , OP2, DV, E64)
    val vmulh_e8    = DvSvlS2vS1(S2S, S1S, MULH , OP2, DV, E8 )
    val vmulh_e16   = DvSvlS2vS1(S2S, S1S, MULH , OP2, DV, E16)
    val vmulh_e32   = DvSvlS2vS1(S2S, S1S, MULH , OP2, DV, E32)
    val vmulh_e64   = DvSvlS2vS1(S2S, S1S, MULH , OP2, DV, E64)

    val vmadd_e8    = DvSvlS2vS1S3v(S2S, S1S, MADD , OP3, DV, E8 )
    val vmadd_e16   = DvSvlS2vS1S3v(S2S, S1S, MADD , OP3, DV, E16)
    val vmadd_e32   = DvSvlS2vS1S3v(S2S, S1S, MADD , OP3, DV, E32)
    val vmadd_e64   = DvSvlS2vS1S3v(S2S, S1S, MADD , OP3, DV, E64)
    val vnmsub_e8   = DvSvlS2vS1S3v(S2S, S1S, NMSUB, OP3, DV, E8 )
    val vnmsub_e16  = DvSvlS2vS1S3v(S2S, S1S, NMSUB, OP3, DV, E16)
    val vnmsub_e32  = DvSvlS2vS1S3v(S2S, S1S, NMSUB, OP3, DV, E32)
    val vnmsub_e64  = DvSvlS2vS1S3v(S2S, S1S, NMSUB, OP3, DV, E64)
    val vmacc_e8    = DvSvlS2vS1S3v(S2S, S1S, MACC , OP3, DV, E8 )
    val vmacc_e16   = DvSvlS2vS1S3v(S2S, S1S, MACC , OP3, DV, E16)
    val vmacc_e32   = DvSvlS2vS1S3v(S2S, S1S, MACC , OP3, DV, E32)
    val vmacc_e64   = DvSvlS2vS1S3v(S2S, S1S, MACC , OP3, DV, E64)
    val vnmsac_e8   = DvSvlS2vS1S3v(S2S, S1S, NMSAC, OP3, DV, E8 )
    val vnmsac_e16  = DvSvlS2vS1S3v(S2S, S1S, NMSAC, OP3, DV, E16)
    val vnmsac_e32  = DvSvlS2vS1S3v(S2S, S1S, NMSAC, OP3, DV, E32)
    val vnmsac_e64  = DvSvlS2vS1S3v(S2S, S1S, NMSAC, OP3, DV, E64)

    val vwmulu_e8   = DvSvlS2vS1(S2U, S1U, MUL, OP2, DW, E8 )
    val vwmulu_e16  = DvSvlS2vS1(S2U, S1U, MUL, OP2, DW, E16)
    val vwmulu_e32  = DvSvlS2vS1(S2U, S1U, MUL, OP2, DW, E32)
    val vwmulsu_e8  = DvSvlS2vS1(S2S, S1U, MUL, OP2, DW, E8 )
    val vwmulsu_e16 = DvSvlS2vS1(S2S, S1U, MUL, OP2, DW, E16)
    val vwmulsu_e32 = DvSvlS2vS1(S2S, S1U, MUL, OP2, DW, E32)
    val vwmul_e8    = DvSvlS2vS1(S2S, S1S, MUL, OP2, DW, E8 )
    val vwmul_e16   = DvSvlS2vS1(S2S, S1S, MUL, OP2, DW, E16)
    val vwmul_e32   = DvSvlS2vS1(S2S, S1S, MUL, OP2, DW, E32)

    val vwmaccu_e8   = DvSvlS2vS1(S2U, S1U, MACC, OP3, DW, E8 )
    val vwmaccu_e16  = DvSvlS2vS1(S2U, S1U, MACC, OP3, DW, E16)
    val vwmaccu_e32  = DvSvlS2vS1(S2U, S1U, MACC, OP3, DW, E32)
    val vwmacc_e8    = DvSvlS2vS1(S2S, S1S, MACC, OP3, DW, E8 )
    val vwmacc_e16   = DvSvlS2vS1(S2S, S1S, MACC, OP3, DW, E16)
    val vwmacc_e32   = DvSvlS2vS1(S2S, S1S, MACC, OP3, DW, E32)
    val vwmaccus_e8  = DvSvlS2vS1(S2S, S1U, MACC, OP3, DW, E8 )
    val vwmaccus_e16 = DvSvlS2vS1(S2S, S1U, MACC, OP3, DW, E16)
    val vwmaccus_e32 = DvSvlS2vS1(S2S, S1U, MACC, OP3, DW, E32)
    val vwmaccsu_e8  = DvSvlS2vS1(S2U, S1S, MACC, OP3, DW, E8 )
    val vwmaccsu_e16 = DvSvlS2vS1(S2U, S1S, MACC, OP3, DW, E16)
    val vwmaccsu_e32 = DvSvlS2vS1(S2U, S1S, MACC, OP3, DW, E32)

    val vsmul_e8  = DvSvlS2vS1(S2S, S1S, SMUL, OP2, DV, E8 ) + VxsatWen + VxrmRen
    val vsmul_e16 = DvSvlS2vS1(S2S, S1S, SMUL, OP2, DV, E16) + VxsatWen + VxrmRen
    val vsmul_e32 = DvSvlS2vS1(S2S, S1S, SMUL, OP2, DV, E32) + VxsatWen + VxrmRen
    val vsmul_e64 = DvSvlS2vS1(S2S, S1S, SMUL, OP2, DV, E64) + VxsatWen + VxrmRen

    def getOp(implicit op: UInt): UInt = op(6, 4)
    def getOpMode(implicit op: UInt): UInt = op(3)
    def getDestMode(implicit op: UInt): UInt = op(2)
    def getSew(implicit op: UInt): UInt = op(1, 0)

    def vs2Sign(implicit op: UInt): UInt = op(8)
    def vs1Sign(implicit op: UInt): UInt = op(7)
    def vdSign(implicit op: UInt): UInt = vs2Sign | vs1Sign
    def isWiden(implicit op: UInt): Bool = getDestMode === DW

    def isMUL(implicit op: UInt): Bool = getOp === MUL && getOpMode === OP2
    def isMULH(implicit op: UInt): Bool = getOp === MULH && getOpMode === OP2
    def isVmulh(implicit op: UInt): Bool = isMULH && vs2Sign === S2S && vs1Sign === S1S && getDestMode === DV
    def isVmacc(implicit op: UInt): Bool = getOp === MACC && getOpMode === OP3
    def isVnmsac(implicit op: UInt): Bool = getOp === NMSAC && getOpMode === OP3
    def isVmadd(implicit op: UInt): Bool = getOp === MADD && getOpMode === OP3
    def isVnmsub(implicit op: UInt): Bool = getOp === NMSUB && getOpMode === OP3
    def isVsmul(implicit op: UInt): Bool = getOp === SMUL && getOpMode === OP2

    def ishighHalf(implicit op: UInt): Bool = isMULH
    def isVmaccType(implicit op: UInt): Bool = isVmacc || isVnmsac || isVmadd || isVnmsub
    def isSub(implicit op: UInt): Bool = isVnmsub || isVnmsac
    def overWriteMultiplicand(implicit op: UInt): Bool = isVmadd || isVnmsub
    def isFixP(implicit op: UInt): Bool = isVsmul

    def isSourceE8(implicit op: UInt): Bool = getSew === E8
    def isSourceE16(implicit op: UInt): Bool = getSew === E16
    def isSourceE32(implicit op: UInt): Bool = getSew === E32
    def isSourceE64(implicit op: UInt): Bool = getSew === E64
    override def getLat(opcode: Opcode): Int = 2
  }

  object VIMacOpcode extends VIMacOpcode

  trait VIDivOpcode extends Opcodes with DataType {
    private val DIVU = bb"00"
    private val DIV  = bb"01"
    private val REMU = bb"10"
    private val REM  = bb"11"

    val vdivu_e8  = DvSvlS2vS1(DIVU, E8 )
    val vdivu_e16 = DvSvlS2vS1(DIVU, E16)
    val vdivu_e32 = DvSvlS2vS1(DIVU, E32)
    val vdivu_e64 = DvSvlS2vS1(DIVU, E64)
    val vdiv_e8   = DvSvlS2vS1(DIV , E8 )
    val vdiv_e16  = DvSvlS2vS1(DIV , E16)
    val vdiv_e32  = DvSvlS2vS1(DIV , E32)
    val vdiv_e64  = DvSvlS2vS1(DIV , E64)
    val vremu_e8  = DvSvlS2vS1(REMU, E8 )
    val vremu_e16 = DvSvlS2vS1(REMU, E16)
    val vremu_e32 = DvSvlS2vS1(REMU, E32)
    val vremu_e64 = DvSvlS2vS1(REMU, E64)
    val vrem_e8   = DvSvlS2vS1(REM , E8 )
    val vrem_e16  = DvSvlS2vS1(REM , E16)
    val vrem_e32  = DvSvlS2vS1(REM , E32)
    val vrem_e64  = DvSvlS2vS1(REM , E64)

    def getOp(implicit op: UInt): UInt = op(3, 2)
    def getSew(implicit op: UInt): UInt = op(1, 0)
    def getDataWidth(implicit op: UInt): UInt = op(1, 0)
    def isSigned(implicit op: UInt): Bool = getOp.isOneOf(DIV, REM)
    def isDiv(implicit op: UInt): Bool = getOp.isOneOf(DIVU, DIV)

    def isSourceE8(implicit op: UInt): Bool  = getSew === E8
    def isSourceE16(implicit op: UInt): Bool = getSew === E16
    def isSourceE32(implicit op: UInt): Bool = getSew === E32
    def isSourceE64(implicit op: UInt): Bool = getSew === E64
  }

  object VIDivOpcode extends VIDivOpcode

  trait VIRedOpcode extends Opcodes with DataType {
    private val DV = bb"0"
    private val DW = bb"1"

    private val SUM  = bb"0000"
    private val AND  = bb"0001"
    private val OR   = bb"0010"
    private val XOR  = bb"0011"
    private val MINU = bb"0100"
    private val MIN  = bb"0101"
    private val MAXU = bb"0110"
    private val MAX  = bb"0111"

    private val SUMU = bb"1000"

    val vredsum_e8   = DvSvlS2vS1(SUM , DV, E8 )
    val vredsum_e16  = DvSvlS2vS1(SUM , DV, E16)
    val vredsum_e32  = DvSvlS2vS1(SUM , DV, E32)
    val vredsum_e64  = DvSvlS2vS1(SUM , DV, E64)
    val vredand_e8   = DvSvlS2vS1(AND , DV, E8 )
    val vredand_e16  = DvSvlS2vS1(AND , DV, E16)
    val vredand_e32  = DvSvlS2vS1(AND , DV, E32)
    val vredand_e64  = DvSvlS2vS1(AND , DV, E64)
    val vredor_e8    = DvSvlS2vS1(OR  , DV, E8 )
    val vredor_e16   = DvSvlS2vS1(OR  , DV, E16)
    val vredor_e32   = DvSvlS2vS1(OR  , DV, E32)
    val vredor_e64   = DvSvlS2vS1(OR  , DV, E64)
    val vredxor_e8   = DvSvlS2vS1(XOR , DV, E8 )
    val vredxor_e16  = DvSvlS2vS1(XOR , DV, E16)
    val vredxor_e32  = DvSvlS2vS1(XOR , DV, E32)
    val vredxor_e64  = DvSvlS2vS1(XOR , DV, E64)
    val vredminu_e8  = DvSvlS2vS1(MINU, DV, E8 )
    val vredminu_e16 = DvSvlS2vS1(MINU, DV, E16)
    val vredminu_e32 = DvSvlS2vS1(MINU, DV, E32)
    val vredminu_e64 = DvSvlS2vS1(MINU, DV, E64)
    val vredmin_e8   = DvSvlS2vS1(MIN , DV, E8 )
    val vredmin_e16  = DvSvlS2vS1(MIN , DV, E16)
    val vredmin_e32  = DvSvlS2vS1(MIN , DV, E32)
    val vredmin_e64  = DvSvlS2vS1(MIN , DV, E64)
    val vredmaxu_e8  = DvSvlS2vS1(MAXU, DV, E8 )
    val vredmaxu_e16 = DvSvlS2vS1(MAXU, DV, E16)
    val vredmaxu_e32 = DvSvlS2vS1(MAXU, DV, E32)
    val vredmaxu_e64 = DvSvlS2vS1(MAXU, DV, E64)
    val vredmax_e8   = DvSvlS2vS1(MAX , DV, E8 )
    val vredmax_e16  = DvSvlS2vS1(MAX , DV, E16)
    val vredmax_e32  = DvSvlS2vS1(MAX , DV, E32)
    val vredmax_e64  = DvSvlS2vS1(MAX , DV, E64)

    val vwredsum_e8   = DvSvlS2vS1(SUM, DW, E8 )
    val vwredsum_e16  = DvSvlS2vS1(SUM, DW, E16)
    val vwredsum_e32  = DvSvlS2vS1(SUM, DW, E32)
    val vwredsumu_e8  = DvSvlS2vS1(SUMU, DW, E8 )
    val vwredsumu_e16 = DvSvlS2vS1(SUMU, DW, E16)
    val vwredsumu_e32 = DvSvlS2vS1(SUMU, DW, E32)
  }

  object VIRedOpcode extends VIRedOpcode

  trait VIPermOpcode extends Opcodes with DataType {

    // funct6(4) ## funct6(1,0) ## funct3(2,1)
    private val RGATHER_V     = bb"00000"
    private val RGATHER_X     = bb"00010"
    private val RGATHER_I     = bb"00001"
    private val RGATHER_EI16  = bb"01000"
    private val SLIDEUP       = bb"01010"
    private val SLIDEDOWN     = bb"01110"
    private val COMPRESS      = bb"11101"
    private val SLIDE1UP      = bb"01001"
    private val SLIDE1DOWN    = bb"01101"

    val vrgather_v_e8     = DvSvlS2vS1(RGATHER_V   , E8 )
    val vrgather_v_e16    = DvSvlS2vS1(RGATHER_V   , E16)
    val vrgather_v_e32    = DvSvlS2vS1(RGATHER_V   , E32)
    val vrgather_v_e64    = DvSvlS2vS1(RGATHER_V   , E64)
    val vrgather_x_e8     = DvSvlS2vS1(RGATHER_X   , E8 )
    val vrgather_x_e16    = DvSvlS2vS1(RGATHER_X   , E16)
    val vrgather_x_e32    = DvSvlS2vS1(RGATHER_X   , E32)
    val vrgather_x_e64    = DvSvlS2vS1(RGATHER_X   , E64)
    val vrgather_i_e8     = DvSvlS2vS1(RGATHER_I   , E8 )
    val vrgather_i_e16    = DvSvlS2vS1(RGATHER_I   , E16)
    val vrgather_i_e32    = DvSvlS2vS1(RGATHER_I   , E32)
    val vrgather_i_e64    = DvSvlS2vS1(RGATHER_I   , E64)
    val vrgather_ei16_e8  = DvSvlS2vS1(RGATHER_EI16, E8 )
    val vrgather_ei16_e16 = DvSvlS2vS1(RGATHER_EI16, E16)
    val vrgather_ei16_e32 = DvSvlS2vS1(RGATHER_EI16, E32)
    val vrgather_ei16_e64 = DvSvlS2vS1(RGATHER_EI16, E64)
    val vslideup_e8       = DvSvlS2vS1(SLIDEUP     , E8 )
    val vslideup_e16      = DvSvlS2vS1(SLIDEUP     , E16)
    val vslideup_e32      = DvSvlS2vS1(SLIDEUP     , E32)
    val vslideup_e64      = DvSvlS2vS1(SLIDEUP     , E64)
    val vslidedown_e8     = DvSvlS2vS1(SLIDEDOWN   , E8 )
    val vslidedown_e16    = DvSvlS2vS1(SLIDEDOWN   , E16)
    val vslidedown_e32    = DvSvlS2vS1(SLIDEDOWN   , E32)
    val vslidedown_e64    = DvSvlS2vS1(SLIDEDOWN   , E64)
    val vcompress_e8      = DvSvlS2vS1(COMPRESS    , E8 )
    val vcompress_e16     = DvSvlS2vS1(COMPRESS    , E16)
    val vcompress_e32     = DvSvlS2vS1(COMPRESS    , E32)
    val vcompress_e64     = DvSvlS2vS1(COMPRESS    , E64)
    val vslide1up_e8      = DvSvlS2vS1(SLIDE1UP    , E8 )
    val vslide1up_e16     = DvSvlS2vS1(SLIDE1UP    , E16)
    val vslide1up_e32     = DvSvlS2vS1(SLIDE1UP    , E32)
    val vslide1up_e64     = DvSvlS2vS1(SLIDE1UP    , E64)
    val vslide1down_e8    = DvSvlS2vS1(SLIDE1DOWN  , E8 )
    val vslide1down_e16   = DvSvlS2vS1(SLIDE1DOWN  , E16)
    val vslide1down_e32   = DvSvlS2vS1(SLIDE1DOWN  , E32)
    val vslide1down_e64   = DvSvlS2vS1(SLIDE1DOWN  , E64)
  }

  object VIPermOpcode extends VIPermOpcode

  trait VMoveOpcode extends Opcodes with DataType {
    // uop of vmv.v.v and vmerge.vvm
    private val MERGE_VV = bb"000"
    // uop of vmv.v.x, vmv.v.i, vfmv.v.f, vmerge.vxm, vmerge.vim, vfmerge.vfm
    private val MERGE_VX = bb"001"
    // uop of vmv1r, vmv2r, vmv4r, vmv8r
    private val MV_NR = bb"010"
    // uop of vmv.s.x, vfmv.s.f
    private val MV_X2VS = bb"101"
    // uop of vmv.x.s, vfmv.f.s
    private val MV_VS2X = bb"110"

    private val TAIL = bb"111"

    val vmerge_vv_e8  = DvSvlS2vS1(MERGE_VV, E8 )
    val vmerge_vv_e16 = DvSvlS2vS1(MERGE_VV, E16)
    val vmerge_vv_e32 = DvSvlS2vS1(MERGE_VV, E32)
    val vmerge_vv_e64 = DvSvlS2vS1(MERGE_VV, E64)
    val vmerge_vx_e8  = DvSvlS2vS1(MERGE_VX, E8 )
    val vmerge_vx_e16 = DvSvlS2vS1(MERGE_VX, E16)
    val vmerge_vx_e32 = DvSvlS2vS1(MERGE_VX, E32)
    val vmerge_vx_e64 = DvSvlS2vS1(MERGE_VX, E64)
    val vmv_v2v_e8    = DvSvlS1(MERGE_VV, E8 )
    val vmv_v2v_e16   = DvSvlS1(MERGE_VV, E16)
    val vmv_v2v_e32   = DvSvlS1(MERGE_VV, E32)
    val vmv_v2v_e64   = DvSvlS1(MERGE_VV, E64)
    val vmv_x2v_e8    = DvSvlS1(MERGE_VX, E8 )
    val vmv_x2v_e16   = DvSvlS1(MERGE_VX, E16)
    val vmv_x2v_e32   = DvSvlS1(MERGE_VX, E32)
    val vmv_x2v_e64   = DvSvlS1(MERGE_VX, E64)

    val vmvnr         = DvS2v(MV_NR   , EX )
    val vmv_x2vs_e8   = DsS1a(MV_X2VS , E8 )
    val vmv_x2vs_e16  = DsS1a(MV_X2VS , E16)
    val vmv_x2vs_e32  = DsS1a(MV_X2VS , E32)
    val vmv_x2vs_e64  = DsS1a(MV_X2VS , E64)
    val vmv_vs2x_e8   = DaS2s(MV_VS2X , E8 )
    val vmv_vs2x_e16  = DaS2s(MV_VS2X , E16)
    val vmv_vs2x_e32  = DaS2s(MV_VS2X , E32)
    val vmv_vs2x_e64  = DaS2s(MV_VS2X , E64)
    val vtail         = Value(TAIL    , EX ) + VpWen

    protected def getSubOp(implicit op: UInt): UInt = op(4, 2)
    protected def getSubOp(op: BitPat): BitPat = op(4, 2)

    def getElemWidth(implicit op: UInt): UInt = op(1, 0)

    def isVS2X(implicit op: UInt): Bool = getSubOp.isOneOf(MV_VS2X)
    def isX2VS(implicit op: UInt): Bool = getSubOp.isOneOf(MV_X2VS)
    def isNR(implicit op: UInt): Bool = getSubOp.isOneOf(MV_NR)
    def isVmerge(implicit op: UInt): Bool = getSubOp.isOneOf(MERGE_VV, MERGE_VX)
    def isVmergeVX(implicit op: UInt): Bool = getSubOp.isOneOf(MERGE_VX)

    def isVfmerge(implicit op: UInt): Bool = op.isOneOf(
      vmerge_vv_e16.encode,
      vmerge_vv_e32.encode,
      vmerge_vv_e64.encode,
      vmerge_vx_e16.encode,
      vmerge_vx_e32.encode,
      vmerge_vx_e64.encode,
    )
    def isVfmv_f_s(implicit op: UInt): Bool = op.isOneOf(
      vmv_vs2x_e16.encode,
      vmv_vs2x_e32.encode,
      vmv_vs2x_e64.encode,
    )
    def isVfmv_s_f(implicit op: UInt): Bool = op.isOneOf(
      vmv_x2vs_e16.encode,
      vmv_x2vs_e32.encode,
      vmv_x2vs_e64.encode,
    )
    def isVfmv(implicit op: UInt): Bool = isVfmv_f_s || isVfmv_s_f
    def isVfScalarMove(implicit op: UInt): Bool = isVfmv
    def isVfScalarMoveFromVec(implicit op: UInt): Bool = isVfmv_f_s
    def isVfScalarMoveToVec(implicit op: UInt): Bool = isVfmv_s_f

    def needNoMask(implicit op: UInt) = getSubOp.isOneOf(MV_NR, MV_X2VS, MERGE_VV, MERGE_VX)
    def vlIsOne(implicit op: UInt) = getSubOp.isOneOf(MV_X2VS)
    def vlIsZeroUpdate(implicit op: UInt) = getSubOp.isOneOf(MV_VS2X, MV_NR)

    def isLegal(implicit op: UInt) = getSubOp.isOneOf(this.allBitPats.map(getSubOp))

    // The latency of all uops in move is 0
    override def getLat(opcode: Opcode): Int = 0
  }

  object VMoveOpcode extends VMoveOpcode

  trait VSha256msOpcode extends Opcodes {
    private val sha256ms = bb"1"

    val vsha256ms = DvSvlS2vS1S3v(sha256ms)

    def isMS(implicit op: UInt): Bool = op === sha256ms
  }

  object VSha256msOpcode extends VSha256msOpcode

  trait VSha256cOpcode extends Opcodes {
    private val sha256cl = bb"10"
    private val sha256ch = bb"11"

    val vsha256cl = DvSvlS2vS1S3v(sha256cl)
    val vsha256ch = DvSvlS2vS1S3v(sha256ch)

    def isCL(implicit op: UInt): Bool = op === sha256cl
    def isCH(implicit op: UInt): Bool = op === sha256ch
    def isLegal(implicit op: UInt): Bool = op.isOneOf(sha256cl, sha256ch)
  }

  object VSha256cOpcode extends VSha256cOpcode

  trait DataType {
    protected val F = bb"0"
    protected val V = bb"1"

    /**
     * The H data type encoding is b10 in instruction.
     * We convert its encoding as b01 which mean fp16
     */
    protected val FP16 = bb"01"

    /**
     * The S data type encoding is b00 in instruction.
     * We convert its encoding as b10 which mean fp32
     */
    protected val FP32 = bb"10"

    /**
     * The D data type encoding is b01 in instruction.
     * We convert its encoding as b11 which mean fp64
     */
    protected val FP64 = bb"11"

    // ATTENTION!!!
    // New floating point DataType such as FP128 and BF16 will cause breaking modification in this class.

    protected val I8  = bb"00"
    protected val I16 = bb"01"
    protected val I32 = bb"10"
    protected val I64 = bb"11"

    protected val E8  = bb"00"
    protected val E16 = bb"01"
    protected val E32 = bb"10"
    protected val E64 = bb"11"
    protected val EX  = bb"??"
  }
}
