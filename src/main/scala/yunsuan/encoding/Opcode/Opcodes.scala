package yunsuan.encoding.Opcode

import chisel3._
import chisel3.util.BitPat
import sourcecode.{Name => SourceName}
import yunsuan.util.ChiselExt.BinaryStringHelper
import yunsuan.util.ChiselExt.UIntToExt

import scala.beans.BeanProperty

abstract class Opcode(val factory: Opcodes) {
  val encode: BitPat
  @BeanProperty
  var name: String = _

  var traits: Set[OpcodeTrait] = Set[OpcodeTrait]()

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

  override def toString: String = {
    val bitString = s"${encode.rawString}".padTo(encode.getWidth, '0')
    f"${getName()}%20s (BitPat<${encode.getWidth}>(b$bitString)) @ ${factory} with traits(${traits.mkString(",")})"
  }
}

object Opcode {
  def apply() = UInt(Opcodes.getWidth.W)
}

abstract class Opcodes {
  class Type(val encode: BitPat) extends Opcode(this)

  private var width: Int = _

  private val records = collection.mutable.ArrayBuffer.empty[Type]

  def all: Seq[Type] = records.toSeq

  def getWidth: Int = width

  private def updateWidth(w: Int): Unit = {
    this.width = w max this.width
  }

  def apply(): UInt = UInt(Opcodes.this.getWidth.W)

  def Value(bp1: BitPat, bp2: BitPat*)(implicit name: SourceName): Type = {
    ValueImpl((bp1 +: bp2).reduce(_ ## _))
  }

  private def ValueImpl(bitpat: BitPat)(implicit name: SourceName): Type = {
    val res = new Type(bitpat)
    res.setName(name.value)
    records.append(res)
    Opcodes.this.updateWidth(bitpat.getWidth)
    Opcodes.updateWidth(bitpat.getWidth)

    res
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
    )

    for (opcodeCls <- opcodes) {
      for (opcode <- opcodeCls.all) {
        println(s"${opcode}")
      }
    }
  }

  def apply(): UInt = UInt(width.W)

  private var width: Int = 0

  def getWidth: Int = width

  def updateWidth(w: Int): Unit = Opcodes.width = w.max(Opcodes.width)

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

    /**
     * sub opcode of [[ADDER]]
     */
    private val ADD  = bb"000"
    private val SUB  = bb"001"
    private val ADC  = bb"010"
    private val SBC  = bb"011"
    private val MINU = bb"100"
    private val MIN  = bb"101"
    private val MAXU = bb"110"
    private val MAX  = bb"111"

    /**
     * sub opcode of [[CMP]]
     */
    private val EQ  = bb"000"
    private val NE  = bb"001"
    private val LTU = bb"010"
    private val LT  = bb"011"
    private val LEU = bb"100"
    private val LE  = bb"101"
    private val GTU = bb"110"
    private val GT  = bb"111"

    /**
     * sub opcode of [[LOGIC]]
     */
    private val ANDN = bb"000"
    private val AND  = bb"001"
    private val OR   = bb"010"
    private val XOR  = bb"011"
    private val ORN  = bb"100"
    private val NAND = bb"101"
    private val NOR  = bb"110"
    private val XNOR = bb"111"

    /**
     * sub opcode of [[MOVE]]
     */
    private val MERGE  = bb"000"
    private val VMV_NR = bb"001"
    private val VMV_VS = bb"010"
    private val ZEXT   = bb"100"
    private val SEXT   = bb"101"

    /**
     * sub opcode of [[CADDER]]
     * [[AADDU]] and [[WADDU]] will be distinguashed by [[S2VDV]] or [[S2VDW]]
     */
    private val AADDU = bb"000"
    private val AADD  = bb"001"
    private val ASUBU = bb"010"
    private val ASUB  = bb"011"
    private val SADDU = bb"100"
    private val SADD  = bb"101"
    private val SSUBU = bb"110"
    private val SSUB  = bb"111"
    private val WADDU = bb"000"
    private val WADD  = bb"001"
    private val WSUBU = bb"010"
    private val WSUB  = bb"011"

    /**
     * WADD4U:
     *   vd(i) = waddu( vs1(2i) + vs2(2i) ) + waddu( vs1(2i + 1) + vs2(2i + 1) )
     */
    private val WADD4U = bb"110"

    /**
     * WADD4:
     *   vd(i) = wadd( vs1(2i) + vs2(2i) ) + wadd( vs1(2i + 1) + vs2(2i + 1) )
     */
    private val WADD4  = bb"111"

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

    /**
     * sub opcode of [[SHIFT]] and [[CSHIFT]]
     */
    private val SLL   = bb"000"
    private val SRL   = bb"001"
    private val SRA   = bb"010"
    private val ROR   = bb"100"
    private val ROL   = bb"101"
    private val CLIPU = bb"110"
    private val CLIP  = bb"111"

    val vadd_e8   = Value(ADD, ADDER, S2VDV, E8)
    val vadd_e16  = Value(ADD, ADDER, S2VDV, E16)
    val vadd_e32  = Value(ADD, ADDER, S2VDV, E32)
    val vadd_e64  = Value(ADD, ADDER, S2VDV, E64)
    val vsub_e8   = Value(SUB, ADDER, S2VDV, E8)
    val vsub_e16  = Value(SUB, ADDER, S2VDV, E16)
    val vsub_e32  = Value(SUB, ADDER, S2VDV, E32)
    val vsub_e64  = Value(SUB, ADDER, S2VDV, E64)
    val vadc_e8   = Value(ADC, ADDER, S2VDV, E8)
    val vadc_e16  = Value(ADC, ADDER, S2VDV, E16)
    val vadc_e32  = Value(ADC, ADDER, S2VDV, E32)
    val vadc_e64  = Value(ADC, ADDER, S2VDV, E64)
    val vmadc_e8  = Value(ADC, ADDER, S2VDM, E8)
    val vmadc_e16 = Value(ADC, ADDER, S2VDM, E16)
    val vmadc_e32 = Value(ADC, ADDER, S2VDM, E32)
    val vmadc_e64 = Value(ADC, ADDER, S2VDM, E64)
    val vsbc_e8   = Value(SBC, ADDER, S2VDV, E8)
    val vsbc_e16  = Value(SBC, ADDER, S2VDV, E16)
    val vsbc_e32  = Value(SBC, ADDER, S2VDV, E32)
    val vsbc_e64  = Value(SBC, ADDER, S2VDV, E64)
    val vmsbc_e8  = Value(SBC, ADDER, S2VDM, E8)
    val vmsbc_e16 = Value(SBC, ADDER, S2VDM, E16)
    val vmsbc_e32 = Value(SBC, ADDER, S2VDM, E32)
    val vmsbc_e64 = Value(SBC, ADDER, S2VDM, E64)

    val vminu_e8  = Value(MINU, ADDER, S2VDV, E8)
    val vminu_e16 = Value(MINU, ADDER, S2VDV, E16)
    val vminu_e32 = Value(MINU, ADDER, S2VDV, E32)
    val vminu_e64 = Value(MINU, ADDER, S2VDV, E64)
    val vmin_e8   = Value(MIN,  ADDER, S2VDV, E8)
    val vmin_e16  = Value(MIN,  ADDER, S2VDV, E16)
    val vmin_e32  = Value(MIN,  ADDER, S2VDV, E32)
    val vmin_e64  = Value(MIN,  ADDER, S2VDV, E64)
    val vmaxu_e8  = Value(MAXU, ADDER, S2VDV, E8)
    val vmaxu_e16 = Value(MAXU, ADDER, S2VDV, E16)
    val vmaxu_e32 = Value(MAXU, ADDER, S2VDV, E32)
    val vmaxu_e64 = Value(MAXU, ADDER, S2VDV, E64)
    val vmax_e8   = Value(MAX,  ADDER, S2VDV, E8)
    val vmax_e16  = Value(MAX,  ADDER, S2VDV, E16)
    val vmax_e32  = Value(MAX,  ADDER, S2VDV, E32)
    val vmax_e64  = Value(MAX,  ADDER, S2VDV, E64)

    val vmseq_e8   = Value(EQ , CMP, S2VDM, E8)
    val vmseq_e16  = Value(EQ , CMP, S2VDM, E16)
    val vmseq_e32  = Value(EQ , CMP, S2VDM, E32)
    val vmseq_e64  = Value(EQ , CMP, S2VDM, E64)
    val vmsne_e8   = Value(NE , CMP, S2VDM, E8)
    val vmsne_e16  = Value(NE , CMP, S2VDM, E16)
    val vmsne_e32  = Value(NE , CMP, S2VDM, E32)
    val vmsne_e64  = Value(NE , CMP, S2VDM, E64)
    val vmsltu_e8  = Value(LTU, CMP, S2VDM, E8)
    val vmsltu_e16 = Value(LTU, CMP, S2VDM, E16)
    val vmsltu_e32 = Value(LTU, CMP, S2VDM, E32)
    val vmsltu_e64 = Value(LTU, CMP, S2VDM, E64)
    val vmslt_e8   = Value(LT , CMP, S2VDM, E8)
    val vmslt_e16  = Value(LT , CMP, S2VDM, E16)
    val vmslt_e32  = Value(LT , CMP, S2VDM, E32)
    val vmslt_e64  = Value(LT , CMP, S2VDM, E64)
    val vmsleu_e8  = Value(LEU, CMP, S2VDM, E8)
    val vmsleu_e16 = Value(LEU, CMP, S2VDM, E16)
    val vmsleu_e32 = Value(LEU, CMP, S2VDM, E32)
    val vmsleu_e64 = Value(LEU, CMP, S2VDM, E64)
    val vmsle_e8   = Value(LE , CMP, S2VDM, E8)
    val vmsle_e16  = Value(LE , CMP, S2VDM, E16)
    val vmsle_e32  = Value(LE , CMP, S2VDM, E32)
    val vmsle_e64  = Value(LE , CMP, S2VDM, E64)
    val vmsgtu_e8  = Value(GTU, CMP, S2VDM, E8)
    val vmsgtu_e16 = Value(GTU, CMP, S2VDM, E16)
    val vmsgtu_e32 = Value(GTU, CMP, S2VDM, E32)
    val vmsgtu_e64 = Value(GTU, CMP, S2VDM, E64)
    val vmsgt_e8   = Value(GT , CMP, S2VDM, E8)
    val vmsgt_e16  = Value(GT , CMP, S2VDM, E16)
    val vmsgt_e32  = Value(GT , CMP, S2VDM, E32)
    val vmsgt_e64  = Value(GT , CMP, S2VDM, E64)

    val vandn_e8  = Value(ANDN, LOGIC, S2VDV, E8)
    val vandn_e16 = Value(ANDN, LOGIC, S2VDV, E16)
    val vandn_e32 = Value(ANDN, LOGIC, S2VDV, E32)
    val vandn_e64 = Value(ANDN, LOGIC, S2VDV, E64)
    val vand_e8   = Value(AND , LOGIC, S2VDV, E8)
    val vand_e16  = Value(AND , LOGIC, S2VDV, E16)
    val vand_e32  = Value(AND , LOGIC, S2VDV, E32)
    val vand_e64  = Value(AND , LOGIC, S2VDV, E64)
    val vor_e8    = Value(OR  , LOGIC, S2VDV, E8)
    val vor_e16   = Value(OR  , LOGIC, S2VDV, E16)
    val vor_e32   = Value(OR  , LOGIC, S2VDV, E32)
    val vor_e64   = Value(OR  , LOGIC, S2VDV, E64)
    val vxor_e8   = Value(XOR , LOGIC, S2VDV, E8)
    val vxor_e16  = Value(XOR , LOGIC, S2VDV, E16)
    val vxor_e32  = Value(XOR , LOGIC, S2VDV, E32)
    val vxor_e64  = Value(XOR , LOGIC, S2VDV, E64)

    val vmandn = Value(ANDN, LOGIC, S2VDM, EX)
    val vmand  = Value(AND , LOGIC, S2VDM, EX)
    val vmor   = Value(OR  , LOGIC, S2VDM, EX)
    val vmxor  = Value(XOR , LOGIC, S2VDM, EX)
    val vmorn  = Value(ORN , LOGIC, S2VDM, EX)
    val vmnand = Value(NAND, LOGIC, S2VDM, EX)
    val vmnor  = Value(NOR , LOGIC, S2VDM, EX)
    val vmxnor = Value(XNOR, LOGIC, S2VDM, EX)

    val vmerge_e8  = Value(MERGE, MOVE, S2VDV, E8 )
    val vmerge_e16 = Value(MERGE, MOVE, S2VDV, E16)
    val vmerge_e32 = Value(MERGE, MOVE, S2VDV, E32)
    val vmerge_e64 = Value(MERGE, MOVE, S2VDV, E64)
    val vmvnr      = Value(VMV_NR, MOVE, S2VDV, EX )
    val vmv_x2vs_e8  = Value(VMV_VS, MOVE, S2VDV, E8 )
    val vmv_x2vs_e16 = Value(VMV_VS, MOVE, S2VDV, E16)
    val vmv_x2vs_e32 = Value(VMV_VS, MOVE, S2VDV, E32)
    val vmv_x2vs_e64 = Value(VMV_VS, MOVE, S2VDV, E64)
    val vzext2_e8  = Value(ZEXT, MOVE, S2F2DV, E8 ) // vzext.vf2 when sew=e16
    val vzext2_e16 = Value(ZEXT, MOVE, S2F2DV, E16)
    val vzext2_e32 = Value(ZEXT, MOVE, S2F2DV, E32)
    val vzext4_e8  = Value(ZEXT, MOVE, S2F4DV, E8 )
    val vzext4_e16 = Value(ZEXT, MOVE, S2F4DV, E16)
    val vzext8_e8  = Value(ZEXT, MOVE, S2F8DV, E8 )
    val vsext2_e8  = Value(SEXT, MOVE, S2F2DV, E8 ) // vsext.vf2 when sew=e16
    val vsext2_e16 = Value(SEXT, MOVE, S2F2DV, E16)
    val vsext2_e32 = Value(SEXT, MOVE, S2F2DV, E32)
    val vsext4_e8  = Value(SEXT, MOVE, S2F4DV, E8 )
    val vsext4_e16 = Value(SEXT, MOVE, S2F4DV, E16)
    val vsext8_e8  = Value(SEXT, MOVE, S2F8DV, E8 )

    val vwaddu_e8    = Value(WADDU, CADDER, S2VDW, E8)
    val vwaddu_e16   = Value(WADDU, CADDER, S2VDW, E16)
    val vwaddu_e32   = Value(WADDU, CADDER, S2VDW, E32)
    val vwadd_e8     = Value(WADD , CADDER, S2VDW, E8)
    val vwadd_e16    = Value(WADD , CADDER, S2VDW, E16)
    val vwadd_e32    = Value(WADD , CADDER, S2VDW, E32)
    val vwsubu_e8    = Value(WSUBU, CADDER, S2VDW, E8)
    val vwsubu_e16   = Value(WSUBU, CADDER, S2VDW, E16)
    val vwsubu_e32   = Value(WSUBU, CADDER, S2VDW, E32)
    val vwsub_e8     = Value(WSUB , CADDER, S2VDW, E8)
    val vwsub_e16    = Value(WSUB , CADDER, S2VDW, E16)
    val vwsub_e32    = Value(WSUB , CADDER, S2VDW, E32)
    val vwaddu_w_e8  = Value(WADDU, CADDER, S2WDW, E8)
    val vwaddu_w_e16 = Value(WADDU, CADDER, S2WDW, E16)
    val vwaddu_w_e32 = Value(WADDU, CADDER, S2WDW, E32)
    val vwadd_w_e8   = Value(WADD , CADDER, S2WDW, E8)
    val vwadd_w_e16  = Value(WADD , CADDER, S2WDW, E16)
    val vwadd_w_e32  = Value(WADD , CADDER, S2WDW, E32)
    val vwsubu_w_e8  = Value(WSUBU, CADDER, S2WDW, E8)
    val vwsubu_w_e16 = Value(WSUBU, CADDER, S2WDW, E16)
    val vwsubu_w_e32 = Value(WSUBU, CADDER, S2WDW, E32)
    val vwsub_w_e8   = Value(WSUB , CADDER, S2WDW, E8)
    val vwsub_w_e16  = Value(WSUB , CADDER, S2WDW, E16)
    val vwsub_w_e32  = Value(WSUB , CADDER, S2WDW, E32)

    // internal uop to support reduction sum
    val vwadd4u_e8  = Value(WADD4U, CADDER, S2VDW, E8 )
    val vwadd4u_e16 = Value(WADD4U, CADDER, S2VDW, E16)
    val vwadd4u_e32 = Value(WADD4U, CADDER, S2VDW, E32)
    val vwadd4_e8   = Value(WADD4 , CADDER, S2VDW, E8 )
    val vwadd4_e16  = Value(WADD4 , CADDER, S2VDW, E16)
    val vwadd4_e32  = Value(WADD4 , CADDER, S2VDW, E32)

    val vaaddu_e8  = Value(AADDU, CADDER, S2VDV, E8)
    val vaaddu_e16 = Value(AADDU, CADDER, S2VDV, E16)
    val vaaddu_e32 = Value(AADDU, CADDER, S2VDV, E32)
    val vaaddu_e64 = Value(AADDU, CADDER, S2VDV, E64)
    val vaadd_e8   = Value(AADD , CADDER, S2VDV, E8)
    val vaadd_e16  = Value(AADD , CADDER, S2VDV, E16)
    val vaadd_e32  = Value(AADD , CADDER, S2VDV, E32)
    val vaadd_e64  = Value(AADD , CADDER, S2VDV, E64)
    val vasubu_e8  = Value(ASUBU, CADDER, S2VDV, E8)
    val vasubu_e16 = Value(ASUBU, CADDER, S2VDV, E16)
    val vasubu_e32 = Value(ASUBU, CADDER, S2VDV, E32)
    val vasubu_e64 = Value(ASUBU, CADDER, S2VDV, E64)
    val vasub_e8   = Value(ASUB , CADDER, S2VDV, E8)
    val vasub_e16  = Value(ASUB , CADDER, S2VDV, E16)
    val vasub_e32  = Value(ASUB , CADDER, S2VDV, E32)
    val vasub_e64  = Value(ASUB , CADDER, S2VDV, E64)
    val vsaddu_e8  = Value(SADDU, CADDER, S2VDV, E8)
    val vsaddu_e16 = Value(SADDU, CADDER, S2VDV, E16)
    val vsaddu_e32 = Value(SADDU, CADDER, S2VDV, E32)
    val vsaddu_e64 = Value(SADDU, CADDER, S2VDV, E64)
    val vsadd_e8   = Value(SADD , CADDER, S2VDV, E8)
    val vsadd_e16  = Value(SADD , CADDER, S2VDV, E16)
    val vsadd_e32  = Value(SADD , CADDER, S2VDV, E32)
    val vsadd_e64  = Value(SADD , CADDER, S2VDV, E64)
    val vssubu_e8  = Value(SSUBU, CADDER, S2VDV, E8)
    val vssubu_e16 = Value(SSUBU, CADDER, S2VDV, E16)
    val vssubu_e32 = Value(SSUBU, CADDER, S2VDV, E32)
    val vssubu_e64 = Value(SSUBU, CADDER, S2VDV, E64)
    val vssub_e8   = Value(SSUB , CADDER, S2VDV, E8)
    val vssub_e16  = Value(SSUB , CADDER, S2VDV, E16)
    val vssub_e32  = Value(SSUB , CADDER, S2VDV, E32)
    val vssub_e64  = Value(SSUB , CADDER, S2VDV, E64)

    val vsll_e8   = Value(SLL, SHIFT, S2VDV, E8)
    val vsll_e16  = Value(SLL, SHIFT, S2VDV, E16)
    val vsll_e32  = Value(SLL, SHIFT, S2VDV, E32)
    val vsll_e64  = Value(SLL, SHIFT, S2VDV, E64)
    val vsrl_e8   = Value(SRL, SHIFT, S2VDV, E8)
    val vsrl_e16  = Value(SRL, SHIFT, S2VDV, E16)
    val vsrl_e32  = Value(SRL, SHIFT, S2VDV, E32)
    val vsrl_e64  = Value(SRL, SHIFT, S2VDV, E64)
    val vsra_e8   = Value(SRA, SHIFT, S2VDV, E8)
    val vsra_e16  = Value(SRA, SHIFT, S2VDV, E16)
    val vsra_e32  = Value(SRA, SHIFT, S2VDV, E32)
    val vsra_e64  = Value(SRA, SHIFT, S2VDV, E64)
    val vror_e8   = Value(ROR, SHIFT, S2VDV, E8)
    val vror_e16  = Value(ROR, SHIFT, S2VDV, E16)
    val vror_e32  = Value(ROR, SHIFT, S2VDV, E32)
    val vror_e64  = Value(ROR, SHIFT, S2VDV, E64)
    val vrol_e8   = Value(ROL, SHIFT, S2VDV, E8)
    val vrol_e16  = Value(ROL, SHIFT, S2VDV, E16)
    val vrol_e32  = Value(ROL, SHIFT, S2VDV, E32)
    val vrol_e64  = Value(ROL, SHIFT, S2VDV, E64)

    val vssrl_e8    = Value(SRL  , CSHIFT, S2VDV, E8)
    val vssrl_e16   = Value(SRL  , CSHIFT, S2VDV, E16)
    val vssrl_e32   = Value(SRL  , CSHIFT, S2VDV, E32)
    val vssrl_e64   = Value(SRL  , CSHIFT, S2VDV, E64)
    val vssra_e8    = Value(SRA  , CSHIFT, S2VDV, E8)
    val vssra_e16   = Value(SRA  , CSHIFT, S2VDV, E16)
    val vssra_e32   = Value(SRA  , CSHIFT, S2VDV, E32)
    val vssra_e64   = Value(SRA  , CSHIFT, S2VDV, E64)
    val vwsll_e8    = Value(SLL  , CSHIFT, S2VDW, E8)
    val vwsll_e16   = Value(SLL  , CSHIFT, S2VDW, E16)
    val vwsll_e32   = Value(SLL  , CSHIFT, S2VDW, E32)
    val vnsrl_e8    = Value(SRL  , CSHIFT, S2WDV, E8)
    val vnsrl_e16   = Value(SRL  , CSHIFT, S2WDV, E16)
    val vnsrl_e32   = Value(SRL  , CSHIFT, S2WDV, E32)
    val vnsra_e8    = Value(SRA  , CSHIFT, S2WDV, E8)
    val vnsra_e16   = Value(SRA  , CSHIFT, S2WDV, E16)
    val vnsra_e32   = Value(SRA  , CSHIFT, S2WDV, E32)
    val vnclipu_e8  = Value(CLIPU, CSHIFT, S2WDV, E8)
    val vnclipu_e16 = Value(CLIPU, CSHIFT, S2WDV, E16)
    val vnclipu_e32 = Value(CLIPU, CSHIFT, S2WDV, E32)
    val vnclip_e8   = Value(CLIP , CSHIFT, S2WDV, E8)
    val vnclip_e16  = Value(CLIP , CSHIFT, S2WDV, E16)
    val vnclip_e32  = Value(CLIP , CSHIFT, S2WDV, E32)

    def getOp(implicit op: UInt): UInt = op(10, 8)
    def getOpClass(implicit op: UInt): UInt = op(7, 5)
    def getDataType(implicit op: UInt): UInt = op(4, 2)
    def getDataWidth(implicit op: UInt): UInt = op(1, 0)

    def isWidenVs2(implicit op: UInt): Bool = getDataType.isOneOf(S2WDV, S2WDW)
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

    def isSext2(implicit op: UInt): Bool = getOpClass === MOVE && getOp === SEXT && getDataType === S2F2DV
    def isSext4(implicit op: UInt): Bool = getOpClass === MOVE && getOp === SEXT && getDataType === S2F4DV
    def isSext8(implicit op: UInt): Bool = getOpClass === MOVE && getOp === SEXT && getDataType === S2F8DV
    def isZext2(implicit op: UInt): Bool = getOpClass === MOVE && getOp === ZEXT && getDataType === S2F2DV
    def isZext4(implicit op: UInt): Bool = getOpClass === MOVE && getOp === ZEXT && getDataType === S2F4DV
    def isZext8(implicit op: UInt): Bool = getOpClass === MOVE && getOp === ZEXT && getDataType === S2F8DV

    // May be encoded in
    def isUnsigned(implicit op: UInt): Bool = dataWidthAndTypeIsOneOf(
      ADDER -> Seq(MINU, MAXU),
      CADDER -> Seq(WADDU, WADD4U, WSUBU, AADDU, ASUBU, SADDU, SSUBU),
      CMP -> Seq(LTU, LEU, GTU),
      MOVE -> Seq(ZEXT),
      SHIFT -> Seq(SRL),
      CSHIFT -> Seq(SRL, CLIPU),
    )

    def isAddCarry(implicit op: UInt): Bool = getOpClass === ADDER && getOp.isOneOf(ADC, SBC)
    def isShift(implicit op: UInt): Bool = getOpClass.isOneOf(SHIFT, CSHIFT)

    def isAdd(implicit op: UInt): Bool = getOp === ADD
    def isSub(implicit op: UInt): Bool = getOp === SUB
    def isVmsbc(implicit op: UInt): Bool = getOp === SBC // Todo: check if it is only vmsbc
    def isBitLogic(implicit op: UInt): Bool = getOpClass === LOGIC
    def isLeftShiftLogic(implicit op: UInt): Bool = isShift && getOp.isOneOf(SLL, ROL)
    def isMaxMin(implicit op: UInt): Bool = getOpClass.isOneOf(ADDER) && getOp.isOneOf(MINU, MIN, MAXU, MAX)
    def isSat(implicit op: UInt): Bool = getOpClass.isOneOf(CADDER) && getOp.isOneOf(SADDU, SADD, SSUBU, SSUB)
    def isAvg(implicit op: UInt): Bool = getOpClass.isOneOf(CADDER) && getOp.isOneOf(AADDU, AADD, ASUBU, ASUB)

    def isNClip(implicit op: UInt): Bool = getOpClass.isOneOf(SHIFT) && getOp.isOneOf(CLIPU, CLIP)

    def isVand(implicit op: UInt): Bool = getOp === AND
    def isVnand(implicit op: UInt): Bool = getOp === NAND
    def isVandn(implicit op: UInt): Bool = getOp === ANDN
    def isVxor(implicit op: UInt): Bool = getOp === XOR
    def isVor(implicit op: UInt): Bool = getOp === OR
    def isVnor(implicit op: UInt): Bool = getOp === NOR
    def isVorn(implicit op: UInt): Bool = getOp === ORN
    def isVxnor(implicit op: UInt): Bool = getOp === XNOR

    def isCmpEq(implicit op: UInt): Bool = getOp === EQ
    def isCmpLt(implicit op: UInt): Bool = getOp === LT
    def isCmpNe(implicit op: UInt): Bool = getOp === NE
    def isCmpLe(implicit op: UInt): Bool = getOp === LE
    def isCmpGt(implicit op: UInt): Bool = getOp === GT

    def isMax(implicit op: UInt): Bool = getOp.isOneOf(MAX, MAXU)

    def isScalVro(implicit op: UInt): Bool = getOpClass === CSHIFT || getOpClass === SHIFT && getOp.isOneOf(ROR, ROL)
    def isNotVro(implicit op: UInt): Bool = getOpClass === CSHIFT
    def isZvbbOthers(implicit op: UInt): Bool = false.B // not supported yet
    def isVcpop(implicit op: UInt): Bool = false.B // not supported yet
    def isVbrev(implicit op: UInt): Bool = false.B // not supported yet
    def isVbrev8(implicit op: UInt): Bool = false.B // not supported yet
    def isVrev8(implicit op: UInt): Bool = false.B // not supported yet
    def isCountZero(implicit op: UInt): Bool = false.B // not supported yet
    def isCtz(implicit op: UInt): Bool = false.B // not supported yet
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

    val vcpop_m = Value(CPOP_M, DX, EX)
    val vfirst  = Value(FIRST , DX, EX)
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

    val vmulhu_e8   = Value(S2U, S1U, MULH , OP2, DV, E8 )
    val vmulhu_e16  = Value(S2U, S1U, MULH , OP2, DV, E16)
    val vmulhu_e32  = Value(S2U, S1U, MULH , OP2, DV, E32)
    val vmulhu_e64  = Value(S2U, S1U, MULH , OP2, DV, E64)
    val vmul_e8     = Value(S2S, S1S, MUL  , OP2, DV, E8 )
    val vmul_e16    = Value(S2S, S1S, MUL  , OP2, DV, E16)
    val vmul_e32    = Value(S2S, S1S, MUL  , OP2, DV, E32)
    val vmul_e64    = Value(S2S, S1S, MUL  , OP2, DV, E64)
    val vmulhsu_e8  = Value(S2S, S1U, MULH , OP2, DV, E8 )
    val vmulhsu_e16 = Value(S2S, S1U, MULH , OP2, DV, E16)
    val vmulhsu_e32 = Value(S2S, S1U, MULH , OP2, DV, E32)
    val vmulhsu_e64 = Value(S2S, S1U, MULH , OP2, DV, E64)
    val vmulh_e8    = Value(S2S, S1S, MULH , OP2, DV, E8 )
    val vmulh_e16   = Value(S2S, S1S, MULH , OP2, DV, E16)
    val vmulh_e32   = Value(S2S, S1S, MULH , OP2, DV, E32)
    val vmulh_e64   = Value(S2S, S1S, MULH , OP2, DV, E64)

    val vmadd_e8    = Value(S2S, S1S, MADD , OP3, DV, E8 )
    val vmadd_e16   = Value(S2S, S1S, MADD , OP3, DV, E16)
    val vmadd_e32   = Value(S2S, S1S, MADD , OP3, DV, E32)
    val vmadd_e64   = Value(S2S, S1S, MADD , OP3, DV, E64)
    val vnmsub_e8   = Value(S2S, S1S, NMSUB, OP3, DV, E8 )
    val vnmsub_e16  = Value(S2S, S1S, NMSUB, OP3, DV, E16)
    val vnmsub_e32  = Value(S2S, S1S, NMSUB, OP3, DV, E32)
    val vnmsub_e64  = Value(S2S, S1S, NMSUB, OP3, DV, E64)
    val vmacc_e8    = Value(S2S, S1S, MACC , OP3, DV, E8 )
    val vmacc_e16   = Value(S2S, S1S, MACC , OP3, DV, E16)
    val vmacc_e32   = Value(S2S, S1S, MACC , OP3, DV, E32)
    val vmacc_e64   = Value(S2S, S1S, MACC , OP3, DV, E64)
    val vnmsac_e8   = Value(S2S, S1S, NMSAC, OP3, DV, E8 )
    val vnmsac_e16  = Value(S2S, S1S, NMSAC, OP3, DV, E16)
    val vnmsac_e32  = Value(S2S, S1S, NMSAC, OP3, DV, E32)
    val vnmsac_e64  = Value(S2S, S1S, NMSAC, OP3, DV, E64)

    val vwmulu_e8   = Value(S2U, S1U, MUL, OP2, DW, E8 )
    val vwmulu_e16  = Value(S2U, S1U, MUL, OP2, DW, E16)
    val vwmulu_e32  = Value(S2U, S1U, MUL, OP2, DW, E32)
    val vwmulsu_e8  = Value(S2S, S1U, MUL, OP2, DW, E8 )
    val vwmulsu_e16 = Value(S2S, S1U, MUL, OP2, DW, E16)
    val vwmulsu_e32 = Value(S2S, S1U, MUL, OP2, DW, E32)
    val vwmul_e8    = Value(S2S, S1S, MUL, OP2, DW, E8 )
    val vwmul_e16   = Value(S2S, S1S, MUL, OP2, DW, E16)
    val vwmul_e32   = Value(S2S, S1S, MUL, OP2, DW, E32)

    val vwmaccu_e8   = Value(S2U, S1U, MACC, OP2, DW, E8 )
    val vwmaccu_e16  = Value(S2U, S1U, MACC, OP2, DW, E16)
    val vwmaccu_e32  = Value(S2U, S1U, MACC, OP2, DW, E32)
    val vwmacc_e8    = Value(S2S, S1S, MACC, OP2, DW, E8 )
    val vwmacc_e16   = Value(S2S, S1S, MACC, OP2, DW, E16)
    val vwmacc_e32   = Value(S2S, S1S, MACC, OP2, DW, E32)
    val vwmaccus_e8  = Value(S2S, S1U, MACC, OP2, DW, E8 )
    val vwmaccus_e16 = Value(S2S, S1U, MACC, OP2, DW, E16)
    val vwmaccus_e32 = Value(S2S, S1U, MACC, OP2, DW, E32)
    val vwmaccsu_e8  = Value(S2U, S1S, MACC, OP2, DW, E8 )
    val vwmaccsu_e16 = Value(S2U, S1S, MACC, OP2, DW, E16)
    val vwmaccsu_e32 = Value(S2U, S1S, MACC, OP2, DW, E32)

    val vsmul_e8  = Value(S2S, S1S, SMUL, OP2, DV, E8 )
    val vsmul_e16 = Value(S2S, S1S, SMUL, OP2, DV, E16)
    val vsmul_e32 = Value(S2S, S1S, SMUL, OP2, DV, E32)
    val vsmul_e64 = Value(S2S, S1S, SMUL, OP2, DV, E64)
  }

  object VIMacOpcode extends VIMacOpcode

  trait VIDivOpcode extends Opcodes with DataType {
    private val DIVU = bb"00"
    private val DIV  = bb"01"
    private val REMU = bb"10"
    private val REM  = bb"11"

    val vdivu_e8  = Value(DIVU, E8 )
    val vdivu_e16 = Value(DIVU, E16)
    val vdivu_e32 = Value(DIVU, E32)
    val vdivu_e64 = Value(DIVU, E64)
    val vdiv_e8   = Value(DIV , E8 )
    val vdiv_e16  = Value(DIV , E16)
    val vdiv_e32  = Value(DIV , E32)
    val vdiv_e64  = Value(DIV , E64)
    val vremu_e8  = Value(REMU, E8 )
    val vremu_e16 = Value(REMU, E16)
    val vremu_e32 = Value(REMU, E32)
    val vremu_e64 = Value(REMU, E64)
    val vrem_e8   = Value(REM , E8 )
    val vrem_e16  = Value(REM , E16)
    val vrem_e32  = Value(REM , E32)
    val vrem_e64  = Value(REM , E64)
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

    val vredsum_e8   = Value(SUM , DV, E8 )
    val vredsum_e16  = Value(SUM , DV, E16)
    val vredsum_e32  = Value(SUM , DV, E32)
    val vredsum_e64  = Value(SUM , DV, E64)
    val vredand_e8   = Value(AND , DV, E8 )
    val vredand_e16  = Value(AND , DV, E16)
    val vredand_e32  = Value(AND , DV, E32)
    val vredand_e64  = Value(AND , DV, E64)
    val vredor_e8    = Value(OR  , DV, E8 )
    val vredor_e16   = Value(OR  , DV, E16)
    val vredor_e32   = Value(OR  , DV, E32)
    val vredor_e64   = Value(OR  , DV, E64)
    val vredxor_e8   = Value(XOR , DV, E8 )
    val vredxor_e16  = Value(XOR , DV, E16)
    val vredxor_e32  = Value(XOR , DV, E32)
    val vredxor_e64  = Value(XOR , DV, E64)
    val vredminu_e8  = Value(MINU, DV, E8 )
    val vredminu_e16 = Value(MINU, DV, E16)
    val vredminu_e32 = Value(MINU, DV, E32)
    val vredminu_e64 = Value(MINU, DV, E64)
    val vredmin_e8   = Value(MIN , DV, E8 )
    val vredmin_e16  = Value(MIN , DV, E16)
    val vredmin_e32  = Value(MIN , DV, E32)
    val vredmin_e64  = Value(MIN , DV, E64)
    val vredmaxu_e8  = Value(MAXU, DV, E8 )
    val vredmaxu_e16 = Value(MAXU, DV, E16)
    val vredmaxu_e32 = Value(MAXU, DV, E32)
    val vredmaxu_e64 = Value(MAXU, DV, E64)
    val vredmax_e8   = Value(MAX , DV, E8 )
    val vredmax_e16  = Value(MAX , DV, E16)
    val vredmax_e32  = Value(MAX , DV, E32)
    val vredmax_e64  = Value(MAX , DV, E64)

    val vwredsum_e8   = Value(SUM, DW, E8 )
    val vwredsum_e16  = Value(SUM, DW, E16)
    val vwredsum_e32  = Value(SUM, DW, E32)
    val vwredsumu_e8  = Value(SUMU, DW, E8 )
    val vwredsumu_e16 = Value(SUMU, DW, E16)
    val vwredsumu_e32 = Value(SUMU, DW, E32)
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

    val vrgather_v_e8     = Value(RGATHER_V   , E8 )
    val vrgather_v_e16    = Value(RGATHER_V   , E16)
    val vrgather_v_e32    = Value(RGATHER_V   , E32)
    val vrgather_v_e64    = Value(RGATHER_V   , E64)
    val vrgather_x_e8     = Value(RGATHER_X   , E8 )
    val vrgather_x_e16    = Value(RGATHER_X   , E16)
    val vrgather_x_e32    = Value(RGATHER_X   , E32)
    val vrgather_x_e64    = Value(RGATHER_X   , E64)
    val vrgather_i_e8     = Value(RGATHER_I   , E8 )
    val vrgather_i_e16    = Value(RGATHER_I   , E16)
    val vrgather_i_e32    = Value(RGATHER_I   , E32)
    val vrgather_i_e64    = Value(RGATHER_I   , E64)
    val vrgather_ei16_e8  = Value(RGATHER_EI16, E8 )
    val vrgather_ei16_e16 = Value(RGATHER_EI16, E16)
    val vrgather_ei16_e32 = Value(RGATHER_EI16, E32)
    val vrgather_ei16_e64 = Value(RGATHER_EI16, E64)
    val vslideup_e8       = Value(SLIDEUP     , E8 )
    val vslideup_e16      = Value(SLIDEUP     , E16)
    val vslideup_e32      = Value(SLIDEUP     , E32)
    val vslideup_e64      = Value(SLIDEUP     , E64)
    val vslidedown_e8     = Value(SLIDEDOWN   , E8 )
    val vslidedown_e16    = Value(SLIDEDOWN   , E16)
    val vslidedown_e32    = Value(SLIDEDOWN   , E32)
    val vslidedown_e64    = Value(SLIDEDOWN   , E64)
    val vcompress_e8      = Value(COMPRESS    , E8 )
    val vcompress_e16     = Value(COMPRESS    , E16)
    val vcompress_e32     = Value(COMPRESS    , E32)
    val vcompress_e64     = Value(COMPRESS    , E64)
    val vslide1up_e8      = Value(SLIDE1UP    , E8 )
    val vslide1up_e16     = Value(SLIDE1UP    , E16)
    val vslide1up_e32     = Value(SLIDE1UP    , E32)
    val vslide1up_e64     = Value(SLIDE1UP    , E64)
    val vslide1down_e8    = Value(SLIDE1DOWN  , E8 )
    val vslide1down_e16   = Value(SLIDE1DOWN  , E16)
    val vslide1down_e32   = Value(SLIDE1DOWN  , E32)
    val vslide1down_e64   = Value(SLIDE1DOWN  , E64)
  }

  object VIPermOpcode extends VIPermOpcode

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
    protected val EX  = bb"00"
  }
}
