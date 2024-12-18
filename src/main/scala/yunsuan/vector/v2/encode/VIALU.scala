package yunsuan.vector.v2.encode

import chisel3._
import yunsuan.util.LiteralCat
import yunsuan.vector.Common.caseToUIntUtil

trait BStringUtil {
  implicit class BinaryStringHelper(private val sc: StringContext) {
    def b(args: Any*): UInt = {
      val str: String = sc.standardInterpolator(x => x, args)
      require(str.forall(c => c == '0' || c == '1'))
      if (str.isEmpty)
        0.U(0.W)
      else
        ("b" + str).U(str.length.W)
    }
  }
}

class VIALUOpType extends Bundle {
  val op = UInt(9.W) // Todo: configuable

  import VIALUOpcode._
  import VIALUOpcodePrefix._

  def VADD    = LiteralCat(NORMAL, SIGNED, ADD,  NOFIXP)
  def VSUB    = LiteralCat(NORMAL, SIGNED, SUB,  NOFIXP)
  def VAADDU  = LiteralCat(AVG,    UNSIGN, ADD,  FIXP  )
  def VAADD   = LiteralCat(AVG,    SIGNED, ADD,  FIXP  )
  def VASUBU  = LiteralCat(AVG,    UNSIGN, SUB,  FIXP  )
  def VASUB   = LiteralCat(AVG,    SIGNED, SUB,  FIXP  )
  def VADC    = LiteralCat(CARRY,  NONE,   ADD,  NOFIXP)
  def VSBC    = LiteralCat(CARRY,  NONE,   SUB,  NOFIXP)
  def VMADC   = LiteralCat(CARRYM, NONE,   ADD,  NOFIXP)
  def VMSBC   = LiteralCat(CARRYM, NONE,   SUB,  NOFIXP)
  def VSADDU  = LiteralCat(SAT,    UNSIGN, ADD,  FIXP  )
  def VSADD   = LiteralCat(SAT,    SIGNED, ADD,  FIXP  )
  def VSSUBU  = LiteralCat(SAT,    UNSIGN, SUB,  FIXP  )
  def VSSUB   = LiteralCat(SAT,    SIGNED, SUB,  FIXP  )
  def VMIN    = LiteralCat(MINMAX, SIGNED, MIN,  NOFIXP)
  def VMAX    = LiteralCat(MINMAX, SIGNED, MAX,  NOFIXP)
  def VMINU   = LiteralCat(MINMAX, UNSIGN, MIN,  NOFIXP)
  def VMAXU   = LiteralCat(MINMAX, UNSIGN, MAX,  NOFIXP)
  def VWADDU  = LiteralCat(WIDEN,  UNSIGN, ADD,  NOFIXP)
  def VWADD   = LiteralCat(WIDEN,  SIGNED, ADD,  NOFIXP)
  def VWSUBU  = LiteralCat(WIDEN,  UNSIGN, SUB,  NOFIXP)
  def VWSUB   = LiteralCat(WIDEN,  SIGNED, SUB,  NOFIXP)
  def VWADDUW = LiteralCat(WIDEN2, UNSIGN, ADD,  NOFIXP)
  def VWADDW  = LiteralCat(WIDEN2, SIGNED, ADD,  NOFIXP)
  def VWSUBUW = LiteralCat(WIDEN2, UNSIGN, SUB,  NOFIXP)
  def VWSUBW  = LiteralCat(WIDEN2, SIGNED, SUB,  NOFIXP)
  def VMSEQ   = LiteralCat(CMP,    NONE,   EQ,   NOFIXP)
  def VMSNE   = LiteralCat(CMP,    NONE,   NE,   NOFIXP)
  def VMSLTU  = LiteralCat(CMP,    NONE,   LTU,  NOFIXP)
  def VMSLT   = LiteralCat(CMP,    NONE,   LT,   NOFIXP)
  def VMSLEU  = LiteralCat(CMP,    NONE,   LEU,  NOFIXP)
  def VMSLE   = LiteralCat(CMP,    NONE,   LE,   NOFIXP)
  def VMSGTU  = LiteralCat(CMP,    NONE,   GTU,  NOFIXP)
  def VMSGT   = LiteralCat(CMP,    NONE,   GT,   NOFIXP)
  def VSRL    = LiteralCat(SHIFT,  NONE,   SRL,  NOFIXP)
  def VSRA    = LiteralCat(SHIFT,  NONE,   SRA,  NOFIXP)
  def VSSRL   = LiteralCat(SHIFT,  NONE,   SSRL, NOFIXP)
  def VSSRA   = LiteralCat(SHIFT,  NONE,   SSRA, NOFIXP)
  def VNCLIPU = LiteralCat(SHIFT,  NONE,   SSRL, FIXP  )
  def VNCLIP  = LiteralCat(SHIFT,  NONE,   SSRA, FIXP  )
  def VNSRL   = LiteralCat(SHIFT,  NONE,   NSRL, NOFIXP)
  def VNSRA   = LiteralCat(SHIFT,  NONE,   NSRA, NOFIXP)
  def VSLL    = LiteralCat(SHIFT,  NONE,   SL,   NOFIXP)
  def VMANDN  = LiteralCat(MLOGIC, NONE,   ANDN, NOFIXP)
  def VMAND   = LiteralCat(MLOGIC, NONE,   AND,  NOFIXP)
  def VMOR    = LiteralCat(MLOGIC, NONE,   OR,   NOFIXP)
  def VMXOR   = LiteralCat(MLOGIC, NONE,   XOR,  NOFIXP)
  def VMORN   = LiteralCat(MLOGIC, NONE,   ORN,  NOFIXP)
  def VMNAND  = LiteralCat(MLOGIC, NONE,   NAND, NOFIXP)
  def VMNOR   = LiteralCat(MLOGIC, NONE,   NOR,  NOFIXP)
  def VMXNOR  = LiteralCat(MLOGIC, NONE,   XNOR, NOFIXP)
  def VAND    = LiteralCat(LOGIC,  NONE,   AND,  NOFIXP)
  def VOR     = LiteralCat(LOGIC,  NONE,   OR,   NOFIXP)
  def VXOR    = LiteralCat(LOGIC,  NONE,   XOR,  NOFIXP)

  def VROL    = LiteralCat(ROTATE, NONE,   RSL,  NOFIXP)
  def VROR    = LiteralCat(ROTATE, NONE,   RSR,  NOFIXP)
  def VWSLL   = LiteralCat(SHIFT,  NONE,   WSL,  NOFIXP)

  def needFixPoint: Bool = this.op(0)

  def isNormal: Bool = VIALUOpcodePrefix.is(_.NORMAL)(this.op)
  def isAvg   : Bool = VIALUOpcodePrefix.is(_.AVG)(this.op)
  def isCarry : Bool = VIALUOpcodePrefix.is(_.CARRY)(this.op)
  def isCarryM: Bool = VIALUOpcodePrefix.is(_.CARRYM)(this.op)
  def isSat   : Bool = VIALUOpcodePrefix.is(_.SAT)(this.op)
  def isWiden : Bool = VIALUOpcodePrefix.is(_.WIDEN)(this.op)
  def isWiden2: Bool = VIALUOpcodePrefix.is(_.WIDEN2)(this.op)
  def isMinMax: Bool = VIALUOpcodePrefix.is(_.MINMAX)(this.op)
  def isCmp   : Bool = VIALUOpcodePrefix.is(_.CMP)(this.op)

  def isShift : Bool = VIALUOpcodePrefix.is(_.SHIFT)(this.op)
  def isRotate: Bool = VIALUOpcodePrefix.is(_.ROTATE)(this.op)

  def isMLogic: Bool = VIALUOpcodePrefix.is(_.MLOGIC)(this.op)
  def isLogic : Bool = VIALUOpcodePrefix.is(_.LOGIC)(this.op)

  def getOp(opWidth: Int): UInt = this.op.drop(1).take(opWidth)

  def getLogicOp  = this.getOp(3)
  def getMLogicOp = this.getOp(3)
  def getAdderOp  = this.getOp(1)
  def getAdderSign = this.op.drop(2).take(1)
  def getCmpOp    = this.getOp(3)
  def getMinMaxOp = this.getOp(1)
  def getShiftOp  = this.getOp(3)
  def getRotateOp = this.getOp(1)

  def isAddSub = this.isNormal
  def isAddWithCarry = this.isCarry || this.isCarryM
  def isVmsbc = this.isCarryM && this.getAdderOp === VIALUOpcode.SUB

  def isBitLogical = this.isLogic || this.isMLogic
  def isVand  = this.getLogicOp === VIALUOpcode.AND
  def isVnand = this.getLogicOp === VIALUOpcode.NAND
  def isVandn = this.getLogicOp === VIALUOpcode.ANDN
  def isVxor  = this.getLogicOp === VIALUOpcode.XOR
  def isVor   = this.getLogicOp === VIALUOpcode.OR
  def isVnor  = this.getLogicOp === VIALUOpcode.NOR
  def isVorn  = this.getLogicOp === VIALUOpcode.ORN
  def isVxnor = this.getLogicOp === VIALUOpcode.XNOR

  def isVmseq = this.getCmpOp === VIALUOpcode.EQ
  def isVmsne = this.getCmpOp === VIALUOpcode.NE
  def isVmslt = this.getCmpOp === VIALUOpcode.LT || this.getCmpOp === VIALUOpcode.LTU
  def isVmsle = this.getCmpOp === VIALUOpcode.LE || this.getCmpOp === VIALUOpcode.LEU
  def isVmsgt = this.getCmpOp === VIALUOpcode.GT || this.getCmpOp === VIALUOpcode.GTU
  def isVmax  = this.getMinMaxOp === VIALUOpcode.MAX
  def isVmin  = this.getMinMaxOp === VIALUOpcode.MIN
  // Todo: rename to isSat
  def isSatAdd = this.isSat
  // Todo: rename to isAvg
  def isAvgAdd = this.isAvg

  def isScalingShift = this.isShift && this.getShiftOp.isOneOf(VIALUOpcode)(_.SSRL, _.SSRA)
  def isShiftSigned = this.getShiftOp.isOneOf(VIALUOpcode.SRA, VIALUOpcode.NSRA, VIALUOpcode.SSRA)
  def isShiftLeft = this.isShift && (
    this.getShiftOp.isOneOf(VIALUOpcode.SL, VIALUOpcode.WSL)
    ) || this.isRotate && (
    this.getRotateOp.isOneOf(VIALUOpcode.RSL)
    )
  def isNarrowShift = this.isShift && this.getShiftOp.isOneOf(VIALUOpcode.NSRA, VIALUOpcode.NSRL)
  def isWidenShift = this.isShift && this.getShiftOp.isOneOf(VIALUOpcode.WSL)
  def isRotateShift = this.isRotate

  def isSub = this.getAdderOp === VIALUOpcode.SUB
  def isMin = this.getMinMaxOp === VIALUOpcode.MIN
  def isEQ = this.getCmpOp === VIALUOpcode.EQ
  def isNE = this.getCmpOp === VIALUOpcode.NE
  def isLT = this.getCmpOp.isOneOf(VIALUOpcode)(_.LT, _.LTU)
  def isLE = this.getCmpOp.isOneOf(VIALUOpcode)(_.LE, _.LEU)
  def isGT = this.getCmpOp.isOneOf(VIALUOpcode)(_.GT, _.GTU)
  def isAdderSigned = this.getAdderSign === VIALUOpcode.SIGNED

  def isAnd = this.getLogicOp === VIALUOpcode.AND
  def isNand = this.getLogicOp === VIALUOpcode.NAND
  def isAndN = this.getLogicOp === VIALUOpcode.ANDN
  def isXor = this.getLogicOp === VIALUOpcode.XOR
  def isOr = this.getLogicOp === VIALUOpcode.OR
  def isNor = this.getLogicOp === VIALUOpcode.NOR
  def isOrn = this.getLogicOp === VIALUOpcode.ORN
  def isXnor = this.getLogicOp === VIALUOpcode.XNOR
}

object VIALUOpcodePrefix extends BStringUtil {
  def NONE   = b""

  def NORMAL = b"00000"
  def AVG    = b"00001"
  def CARRY  = b"000100"
  def CARRYM = b"000101"
  def ROTATE = b"000111"
  def SAT    = b"00100"
  def MINMAX = b"00101"
  def WIDEN  = b"00110"
  def WIDEN2 = b"00111"
  def CMP    = b"0100"
  def SHIFT  = b"0101"
  def MLOGIC = b"0110"
  def LOGIC  = b"0111"
  def MASKOP = b"1000"

  def is(enum: this.type => UInt)(uint: UInt): Bool = {
    uint.head(enum(this).getWidth) === enum(this)
  }
}

object VIALUOpcode extends BStringUtil {
  def SIGNED = b"1"
  def UNSIGN = b"0"

  def ADDER  = b"1"
  def MISC   = b"0"

  def FIXP   = b"1"
  def NOFIXP = b"0"

  def ADD    = b"0"
  def SUB    = b"1"

  def EQ     = b"000"
  def NE     = b"001"
  def LTU    = b"010"
  def LT     = b"011"
  def LEU    = b"100"
  def LE     = b"101"
  def GTU    = b"110"
  def GT     = b"111"

  def ANDN   = b"000"
  def AND    = b"001"
  def OR     = b"010"
  def XOR    = b"011"
  def ORN    = b"100"
  def NAND   = b"101"
  def NOR    = b"110"
  def XNOR   = b"111"

  def MIN    = b"0"
  def MAX    = b"1"

  def SRL    = b"000"
  def SRA    = b"001"
  def SSRL   = b"010"
  def SSRA   = b"011"
  def NSRL   = b"100"
  def NSRA   = b"101"
  def SL     = b"110"
  def WSL    = b"111"

  def RSL    = b"0"
  def RSR    = b"1"

  def CLZ    = b"000"
  def CTZ    = b"001"
  def CPOPM  = b"010"
  def CPOPV  = b"011"
  def BREV   = b"100"
  def BREV8  = b"101"
  def REV8   = b"111"
}
