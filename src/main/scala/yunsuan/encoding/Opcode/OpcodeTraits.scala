package yunsuan.encoding.Opcode

import chisel3.UInt

object OpcodeTraits {
  trait OpcodeTrait

  trait WenTrait extends OpcodeTrait
  trait VecWenTrait extends WenTrait

  case object GpWen extends WenTrait
  case object VlWen extends WenTrait
  case object FpWen extends WenTrait
  case object VpWen extends VecWenTrait
  case object VsWen extends VecWenTrait
  case object VwWen extends VecWenTrait
  case object VmWen extends VecWenTrait
  case object VwsWen extends VecWenTrait

  abstract class Src1Trait(val srcType: SrcType) extends OpcodeTrait

  case object Src1En  extends OpcodeTrait
  case object Src1Gp  extends Src1Trait(Gp)
  case object Src1Fp  extends Src1Trait(Fp)
  case object Src1Vp  extends Src1Trait(Vp)
  case object Src1Vm  extends Src1Trait(Vm)
  case object Src1Vw  extends Src1Trait(Vw)
  case object Src1Vs  extends Src1Trait(Vs)
  case object Src1Vws extends Src1Trait(Vws)
  case class Src1Imm(encode: UInt) extends Src1Trait(Imm)

  abstract class Src2Trait(val srcType: SrcType) extends OpcodeTrait

  case object Src2En extends OpcodeTrait
  case object Src2Gp extends Src2Trait(Gp)
  case object Src2Fp extends Src2Trait(Fp)
  case object Src2Vp extends Src2Trait(Vp)
  case object Src2Vw extends Src2Trait(Vw)
  case object Src2Vs extends Src2Trait(Vs)
  case object Src2Vm extends Src2Trait(Vm)
  case class Src2Imm(encode: UInt) extends Src2Trait(Imm)

  abstract class Src3Trait(val srcType: SrcType) extends OpcodeTrait
  case object Src3Gp extends Src3Trait(Gp)
  case object Src3Fp extends Src3Trait(Fp)
  case object Src3Vp extends Src3Trait(Vp)
  case object Src3Vw extends Src3Trait(Vw)
  case object Src3Vs extends Src3Trait(Vs)
  case object Src3Vm extends Src3Trait(Vm)

  trait V0Trait extends OpcodeTrait
  case object V0RenAsMask extends V0Trait
  case object V0RenAsSrc extends V0Trait

  case object VlRen extends OpcodeTrait

  trait CtrlRen extends OpcodeTrait
  case object VxrmRen extends CtrlRen
  case object FrmRen extends CtrlRen

  trait CtrlWen extends OpcodeTrait
  case object VxsatWen extends CtrlWen
  case object FflagsWen extends CtrlWen

  case object Order extends OpcodeTrait

  case object NoSpec extends OpcodeTrait

  case object BlockBack extends OpcodeTrait

  case object FlushPipe extends OpcodeTrait

  case object CannotRobCompress extends OpcodeTrait

  abstract class SrcType
  case object Imm extends SrcType
  case object Gp extends SrcType
  case object Fp extends SrcType
  case object Vl extends SrcType

  abstract class Vec extends SrcType
  case object Vp extends Vec

  /**
   * VpMask
   */
  case object Vm extends Vec

  /**
   * VpScala
   */
  case object Vs extends Vec

  /**
   * VpWiden
   */
  case object Vw extends Vec

  case object Vws extends Vec

  case object NeedVecEnable extends OpcodeTrait

  // Force specific vector floating-point ops to be scheduled as VFALU.
  case object ForceVfAlu extends OpcodeTrait

  case object Src12Rev extends OpcodeTrait

  case object NoDestAlloc extends OpcodeTrait

  trait MaskTrait extends OpcodeTrait

  // The UOP need no mask
  case object NoMask extends MaskTrait
  // The UOP need mask to select src2 as Mux(mask(i), src(i), 0)
  case object Src2Mask extends MaskTrait
  // The UOP need mask to select both src1 and src2 as Mux(mask(i), src(i), 0)
  case object Src12Mask extends MaskTrait
  // The UOP need mask to select the result as Mux(mask(i), vd(i), oldvd | 1s)
  case object DestMask extends MaskTrait
}
