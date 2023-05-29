package yunsuan

import chisel3._

package object encoding {
  object Vs1IntType extends VecInt.Vs1Type
  object Vs2IntType extends VecInt.Vs2Type

  object Vs1FpType extends VecFp.Vs1Type
  object Vs2FpType extends VecFp.Vs2Type

  object VdType extends VecDstType

  object VsType {
    def width: Int = Vs1IntType.width max Vs2IntType.width

    def apply(): UInt = UInt(width.W)
  }

  class Vs2Vs1VdType extends Bundle {
    val vs2 = Vs2IntType()
    val vs1 = Vs1IntType()
    val vd  = VdType()
  }
  
  object Vs2Vs1VdType {
    def apply(): Vs2Vs1VdType = new Vs2Vs1VdType()
  }
}
