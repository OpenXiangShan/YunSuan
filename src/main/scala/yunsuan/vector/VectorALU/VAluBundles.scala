
package yunsuan.vector

import chisel3._
import chisel3.util._
import yunsuan.vector.alu.VAluOpcode._

class VAluOpcode extends Bundle{
  val op = UInt(6.W)
  // Alu opcode:
  def isAddSub = op === vadd || op === vsub
  def isVext = op === vext
  def isAddWithCarry = op === vadc || op === vmadc || op === vsbc || op === vmsbc
  def isVmsbc = op === vmsbc
  def isVand = op === vand
  def isVnand = op === vnand
  def isVandn = op === vandn
  def isVxor = op === vxor
  def isVor = op === vor
  def isVnor = op === vnor
  def isVorn = op === vorn
  def isVxnor = op === vxnor
  def isBitLogical = isVand || isVnand || isVandn || isVxor || isVor || isVnor || isVorn || isVxnor
  def isVmseq = op === vmseq
  def isVmsne = op === vmsne
  def isVmslt = op === vmslt
  def isVmsle = op === vmsle
  def isVmsgt = op === vmsgt
  def isVmax = op === vmax
  def isVmin = op === vmin
  def isVmerge = op === vmerge
  def isSatAdd = op === vsadd || op === vssub
  def isAvgAdd = op === vaadd || op === vasub
  def isScalingShift = op === vssrl || op === vssra
  def isSignedShift = op === vsra || op === vssra
  def isShift = op === vsll || op === vsrl || op === vssrl || isSignedShift
  def isLeftShift = op === vsll
  def isReduction = (op >= vredsum) && (op <= vredxor) 
  def isVredsum = op === vredsum
  def isVredmax = op === vredmax
  def isVredmin = op === vredmin  
  def isVredand = op === vredand
  def isVredor  = op === vredor
  def isVredxor = op === vredxor
  def isVcpop   = op === vcpop 
  def isVfirst  = op === vfirst
  def isVmsbf   = op === vmsbf 
  def isVmsif   = op === vmsif 
  def isVmsof   = op === vmsof 
  def isViota   = op === viota 
  def isVid     = op === vid   
  def isIntFixp = op < vredsum || op === vmvsx
  def isVmvsx = op === vmvsx
  def isVmvxs = op === vmvxs
  def isVmergeMove = op === vmerge || op === vmv || op === vmvsx
  // IMac opcode:
  def op3b = op(2, 0)
  def highHalf = op3b === 1.U
  def isMacc = op3b === 2.U || op3b === 3.U || op3b === 4.U || op3b === 5.U
  def isSub = op3b === 3.U || op3b === 5.U
  def isFixP = op3b === 6.U
  def overWriteMultiplicand = op3b === 4.U || op3b === 5.U
  // Zvbb opcode:
  def isVbrev   = op === vbrev
  def isVbrev8  = op === vbrev8
  def isVrev8   = op === vrev8
  def isClz     = op === vclz
  def isCtz     = op === vctz
  def isVrol    = op === vrol
  def isVror    = op === vror
  def isVwsll   = op === vwsll
  def isVrev    = op === vbrev || op === vbrev8 || op === vrev8
  def isVCount  = op === vclz || op === vctz || op === vcpop
  def isVro     = op === vrol || op === vror
}

class VIFuInfo extends Bundle {
  val vm = Bool()
  val ma = Bool()
  val ta = Bool()
  val vlmul = UInt(3.W)
  val vl = UInt(8.W)
  val vstart = UInt(7.W)
  val uopIdx = UInt(6.W)
  val vxrm = UInt(2.W)
}

class VIFuInput extends Bundle {
  val opcode = new VAluOpcode
  val info = new VIFuInfo
  val srcType = Vec(2, UInt(4.W))  // 0: vs2   1: vs1
  val vdType  = UInt(4.W)
  val vs1 = UInt(128.W)
  val vs2 = UInt(128.W)
  val old_vd = UInt(128.W)
  val mask = UInt(128.W)
}

class VIFuOutput extends Bundle {
  val vd = UInt(128.W)
  val vxsat = Bool()
}

class SewOH extends Bundle {  // 0   1   2   3
  val oneHot = UInt(4.W) // b0-b3: 8, 16, 32, 64
  def is8 = oneHot(0)
  def is16 = oneHot(1)
  def is32 = oneHot(2)
  def is64 = oneHot(3)
}
object SewOH {
  def apply(vsew: UInt): SewOH = {
    val sew = Wire(new SewOH)
    sew.oneHot := VecInit(Seq.tabulate(4)(i => vsew === i.U)).asUInt
    sew
  }
}
