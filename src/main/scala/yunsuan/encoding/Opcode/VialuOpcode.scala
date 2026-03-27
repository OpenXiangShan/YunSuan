package yunsuan.encoding.Opcode

import chisel3._

object FixedPointRoundingMode {
  def RNU: UInt = 0.U(2.W)
  def RNE: UInt = 1.U(2.W)
  def RDN: UInt = 2.U(2.W)
  def ROD: UInt = 3.U(2.W)
}

object FixedPointConst {
  def unsignedMax: UInt = "hff".U(8.W)
  def unsignedMin: UInt = 0.U(8.W)
  def signedMax: UInt = "h7f".U(8.W)
  def signedMin: UInt = "h80".U(8.W)
}

class VIAluOpcode extends Bundle {
  val op = UInt(6.W)
  def isAdd: Bool = !op(3) & !op(2) & !op(1) & !op(0) // op === VialuOpcode.vadd
  def isSub: Bool = op(4)
  def isVmsbc: Bool = op(4) & !op(3) &  op(2) & !op(1) & !op(0)
  def isBitLogic:   Bool = !op(4) & !op(3)
  def isShiftLogic: Bool = !op(4) &  op(3)
  def isLeftShiftLogic:  Bool = isShiftLogic & !op(2) & !op(1)
  def isMaxMinLogic: Bool = op(3)
  def isSatLogic: Bool = !op(2) & op(1) & !op(0)
  def isAvgLogic: Bool = !op(2) & op(1) &  op(0)

  def isVand : Bool = isBitLogic & !op(2) & !op(1) &  op(0)
  def isVnand: Bool = isBitLogic & !op(2) &  op(1) & !op(0)
  def isVandn: Bool = isBitLogic & !op(2) &  op(1) &  op(0)
  def isVxor : Bool = isBitLogic &  op(2) & !op(1) & !op(0)
  def isVor  : Bool = isBitLogic &  op(2) & !op(1) &  op(0)
  def isVnor : Bool = isBitLogic &  op(2) &  op(1) & !op(0)
  def isVorn : Bool = isBitLogic &  op(2) &  op(1) &  op(0)
  def isVxnor: Bool = isBitLogic & !op(2) & !op(1) & !op(0)

  def isCmpEqLt: Bool = op(2) & !op(1) & op(0)
  def isCmpNeLeGt: Bool = op(2) & op(1)
  def isCmpEq: Bool = !isSub & isCmpEqLt
  def isCmpLt: Bool =  isSub & isCmpEqLt
  def isCmpNe: Bool = !isSub & isCmpNeLeGt & !op(0)
  def isCmpLe: Bool =  isSub & isCmpNeLeGt & !op(0)
  def isCmpGt: Bool =  isSub & isCmpNeLeGt &  op(0)

  def isMax: Bool = isMaxMinLogic & op(0)

  def isScalVro: Bool = op(0)
  def isNotVro: Bool = op(2) ^ op(1)
  def isZvbbOthers: Bool = isSub
  def isVcpop:  Bool = !op(1) & !op(0)
  def isVbrev:  Bool = !op(1) &  op(0)
  def isVbrev8: Bool =  op(1) & !op(0)
  def isVrev8:  Bool =  op(1) &  op(0)
  def isCountZero: Bool = op(2)
  def isCtz: Bool =  op(0)
}

object VialuOpcode {
  def width = 6

  def vadd   = "b000000".U(width.W)   // vd = vs2 + vs1, vm
  def vadc   = "b000001".U(width.W)   // vsd[i] = vs2[i] + vs1[i] + v0.mask[i]
  def vsadd  = "b000010".U(width.W)
  def vaadd  = "b000011".U(width.W)
  def vmadc  = "b000100".U(width.W)
  def vmseq  = "b000101".U(width.W)
  def vmsne  = "b000110".U(width.W)

  def vsub   = "b010000".U(width.W)   // vd = vs2 - vs1, vm = vs2 + ~(vs1) + 1, vm
  def vsbc   = "b010001".U(width.W)
  def vssub  = "b010010".U(width.W)
  def vasub  = "b010011".U(width.W)
  def vmsbc  = "b010100".U(width.W)
  def vmslt  = "b010101".U(width.W)
  def vmsle  = "b010110".U(width.W)
  def vmsgt  = "b010111".U(width.W)
  def vmin   = "b011000".U(width.W)
  def vmax   = "b011001".U(width.W)

  def vext   = "b111111".U(width.W)
  def vand   = "b100001".U(width.W)
  def vnand  = "b100010".U(width.W)
  def vandn  = "b100011".U(width.W)

  def vxor   = "b100100".U(width.W)
  def vor    = "b100101".U(width.W)
  def vnor   = "b100110".U(width.W)
  def vorn   = "b100111".U(width.W)
  def vxnor  = "b100000".U(width.W)

  def vsll   = "b101000".U(width.W)
  def vrol   = "b101001".U(width.W)

  def vsrl   = "b101010".U(width.W)
  def vssrl  = "b101011".U(width.W)
  def vsra   = "b101100".U(width.W)
  def vssra  = "b101101".U(width.W)
  def vror   = "b101111".U(width.W)

  // Zvbb
  def vcpop  = "b110000".U(width.W)
  def vbrev  = "b110001".U(width.W)
  def vbrev8 = "b110010".U(width.W)
  def vrev8  = "b110011".U(width.W)

  def vclz   = "b110100".U(width.W)
  def vctz   = "b110101".U(width.W)
}
