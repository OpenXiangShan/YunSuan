package race.vpu

import chisel3._
import chisel3.util._
import VParams._
import utility.CircularQueuePtr

class RobPtr extends CircularQueuePtr[RobPtr](VRobSize)

class Dispatch_S2V extends Bundle {
  val robIdx = new RobPtr
  val inst = UInt(32.W)
  val vcsr = new VCsr
  val rs1 = UInt(xLen.W)
  val rs2 = UInt(xLen.W)
}

class VCsr extends Bundle {
  val vstart = UInt(bVstart.W)
  val vl = UInt(bVL.W)
  val vxrm = UInt(2.W)
  val frm = UInt(3.W)
  val vlmul = UInt(3.W)
  val vsew = UInt(3.W)
  val vill = Bool()
  val ma = Bool()
  val ta = Bool()
}

// Decoder output
class VCtrl extends Bundle {
  //-- Fields of instruction --
  val lsrc = Vec(2, UInt(5.W)) //0: vs1/imm5   1: vs2
  val ldest = UInt(5.W)
  val vm = Bool() // vector mask
  val funct6 = UInt(6.W)
  val funct3 = UInt(3.W) // 14..12

  //-- Decoded signals --
  val illegal = Bool()
  val lsrcVal = Vec(3, Bool()) //0: vs1   1: vs2   2: 3rd operand (and vs3?)
  val ldestVal = Bool()
  val rdVal = Bool()  // Has scalar dest operand?
  val load = Bool()
  val store = Bool()
  val arith = Bool()
  val crossLane = Bool() // Goto Cross-lane EXU
  val alu = Bool() // All low-latency operations
  val mul = Bool()
  val fp = Bool()
  val div = Bool()
  val fixP = Bool()
  val redu = Bool()
  val mask = Bool()
  val perm = Bool()
  val widen = Bool()  // 2*sew = sew op sew  //Reduction not included
  val widen2 = Bool() // 2*sew = 2*sew op sew
  val narrow = Bool() // sew = 2*sew op sew
  val narrow_to_1 = Bool() // Compare, carry-out producing instructions
  def vv = !funct3(2) && !(funct3(1) && funct3(0))
  def vx = funct3(2) 
  def vi = !funct3(2) && funct3(1) && funct3(0) 
  def fuSel = Seq(alu, mul, fp, redu, mask, perm, div)
  def laneExu = arith && !crossLane
  def isLdst = load || store
  def vs1_imm = lsrc(0)
  def opi = funct3(0) === funct3(1) // OPIVV/X/I
  def opm = funct3(1, 0) === 2.U //OPMVV/X
  def opf = funct3(1, 0) === 1.U // OPFVV/F
}

class VCtrlCsr extends Bundle {
  val ctrl = new VCtrl
  val csr = new VCsr
}

class VMacroOp extends VCtrlCsr {
  val robIdx = new RobPtr
  val destEew = UInt(3.W) // Destination EEW
  val emulVd = UInt(4.W) // EMUL of vd
  val emulVs2 = UInt(4.W)
  // val rs1 = UInt(xLen.W)  // scalar operand
  // val rs2 = UInt(xLen.W)  // scalar operand
}

class VUop extends VCtrlCsr {
  val robIdx = new RobPtr
  val uopIdx = UInt(3.W)
  val uopEnd = Bool()
}

class VEXUInput extends Bundle {
  val vuop = new VUop
  val vSrc = Vec(4, UInt(VLEN.W)) //vs1, vs2, old_vd, mask
  val rs1 = UInt(xLen.W)
}
