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
  val rs1 = UInt(XLEN.W)
  val rs2 = UInt(XLEN.W)
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
  val vfa = Bool()
  val vfma = Bool()
  val vfcvt = Bool()

  val widen = Bool()  // 2*sew = sew op sew  //Reduction not included
  val widen2 = Bool() // 2*sew = 2*sew op sew
  val narrow = Bool() // sew = 2*sew op sew
  val narrow_to_1 = Bool() // Compare, carry-out producing instructions
  def vv = !funct3(2) && !(funct3(1) && funct3(0))
  def vx = funct3(2) 
  def vi = !funct3(2) && funct3(1) && funct3(0) 
  def fuSel = Seq(alu, mul, fp, redu, mask, perm, div, vrg, vfred)
  def laneExu = arith && !crossLane
  def isLdst = load || store
  def vs1_imm = lsrc(0)
  def opi = funct3(0) === funct3(1) // OPIVV/X/I
  def opm = funct3(1, 0) === 2.U //OPMVV/X
  def opf = funct3(1, 0) === 1.U // OPFVV/F

  def vfdiv = fp && div
  def vrg = perm
  def vfred = fp && redu
}

class VCtrlCsr extends Bundle {
  val ctrl = new VCtrl
  val csr = new VCsr
}

class VMacroOp extends VCtrlCsr {
  val robIdx = new RobPtr
  val veewVd = UInt(3.W) // Destination EEW
  val emulVd = UInt(4.W) // EMUL of vd
  val emulVs2 = UInt(4.W)
  // val rs1 = UInt(XLEN.W)  // scalar operand
  // val rs2 = UInt(XLEN.W)  // scalar operand
}

class VUop extends VCtrlCsr {
  val robIdx = new RobPtr
  val veewVd = UInt(3.W) // Destination EEW
  val emulVd = Option.when(debugMode)(UInt(4.W))
  val uopIdx = UInt(3.W)
  val uopEnd = Bool()
  val lsrcUop = Vec(2, UInt(5.W)) //0: vs1   1: vs2
  val ldestUop = UInt(5.W)
  val lsrcValUop = Vec(3, Bool())
  val lmaskValUop = Bool()
  val ldestValUop = Bool()
}

class LdstCtrl extends Bundle {
  val unitStride = Bool()
  val mask = Bool()
  val strided = Bool()
  val indexed = Bool()
  val fof = Bool()
  val segment = Bool()
  val wholeReg = Bool()
  def idx_noSeg = indexed && !segment
}

object LdstDecoder {
  def apply(funct6: UInt, vs2: UInt): LdstCtrl = {
    val nf = funct6(5, 3)
    val mop = funct6(1, 0)
    val lumop = vs2
    val ctrl = Wire(new LdstCtrl)
    ctrl.unitStride := mop === 0.U
    ctrl.mask := lumop === "b01011".U && ctrl.unitStride
    ctrl.strided := mop === 2.U
    ctrl.indexed := mop(0)
    ctrl.fof := lumop === "b10000".U && ctrl.unitStride
    ctrl.segment := nf =/= 0.U && !ctrl.wholeReg
    ctrl.wholeReg := lumop === "b01000".U && ctrl.unitStride
    ctrl
  }  
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

class VExuInput extends Bundle {
  val uop = new VUop
  val vSrc = Vec(4, UInt(VLEN.W)) //vs1, vs2, old_vd, mask
  val rs1 = UInt(XLEN.W)
}


class VExuOutput extends Bundle {
  val uop = new VUop
  val vd  = UInt(VLEN.W)
  val fflags = Vec(VLEN/16, UInt(5.W))
}

// TODO: so far only unit-stride load/store is supported
class VLsuLoadReq extends Bundle {
  val uop = new VUop
  val ldstCtrl = new LdstCtrl
  val paddr = UInt(PAddrBits.W)
}

class VLsuLoadWb extends Bundle {
  val uop = new VUop
  val vd = UInt(VLEN.W) // Load data
}

class VLsuStoreReq extends Bundle {
  val uop = new VUop
  val ldstCtrl = new LdstCtrl
  val vs3 = UInt(VLEN.W) // Store data
  val paddr = UInt(PAddrBits.W)
}

class VLsuStoreAck extends Bundle {
  val uop = new VUop
}

class VL2LoadReq extends Bundle {
  val addr = Vec(nPortsL2, UInt(PAddrBits.W))
}

class VL2LoadRsp extends Bundle {
  val data = Vec(nPortsL2, UInt(CachelineBits.W))
}

class VL2StoreReq extends Bundle {
  val addr = Vec(nPortsL2, UInt(PAddrBits.W))
  val data = Vec(nPortsL2, UInt(CachelineBits.W))
}

class VL2StoreAck extends Bundle {
  val dummy = Bool()
}
