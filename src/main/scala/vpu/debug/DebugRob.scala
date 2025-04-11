package race.vpu.debug

import chisel3._
import chisel3.util._
import race.vpu.VParams._
import race.vpu._
import chisel3.util.log2Ceil // Ensure log2Ceil is imported

/**
  * Simplified debug ROB for writeback data
  * Caution: will overflow when the out-of-order ordering of 
  *          wb instrns exceeds the size of the ROB
  * Note: only support 2 ports writeback for now
  * 
  * Why not a complete ROB?
  *   To reduce the area of data (VLEN) registers
  */

object DebugRobParmas {
  val robSize = 12
  val robEntry_dataDepth = 2 // EMUL > robEntry_dataDepth will cause error
}
import DebugRobParmas._

class DebugRob extends Module {
  val io = IO(new Bundle {
    val fromCtrl = Input(new FromCtrlToDebugRob)
    val commit = Output(ValidIO(new Bundle {
      val wrRf = Bool()
      val rfAddr = UInt(5.W)
      val emulVd = UInt(4.W)
      val data = Vec(robEntry_dataDepth, UInt(VLEN.W))
      val isStore = Bool()
    }))
  })

  val robUop = Reg(Vec(robSize, new VUop))
  val robData = Reg(Vec(robSize, Vec(robEntry_dataDepth, UInt(VLEN.W))))
  val robValid = RegInit(VecInit.fill(robSize)(false.B))
//   val robUopEnd = RegInit(VecInit.fill(robSize)(false.B))
  val robWrRf = RegInit(VecInit.fill(robSize)(false.B))

  // Find a one-hot enqueue (writeback port 0)
  val enqOH_wbPort_exu = FindLowestOneHot(~(robValid.asUInt))
  assert(enqOH_wbPort_exu =/= 0.U, "Error: Debug ROB is full !!!!")
  // Find the second lowest set bit (writeback port 1)
  val enqOH_wbPort_load = FindLowestOneHot(~(robValid.asUInt) & ~enqOH_wbPort_exu)
  assert(enqOH_wbPort_load =/= 0.U, "Error: Debug ROB is full !!!!")
  // Find the third lowest set bit (store req)
  val enqOH_wbPort_store = FindLowestOneHot(~(robValid.asUInt) & ~enqOH_wbPort_exu & ~enqOH_wbPort_load)
  assert(enqOH_wbPort_store =/= 0.U, "Error: Debug ROB is full !!!!")

  /**
    * Enqueue
    */
  when (io.fromCtrl.wbStore.valid && io.fromCtrl.wbStore.bits.uop.uopEnd) {
    for (i <- 0 until robSize) {
      when (enqOH_wbPort_store(i)) {
        robUop(i) := io.fromCtrl.wbStore.bits.uop
        robValid(i) := true.B
        // robUopEnd(i) := true.B
        robWrRf(i) := false.B
      }
    }
  }

  val wbExuUop = io.fromCtrl.wbExu.bits.uop
  when (io.fromCtrl.wbExu.valid) {
    for (i <- 0 until robSize) {
      when (enqOH_wbPort_exu(i) && wbExuUop.ldestValUop) {
        robData(i)(wbExuUop.ldestUop - wbExuUop.ctrl.ldest) := io.fromCtrl.wbExu.bits.vd
      }
      when (enqOH_wbPort_exu(i) && wbExuUop.uopEnd) {
        robUop(i) := wbExuUop
        robValid(i) := true.B
        // robUopEnd(i) := true.B
        robWrRf(i) := wbExuUop.ctrl.ldestVal
      }
    }
  }

  val wbLoadUop = io.fromCtrl.wbLoad.bits.uop
  when (io.fromCtrl.wbLoad.valid) {
    for (i <- 0 until robSize) {
      when (enqOH_wbPort_load(i) && wbLoadUop.ldestValUop) {
        robData(i)(wbLoadUop.ldestUop - wbLoadUop.ctrl.ldest) := io.fromCtrl.wbLoad.bits.vd
      }
      when (enqOH_wbPort_load(i) && wbLoadUop.uopEnd) {
        robUop(i) := wbLoadUop
        robValid(i) := true.B
        // robUopEnd(i) := true.B
        robWrRf(i) := wbLoadUop.ctrl.ldestVal
      }
    }
  }

  /**
    * RobIdx FIFO : to ensure in-order commit of debugROB
    *   (Note: here robIdx is the global rob index, not the rob index of debugROB)
    */
  val globalRobIdxFIFO = Module(new Queue(new RobPtr, 64)) // Note: 64 is not a accurate value for FIFO size
  globalRobIdxFIFO.io.enq.valid := io.fromCtrl.issuedUopRobIdx.valid
  globalRobIdxFIFO.io.enq.bits := io.fromCtrl.issuedUopRobIdx.bits
  assert(!(!globalRobIdxFIFO.io.enq.ready && io.fromCtrl.issuedUopRobIdx.valid), "Error: globalRobIdxFIFO of debugROB is full !!!!")
  
  val deq_globalRobIdx = globalRobIdxFIFO.io.deq.bits
  val deq_globalRobIdxValid = globalRobIdxFIFO.io.deq.valid

  // CAM if global robIdx matched
  val globalRobIdx_match_OH = robUop zip robValid map { case (uop, valid) =>
    uop.robIdx === deq_globalRobIdx && valid
  }
  globalRobIdxFIFO.io.deq.ready := globalRobIdx_match_OH.reduce(_ || _)
  
  /**
    * Dequeue
    */
  val deqFire = globalRobIdxFIFO.io.deq.fire
  val commitUop = Reg(new VUop)
  val commitData = Reg(Vec(robEntry_dataDepth, UInt(VLEN.W)))
  when (deqFire) {
    commitUop := Mux1H(globalRobIdx_match_OH, robUop)
    commitData := Mux1H(globalRobIdx_match_OH, robData)
    for (i <- 0 until robSize) {
      when (globalRobIdx_match_OH(i)) {
        robValid(i) := false.B
      }
    }
  }

  io.commit.valid := RegNext(deqFire)
  io.commit.bits.wrRf := commitUop.ctrl.ldestVal
  io.commit.bits.rfAddr := commitUop.ctrl.ldest
  if (debugMode) {
    io.commit.bits.emulVd := commitUop.emulVd.get
  }
  io.commit.bits.data := commitData
  io.commit.bits.isStore := commitUop.ctrl.store

  //TODO: change dpi-c interface to tell the number of regs to be compared

} 