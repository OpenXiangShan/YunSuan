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
  val robEntry_dataDepth = 8 // EMUL > robEntry_dataDepth will cause error
}
import DebugRobParmas._

class DebugRob extends Module {
  val io = IO(new Bundle {
    val fromCtrl = Input(new FromCtrlToDebugRob)
    val commit = Output(ValidIO(new Bundle {
      val robIdx = new RobPtr
      val wrRf = Bool()
      val rfAddr = UInt(5.W)
      val emulVd = UInt(4.W)
      val data = Vec(robEntry_dataDepth, UInt(VLEN.W))
      val isStore = Bool()
      val veewVd = UInt(3.W)
      val isFp = Bool()
    }))
  })

  val robUop = Reg(Vec(robSize, new VUop))
  val robData = Reg(Vec(robSize, Vec(robEntry_dataDepth, UInt(VLEN.W))))
  val robValid = RegInit(VecInit.fill(robSize)(false.B))
  val robUopEnd = RegInit(VecInit.fill(robSize)(false.B))
  // val robWrRf = RegInit(VecInit.fill(robSize)(false.B))

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
  // Store指令，此处认为在发射时，就已经可以写回rob了。只有在uopEnd时才写回。
  when (io.fromCtrl.wbStore.valid && io.fromCtrl.wbStore.bits.uop.uopEnd) {
    for (i <- 0 until robSize) {
      when (enqOH_wbPort_store(i)) {
        robUop(i) := io.fromCtrl.wbStore.bits.uop
        robValid(i) := true.B
        robUopEnd(i) := true.B
        // robWrRf(i) := false.B
      }
    }
  }

  val wbExuUop = io.fromCtrl.wbExu.bits.uop
  // //---- 在当前指令的第一个有效的uop写回时，记录enqOH_wbPort_exu并保持。直到下一条指令的第一个有效uop写回时，再记录新的enqOH_wbPort_exu
  // val exu_wb_STATE = RegInit(false.B)
  // val enqOH_wbPort_exu_reg = Reg(chiselTypeOf(enqOH_wbPort_exu))
  // when (io.fromCtrl.wbExu.valid && wbExuUop.uopEnd) {
  //   exu_wb_STATE := false.B
  // }.elsewhen (io.fromCtrl.wbExu.valid && wbExuUop.ldestValUop) {
  //   exu_wb_STATE := true.B
  // }
  // // 当前指令的第一个有效的uop写回
  // val exu_wb_start = io.fromCtrl.wbExu.valid && wbExuUop.ldestValUop && exu_wb_STATE === false.B
  // when (exu_wb_start) {
  //   enqOH_wbPort_exu_reg := enqOH_wbPort_exu
  // }
  // val enqOH_wbPort_exu_final = Mux(exu_wb_start, enqOH_wbPort_exu, enqOH_wbPort_exu_reg)
  // // -------------

  
  // Exu uop写回，查询如果已有相同robIdx的uop写回，则更新已有entry。否则更新enqOH_wbPort_exu所指entry。
  when (io.fromCtrl.wbExu.valid) {
    // 和已有robIdx的所有entry比较
    val robIdx_match_OH_exu = robUop.zipWithIndex.map { case (uop, idx) => uop.robIdx === wbExuUop.robIdx && robValid(idx) }
    val robIdx_match_hit_exu = robIdx_match_OH_exu.reduce(_ || _)
    for (i <- 0 until robSize) {
      when(robIdx_match_hit_exu) { // 已有相同robIdx的uop写回
        when (robIdx_match_OH_exu(i) && wbExuUop.ldestValUop) {
          robData(i)(wbExuUop.ldestUop - wbExuUop.ctrl.ldest) := io.fromCtrl.wbExu.bits.vd
        }
        when (robIdx_match_OH_exu(i) && wbExuUop.uopEnd) {
          robUopEnd(i) := true.B
        }
      }.elsewhen (enqOH_wbPort_exu(i) && wbExuUop.ldestValUop) { // 没有相同robIdx的entry, 这是第一个uop写回
        robData(i)(wbExuUop.ldestUop - wbExuUop.ctrl.ldest) := io.fromCtrl.wbExu.bits.vd
        robUop(i) := wbExuUop
        robValid(i) := true.B
        when (wbExuUop.uopEnd) {
          robUopEnd(i) := true.B
        }
      }
    }
  }


  val wbLoadUop = io.fromCtrl.wbLoad.bits.uop
  // //---- 在当前指令的第一个有效的uop写回时，记录enqOH_wbPort_load并保持。直到下一条指令的第一个有效uop写回时，再记录新的enqOH_wbPort_load
  // val load_wb_STATE = RegInit(false.B)
  // val enqOH_wbPort_load_reg = Reg(chiselTypeOf(enqOH_wbPort_load))
  // when (io.fromCtrl.wbLoad.valid && wbLoadUop.ldestValUop) {
  //   load_wb_STATE := true.B
  // }.elsewhen (io.fromCtrl.wbLoad.valid && wbLoadUop.uopEnd) {
  //   load_wb_STATE := false.B
  // }
  // // 当前指令的第一个有效的uop写回
  // val load_wb_start = io.fromCtrl.wbLoad.valid && wbLoadUop.ldestValUop && load_wb_STATE === false.B
  // when (load_wb_start) {
  //   enqOH_wbPort_load_reg := enqOH_wbPort_load
  // }
  // val enqOH_wbPort_load_final = Mux(load_wb_start, enqOH_wbPort_load, enqOH_wbPort_load_reg)
  // // -------------

  // Load指令，uop写回，查询如果已有相同robIdx的uop写回，则更新已有entry。否则更新enqOH_wbPort_load所指entry。
  when (io.fromCtrl.wbLoad.valid) {
    // 和已有robIdx的所有entry比较
    val robIdx_match_OH_load = robUop.zipWithIndex.map { case (uop, idx) => uop.robIdx === wbLoadUop.robIdx && robValid(idx) }
    val robIdx_match_hit_load = robIdx_match_OH_load.reduce(_ || _)
    for (i <- 0 until robSize) {
      when(robIdx_match_hit_load) { // 已有相同robIdx的uop写回
        when (robIdx_match_OH_load(i) && wbLoadUop.ldestValUop) {
          robData(i)(wbLoadUop.ldestUop - wbLoadUop.ctrl.ldest) := io.fromCtrl.wbLoad.bits.vd
        }
        when (robIdx_match_OH_load(i) && wbLoadUop.uopEnd) {
          robUopEnd(i) := true.B
        }
      }.elsewhen (enqOH_wbPort_load(i) && wbLoadUop.ldestValUop) { // 没有相同robIdx的entry, 这是第一个uop写回
        robData(i)(wbLoadUop.ldestUop - wbLoadUop.ctrl.ldest) := io.fromCtrl.wbLoad.bits.vd
        robUop(i) := wbLoadUop
        robValid(i) := true.B
        when (wbLoadUop.uopEnd) {
          robUopEnd(i) := true.B
        }
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
  val globalRobIdx_match_OH = robUop zip robValid zip robUopEnd map { case ((uop, valid), uopEnd) =>
    uop.robIdx === deq_globalRobIdx && valid && uopEnd
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
        robUopEnd(i) := false.B
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
  io.commit.bits.veewVd := commitUop.veewVd
  io.commit.bits.isFp := commitUop.ctrl.fp

  io.commit.bits.robIdx := commitUop.robIdx
  dontTouch(io.commit.bits.robIdx)

} 