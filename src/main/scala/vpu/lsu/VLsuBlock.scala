package race.vpu

import chisel3._
import chisel3.util._
import VParams._

class VLsuBlock extends Module {
  val io = IO(new Bundle {
    val ctrl = new Bundle {
      val loadReq = Flipped(DecoupledIO(new VLsuLoadReq))
      val loadWb = ValidIO(new VLsuLoadWb)
      val storeReq = Flipped(DecoupledIO(new VLsuStoreReq))
      val storeAck = ValidIO(new VLsuStoreAck)
    }
    val l2 = new Bundle {
      val loadReq = DecoupledIO(new VL2LoadReq)
      val loadRsp = Input(ValidIO(new VL2LoadRsp))
      val storeReq = DecoupledIO(new VL2StoreReq)
      val storeAck = Input(ValidIO(new VL2StoreAck))
    }
  })

  /**
    * Load Pipe
    */
  // Create a FIFO with 32 entries to store uops from load requests
  val loadUopQueue = Module(new Queue(new VUop, 32))
  // Load Request - uop enqueue
  loadUopQueue.io.enq.valid := io.ctrl.loadReq.valid
  loadUopQueue.io.enq.bits := io.ctrl.loadReq.bits.uop
  // Load Request
  io.ctrl.loadReq.ready := io.l2.loadReq.ready && loadUopQueue.io.enq.ready
  io.l2.loadReq.valid := io.ctrl.loadReq.valid && loadUopQueue.io.enq.ready
  io.l2.loadReq.bits.addr(0) := io.ctrl.loadReq.bits.paddr
  for (i <- 1 until nPortsL2) {
    io.l2.loadReq.bits.addr(i) := io.ctrl.loadReq.bits.paddr + (i * (vlenb / nPortsL2)).U
  }
  
  // Load Response - uop dequeue
  loadUopQueue.io.deq.ready := io.l2.loadRsp.valid
  // Load Response
  io.ctrl.loadWb.valid := io.l2.loadRsp.valid
  io.ctrl.loadWb.bits.vd := io.l2.loadRsp.bits.data.asUInt
  io.ctrl.loadWb.bits.uop := loadUopQueue.io.deq.bits

  /**
    * Store Pipe
    */
  // Create a FIFO with 32 entries to store uops from store requests
  val storeUopQueue = Module(new Queue(new VUop, 32))
  // Store Request - uop enqueue
  storeUopQueue.io.enq.valid := io.ctrl.storeReq.valid
  storeUopQueue.io.enq.bits := io.ctrl.storeReq.bits.uop
  // Store Request
  io.ctrl.storeReq.ready := io.l2.storeReq.ready && storeUopQueue.io.enq.ready
  io.l2.storeReq.valid := io.ctrl.storeReq.valid && storeUopQueue.io.enq.ready
  io.l2.storeReq.bits.addr(0) := io.ctrl.storeReq.bits.paddr
  for (i <- 1 until nPortsL2) {
    io.l2.storeReq.bits.addr(i) := io.ctrl.storeReq.bits.paddr + (i * (vlenb / nPortsL2)).U
  }
  for (i <- 0 until nPortsL2) {
    io.l2.storeReq.bits.data(i) := UIntSplit(io.ctrl.storeReq.bits.vs3, CachelineBits)(i)
  }
  
  // Store Response - uop dequeue
  storeUopQueue.io.deq.ready := io.l2.storeAck.valid
  // Store Response
  io.ctrl.storeAck.valid := io.l2.storeAck.valid
  io.ctrl.storeAck.bits.uop := storeUopQueue.io.deq.bits
}
