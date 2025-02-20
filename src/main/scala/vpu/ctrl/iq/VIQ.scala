package race.vpu.ctrl

import chisel3._
import chisel3.util._
import race.vpu.VParams._
import race.vpu._
import utility.{CircularQueuePtr, HasCircularQueuePtrHelper}

class RS extends Bundle {
  val rs1 = UInt(XLEN.W)
  val rs2 = UInt(XLEN.W)
}

class VIQInput extends Bundle {
  val mop = new VMacroOp
  val rs = new RS
}
class VIQOutput extends VIQInput {
  val expdInfo = new ExpdInfo
}

class VIQPtr extends CircularQueuePtr[VIQPtr](VIQSize) 

class VIQ extends Module with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new VIQInput))
    val in_expdInfo = Input(new ExpdInfo)
    val flush = Input(Bool())
    val out = DecoupledIO(new VIQOutput)
  })

  val viq = Reg(Vec(VIQSize, new VMacroOp))
  val rs = Reg(Vec(VIQSize, new RS))
  val expdInfo = Reg(Vec(VIQSize, new ExpdInfo))
  val valid = RegInit(VecInit.fill(VIQSize)(false.B))

  val enqPtr = RegInit(0.U.asTypeOf(new VIQPtr))
  val deqPtr = RegInit(0.U.asTypeOf(new VIQPtr))
  assert(!(isFull(enqPtr, deqPtr) && io.in.valid), "Error: Vector Issue Queue is overflow !!!!")
  io.in.ready := !isFull(enqPtr, deqPtr)

  // Enq
  when (io.in.fire) {
    viq(enqPtr.value) := io.in.bits.mop
    rs(enqPtr.value) := io.in.bits.rs
    expdInfo(enqPtr.value) := io.in_expdInfo
    valid(enqPtr.value) := true.B
    enqPtr := enqPtr + 1.U
  }

  // Deq
  io.out.valid := !isEmpty(enqPtr, deqPtr) && !io.flush
  io.out.bits.mop := viq(deqPtr.value)
  io.out.bits.rs := rs(deqPtr.value)
  io.out.bits.expdInfo := expdInfo(deqPtr.value)
  when (io.out.fire) {
    valid(deqPtr.value) := false.B
    deqPtr := deqPtr + 1.U
  }

  // Flush
  //   when (io.flush) {
  //     enqPtr := 0.U.asTypeOf(new VIQPtr)
  //     deqPtr := 0.U.asTypeOf(new VIQPtr)
  //     valid.foreach(_ := false.B)
  //   }

}

