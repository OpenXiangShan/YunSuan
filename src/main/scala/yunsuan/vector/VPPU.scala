package yunsuan.vector

import chisel3._
import chisel3.util._
import yunsuan.{OpType, VipuType, VectorElementFormat}


class FToSIO extends Bundle {
  // in
  val in = new Bundle{
    val valid = Input(Bool())
    val src0 = Input(UInt(64.W))  // for f[rs1]
    val src1 = Input(UInt(128.W)) // for vd[other]

    val vstart = Input(UInt(64.W)) // If vstart ≥ vl, no operation is performed and the destination register is not updated.
    val vl = Input(UInt(8.W)) //
    val vsew = Input(UInt(3.W)) // SEW = 2^(3+vsew[2:0]) 0->8 1->16 11->64  larger bit width for reservation
    val vta = Input(Bool())
    val vma = Input(Bool())
    val vlmul = Input(UInt(3.W))
    val v0 = Input(UInt(128.W)) // mask
  }
  // out
  val out = new Bundle{
    val vd = Output(UInt(128.W))
  }
}

/**
  * vfmv.s.f vd, rs1  # vd[0] = f[rs1] (vs2=0)
  */
class FToSModule() extends Module {
  val io = IO(new FToSIO)
  assert(!(io.in.valid && (io.in.vsew === 0.U || io.in.vsew === 1.U)), "8 or 16 bits not supported in FToSModule")

  val vdOut0 = Mux1H(Seq(
    //    (io.in.vsew(1,0) === 0.U(2.W)) -> Cat(io.in.src1(127,8 ),io.in.src0(7 ,0)),
    //    (io.in.vsew(1,0) === 1.U(2.W)) -> Cat(io.in.src1(127,16),io.in.src0(15,0)),
    (io.in.vsew(1,0) === 2.U(2.W)) -> Cat(io.in.src1(127,32),io.in.src0(31,0)),
    (io.in.vsew(1,0) === 3.U(2.W)) -> Cat(io.in.src1(127,64),io.in.src0(63,0)),
  ))
  val vdOut1 = Mux1H(Seq(
    //    (io.in.vsew(1, 0) === 0.U(2.W)) -> Cat(Fill(120, 1.U(1.W)), io.in.src0(7 , 0)),
    //    (io.in.vsew(1, 0) === 1.U(2.W)) -> Cat(Fill(112, 1.U(1.W)), io.in.src0(15, 0)),
    (io.in.vsew(1, 0) === 2.U(2.W)) -> Cat(Fill(96 , 1.U(1.W)), io.in.src0(31, 0)),
    (io.in.vsew(1, 0) === 3.U(2.W)) -> Cat(Fill(64 , 1.U(1.W)), io.in.src0(63, 0)),
  ))

  io.out.vd := Mux(io.in.vstart < io.in.vl, Mux(io.in.vta, vdOut1, vdOut0), io.in.src1)
}
/**
  * vfslide1up.vf vd, vs2, rs1, vm        # vd[0]=f[rs1], vd[i+1] = vs2[i]
  * */
class Vfslide1upModule() extends Module(){
  val io = IO(new FToSIO)
  assert(!(io.in.valid && (io.in.vsew === 0.U || io.in.vsew === 1.U)), "8 or 16 bits not supported in Vfslide1upModule")

  val vsew_val = Mux1H(Seq(
    // (io.in.vsew === 0.U) -> 8.U,
    // (io.in.vsew === 1.U) -> 16.U,
    (io.in.vsew === 2.U) -> 32.U,
    (io.in.vsew === 3.U) -> 64.U,
  ))
  // for some mask
  val active_mask = Mux1H(Seq( // max 128bits
    // (io.in.vsew === 0.U) -> Cat(io.in.v0(7,0).asBools.reverse.map(x => Fill(8, x))),
    // (io.in.vsew === 1.U) -> Cat(io.in.v0(3,0).asBools.reverse.map(x => Fill(16, x))),
    (io.in.vsew === 2.U) -> Cat(Fill(32, io.in.v0(3)), Fill(32, io.in.v0(2)), Fill(32, io.in.v0(1)), Fill(32, io.in.v0(0))),
    (io.in.vsew === 3.U) -> Cat(Fill(64, io.in.v0(1)), Fill(64, io.in.v0(0))),
  ))
  assert(!(io.in.valid && (io.in.vstart=/=0.U || io.in.vl =/= 128.U/vsew_val)), "not support when vstart!=0 or vl!=")
  assert(!(io.in.valid && (io.in.vlmul =/= 0.U)), "not support when LMUL!=1")
  val body_mask = Fill(128 , 1.U(1.W)) // TODO
  val tail_mask = Fill(128 , 0.U(1.W)) // TODO

  // body
  val body = io.in.src1
  val body_result_temp = Mux1H(Seq(
    // (io.in.vsew === 0.U) -> Cat(body(119,0),io.in.src0(7,0)),
    // (io.in.vsew === 1.U) -> Cat(body(111,0),io.in.src0(15,0)),
    (io.in.vsew === 2.U) -> Cat(body(95,0),io.in.src0(31,0)),
    (io.in.vsew === 3.U) -> Cat(body(63,0),io.in.src0(63,0)),
  ))
  val result_vtm0 = (body_result_temp & body_mask & active_mask) | (io.in.src1 & body_mask & (~active_mask).asUInt) // vma=0

  val result_vtm1 = (body_result_temp & body_mask & active_mask) | (Fill(128, 1.U(1.W)) & body_mask & (~active_mask).asUInt) // vma=1

  val resutl_vtm = Mux(io.in.vma, result_vtm1, result_vtm0)

  // tail
  val tail = io.in.src1
  val tail_result = Mux1H(Seq(
//    (io.in.vsew === 0.U) -> Cat(body(119, 0), io.in.src0(7, 0)),
//    (io.in.vsew === 1.U) -> Cat(body(111, 0), io.in.src0(15, 0)),
    (io.in.vsew === 2.U) -> Cat(tail(95, 0), io.in.src0(31, 0)),
    (io.in.vsew === 3.U) -> Cat(tail(63, 0), io.in.src0(63, 0)),
  ))

  val result_vta0 = tail & tail_mask // vta=0
  val result_vta1 = (Fill(128, 1.U(1.W)) & tail_mask) | (tail & (~tail_mask).asUInt)  // vta=1
  val result_vta = Mux(io.in.vta, result_vta1, result_vta0)

  // body + tail
  val result = (resutl_vtm &  body_mask) | (result_vta & tail_mask)

  // out
  io.out.vd := result
}
