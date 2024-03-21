
package yunsuan.vectortest.perm

import chisel3._
import chisel3.util._
import yunsuan.vector.{VIFuInfo, VPermInput, VIFuOutput}
import yunsuan.vector.perm.{Permutation}

class VPermWrapper extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VPermInput))
    val out = Decoupled(new VIFuOutput)
  })

  val vPerm = Module(new Permutation)
  vPerm.io.in.bits := io.in.bits
  io.out.bits.vd := vPerm.io.out.vd
  io.out.bits.vxsat := vPerm.io.out.vxsat


  vPerm.io.in.valid := io.in.valid
  io.out.valid := RegNext(RegNext(io.in.valid))
  io.in.ready := io.out.ready
}
