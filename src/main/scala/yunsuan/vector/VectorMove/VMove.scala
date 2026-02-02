package yunsuan.vector.VectorMove

import chisel3._
import chisel3.util._
import yunsuan.VMoveOpcode
import yunsuan.VMoveOpcode._
import yunsuan.vector.alu.VSew._
import yunsuan.vector.BitsExtend
import yunsuan.vector.Common.SewOH

class VMoveInfo extends Bundle {
  val vm = Bool()
  val vsew = UInt(2.W)
}

class VectorMoveInputBundle(vlen: Int) extends Bundle {
  val opcode = VMoveOpcode()
  val info = new VMoveInfo
  val vs2 = UInt(vlen.W)
  val vs1 = UInt(vlen.W)
  val mask = UInt(16.W)
}

class VectorMoveOutputBundle(vlen: Int) extends Bundle {
  val vd = UInt(vlen.W)
}

class VectorMove extends Module {
  private val VLEN = 128
  private val numBytes = VLEN / 8
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(new VectorMoveInputBundle(VLEN)))
    val out = ValidIO(new VectorMoveOutputBundle(VLEN))
  })
  val valid = io.in.valid
  val vsew = io.in.bits.info.vsew
  val vm = io.in.bits.info.vm
  val vs2 = io.in.bits.vs2
  val vs1 = io.in.bits.vs1
  val mask = io.in.bits.mask
  implicit val opcode = io.in.bits.opcode

  val eewVd = SewOH(vsew)

  // Integer Merge/Move, vmv.s.x
  // Floating-Point Merge/Move, vfmv.s.f
  val vmaskAdjust = Mux1H(eewVd.oneHot, Seq(1, 2, 4, 8).map(k =>
    Cat(Seq.tabulate(numBytes/k)(i => Fill(k, mask(i))).reverse)
  ))
  val vmergeTmp = Wire(Vec(numBytes, UInt(8.W)))
  for (i <- 0 until numBytes) {
    vmergeTmp(i) := Mux(vmaskAdjust(i), vs1(8*i+7, 8*i), vs2(8*i+7, 8*i))
  }
  val vmergeResult = Wire(UInt(VLEN.W))
  vmergeResult := Mux(vm || isX2VS, vs1, vmergeTmp.asUInt)

  // vmv.x.s, vfmv.f.s
  val vmvResult = Wire(UInt(64.W))
  vmvResult := Mux1H(Seq(
    (vsew === e8)  -> BitsExtend(vs2( 7, 0), 64, true.B),
    (vsew === e16) -> BitsExtend(vs2(15, 0), 64, true.B),
    (vsew === e32) -> BitsExtend(vs2(31, 0), 64, true.B),
    (vsew === e64) -> vs2(63, 0)
  ))

  // vmv<nr>r.v
  val vmvnrResult = Wire(UInt(VLEN.W))
  vmvnrResult := vs2

  val vd = Wire(UInt(VLEN.W))
  vd := Mux(isVS2X, vmvResult, Mux(isNR, vmvnrResult, vmergeResult))

  io.out.valid := valid
  io.out.bits.vd := vd
}
