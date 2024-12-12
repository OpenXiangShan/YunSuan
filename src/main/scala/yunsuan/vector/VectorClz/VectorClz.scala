package yunsuan.vector.VectorClz

import chisel3._
import chisel3.util._
import yunsuan.vector._
import yunsuan.vector.VectorConvert.util.CLZ

class VClzInputIO extends Bundle {
  val fire = Bool()
  val opcode = new VClzOpcode
  val vsew = UInt(2.W)
  val vs2 = UInt(64.W)
}

class VClzOutputIO extends Bundle {
  val vd = UInt(64.W)
}

class VectorClz extends Module {
  val io = IO(new Bundle {
    val in = Input(new VClzInputIO())
    val out = Output(new VClzOutputIO())
  })

  val fire = io.in.fire
  val opcode = io.in.opcode
  val vsew = io.in.vsew
  val vs2 = io.in.vs2

  val vs2Clz = Wire(Vec(8, UInt(8.W)))
  val vs2Ctz = Wire(Vec(8, UInt(8.W)))
  val vs2Tmp = Wire(Vec(8, UInt(8.W)))
  val clz = Wire(Vec(8, UInt(4.W)))
  val eleOR = Wire(Vec(8, Bool()))

  vs2Clz := vs2.asTypeOf(vs2Clz)

  vs2.asTypeOf(vs2Ctz).zipWithIndex.foreach { case(clz, i) =>
    vs2Ctz(i) := VecInit(clz.asBools.reverse).asUInt
  }

  vs2Tmp := Mux1H(Seq(
    opcode.isClz -> vs2Clz,
    opcode.isCtz -> vs2Ctz,
  ))

  for (i <- 0 until 8) {
    eleOR(i) := vs2Tmp(i).orR
    clz(i) := CLZ(vs2Tmp(i))
  }

  val ele16OR = Wire(Vec(4, UInt(2.W)))
  val ele32OR = Wire(Vec(2, UInt(4.W)))


  ele16OR := eleOR.asTypeOf(ele16OR)
  ele32OR := eleOR.asTypeOf(ele32OR)

  val clz16Tmp = Wire(Vec(4, Bool()))
  val clz32Tmp = Wire(Vec(2, UInt(3.W)))
  val clz64Tmp = Wire(UInt(4.W))

  for (i <- 0 until 4) {
    clz16Tmp(i) := ele16OR(i)(1)
  }
  for (i <- 0 until 2) {
    clz32Tmp(i) := CLZ(ele32OR(i))
  }
  clz64Tmp := CLZ(eleOR)

  val vsewReg = RegEnable(vsew, fire)
  val eewVdReg = SewOH(vsewReg)

  val clzReg = RegEnable(clz, fire)
  val clz16TmpReg = RegEnable(clz16Tmp, fire)
  val clz32TmpReg = RegEnable(clz32Tmp, fire)
  val clz64TmpReg = RegEnable(clz64Tmp, fire)

  val clz8  = Wire(Vec(8, UInt(8.W)))
  val clz16 = Wire(Vec(4, UInt(16.W)))
  val clz32 = Wire(Vec(2, UInt(32.W)))
  val clz64 = Wire(UInt(64.W))

  for (i <- 0 until 8) {
    clz8(i) := clzReg(i)
  }

  for (i <- 0 until 4) {
    clz16(i) := Mux(clz16TmpReg(i), clzReg(i*2+1), clzReg(i*2) + 8.U)
  }

  for (i <- 0 until 2) {
    clz32(i) := MuxCase(clz32TmpReg(i), Seq(
      (clz32TmpReg(i) === 0.U) -> clzReg(i*4+3),
      (clz32TmpReg(i) === 1.U) -> (clzReg(i*4+2) + 8.U),
      (clz32TmpReg(i) === 2.U) -> (clzReg(i*4+1) + 16.U),
      (clz32TmpReg(i) === 3.U || clz32TmpReg(i) === 4.U) -> (clzReg(i*4) + 24.U),
    ))
  }

  clz64 := MuxCase(clz64TmpReg, Seq(
    (clz64TmpReg === 0.U) -> clzReg(7),
    (clz64TmpReg === 1.U) -> (clzReg(6) + 8.U),
    (clz64TmpReg === 2.U) -> (clzReg(5) + 16.U),
    (clz64TmpReg === 3.U) -> (clzReg(4) + 24.U),
    (clz64TmpReg === 4.U) -> (clzReg(3) + 32.U),
    (clz64TmpReg === 5.U) -> (clzReg(2) + 40.U),
    (clz64TmpReg === 6.U) -> (clzReg(1) + 48.U),
    (clz64TmpReg === 7.U || clz64TmpReg === 8.U) -> (clzReg(0) + 56.U),
  ))


  val result = Wire(UInt(64.W))
  result := Mux1H(Seq(
    eewVdReg.is8 -> clz8.asUInt,
    eewVdReg.is16 -> clz16.asUInt,
    eewVdReg.is32 -> clz32.asUInt,
    eewVdReg.is64 -> clz64,
  ))

  io.out.vd := result
}
