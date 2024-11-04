package yunsuan.vector

import chisel3._
import chisel3.util._
import yunsuan.util.{LiteralCat, SignExt, ZeroExt}
import yunsuan.util.Reflect.{getUIntMaxWidthOfObject, getUIntWidthOfObject}
import yunsuan.vector.Common._

import scala.language.{existentials, implicitConversions, postfixOps}

object VectorMergeEncode {
  implicit class BinaryStringHelper(private val sc: StringContext) extends AnyVal {
    def b(args: Any*): UInt = {
      val str: String = sc.standardInterpolator(x => x, args)
      require(str.forall(c => c == '0' || c == '1'))
      ("b" + str).U(str.length.W)
    }
  }

  object Type {
    val VMSIC      = b"00"
    val VEXT       = b"010"
    val O2V        = b"01100"
    val V2O        = b"01101"
    val VMV        = b"01110"
    val VMERGE     = b"01111"
    val VMERGE_UOP = b"1"

    def is(enum: Type.type => UInt)(uint: UInt): Bool = {
      uint.head(enum(this).getWidth) === enum(this)
    }

    lazy val width: Int = getUIntMaxWidthOfObject(this)
  }

  val vmerge_xfi = LiteralCat(Type.VMERGE, b"0")
  val vmerge_v   = LiteralCat(Type.VMERGE, b"1")

  // vmv.s.x, vfmv.s.f
  val vmv_s_xf   = LiteralCat(Type.O2V,    b"0")
  // vmv.v.x, vmv.v.i, vfmv.v.f
  val vmv_v_xfi  = LiteralCat(Type.O2V,    b"1")
  // vmv.x.s, vfmv.f.s
  val vmv_x_s    = LiteralCat(Type.V2O,    b"0")
  val vmv_f_s    = LiteralCat(Type.V2O,    b"1")

  val vzext_vf8  = LiteralCat(Type.VEXT,   b"010")
  val vzext_vf4  = LiteralCat(Type.VEXT,   b"100")
  val vzext_vf2  = LiteralCat(Type.VEXT,   b"110")

  val vsext_vf8  = LiteralCat(Type.VEXT,   b"011")
  val vsext_vf4  = LiteralCat(Type.VEXT,   b"101")
  val vsext_vf2  = LiteralCat(Type.VEXT,   b"111")

  val vid        = LiteralCat(Type.VMSIC,  b"0000")

  val vmv_v_v    = LiteralCat(Type.VMV,    b"0")
  // vmv1r.v, vmv2r.v, vmv4r.v, vmv8r.v
  val vmvnr_v    = LiteralCat(Type.VMV,    b"1")

  val vmerge_uop = LiteralCat(Type.VMERGE_UOP, b"00000")

  def is(enum: Type.type => UInt)(uint: UInt): Bool = Type.is(enum)(uint)

  lazy val width: Int = getUIntWidthOfObject(this)
}

class VectorMerge(val vlen: Int) extends Module with VectorConfig {
  val io = IO(new VectorMergeIO(vlen))

  val valid = io.in.valid
  val vs1 = io.in.bits.src1
  val vs2 = io.in.bits.src2
  val oldVd = io.in.bits.oldVd
  val srcMask = io.in.bits.srcMask
  val uopIdx = io.in.bits.uopIdx
  val vstart = io.in.bits.vstart
  val dveew = io.in.bits.dveew
  val eewOH = UIntToOH(dveew, 4)
  val op = io.in.bits.op
  val vl = io.in.bits.vl

  val vlIsZero = vl === 0.U

  val isTypeVMerge    = VectorMergeEncode.is(_.VMERGE)    (op) && valid
  val isTypeO2V       = VectorMergeEncode.is(_.O2V)       (op) && valid
  val isTypeV2O       = VectorMergeEncode.is(_.V2O)       (op) && valid
  val isTypeVEXT      = VectorMergeEncode.is(_.VEXT)      (op) && valid
  val isTypeVMV       = VectorMergeEncode.is(_.VMV)       (op) && valid
  val isTypeVMSIC     = VectorMergeEncode.is(_.VMSIC)     (op) && valid
  val isTypeMergeUop  = VectorMergeEncode.is(_.VMERGE_UOP)(op) && valid

  val isVMERGE_V    = isTypeVMerge && op === VectorMergeEncode.vmerge_v
  val isVMERGE_XIF  = isTypeVMerge && op === VectorMergeEncode.vmerge_xfi

  val isO2V_V       = isTypeO2V    && op === VectorMergeEncode.vmv_v_xfi
  val isO2V_S       = isTypeO2V    && op === VectorMergeEncode.vmv_s_xf

  val isV2X         = isTypeV2O    && op === VectorMergeEncode.vmv_x_s
  val isV2F         = isTypeV2O    && op === VectorMergeEncode.vmv_f_s

  val isVZEXT_VF8   = isTypeVEXT   && op === VectorMergeEncode.vzext_vf8
  val isVZEXT_VF4   = isTypeVEXT   && op === VectorMergeEncode.vzext_vf4
  val isVZEXT_VF2   = isTypeVEXT   && op === VectorMergeEncode.vzext_vf2
  val isVSEXT_VF8   = isTypeVEXT   && op === VectorMergeEncode.vsext_vf8
  val isVSEXT_VF4   = isTypeVEXT   && op === VectorMergeEncode.vsext_vf4
  val isVSEXT_VF2   = isTypeVEXT   && op === VectorMergeEncode.vsext_vf2

  val isVID         = isTypeVMSIC  && op === VectorMergeEncode.vid

  val isVMV_V_V     = isTypeVMV    && op === VectorMergeEncode.vmv_v_v
  val isVMVNR_V     = isTypeVMV    && op === VectorMergeEncode.vmvnr_v

  val mergeMod = Module(new VectorDataMergeUnit(vlen))

  val vs1E8Vec  = vs1.to8bitVec
  val vs1E16Vec = vs1.to16bitVec
  val vs1E32Vec = vs1.to32bitVec
  val vs1E64Vec = vs1.to64bitVec
  val vs2E8Vec  = vs2.to8bitVec
  val vs2E16Vec = vs2.to16bitVec
  val vs2E32Vec = vs2.to32bitVec
  val vs2E64Vec = vs2.to64bitVec
  val vs2Vf2Vec = vs2.toVf2Vec
  val vs2Vf4Vec = vs2.toVf4Vec
  val vs2Vf8Vec = vs2.toVf8Vec

  // vmv.s.x and vfmv.s.f
  val o2vVdE8  = Wire(VecE8)
  val o2vVdE16 = Wire(VecE16)
  val o2vVdE32 = Wire(VecE32)
  val o2vVdE64 = Wire(VecE64)
  val o2vVd    = Mux1H(eewOH, Seq(
    o2vVdE8.asUInt, o2vVdE16.asUInt, o2vVdE32.asUInt, o2vVdE64.asUInt
  ))

  o2vVdE8(0)  := vs1E8Vec(0)
  o2vVdE16(0) := vs1E16Vec(0)
  o2vVdE32(0) := vs1E32Vec(0)
  o2vVdE64(0) := vs1E64Vec(0)

  for (i <- 1 until VLENB) {
    o2vVdE8(i) := Mux1H(Seq(
      isO2V_S -> fill8b1s,
      isO2V_V -> vs1E8Vec(0),
    ))
  }

  for (i <- 1 until VLENB / 2) {
    o2vVdE16(i) := Mux1H(Seq(
      isO2V_S -> fill16b1s,
      isO2V_V -> vs1E16Vec(0),
    ))
  }

  for (i <- 1 until VLENB / 4) {
    o2vVdE32(i) := Mux1H(Seq(
      isO2V_S -> fill32b1s,
      isO2V_V -> vs1E32Vec(0),
    ))
  }

  for (i <- 1 until VLENB / 8) {
    o2vVdE64(i) := Mux1H(Seq(
      isO2V_S -> fill64b1s,
      isO2V_V -> vs1E64Vec(0),
    ))
  }

  val v2oData = Wire(UInt(64.W))

  val v2fData = Mux1H(eewOH, Seq(
    Cat(Fill(64 -  8, 1.U(1.W)),  vs2E8Vec(0)),
    Cat(Fill(64 - 16, 1.U(1.W)), vs2E16Vec(0)),
    Cat(Fill(64 - 32, 1.U(1.W)), vs2E32Vec(0)),
    vs2E64Vec(0),
  ))

  val v2xData = Mux1H(eewOH, Seq(
    vs2E8Vec(0),
    vs2E16Vec(0),
    vs2E32Vec(0),
    vs2E64Vec(0),
  ))

  v2oData := Mux1H(Seq(
    isV2F -> v2fData,
    isV2X -> v2xData,
  ))

  val vextVd = Wire(UIntVlen)
  val vextE16Vd = Wire(VecE16)
  val vextE32Vd = Wire(VecE32)
  val vextE64Vd = Wire(VecE64)

  val vextVf2Vs = WireInitFixedWidth(vs2Vf2Vec(uopIdx.take(1)))
  val vextVf4Vs = WireInitFixedWidth(vs2Vf4Vec(uopIdx.take(2)))
  val vextVf8Vs = WireInitFixedWidth(vs2Vf8Vec(uopIdx.take(3)))

  dontTouch(vextVf2Vs)
  dontTouch(vextVf4Vs)
  dontTouch(vextVf8Vs)

  vextE16Vd.zipWithIndex.foreach { case (ve16, i) =>
    ve16 := Mux1H(Seq(
      (isVZEXT_VF2 && eewOH(0)) -> ZeroExt(vextVf2Vs.to8bitVec(i), 16),
      (isVSEXT_VF2 && eewOH(1)) -> SignExt(vextVf2Vs.to8bitVec(i), 16),
    ))
  }

  vextE32Vd.zipWithIndex.foreach { case (ve32, i) =>
    ve32 := Mux1H(Seq(
      (isVZEXT_VF4 && eewOH(0)) -> ZeroExt(vextVf4Vs.to8bitVec(i),  32),
      (isVZEXT_VF2 && eewOH(1)) -> ZeroExt(vextVf2Vs.to16bitVec(i), 32),
      (isVSEXT_VF4 && eewOH(0)) -> SignExt(vextVf4Vs.to8bitVec(i),  32),
      (isVSEXT_VF2 && eewOH(1)) -> SignExt(vextVf2Vs.to16bitVec(i), 32),
    ))
  }

  vextE64Vd.zipWithIndex.foreach { case (ve64, i) =>
    ve64 := Mux1H(Seq(
      (isVZEXT_VF8 && eewOH(0)) -> ZeroExt(vextVf8Vs.to8bitVec(i),  64),
      (isVZEXT_VF4 && eewOH(1)) -> ZeroExt(vextVf4Vs.to16bitVec(i), 64),
      (isVZEXT_VF2 && eewOH(2)) -> ZeroExt(vextVf2Vs.to32bitVec(i), 64),
      (isVSEXT_VF8 && eewOH(0)) -> SignExt(vextVf8Vs.to8bitVec(i),  64),
      (isVSEXT_VF4 && eewOH(1)) -> SignExt(vextVf4Vs.to16bitVec(i), 64),
      (isVSEXT_VF2 && eewOH(2)) -> SignExt(vextVf2Vs.to32bitVec(i), 64),
    ))
  }

  vextVd := vextE16Vd.asUInt | vextE32Vd.asUInt | vextE64Vd.asUInt

  val vidVd = Wire(UIntVlen)

  val vidE8Vd  = WireInit(VecInit.tabulate(vlen / 1){ i => i.U( 8.W)}.toVf8Vec)
  val vidE16Vd = WireInit(VecInit.tabulate(vlen / 2){ i => i.U(16.W)}.toVf8Vec)
  val vidE32Vd = WireInit(VecInit.tabulate(vlen / 4){ i => i.U(32.W)}.toVf8Vec)
  val vidE64Vd = WireInit(VecInit.tabulate(vlen / 8){ i => i.U(64.W)}.toVf8Vec)

  vidVd := Mux1H(eewOH, Seq(
    vidE8Vd .asUInt,
    vidE16Vd.asUInt,
    vidE32Vd.asUInt,
    vidE64Vd.asUInt,
  ))

  val vdVmvVV = WireInit(vs1)
  val vdVmvNR = WireInit(vs2)

  val mergeVdIdx = io.in.bits.uopIdx
  val mergeNewVd = Mux1H(Seq(
    (isVMERGE_XIF) -> Mux1H(eewOH, Seq(
      Fill(vlen /  8, vs1.to8bitVec(0)),
      Fill(vlen / 16, vs1.to16bitVec(0)),
      Fill(vlen / 32, vs1.to32bitVec(0)),
      Fill(vlen / 64, vs1.to64bitVec(0)),
    )),
    (isVMERGE_V)   -> vs1,
    (isTypeO2V) -> o2vVd.asUInt,
    (isTypeVEXT) -> vextVd.asUInt,
    (isTypeVMSIC) -> vidVd.asUInt,
  ))
  val mergeOldVd = vs2
  val mergeModVl = Mux(isO2V_S, Mux(vl === 0.U, 0.U, 1.U), vl)
  val mergeModVm  = isTypeVMerge || isTypeO2V || isTypeVMV

  mergeMod.io.in.valid := isTypeVMerge || isTypeO2V || isTypeVEXT || isTypeVMSIC || isTypeMergeUop
  mergeMod.io.in.bits match {
    case bits =>
      bits.vdIdx    := mergeVdIdx
      bits.newVd    := mergeNewVd
      bits.oldVd    := mergeOldVd
      bits.srcMask  := io.in.bits.srcMask
      bits.vstart   := io.in.bits.vstart
      bits.vl       := mergeModVl
      bits.dveew    := io.in.bits.dveew
      bits.vm       := mergeModVm
      bits.vta      := io.in.bits.vta
      bits.vma      := io.in.bits.vma
  }

  io.out.valid := io.in.valid
  io.out.bits.vd := MuxCase(
    mergeMod.io.out.bits.vd,
    Seq(
      isVMVNR_V -> vdVmvNR,
      vlIsZero  -> oldVd,
      isVMV_V_V -> vdVmvVV,
    )
  )
  io.out.bits.fp := v2fData
  io.out.bits.gp := v2xData

  dontTouch(eewOH)
}

class VectorMergeIO(val vlen: Int) extends Bundle with VectorConfig {
  val in = Input(ValidIO(new Bundle {
    val uopIdx  = UInt(3.W)
    val src1    = UInt(vlen.W)
    val src2    = UInt(vlen.W)
    val srcMask = UInt(vlen.W)
    val vstart  = UInt((VlWidth - 1).W)
    val vl      = UInt(VlWidth.W)
    // data veew
    val dveew   = VSew()
    val vta     = Bool()
    val vma     = Bool()
    val op      = UInt(VectorMergeEncode.width.W)

    val oldVd   = UInt(vlen.W)
//    def oldVd   = src2
  }))
  val out = Output(ValidIO(new Bundle {
    val vd = UInt(vlen.W)
    val fp = UInt(64.W)
    val gp = UInt(64.W)
  }))
}

class VectorDataMergeUnit(val vlen: Int) extends Module with VectorConfig {
  val io = IO(new Bundle {
    val in = Input(ValidIO(new Bundle {
      val vdIdx   = UInt(3.W)
      val newVd   = UIntVlen
      val oldVd   = UIntVlen
      val srcMask = UIntVlen
      val vstart  = UInt(VstartWidth.W)
      val vl      = UInt(VlWidth.W)
      // data veew
      val dveew   = VSew()
      val vm      = Bool()
      val vta     = Bool()
      val vma     = Bool()
    }))
    val out = Output(ValidIO(new Bundle {
      val vd = UIntVlen
    }))
  })

  val valid   = io.in.valid
  val newVd   = io.in.bits.newVd
  val oldVd   = io.in.bits.oldVd
  val srcMask = io.in.bits.srcMask
  val vdIdx   = io.in.bits.vdIdx
  val vstart  = io.in.bits.vstart
  val dveew   = io.in.bits.dveew
  val eewOH   = UIntToOH(dveew, 4)
  val vl      = io.in.bits.vl
  val vm      = io.in.bits.vm
  val vta     = io.in.bits.vta
  val vma     = io.in.bits.vma

  val e8vl = (vl << dveew).asUInt.take(VlWidth)

  val e8vstart = (vstart << dveew).asUInt.take(VstartWidth)

  // These three masks are one-hot
  val prestartMask8b  = Wire(UInt(VLENB.W))
  val bodyMask8b      = Wire(UInt(VLENB.W))
  val tailMask8b      = Wire(UInt(VLENB.W))

  val e8Idxes = VecInit.tabulate(VLENB)(i => Cat(vdIdx, i.U(ElemIdxWidth.W)))

  prestartMask8b := Cat((0 until VLENB).map(i => e8Idxes(i)  < e8vstart                      ).reverse)
  bodyMask8b     := Cat((0 until VLENB).map(i => e8Idxes(i) >= e8vstart && e8Idxes(i) <= e8vl).reverse)
  tailMask8b     := Cat((0 until VLENB).map(i =>                           e8Idxes(i) >  e8vl).reverse)

  val srcMask2x = Wire(UInt(vlen.W))
  val srcMask4x = Wire(UInt(vlen.W))
  val srcMask8x = Wire(UInt(vlen.W))
  srcMask2x := Cat(srcMask.take(vlen / 2).asBools.map(b => Fill(2, b)).reverse)
  srcMask4x := Cat(srcMask.take(vlen / 4).asBools.map(b => Fill(4, b)).reverse)
  srcMask8x := Cat(srcMask.take(vlen / 8).asBools.map(b => Fill(8, b)).reverse)

  val srcMaskVlen = Mux1H(
    eewOH,
    Seq(
      srcMask,
      srcMask2x,
      srcMask4x,
      srcMask8x,
    )
  )

  val srcMask8b = Fill(VLENB, vm) & srcMaskVlen.toVf8Vec(vdIdx)

  val activeMask8b   = bodyMask8b & srcMask8b
  val inactiveMask8b = bodyMask8b & (~srcMask8b).asUInt

  val newVdE8Vec = newVd.to8bitVec
  val oldVdE8Vec = oldVd.to8bitVec
  val vdE8Vec = Wire(VecE8)

  val byte1s = "hff".U

  for (i <- 0 until VLENB) {
    vdE8Vec(i) := Mux1H(Seq(
      prestartMask8b(i)           -> oldVdE8Vec(i),
      activeMask8b(i)             -> newVdE8Vec(i),
      (inactiveMask8b(i) && vma)  -> byte1s,
      (inactiveMask8b(i) && !vma) -> oldVdE8Vec(i),
      (tailMask8b(i)     && vta)  -> byte1s,
      (tailMask8b(i)     && !vta) -> oldVdE8Vec(i),
    ))
  }

  io.out.bits.vd := vdE8Vec.asUInt
  io.out.valid := io.in.valid

  dontTouch(e8vl)
  dontTouch(e8vstart)
  dontTouch(prestartMask8b)
  dontTouch(activeMask8b)
  dontTouch(inactiveMask8b)
  dontTouch(tailMask8b)
  dontTouch(eewOH)
}

object VectorDataMergeUnitMain extends App {
  println("Generating the VectorDataMergeUnit hardware")
  emitVerilog(new VectorDataMergeUnit(128), Array("--target-dir", "build/vector"))
  println("done")
}

object VectorMergeMain extends App {
  println("Generating the VectorMerge hardware")
  emitVerilog(new VectorMerge(128), Array("--target-dir", "build/vector"))
  println("done")
}