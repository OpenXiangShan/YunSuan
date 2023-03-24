package yunsuan.vector

import chisel3._
import chisel3.util._
import yunsuan.util._
import yunsuan.vector.vpermutil._


class VPermTop extends VPermModule {
    val io = IO(new VPermIO)

    val vslideup_module    = Module(new SlideUpLookupModule)
    val vslidedown_module  = Module(new SlideDownLookupModule)
    val vslide1up_module   = Module(new Slide1UpModule)
    val vslide1down_module = Module(new Slide1DownModule)
    val vrgather_module    = Module(new VRGatherLookupModule)
    val vcompress_module   = Module(new CompressModule)

    val vperm_fu_module = VecInit(Seq(vslideup_module.io, vslidedown_module.io, vslide1up_module.io, vslide1down_module.io, vrgather_module.io, vcompress_module.io))
    for(i <- 0 until 6) {
        vperm_fu_module(i).vs1      := io.vs1
        vperm_fu_module(i).vs1_type := io.vs1_type
        vperm_fu_module(i).vs2      := io.vs2
        vperm_fu_module(i).vs2_type := io.vs2_type
        vperm_fu_module(i).old_vd   := io.old_vd
        vperm_fu_module(i).vd_type  := io.vd_type
        vperm_fu_module(i).opcode   := io.opcode
        vperm_fu_module(i).uop_idx  := io.uop_idx
        vperm_fu_module(i).mask     := io.mask
        vperm_fu_module(i).vm       := io.vm
        vperm_fu_module(i).ta       := io.ta
        vperm_fu_module(i).ma       := io.ma
        vperm_fu_module(i).vstart   := io.vstart
        vperm_fu_module(i).vl       := io.vl
        vperm_fu_module(i).vlmul    := io.vlmul
    }

    io.res_vd := LookupTreeDefault(io.opcode(3,0), 0.U(VLEN.W), List(
        VPermType.vslideup    -> vslideup_module.io.res_vd,
        VPermType.vslidedown  -> vslidedown_module.io.res_vd,
        VPermType.vslide1up   -> vslide1up_module.io.res_vd,
        VPermType.vslide1down -> vslide1down_module.io.res_vd,
        VPermType.vrgather    -> vrgather_module.io.res_vd,
        VPermType.vrgatherrs1 -> vrgather_module.io.res_vd,
        VPermType.vcompress   -> vcompress_module.io.res_vd
    ))
}
