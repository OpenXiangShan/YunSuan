package race.vpu.yunsuan

import chisel3._
import chisel3.util._
import race.vpu._
import race.vpu.VParams._

class Vfa_setuop extends Bundle {
  val funct = UInt(9.W)
  val vm  = UInt(1.W)
  val vs1 = UInt(5.W)
  val vs2 = UInt(5.W)
  val op = UInt(5.W)

  def funct6 = funct(8, 3)
  def funct3 = funct(2, 0)
  def ivv = (funct3 === "b000".U)
  def ivx = (funct3 === "b100".U)
  def ivi = (funct3 === "b011".U)
  def mvv = (funct3 === "b010".U)
  def mvx = (funct3 === "b110".U)
  def fvv = (funct3 === "b001".U)
  def fvf = (funct3 === "b101".U)

    // without default
  def op_gen = Mux1H(Seq(
    (funct6 === Vfaddfunc6.fadd      & (fvv | fvf))                                -> VfaddOpCode.fadd,
    (funct6 === Vfaddfunc6.fsub      & (fvv | fvf))                                -> VfaddOpCode.fsub,
    (funct6 === Vfaddfunc6.fmin      & (fvv | fvf))                                -> VfaddOpCode.fmin,
    (funct6 === Vfaddfunc6.fmax      & (fvv | fvf))                                -> VfaddOpCode.fmax,
    (funct6 === Vfaddfunc6.fmerge    & (fvf) & (~vm)).asBool                       -> VfaddOpCode.fmerge,
    (funct6 === Vfaddfunc6.fmove     & (fvf) & (vm) & (vs2 === "b00000".U)).asBool -> VfaddOpCode.fmove,
    (funct6 === Vfaddfunc6.fsgnj     & (fvv | fvf))                                -> VfaddOpCode.fsgnj,
    (funct6 === Vfaddfunc6.fsgnjn    & (fvv | fvf))                                -> VfaddOpCode.fsgnjn,
    (funct6 === Vfaddfunc6.fsgnjx    & (fvv | fvf))                                -> VfaddOpCode.fsgnjx,
    (funct6 === Vfaddfunc6.feq       & (fvv | fvf))                                -> VfaddOpCode.feq,
    (funct6 === Vfaddfunc6.fne       & (fvv | fvf))                                -> VfaddOpCode.fne,
    (funct6 === Vfaddfunc6.flt       & (fvv | fvf))                                -> VfaddOpCode.flt,
    (funct6 === Vfaddfunc6.fle       & (fvv | fvf))                                -> VfaddOpCode.fle,
    (funct6 === Vfaddfunc6.fgt       & (fvf))                                      -> VfaddOpCode.fgt,
    (funct6 === Vfaddfunc6.fge       & (fvf))                                      -> VfaddOpCode.fge,
    (funct6 === Vfaddfunc6.fclass    & (fvv) & (vs1 === "b10000".U))               -> VfaddOpCode.fclass,
    (funct6 === Vfaddfunc6.fmv_f_s   & (fvv) & (vs1 === "b00000".U))               -> VfaddOpCode.fmv_f_s,
    (funct6 === Vfaddfunc6.fmv_s_f   & (fvf) & (vs2 === "b00000".U))               -> VfaddOpCode.fmv_s_f,
    (funct6 === Vfaddfunc6.fredsum_u & (fvv))                                      -> VfaddOpCode.fsum_ure ,
    (funct6 === Vfaddfunc6.fredmin   & (fvv))                                      -> VfaddOpCode.fmin_re  ,
    (funct6 === Vfaddfunc6.fredmax   & (fvv))                                      -> VfaddOpCode.fmax_re  ,
    (funct6 === Vfaddfunc6.fredsum_o & (fvv))                                      -> VfaddOpCode.fsum_ore ,
    (funct6 === Vfaddfunc6.dummy    )                                              -> VfaddOpCode.fminm, // reserved
    (funct6 === Vfaddfunc6.dummy    )                                              -> VfaddOpCode.fmaxm, // reserved
    (funct6 === Vfaddfunc6.dummy    )                                              -> VfaddOpCode.fleq,  // reserved
    (funct6 === Vfaddfunc6.dummy    )                                              -> VfaddOpCode.fltq   // reserved
    ))

}

// fma
class Vff_setuop extends Bundle {
  val funct = UInt(9.W)
  val vm  = UInt(1.W)
  val vs1 = UInt(5.W)
  val vs2 = UInt(5.W)
  val op = UInt(4.W)

  def funct6 = funct(8, 3)
  def funct3 = funct(2, 0)
  def fvv = (funct3 === "b001".U)
  def fvf = (funct3 === "b101".U)
  
  def op_gen = Mux1H(Seq(
    (funct6 === Vfffunc6.fmul    & (fvv  | fvf))       -> VfmaOpCode.vfmul,
    (funct6 === Vfffunc6.fmacc   & (fvv  | fvf))       -> VfmaOpCode.vfmacc ,
    (funct6 === Vfffunc6.fnmacc  & (fvv  | fvf))       -> VfmaOpCode.vfnmacc,
    (funct6 === Vfffunc6.fmsac   & (fvv  | fvf))       -> VfmaOpCode.vfmsac ,
    (funct6 === Vfffunc6.fnmsac  & (fvv  | fvf))       -> VfmaOpCode.vfnmsac,
    (funct6 === Vfffunc6.fmadd   & (fvv  | fvf))       -> VfmaOpCode.vfmadd ,
    (funct6 === Vfffunc6.fnmadd  & (fvv  | fvf))       -> VfmaOpCode.vfnmadd,
    (funct6 === Vfffunc6.fmsub   & (fvv  | fvf))       -> VfmaOpCode.vfmsub ,
    (funct6 === Vfffunc6.fnmsub  & (fvv  | fvf))       -> VfmaOpCode.vfnmsub
  ))

}

// 
class Vfd_setuop extends Bundle {
  val funct = UInt(9.W)
  val vm  = UInt(1.W)
  val vs1 = UInt(5.W)
  val vs2 = UInt(5.W)
  val op = UInt(1.W)

  def funct6 = funct(8, 3)
  def funct3 = funct(2, 0)
  def ivv = (funct3 === "b000".U)
  def ivx = (funct3 === "b100".U)
  def ivi = (funct3 === "b011".U)
  def mvv = (funct3 === "b010".U)
  def mvx = (funct3 === "b110".U)
  def fvv = (funct3 === "b001".U)
  def fvf = (funct3 === "b101".U)

  // TODO:  
  // val is_frs1 = Bool()
  // val is_frs2 = Bool()

  def op_gen = Mux(funct6 === Vfdfunc6.fsqrt, VfdOpCode.vfsqrt, VfdOpCode.vfdiv)
  
  def is_frs1 = op === funct6 === Vfdfunc6.fdiv  && fvf
  def is_frs2 = op === funct6 === Vfdfunc6.frdiv && fvf

}

class Vcvt_setuop extends Bundle {
  val funct = UInt(9.W)
  val vm  = UInt(1.W)
  val vs1 = UInt(5.W)
  val vs2 = UInt(5.W)
  val op = UInt(8.W)

  def funct6 = funct(8, 3)
  def funct3 = funct(2, 0)
  def ivv = (funct3 === "b000".U)
  def ivx = (funct3 === "b100".U)
  def ivi = (funct3 === "b011".U)
  def mvv = (funct3 === "b010".U)
  def mvx = (funct3 === "b110".U)
  def fvv = (funct3 === "b001".U)
  def fvf = (funct3 === "b101".U)
  
  //
  // val is_frs1 = Bool()
  // val is_frs2 = Bool()
  
  def op_gen = Mux1H(Seq(
  (funct6 === Vfcvtfunc6.vfncvt_xfw    & fvv & (vs1 === "b10001".U))   -> VfcvtOpCode.vfncvt_xfw,
  (funct6 === Vfcvtfunc6.vfcvt_xfv     & fvv & (vs1 === "b00001".U))   -> VfcvtOpCode.vfcvt_xfv,
  (funct6 === Vfcvtfunc6.vfwcvt_fxv    & fvv & (vs1 === "b01011".U))   -> VfcvtOpCode.vfwcvt_fxv,
  (funct6 === Vfcvtfunc6.vfcvt_fxv     & fvv & (vs1 === "b00011".U))   -> VfcvtOpCode.vfcvt_fxv,
  ))
  
}

class Vrg_setuop extends Bundle {
  val funct = UInt(9.W)
  val vm  = UInt(1.W)
  val vs1 = UInt(5.W)
  val vs2 = UInt(5.W)
  val op = UInt(1.W)

  def funct6 = funct(8, 3)
  def funct3 = funct(2, 0)
  def ivv = (funct3 === "b000".U)
  def ivx = (funct3 === "b100".U)
  def ivi = (funct3 === "b011".U)
  def mvv = (funct3 === "b010".U)
  def mvx = (funct3 === "b110".U)
  def fvv = (funct3 === "b001".U)
  def fvf = (funct3 === "b101".U)
  
  def op_gen = Mux1H(Seq(
  (funct6 === Vrgfunc6.vrgather   & (ivv | ivx | ivi))   -> VrgOpCode.vrgather,
  ))

}

// for vfred
class Vfred_setuop extends Bundle {
  val funct = UInt(9.W)
  val vm  = UInt(1.W)
  val vs1 = UInt(5.W)
  val vs2 = UInt(5.W)
  val op = UInt(5.W)

  def funct6 = funct(8, 3)
  def funct3 = funct(2, 0)
  def ivv = (funct3 === "b000".U)
  def ivx = (funct3 === "b100".U)
  def ivi = (funct3 === "b011".U)
  def mvv = (funct3 === "b010".U)
  def mvx = (funct3 === "b110".U)
  def fvv = (funct3 === "b001".U)
  def fvf = (funct3 === "b101".U)
  
  def op_gen = Mux1H(Seq(
  ((funct6 === Vfredfunc6.vfredmax )  && (fvv))   -> VfredOpCode.fmax,
  ((funct6 === Vfredfunc6.vfredusum)  && (fvv))   -> VfredOpCode.fadd
  ))

}

class Vfredctrl extends Bundle{
  val mask          = UInt(VLEN.W)
  val op_code       = UInt(5.W)
  val vs1           = UInt(XLEN.W)
  val uop           = new VUop
  val fire          = Bool()
} 


class VfredInput extends Vfredctrl{
  val vs2           = UInt(VLEN.W)
}

class VfredOutput extends Bundle{
  val uop           = new VUop
  val result        = UInt(XLEN.W)
  val fflags        = UInt(5.W)
}

