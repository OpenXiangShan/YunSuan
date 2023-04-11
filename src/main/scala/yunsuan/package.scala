import chisel3._
import chisel3.util._

package object yunsuan {
  def OpTypeWidth: Int = 8

  object OpType {
    def apply() = UInt(OpTypeWidth.W)
  }

  object VipuType0 {
    def dummy                          = "b00001111".U(OpTypeWidth.W) // exu not implemented
    def add                            = "b00000000".U(OpTypeWidth.W) // src1 + src2 //vadd
    def sub                            = "b00000010".U(OpTypeWidth.W) // src1 - src2 //vsub
    def adc                            = "b00000001".U(OpTypeWidth.W) // src1 + src2 + carry //vadc
    def subBorrow                      = "b00000011".U(OpTypeWidth.W) // src1 + borrow - src2 //vsbc vmsbc
    def unsignedWideningAdd            = "b00000100".U(OpTypeWidth.W) //vwaddu
    def unsignedWideningsub            = "b00000101".U(OpTypeWidth.W) //vwsubu
    def signedWideningAdd              = "b00000110".U(OpTypeWidth.W) //vwadd
    def signedWideningSub              = "b00000111".U(OpTypeWidth.W) //vwsub
    def unsignedWideningAddIn0Widening = "b00001000".U(OpTypeWidth.W) //vwaddu
    def unsignedWideningSubIn0Widening = "b00001001".U(OpTypeWidth.W) //vwsubu
    def signedWideningAddIn0Widening   = "b00001010".U(OpTypeWidth.W) //vwadd
    def signedWideningSubIn0Widening   = "b00001011".U(OpTypeWidth.W) //vwsub
    def maxUnsigned                    = "b00001100".U(OpTypeWidth.W) //vmaxu
    def minUnsigned                    = "b00001101".U(OpTypeWidth.W) //vminu
    def maxSigned                      = "b00001110".U(OpTypeWidth.W) //vmax
    def minSigned                      = "b00001111".U(OpTypeWidth.W) //vmin
    def equal                          = "b00010000".U(OpTypeWidth.W) //vmseq
    def notEqual                       = "b00010001".U(OpTypeWidth.W) //vmsne
    def lessThanUnsigned               = "b00010010".U(OpTypeWidth.W) //vmsltu
    def lessThanSigned                 = "b00010011".U(OpTypeWidth.W) //vmslt
    def lessThanOrEqualUnsigned        = "b00010100".U(OpTypeWidth.W) //vmsleu
    def lessThanOrEqualSigned          = "b00010101".U(OpTypeWidth.W) //vmsle
    def greaterThanUnsigned            = "b00010110".U(OpTypeWidth.W) //vmsgtu
    def greaterThanSigned              = "b00010111".U(OpTypeWidth.W) //vmsgt
    def greaterThanOrEqualUnsigned     = "b00011000".U(OpTypeWidth.W) //vmsgeu
    def greaterThanOrEqualSigned       = "b00011001".U(OpTypeWidth.W) //vmsge
    def bitwiseLogicalAnd              = "b00011010".U(OpTypeWidth.W) //vand
    def bitwiseLogicalNand             = "b00011011".U(OpTypeWidth.W) //vnand
    def bitwiseLogicalAndn             = "b00011100".U(OpTypeWidth.W) //vandn
    def bitwiseLogicalOr               = "b00011101".U(OpTypeWidth.W) //vor
    def bitwiseLogicalNor              = "b00011110".U(OpTypeWidth.W) //vnor
    def bitwiseLogicalOrn              = "b00011111".U(OpTypeWidth.W) //vorn
    def bitwiseLogicalXor              = "b00100000".U(OpTypeWidth.W) //vxor
    def bitwiseLogicalXnor             = "b00100001".U(OpTypeWidth.W) //vxnor
    def shiftLeftLogical               = "b00100010".U(OpTypeWidth.W) //vsll
    def shiftRightLogical              = "b00100011".U(OpTypeWidth.W) //vsrl
    def shiftRightArithmetic           = "b00100100".U(OpTypeWidth.W) //vsra
    def scalingShiftRightLogical       = "b00100101".U(OpTypeWidth.W) //vssrl
    def scalingShiftRightArithmetic    = "b00100110".U(OpTypeWidth.W) //vssra
    def rsub                           = "b00100111".U(OpTypeWidth.W) // src2 - src1 //vrsub
    def madc                           = "b00101000".U(OpTypeWidth.W) // src1 + src2 + carry // vmadc.vvm
    def madc0                          = "b00101001".U(OpTypeWidth.W) // src1 + src2 + carry // vmadc.vv
    // TODO: other op and method
    // TODO: other op and method
    def needReverse(vipuType: UInt) = vipuType === sub // TODO : Modify the internal logic of VectorIntAdder to deal with this dirty code
    def outIsCarry(vipuType: UInt) = vipuType === madc || vipuType === madc0
  }


  object VialuFixType {
    // format:2bits   sign:1bit    opcode:5bits
    def dummy                          = "b01111111".U(OpTypeWidth.W) // exu not implemented
    def vadd_vv                        = "b00000000".U(OpTypeWidth.W) // vd[i] = vs2[i] + vs1[i] vadd
    def vsub_vv                        = "b00000001".U(OpTypeWidth.W) // vd[i] = vs2[i] - vs1[i] vsub
    def vrsub_vv                       = "b11000001".U(OpTypeWidth.W) // vd[i] = vs1[i] - vs2[i] vsub
    def vwaddu_vv                      = "b01000000".U(OpTypeWidth.W) // vadd
    def vwsubu_vv                      = "b01000001".U(OpTypeWidth.W) // vsub
    def vwadd_vv                       = "b01100000".U(OpTypeWidth.W) // vadd
    def vwsub_vv                       = "b01100001".U(OpTypeWidth.W) // vsub
    def vwaddu_wv                      = "b10000000".U(OpTypeWidth.W) // vadd
    def vwsubu_wv                      = "b10000001".U(OpTypeWidth.W) // vsub
    def vwadd_wv                       = "b10100000".U(OpTypeWidth.W) // vadd
    def vwsub_wv                       = "b10100001".U(OpTypeWidth.W) // vsub
    def vzext_vf2                      = "b00000010".U(OpTypeWidth.W) // vext
    def vsext_vf2                      = "b00100010".U(OpTypeWidth.W) // vext
    def vzext_vf4                      = "b01000010".U(OpTypeWidth.W) // vext
    def vsext_vf4                      = "b01100010".U(OpTypeWidth.W) // vext
    def vzext_vf8                      = "b10000010".U(OpTypeWidth.W) // vext
    def vsext_vf8                      = "b10100010".U(OpTypeWidth.W) // vext
    def vadc_vvm                       = "b00000011".U(OpTypeWidth.W) // vadc
    def vmadc_vvm                      = "b01100100".U(OpTypeWidth.W) // vmadc
    def vmadc_vv                       = "b01000100".U(OpTypeWidth.W) // vmadc
    def vsbc_vvm                       = "b00000101".U(OpTypeWidth.W) // vsbc
    def vmsbc_vvm                      = "b01100110".U(OpTypeWidth.W) // vmsbc
    def vmsbc_vv                       = "b01000110".U(OpTypeWidth.W) // vmsbc
    def vand_vv                        = "b00000111".U(OpTypeWidth.W) // vand
    def vor_vv                         = "b00001011".U(OpTypeWidth.W) // vor
    def vxor_vv                        = "b00001010".U(OpTypeWidth.W) // vxor
    def vsll_vv                        = "b00001111".U(OpTypeWidth.W) // vsll
    def vsrl_vv                        = "b00010000".U(OpTypeWidth.W) // vsrl
    def vsra_vv                        = "b00010001".U(OpTypeWidth.W) // vsra
    def vnsrl_wv                       = "b11010000".U(OpTypeWidth.W) // vsrl
    def vnsra_wv                       = "b11010001".U(OpTypeWidth.W) // vsra
    def vmseq_vv                       = "b01010010".U(OpTypeWidth.W) // vmseq
    def vmsne_vv                       = "b01010011".U(OpTypeWidth.W) // vmsne
    def vmsltu_vv                      = "b01010100".U(OpTypeWidth.W) // vmslt
    def vmslt_vv                       = "b01110100".U(OpTypeWidth.W) // vmslt
    def vmsleu_vv                      = "b01010101".U(OpTypeWidth.W) // vmsle
    def vmsle_vv                       = "b01110101".U(OpTypeWidth.W) // vmsle
    def vmsgtu_vv                      = "b01010110".U(OpTypeWidth.W) // vmsgt
    def vmsgt_vv                       = "b01110110".U(OpTypeWidth.W) // vmsgt
    def vminu_vv                       = "b00010111".U(OpTypeWidth.W) // vmin
    def vmin_vv                        = "b00110111".U(OpTypeWidth.W) // vmin
    def vmaxu_vv                       = "b00011000".U(OpTypeWidth.W) // vmax
    def vmax_vv                        = "b00111000".U(OpTypeWidth.W) // vmax
    def vmerge_vvm                     = "b00011001".U(OpTypeWidth.W) // vmerge
    def vmv_v_v                        = "b00011010".U(OpTypeWidth.W) // vmv
    def vsaddu_vv                      = "b00011011".U(OpTypeWidth.W) // vsadd
    def vsadd_vv                       = "b00111011".U(OpTypeWidth.W) // vsadd
    def vssubu_vv                      = "b00011100".U(OpTypeWidth.W) // vssub
    def vssub_vv                       = "b00111100".U(OpTypeWidth.W) // vssub
    def vaaddu_vv                      = "b00011101".U(OpTypeWidth.W) // vaadd
    def vaadd_vv                       = "b00111101".U(OpTypeWidth.W) // vaadd
    def vasubu_vv                      = "b00011110".U(OpTypeWidth.W) // vasub
    def vasub_vv                       = "b00111110".U(OpTypeWidth.W) // vasub
    def vssrl_vv                       = "b00011111".U(OpTypeWidth.W) // vssrl
    def vssra_vv                       = "b00111111".U(OpTypeWidth.W) // vssra
    def vnclipu_wv                     = "b11011111".U(OpTypeWidth.W) // vssrl --- 
    def vnclip_wv                      = "b11111111".U(OpTypeWidth.W) // vssra
    def vmand_mm                       = "b10000111".U(OpTypeWidth.W) // vand
    def vmnand_mm                      = "b10001000".U(OpTypeWidth.W) // vnand
    def vmandn_mm                      = "b10001001".U(OpTypeWidth.W) // vandn
    def vmxor_mm                       = "b10001010".U(OpTypeWidth.W) // vxor
    def vmor_mm                        = "b10001011".U(OpTypeWidth.W) // vor
    def vmnor_mm                       = "b10001100".U(OpTypeWidth.W) // vnor
    def vmorn_mm                       = "b10001101".U(OpTypeWidth.W) // vorn
    def vmxnor_mm                      = "b10001110".U(OpTypeWidth.W) // vxnor

    private def getOpcodeGeneral(fuOpType: UInt) = fuOpType(4,0)
    def getOpcode(fuOpType: UInt) = Mux(fuOpType(5,0) === vssra_vv(5,0), "b100000".U, Cat(0.U(1.W), getOpcodeGeneral(fuOpType))) // dirty code for opcode of vssra
    def getSrcVdType(fuOpType: UInt, sew:UInt) = {
      val isSpecificOpcode = (getOpcodeGeneral(fuOpType) === getOpcodeGeneral(vssra_vv) 
                          || getOpcodeGeneral(fuOpType) === getOpcodeGeneral(vmadc_vvm)
                          || getOpcodeGeneral(fuOpType) === getOpcodeGeneral(vmsbc_vvm))
      val sign = Mux(isSpecificOpcode, 0.U(1.W), fuOpType(5)) // dirty code for opcode of vssra vmadc vmsbc
      val Sew = Cat(0.U(1.W) ,sign, sew(1,0))
      val Sew2 = Cat(0.U(1.W) ,sign, (sew(1,0) + 1.U))
      val Sewf2 = Cat(0.U(1.W) ,sign, (sew(1,0) - 1.U))
      val Sewf4 = Cat(0.U(1.W) ,sign, (sew(1,0) - 2.U))
      val Sewf8 = Cat(0.U(1.W) ,sign, (sew(1,0) - 3.U))
      val Mask = "b1111".U(4.W)
      val formatOH = fuOpType(7,6)

      val format = Mux(
        (getOpcodeGeneral(fuOpType) === getOpcodeGeneral(vadd_vv) || getOpcodeGeneral(fuOpType) === getOpcodeGeneral(vsub_vv)),
        Mux1H(Seq( // format for vadd/vsub : 00 vvv   01 vvw   10 wvw   11 rvvv
          (formatOH === "b00".U) -> Cat(Sew  , Sew  , Sew  ).asUInt(),
          (formatOH === "b01".U) -> Cat(Sew  , Sew  , Sew2 ).asUInt(),
          (formatOH === "b10".U) -> Cat(Sew2 , Sew  , Sew2 ).asUInt(),
          (formatOH === "b11".U) -> Cat(Sew  , Sew  , Sew  ).asUInt(),
        )),
        Mux(
          (getOpcodeGeneral(fuOpType) === getOpcodeGeneral(vzext_vf2)),
            Mux1H(Seq( // format for vext : 00 22v   01 44v   10 88v
              (formatOH === "b00".U) -> Cat(Sewf2, Sewf2, Sew  ).asUInt(),
              (formatOH === "b01".U) -> Cat(Sewf4, Sewf4, Sew  ).asUInt(),
              (formatOH === "b10".U) -> Cat(Sewf8, Sewf8, Sew  ).asUInt(),
            )),
          Mux1H(Seq( // format for general opcode : 00 vvv   01 vvm   10 mmm   11 wvv
            (formatOH === "b00".U) -> Cat(Sew , Sew , Sew   ).asUInt(),
            (formatOH === "b01".U) -> Cat(Sew , Sew , Mask  ).asUInt(),
            (formatOH === "b10".U) -> Cat(Mask, Mask, Mask  ).asUInt(),
            (formatOH === "b11".U) -> Cat(Sew2, Sew , Sew   ).asUInt(),
          ))))
      format
    }
    def needReverse(fuOpType: UInt) = fuOpType === vrsub_vv
    def needClearMask(fuOpType: UInt) = fuOpType === vmadc_vv | fuOpType === vmsbc_vv
  }

  object VipuType {
    def dummy                          = "b11111111".U(OpTypeWidth.W) // exu not implemented
    // 8
    def vredsum_vs                     = "b00111001".U(OpTypeWidth.W) // vredsum
    def vredmaxu_vs                    = "b00111010".U(OpTypeWidth.W) // vredmax
    def vredmax_vs                     = "b00111011".U(OpTypeWidth.W) // vredmax
    def vredminu_vs                    = "b00111100".U(OpTypeWidth.W) // vredmin
    def vredmin_vs                     = "b00111101".U(OpTypeWidth.W) // vredmin
    def vredand_vs                     = "b00111110".U(OpTypeWidth.W) // vredand
    def vredor_vs                      = "b00111111".U(OpTypeWidth.W) // vredor
    def vredxor_vs                     = "b01000000".U(OpTypeWidth.W) // vredxor
    // 2
    def vwredsumu_vs                   = "b01000001".U(OpTypeWidth.W) // vredsum
    def vwredsum_vs                    = "b01000010".U(OpTypeWidth.W) // vredsum
    // 5
    def vcpop_m                        = "b01001011".U(OpTypeWidth.W) // vcpop
    def vfirst_m                       = "b01001100".U(OpTypeWidth.W) // vfirst
    def vmsbf_m                        = "b01001101".U(OpTypeWidth.W) // vmsbf
    def vmsif_m                        = "b01001110".U(OpTypeWidth.W) // vmsif
    def vmsof_m                        = "b01001111".U(OpTypeWidth.W) // vmsof
    // 2
    def viota_m                        = "b01010000".U(OpTypeWidth.W) // viota
    def vid_v                          = "b01010001".U(OpTypeWidth.W) // vid
    // 1
    def vmv_s_x                        = "b01010010".U(OpTypeWidth.W) // vmvsx TODO Integer Scalar Move vmv.s.x vd, rs1
  }

  object VfpuType {
    def dummy         = "b11111111".U(OpTypeWidth.W) // exu not implemented
    def isVfalu  = BitPat("b000?????")
    def isVfmacc = BitPat("b001?????")
    def isVfdiv  = BitPat("b010?????")
    def vfadd    = "b00000000".U(8.W)
    def vfsub    = "b00000001".U(8.W)
    def vfmin    = "b00000010".U(8.W)
    def vfmax    = "b00000011".U(8.W)
    def vfmerge  = "b00000100".U(8.W)
    def vfmove   = "b00000101".U(8.W)
    def vfsgnj   = "b00000110".U(8.W)
    def vfsgnjn  = "b00000111".U(8.W)
    def vfsgnjx  = "b00001000".U(8.W)
    def vfeq     = "b00001001".U(8.W)
    def vfne     = "b00001010".U(8.W)
    def vflt     = "b00001011".U(8.W)
    def vfle     = "b00001100".U(8.W)
    def vfgt     = "b00001101".U(8.W)
    def vfge     = "b00001110".U(8.W)
    def fclass   = "b00001111".U(8.W)
    def vfmul    = "b00100000".U(8.W)
    def vfmacc   = "b00100001".U(8.W)
    def vfnmacc  = "b00100010".U(8.W)
    def vfmsac   = "b00100011".U(8.W)
    def vfnmsac  = "b00100100".U(8.W)
    def vfmadd   = "b00100101".U(8.W)
    def vfnmadd  = "b00100110".U(8.W)
    def vfmsub   = "b00100111".U(8.W)
    def vfnmsub  = "b00101000".U(8.W)
    def vfdiv    = "b01000000".U(8.W)
  }

  object VpermType {
    // format:2bits   sign:1bit    opcode:5bits
    // 00 -> vvv
    def dummy              = "b11111111".U(OpTypeWidth.W) // exu not implemented
    def vslideup           = "b00000000".U(OpTypeWidth.W) // 
    def vslidedown         = "b00000001".U(OpTypeWidth.W) // 
    def vslide1up          = "b00000010".U(OpTypeWidth.W) // vd[0]=f[rs1], vd[i+1] = vs2[i]
    def vfslide1up         = "b01000010".U(OpTypeWidth.W) // 
    def vslide1down        = "b00000011".U(OpTypeWidth.W) // 
    def vfslide1down       = "b01000011".U(OpTypeWidth.W) // 
    def vrgather           = "b00000100".U(OpTypeWidth.W) // 
    def vrgather_vx        = "b00000101".U(OpTypeWidth.W) // 
    def vcompress          = "b00000110".U(OpTypeWidth.W) // 
    def vmvnr              = "b00000111".U(OpTypeWidth.W) // 
    def vfmv_s_f           = "b00001000".U(OpTypeWidth.W) // 

    def getOpcode(fuOpType: UInt) = fuOpType(5,0)
    def getSrcVdType(fuOpType: UInt, sew: UInt) = {
      val sign = fuOpType(6)
      val sSew =   Cat(sign ,sign,  sew(1,0))
      val uSew =   Cat(1.U(1.W), 1.U(1.W), sew(1,0))
      val format = Cat(uSew, sSew, uSew).asUInt()
      format
    }
    def isVsilde(fuOpType: UInt) = !fuOpType(7,1).orR
  }

  object VfaddOpCode {
    def dummy   = "b11111".U(5.W)
    def fadd    = "b00000".U(5.W)
    def fsub    = "b00001".U(5.W)
    def fmin    = "b00010".U(5.W)
    def fmax    = "b00011".U(5.W)
    def fmerge  = "b00100".U(5.W)
    def fmove   = "b00101".U(5.W)
    def fsgnj   = "b00110".U(5.W)
    def fsgnjn  = "b00111".U(5.W)
    def fsgnjx  = "b01000".U(5.W)
    def feq     = "b01001".U(5.W)
    def fne     = "b01010".U(5.W)
    def flt     = "b01011".U(5.W)
    def fle     = "b01100".U(5.W)
    def fgt     = "b01101".U(5.W)
    def fge     = "b01110".U(5.W)
    def fclass  = "b01111".U(5.W)
//    def fsum_re   = "b10000".U(5.W) // unorder
//    def fmin_re   = "b10001".U(5.W)
//    def fmax_re   = "b10010".U(5.W)
  }

  object VfmaOpCode {
    def dummy   = "b1111".U(5.W)
    def vfmul   = "b0000".U(5.W)
    def vfmacc  = "b0001".U(5.W)
    def vfnmacc = "b0010".U(5.W)
    def vfmsac  = "b0011".U(5.W)
    def vfnmsac = "b0100".U(5.W)
    def vfmadd  = "b0101".U(5.W)
    def vfnmadd = "b0110".U(5.W)
    def vfmsub  = "b0111".U(5.W)
    def vfnmsub = "b1000".U(5.W)
  }

  object VfdivOpCode {
    def vfdiv   = "b0".U(1.W)
    def vfsqrt  = "b1".U(1.W)
  }

  object VectorElementFormat {
    def width = 2
    def b = "b00".U(width.W)
    def h = "b01".U(width.W)  // f16
    def w = "b10".U(width.W)  // f32
    def d = "b11".U(width.W)  // f64

    def apply() = UInt(width.W)
  }
  }