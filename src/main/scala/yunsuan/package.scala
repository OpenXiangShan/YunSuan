import chisel3._

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

  object VipuType {
    def dummy                          = "b11111111".U(OpTypeWidth.W) // exu not implemented
    // 3
    def vadd_vv                        = "b00000000".U(OpTypeWidth.W) // vd[i] = vs2[i] + vs1[i] vadd
    def vsub_vv                        = "b00000001".U(OpTypeWidth.W) // vd[i] = vs2[i] - vs1[i] vsub
    def vrsub_vv                       = "b00000010".U(OpTypeWidth.W) // vd[i] = vs1[i] - vs2[i] vsub
    // 8
    def vwaddu_vv                      = "b00000011".U(OpTypeWidth.W) // vadd
    def vwsubu_vv                      = "b00000100".U(OpTypeWidth.W) // vsub
    def vwadd_vv                       = "b00000101".U(OpTypeWidth.W) // vadd
    def vwsub_vv                       = "b00000110".U(OpTypeWidth.W) // vsub
    def vwaddu_wv                      = "b00000111".U(OpTypeWidth.W) // vadd
    def vwsubu_wv                      = "b00001000".U(OpTypeWidth.W) // vsub
    def vwadd_wv                       = "b00001001".U(OpTypeWidth.W) // vadd
    def vwsub_wv                       = "b00001010".U(OpTypeWidth.W) // vsub
    // 6
    def vzext_vf2                      = "b00001011".U(OpTypeWidth.W) // vext
    def vsext_vf2                      = "b00001100".U(OpTypeWidth.W) // vext
    def vzext_vf4                      = "b00001101".U(OpTypeWidth.W) // vext
    def vsext_vf4                      = "b00001110".U(OpTypeWidth.W) // vext
    def vzext_vf8                      = "b00001111".U(OpTypeWidth.W) // vext
    def vsext_vf8                      = "b00010000".U(OpTypeWidth.W) // vext
    // 3
    def vadc_vvm                       = "b00010001".U(OpTypeWidth.W) // vadc
    def vmadc_vvm                      = "b00010010".U(OpTypeWidth.W) // vmadc
    def vmadc_vv                       = "b00010011".U(OpTypeWidth.W) // vmadc
    // 3
    def vsbc_vvm                       = "b00010100".U(OpTypeWidth.W) // vsbc
    def vmsbc_vvm                      = "b00010101".U(OpTypeWidth.W) // vmsbc
    def vmsbc_vv                       = "b00010110".U(OpTypeWidth.W) // vmsbc
    // 3
    def vand_vv                        = "b00010111".U(OpTypeWidth.W) // vand
    def vor_vv                         = "b00011000".U(OpTypeWidth.W) // vor
    def vxor_vv                        = "b00011001".U(OpTypeWidth.W) // vxor
    // 3
    def vsll_vv                        = "b00011010".U(OpTypeWidth.W) // vsll
    def vsrl_vv                        = "b00011011".U(OpTypeWidth.W) // vsrl
    def vsra_vv                        = "b00011100".U(OpTypeWidth.W) // vsra
    // 2
    def vnsrl_wv                       = "b00011101".U(OpTypeWidth.W) // vsrl
    def vnsra_wv                       = "b00011110".U(OpTypeWidth.W) // vsra
    // 8
    def vmseq_vv                       = "b00011111".U(OpTypeWidth.W) // vmseq
    def vmsne_vv                       = "b00100000".U(OpTypeWidth.W) // vmsne
    def vmsltu_vv                      = "b00100001".U(OpTypeWidth.W) // vmslt
    def vmslt_vv                       = "b00100010".U(OpTypeWidth.W) // vmslt
    def vmsleu_vv                      = "b00100011".U(OpTypeWidth.W) // vmsle
    def vmsle_vv                       = "b00100100".U(OpTypeWidth.W) // vmsle
    def vmsgtu_vv                      = "b00100101".U(OpTypeWidth.W) // vmsgt
    def vmsgt_vv                       = "b00100110".U(OpTypeWidth.W) // vmsgt 39
    // 4
    def vminu_vv                       = "b00100111".U(OpTypeWidth.W) // vmin
    def vmin_vv                        = "b00101000".U(OpTypeWidth.W) // vmin
    def vmaxu_vv                       = "b00101001".U(OpTypeWidth.W) // vmax
    def vmax_vv                        = "b00101010".U(OpTypeWidth.W) // vmax
    // 1
    def vmerge_vvm                     = "b00101011".U(OpTypeWidth.W) // vmerge
    // 1
    def vmv_v_v                        = "b00101100".U(OpTypeWidth.W) // vmv
    // 4
    def vsaddu_vv                      = "b00101101".U(OpTypeWidth.W) // vsadd
    def vsadd_vv                       = "b00101110".U(OpTypeWidth.W) // vsadd
    def vssubu_vv                      = "b00101111".U(OpTypeWidth.W) // vssub
    def vssub_vv                       = "b00110000".U(OpTypeWidth.W) // vssub
    // 4
    def vaaddu_vv                      = "b00110001".U(OpTypeWidth.W) // vaadd
    def vaadd_vv                       = "b00110010".U(OpTypeWidth.W) // vaadd
    def vasubu_vv                      = "b00110011".U(OpTypeWidth.W) // vasub
    def vasub_vv                       = "b00110100".U(OpTypeWidth.W) // vasub
    // 2
    def vssrl_vv                       = "b00110101".U(OpTypeWidth.W) // vssrl
    def vssra_vv                       = "b00110110".U(OpTypeWidth.W) // vssra
    // 2
    def vnclipu_wv                     = "b00110111".U(OpTypeWidth.W) // vssrl --- 
    def vnclip_wv                      = "b00111000".U(OpTypeWidth.W) // vssra
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
    // 8
    def vmand_mm                       = "b01000011".U(OpTypeWidth.W) // vand
    def vmnand_mm                      = "b01000100".U(OpTypeWidth.W) // vnand
    def vmandn_mm                      = "b01000101".U(OpTypeWidth.W) // vandn
    def vmxor_mm                       = "b01000110".U(OpTypeWidth.W) // vxor
    def vmor_mm                        = "b01000111".U(OpTypeWidth.W) // vor
    def vmnor_mm                       = "b01001000".U(OpTypeWidth.W) // vnor
    def vmorn_mm                       = "b01001001".U(OpTypeWidth.W) // vorn
    def vmxnor_mm                      = "b01001010".U(OpTypeWidth.W) // vxnor
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
    def vmv_s_x                        = "b01010010".U(OpTypeWidth.W) // TODO Integer Scalar Move vmv.s.x vd, rs1

    def needReverse(fuOpType: UInt) = fuOpType === vrsub_vv
    def needClearMask(fuOpType: UInt) = fuOpType === vmadc_vv | fuOpType === vmsbc_vv
  }//83

  object VfpuType {
    def dummy         = "b11111111".U(OpTypeWidth.W) // exu not implemented
    def fadd          = "b10000000".U(OpTypeWidth.W) // src1 + src2
    def fmin          = "b10000001".U(OpTypeWidth.W) // fmin(src1,src2)
    def fmax          = "b10000010".U(OpTypeWidth.W) // fmax(src1,src2)
    def feq           = "b10000011".U(OpTypeWidth.W) // feq(src1,src2)
    def fne           = "b10000100".U(OpTypeWidth.W) // fne(src1,src2)
    def flt           = "b10000101".U(OpTypeWidth.W) // flt(src1,src2)
    def fle           = "b10000110".U(OpTypeWidth.W) // fle(src1,src2)
    def fgt           = "b10000111".U(OpTypeWidth.W) // fgt(src1,src2)
    def fge           = "b10001000".U(OpTypeWidth.W) // fge(src1,src2)
    def fsub          = "b10001001".U(OpTypeWidth.W) // src1 - src2
    def fmacc         = "b00001010".U(OpTypeWidth.W) // vd = +(src1 * src2) + vd
    def fdiv          = "b00001011".U(OpTypeWidth.W) // vd = src2 / src1
    
    def isVfalu(vfpuType: UInt) = vfpuType(7) & !vfpuType(6)
  }

  object VppuType {
    def dummy = "b11111111".U(OpTypeWidth.W) // exu not implemented
    def f2s   = "b10000000".U(OpTypeWidth.W) // vd[0] = f[rs1] (vs2=0)
    def vslide1up = "b10000001".U(OpTypeWidth.W) // vd[0]=f[rs1], vd[i+1] = vs2[i]
  }

  object VfaddOpCode {
    def dummy = "b11111".U(5.W)
    def fadd  = "b00000".U(5.W)
    def fsub  = "b00001".U(5.W)
    def fmin  = "b00010".U(5.W)
    def fmax  = "b00011".U(5.W)
//    def fmerge  = "b00100".U(5.W)
//    def fmove   = "b00101".U(5.W)
//    def fSI     = "b00110".U(5.W)
//    def fSInor  = "b00111".U(5.W)
//    def fSIxor  = "b01000".U(5.W)
    def feq  = "b01001".U(5.W)
    def fne  = "b01010".U(5.W)
    def flt  = "b01011".U(5.W)
    def fle  = "b01100".U(5.W)
    def fgt  = "b01101".U(5.W)
    def fge  = "b01110".U(5.W)
//    def fclassify = "b01111".U(5.W)
//    def fsum_re   = "b10000".U(5.W) // unorder
//    def fmin_re   = "b10001".U(5.W)
//    def fmax_re   = "b10010".U(5.W)
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