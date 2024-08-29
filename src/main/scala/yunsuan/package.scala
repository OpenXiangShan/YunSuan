import chisel3._
import chisel3.util._
import yunsuan.encoding.Opcode.VimacOpcode
import yunsuan.util.LiteralCat
import yunsuan.vector.alu.VAluOpcode

package object yunsuan {
  def OpTypeWidth: Int = 9

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
    import yunsuan.encoding.Opcode.VialuOpcode

    object FMT {
      def width = 2

      def VVV   = "b00".U(width.W)

      // widen
      def VVW   = "b01".U(width.W) // (sew,   sew) -> sewX2
      def WVW   = "b10".U(width.W) // (sewX2, sew) -> sewX2
      // narrow                       (sewX2, sew) -> sew
      def WVV   = "b11".U(width.W)

      /**
        * ext
        */
      def VF2   = "b00".U(width.W)
      def VF4   = "b01".U(width.W)
      def VF8   = "b10".U(width.W)

      /**
        * other
        */
      // carry
      // vs2(V), vs1(V), v0(M), vd(V) | (sew, sew) -> sew
      def VVMV  = "b00".U(width.W)
      // vs2(V), vs1(V), vd(M)          (sew, sew) -> mask
      def VVM   = "b01".U(width.W)
      // vs2(V), vs1(V), v0(M), vd(M) | (sew, sew) -> mask
      def VVMM  = "b10".U(width.W)

      // mask-op                      | (mask, mask) -> mask
      def MMM   = "b11".U(width.W)

      // move
      // vmv.s.x only, Z = Zero       | (sew, sew) -> sew
      def ZXV   = "b00".U(width.W)
      // vmv.v.v                      | (sew, sew) -> sew
      def ZVV   = "b00".U(width.W)

    }

    private def SINT = "b1".U(1.W)
    private def UINT = "b0".U(1.W)

    // format:2bits   sign:1bit    opcode:6bits
    def dummy       = "b00_0_000000".U(OpTypeWidth.W) // exu not implemented
    def vadd_vv     = LiteralCat(FMT.VVV  , SINT, VialuOpcode.vadd)   // "b00_0_000000".U(OpTypeWidth.W) // vadd
    def vsub_vv     = LiteralCat(FMT.VVV  , SINT, VialuOpcode.vsub)   // "b00_0_000001".U(OpTypeWidth.W) // vsub
    def vrsub_vv    = LiteralCat(FMT.VVV  , SINT, VialuOpcode.vsub)   // "b00_0_000001".U(OpTypeWidth.W) // vsub
    def vwaddu_vv   = LiteralCat(FMT.VVW  , UINT, VialuOpcode.vadd)   // "b01_0_000000".U(OpTypeWidth.W) // vadd
    def vwsubu_vv   = LiteralCat(FMT.VVW  , UINT, VialuOpcode.vsub)   // "b01_0_000001".U(OpTypeWidth.W) // vsub
    def vwadd_vv    = LiteralCat(FMT.VVW  , SINT, VialuOpcode.vadd)   // "b01_1_000000".U(OpTypeWidth.W) // vadd
    def vwsub_vv    = LiteralCat(FMT.VVW  , SINT, VialuOpcode.vsub)   // "b01_1_000001".U(OpTypeWidth.W) // vsub
    def vwaddu_wv   = LiteralCat(FMT.WVW  , UINT, VialuOpcode.vadd)   // "b10_0_000000".U(OpTypeWidth.W) // vadd
    def vwsubu_wv   = LiteralCat(FMT.WVW  , UINT, VialuOpcode.vsub)   // "b10_0_000001".U(OpTypeWidth.W) // vsub
    def vwadd_wv    = LiteralCat(FMT.WVW  , SINT, VialuOpcode.vadd)   // "b10_1_000000".U(OpTypeWidth.W) // vadd
    def vwsub_wv    = LiteralCat(FMT.WVW  , SINT, VialuOpcode.vsub)   // "b10_1_000001".U(OpTypeWidth.W) // vsub
    def vzext_vf2   = LiteralCat(FMT.VF2  , UINT, VialuOpcode.vext)   // "b00_0_000010".U(OpTypeWidth.W) // vext
    def vsext_vf2   = LiteralCat(FMT.VF2  , SINT, VialuOpcode.vext)   // "b00_1_000010".U(OpTypeWidth.W) // vext
    def vzext_vf4   = LiteralCat(FMT.VF4  , UINT, VialuOpcode.vext)   // "b01_0_000010".U(OpTypeWidth.W) // vext
    def vsext_vf4   = LiteralCat(FMT.VF4  , SINT, VialuOpcode.vext)   // "b01_1_000010".U(OpTypeWidth.W) // vext
    def vzext_vf8   = LiteralCat(FMT.VF8  , UINT, VialuOpcode.vext)   // "b10_0_000010".U(OpTypeWidth.W) // vext
    def vsext_vf8   = LiteralCat(FMT.VF8  , SINT, VialuOpcode.vext)   // "b10_1_000010".U(OpTypeWidth.W) // vext
    def vadc_vvm    = LiteralCat(FMT.VVMV , UINT, VialuOpcode.vadc)   // "b00_0_000011".U(OpTypeWidth.W) // vadc
    def vmadc_vvm   = LiteralCat(FMT.VVMM , UINT, VialuOpcode.vmadc)  // "b01_0_000100".U(OpTypeWidth.W) // vmadc
    def vmadc_vv    = LiteralCat(FMT.VVM  , UINT, VialuOpcode.vmadc)  // "b10_0_000100".U(OpTypeWidth.W) // vmadc
    def vsbc_vvm    = LiteralCat(FMT.VVMV , UINT, VialuOpcode.vsbc)   // "b00_0_000101".U(OpTypeWidth.W) // vsbc
    def vmsbc_vvm   = LiteralCat(FMT.VVMM , UINT, VialuOpcode.vmsbc)  // "b01_0_000110".U(OpTypeWidth.W) // vmsbc
    def vmsbc_vv    = LiteralCat(FMT.VVM  , UINT, VialuOpcode.vmsbc)  // "b10_0_000110".U(OpTypeWidth.W) // vmsbc
    def vand_vv     = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vand)   // "b00_0_000111".U(OpTypeWidth.W) // vand
    def vor_vv      = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vor)    // "b00_0_001011".U(OpTypeWidth.W) // vor
    def vxor_vv     = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vxor)   // "b00_0_001010".U(OpTypeWidth.W) // vxor
    def vsll_vv     = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vsll)   // "b00_0_001111".U(OpTypeWidth.W) // vsll
    def vsrl_vv     = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vsrl)   // "b00_0_010000".U(OpTypeWidth.W) // vsrl
    def vsra_vv     = LiteralCat(FMT.VVV  , SINT, VialuOpcode.vsra)   // "b00_1_010001".U(OpTypeWidth.W) // vsra
    def vnsrl_wv    = LiteralCat(FMT.WVV  , UINT, VialuOpcode.vsrl)   // "b11_0_010000".U(OpTypeWidth.W) // vsrl
    def vnsra_wv    = LiteralCat(FMT.WVV  , SINT, VialuOpcode.vsra)   // "b11_1_010001".U(OpTypeWidth.W) // vsra
    def vmseq_vv    = LiteralCat(FMT.VVM  , UINT, VialuOpcode.vmseq)  // "b01_0_010010".U(OpTypeWidth.W) // vmseq
    def vmsne_vv    = LiteralCat(FMT.VVM  , UINT, VialuOpcode.vmsne)  // "b01_0_010011".U(OpTypeWidth.W) // vmsne
    def vmsltu_vv   = LiteralCat(FMT.VVM  , UINT, VialuOpcode.vmslt)  // "b01_0_010100".U(OpTypeWidth.W) // vmslt
    def vmslt_vv    = LiteralCat(FMT.VVM  , SINT, VialuOpcode.vmslt)  // "b01_1_010100".U(OpTypeWidth.W) // vmslt
    def vmsleu_vv   = LiteralCat(FMT.VVM  , UINT, VialuOpcode.vmsle)  // "b01_0_010101".U(OpTypeWidth.W) // vmsle
    def vmsle_vv    = LiteralCat(FMT.VVM  , SINT, VialuOpcode.vmsle)  // "b01_1_010101".U(OpTypeWidth.W) // vmsle
    def vmsgtu_vv   = LiteralCat(FMT.VVM  , UINT, VialuOpcode.vmsgt)  // "b01_0_010110".U(OpTypeWidth.W) // vmsgt
    def vmsgt_vv    = LiteralCat(FMT.VVM  , SINT, VialuOpcode.vmsgt)  // "b01_1_010110".U(OpTypeWidth.W) // vmsgt
    def vminu_vv    = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vmin)   // "b00_0_010111".U(OpTypeWidth.W) // vmin
    def vmin_vv     = LiteralCat(FMT.VVV  , SINT, VialuOpcode.vmin)   // "b00_1_010111".U(OpTypeWidth.W) // vmin
    def vmaxu_vv    = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vmax)   // "b00_0_011000".U(OpTypeWidth.W) // vmax
    def vmax_vv     = LiteralCat(FMT.VVV  , SINT, VialuOpcode.vmax)   // "b00_1_011000".U(OpTypeWidth.W) // vmax
    def vmerge_vvm  = LiteralCat(FMT.VVMV , UINT, VialuOpcode.vmerge) // "b00_0_011001".U(OpTypeWidth.W) // vmerge
    def vmv_v_v     = LiteralCat(FMT.ZVV  , UINT, VialuOpcode.vmv)    // "b00_0_011010".U(OpTypeWidth.W) // vmv
    def vsaddu_vv   = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vsadd)  // "b00_0_011011".U(OpTypeWidth.W) // vsadd
    def vsadd_vv    = LiteralCat(FMT.VVV  , SINT, VialuOpcode.vsadd)  // "b00_1_011011".U(OpTypeWidth.W) // vsadd
    def vssubu_vv   = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vssub)  // "b00_0_011100".U(OpTypeWidth.W) // vssub
    def vssub_vv    = LiteralCat(FMT.VVV  , SINT, VialuOpcode.vssub)  // "b00_1_011100".U(OpTypeWidth.W) // vssub
    def vaaddu_vv   = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vaadd)  // "b00_0_011101".U(OpTypeWidth.W) // vaadd
    def vaadd_vv    = LiteralCat(FMT.VVV  , SINT, VialuOpcode.vaadd)  // "b00_1_011101".U(OpTypeWidth.W) // vaadd
    def vasubu_vv   = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vasub)  // "b00_0_011110".U(OpTypeWidth.W) // vasub
    def vasub_vv    = LiteralCat(FMT.VVV  , SINT, VialuOpcode.vasub)  // "b00_1_011110".U(OpTypeWidth.W) // vasub
    def vssrl_vv    = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vssrl)  // "b00_0_011111".U(OpTypeWidth.W) // vssrl
    def vssra_vv    = LiteralCat(FMT.VVV  , SINT, VialuOpcode.vssra)  // "b00_1_100000".U(OpTypeWidth.W) // vssra // Todo
    def vnclipu_wv  = LiteralCat(FMT.WVV  , UINT, VialuOpcode.vssrl)  // "b11_0_011111".U(OpTypeWidth.W) // vssrl ---
    def vnclip_wv   = LiteralCat(FMT.WVV  , SINT, VialuOpcode.vssra)  // "b11_1_100000".U(OpTypeWidth.W) // vssra
    def vmand_mm    = LiteralCat(FMT.MMM  , UINT, VialuOpcode.vand)   // "b10_0_000111".U(OpTypeWidth.W) // vand
    def vmnand_mm   = LiteralCat(FMT.MMM  , UINT, VialuOpcode.vnand)  // "b10_0_001000".U(OpTypeWidth.W) // vnand
    def vmandn_mm   = LiteralCat(FMT.MMM  , UINT, VialuOpcode.vandn)  // "b10_0_001001".U(OpTypeWidth.W) // vandn
    def vmxor_mm    = LiteralCat(FMT.MMM  , UINT, VialuOpcode.vxor)   // "b10_0_001010".U(OpTypeWidth.W) // vxor
    def vmor_mm     = LiteralCat(FMT.MMM  , UINT, VialuOpcode.vor)    // "b10_0_001011".U(OpTypeWidth.W) // vor
    def vmnor_mm    = LiteralCat(FMT.MMM  , UINT, VialuOpcode.vnor)   // "b10_0_001100".U(OpTypeWidth.W) // vnor
    def vmorn_mm    = LiteralCat(FMT.MMM  , UINT, VialuOpcode.vorn)   // "b10_0_001101".U(OpTypeWidth.W) // vorn
    def vmxnor_mm   = LiteralCat(FMT.MMM  , UINT, VialuOpcode.vxnor)  // "b10_0_001110".U(OpTypeWidth.W) // vxnor
    def vmv_s_x     = LiteralCat(FMT.ZXV  , SINT, VialuOpcode.vmvsx)  // "b00_1_101110".U(OpTypeWidth.W) // vmvsx
    // Zvbb
    def vandn_vv    = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vandn)  // "b00_0_001001".U(OPTypeWidth.W) // vandn
    def vbrev_v     = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vbrev)  // "b00_0_101111".U(OpTypeWidth.W) // vbrev
    def vbrev8_v    = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vbrev8) // "b00_0_110000".U(OpTypeWidth.W) // vbrev8
    def vrev8_v     = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vrev8)  // "b00_0_110001".U(OpTypeWidth.W) // vrev8
    def vclz_v      = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vclz)   // "b00_0_110010".U(OpTypeWidth.W) // vclz
    def vctz_v      = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vctz)   // "b00_0_110011".U(OpTypeWidth.W) // vctz
    def vcpop_v     = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vcpop)  // "b00_0_110100".U(OpTypeWidth.W) // vcpop
    def vrol_vv     = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vrol)   // "b00_0_110101".U(OpTypeWidth.W) // vrol
    def vror_vv     = LiteralCat(FMT.VVV  , UINT, VialuOpcode.vror)   // "b00_0_110110".U(OpTypeWidth.W) // vror
    def vwsll_vv    = LiteralCat(FMT.WVW  , UINT, VialuOpcode.vwsll)  // "b10_0_110111".U(OpTypeWidth.W) // vwsll

    def getOpcode(fuOpType: UInt) : UInt = fuOpType(5, 0)

    def isSigned(fuOpType: UInt) : Bool = fuOpType(6)

    def getFormat(fuOpType: UInt) : UInt = fuOpType(8, 7)

    def needReverse(fuOpType: UInt) = fuOpType === vrsub_vv
    /**
     * needClearMask: used when the input mask of fu should be all zeros
     * needNoMask: used when the input mask of mgu should be all ones 
     */
    def needClearMask(fuOpType: UInt) = fuOpType === vmadc_vv | fuOpType === vmsbc_vv
    def needNoMask(fuOpType: UInt) = Seq(vadc_vvm, vmadc_vvm, vmadc_vv, vsbc_vvm, vmsbc_vvm, vmsbc_vv, vmerge_vvm).map(_ === fuOpType).reduce(_ || _)
    def notNeedSew(fuOpType: UInt) = fuOpType === vmv_s_x

    private def getOpcodeGeneral(fuOpType: UInt) = fuOpType(5, 0)

    def getSrcVdType(fuOpType: UInt, sew: UInt) = {
      val isVssra = fuOpType(5, 0) === vssra_vv(5, 0)
      val isVmvsx = fuOpType(5, 0) === vmv_s_x(5, 0)
      val isVmadc_vvm = fuOpType(5, 0) === vmadc_vvm(5, 0)
      val isVmsbc_vvm = fuOpType(5, 0) === vmsbc_vvm(5, 0)
      val isSpecificOpcode = (isVssra
        || isVmvsx
        || isVmadc_vvm
        || isVmsbc_vvm)
      val sign = Mux(isSpecificOpcode, 0.U(1.W), fuOpType(5)) // dirty code for opcode of vssra vmadc vmsbc
      val Sew = Cat(0.U(1.W), sign, sew(1, 0))
      val Sew2 = Cat(0.U(1.W), sign, (sew(1, 0) + 1.U))
      val Sewf2 = Cat(0.U(1.W), sign, (sew(1, 0) - 1.U))
      val Sewf4 = Cat(0.U(1.W), sign, (sew(1, 0) - 2.U))
      val Sewf8 = Cat(0.U(1.W), sign, (sew(1, 0) - 3.U))
      val Mask = "b1111".U(4.W)
      val formatOH = fuOpType(7, 6)

      val format = Mux(
        (getOpcodeGeneral(fuOpType) === getOpcodeGeneral(vadd_vv) || getOpcodeGeneral(fuOpType) === getOpcodeGeneral(vsub_vv)),
        Mux1H(Seq( // format for vadd/vsub : 00 vvv   01 vvw   10 wvw   11 rvvv
          (formatOH === "b00".U) -> Cat(Sew, Sew, Sew).asUInt,
          (formatOH === "b01".U) -> Cat(Sew, Sew, Sew2).asUInt,
          (formatOH === "b10".U) -> Cat(Sew2, Sew, Sew2).asUInt,
          (formatOH === "b11".U) -> Cat(Sew, Sew, Sew).asUInt,
        )
        ),
        Mux(
          (getOpcodeGeneral(fuOpType) === getOpcodeGeneral(vzext_vf2)),
          Mux1H(Seq( // format for vext : 00 22v   01 44v   10 88v
            (formatOH === "b00".U) -> Cat(Sewf2, Sewf2, Sew).asUInt,
            (formatOH === "b01".U) -> Cat(Sewf4, Sewf4, Sew).asUInt,
            (formatOH === "b10".U) -> Cat(Sewf8, Sewf8, Sew).asUInt,
          )
          ),
          Mux1H(Seq( // format for general opcode : 00 vvv/0xv   01 vvm   10 mmm   11 wvv
            (formatOH === "b00".U) -> Cat(Sew, Sew, Sew).asUInt,
            (formatOH === "b01".U) -> Cat(Sew, Sew, Mask).asUInt,
            (formatOH === "b10".U) -> Cat(Mask, Mask, Mask).asUInt,
            (formatOH === "b11".U) -> Cat(Sew2, Sew, Sew).asUInt,
          )
          )
        )
      )
      format
    }
  }

  object VipuType {
    def dummy                          = "b11111111".U(OpTypeWidth.W) // exu not implemented

    def vredsum_vs                     = "b00111001".U(OpTypeWidth.W) // vredsum
    def vredmaxu_vs                    = "b00111010".U(OpTypeWidth.W) // vredmax
    def vredmax_vs                     = "b00111011".U(OpTypeWidth.W) // vredmax
    def vredminu_vs                    = "b00111100".U(OpTypeWidth.W) // vredmin
    def vredmin_vs                     = "b00111101".U(OpTypeWidth.W) // vredmin
    def vredand_vs                     = "b00111110".U(OpTypeWidth.W) // vredand
    def vredor_vs                      = "b00111111".U(OpTypeWidth.W) // vredor
    def vredxor_vs                     = "b01000000".U(OpTypeWidth.W) // vredxor

    def vwredsumu_vs                   = "b01000001".U(OpTypeWidth.W) // vredsum
    def vwredsum_vs                    = "b01000010".U(OpTypeWidth.W) // vredsum

    def vcpop_m                        = "b01001011".U(OpTypeWidth.W) // vcpop
    def vfirst_m                       = "b01001100".U(OpTypeWidth.W) // vfirst
    def vmsbf_m                        = "b01001101".U(OpTypeWidth.W) // vmsbf
    def vmsif_m                        = "b01001110".U(OpTypeWidth.W) // vmsif
    def vmsof_m                        = "b01001111".U(OpTypeWidth.W) // vmsof

    def viota_m                        = "b01010000".U(OpTypeWidth.W) // viota
    def vid_v                          = "b01010001".U(OpTypeWidth.W) // vid

    def vmv_x_s                        = "b01010011".U(OpTypeWidth.W) // vmvxs
  }

  object VidivType {
    def dummy                           = "b11111111".U(OpTypeWidth.W) // exu not implemented
    def vremu                           = "b00000000".U(OpTypeWidth.W) // vremu
    def vrem                            = "b00000001".U(OpTypeWidth.W) // vrem
    def vdivu                           = "b00000010".U(OpTypeWidth.W) // vdivu
    def vdiv                            = "b00000011".U(OpTypeWidth.W) // vdiv
    def isSigned(fuOpType: UInt) : Bool = fuOpType(0)
    def isDiv(fuOpType: UInt)    : Bool = fuOpType(1)
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
    // notNeedSew:1bit  isFp:1bit  specialSrcType1:1bit vmvn:2bits  opcode:3bits
    // 00 -> vvv
    def dummy              = "b1_1_1_11_111".U(OpTypeWidth.W) // exu not implemented
    def vslideup           = "b1_0_0_00_000".U(OpTypeWidth.W) // 
    def vslidedown         = "b1_0_0_00_001".U(OpTypeWidth.W) // 
    def vslide1up          = "b0_0_0_00_010".U(OpTypeWidth.W) // vd[0]=f[rs1], vd[i+1] = vs2[i]
    def vfslide1up         = "b0_1_0_00_010".U(OpTypeWidth.W) // 
    def vslide1down        = "b0_0_0_00_011".U(OpTypeWidth.W) // 
    def vfslide1down       = "b0_1_0_00_011".U(OpTypeWidth.W) // 
    def vrgather           = "b0_0_0_00_100".U(OpTypeWidth.W) // 
    def vrgatherei16       = "b0_0_1_00_100".U(OpTypeWidth.W) // 
    def vrgather_vx        = "b1_0_0_00_101".U(OpTypeWidth.W) // 
    def vcompress          = "b0_0_1_00_110".U(OpTypeWidth.W) // 
    def vmv1r              = "b0_0_0_00_111".U(OpTypeWidth.W) // vmvnr
    def vmv2r              = "b0_0_0_01_111".U(OpTypeWidth.W) // vmvnr
    def vmv4r              = "b0_0_0_10_111".U(OpTypeWidth.W) // vmvnr
    def vmv8r              = "b0_0_0_11_111".U(OpTypeWidth.W) // vmvnr

    def isVmvnr(fuOpType: UInt) = fuOpType(2,0) === "b111".U(3.W) && fuOpType(7,5) === "b000".U(3.W)
    def getEmulVmvnr(fuOpType: UInt) = Cat(0.U(1.W), fuOpType(4,3))
    def getOpcode(fuOpType: UInt) = Cat(0.U(3.W), fuOpType(2,0))
    def getSrcVdType(fuOpType: UInt, sew: UInt) = {
      val isFp = fuOpType(6)
      val isvrgatherei16 = fuOpType(5) && !fuOpType(1)
      val isvcompress = fuOpType(5) && fuOpType(1)
      val srcType1 =  Mux1H(Seq(
        isvrgatherei16                     -> "b0001".U,
        isvcompress                        -> "b1111".U,
        !(isvrgatherei16|isvrgatherei16)   -> Cat(isFp ,isFp,  sew(1,0)),
      ))
      val uSew =   Cat(0.U(1.W), 0.U(1.W), sew(1,0))
      val format = Cat(uSew, srcType1, uSew).asUInt
      format
    }
    def notNeedSew(fuOpType: UInt) = fuOpType(7)
  }

  object VimacType {

    object INT {
      def width = 1

      def S = "b1".U(width.W)
      def U = "b0".U(width.W)
      def X = "b0".U(width.W)
    }

    object FMT {
      def width = 1

      def VVV = 0.U(width.W)
      def VVW = 1.U(width.W)
    }

    // sign:3bits(vs2,vs1,vd) isWiden:1bit opcode:3bits
    def dummy              = BitPat.Y(OpTypeWidth) // exu not implemented
    //                                  pad       vs2   vs1  vd_old format   opcode
    def vmul               = LiteralCat(0.U(2.W), INT.S, INT.S, INT.X, FMT.VVV, VimacOpcode.vmul)   // "b00_110_0_000".U(OpTypeWidth.W) // vmul    vmul.vv/vmul.vx
    def vwmul              = LiteralCat(0.U(2.W), INT.S, INT.S, INT.X, FMT.VVW, VimacOpcode.vmul)   // "b00_110_1_000".U(OpTypeWidth.W) // vmul    vwmul.vv/vwmul.vx
    def vwmulu             = LiteralCat(0.U(2.W), INT.U, INT.U, INT.X, FMT.VVW, VimacOpcode.vmul)   // "b00_000_1_000".U(OpTypeWidth.W) // vmul    vwmulu.vv/vwmulu.vx
    def vwmulsu            = LiteralCat(0.U(2.W), INT.S, INT.U, INT.X, FMT.VVW, VimacOpcode.vmul)   // "b00_100_1_000".U(OpTypeWidth.W) // vmul    vwmulsu.vv/vwmulsu.vx
    def vmulh              = LiteralCat(0.U(2.W), INT.S, INT.S, INT.X, FMT.VVV, VimacOpcode.vmulh)  // "b00_110_0_001".U(OpTypeWidth.W) // vmulh   vmulh.vv/vmulh.vx
    def vmulhu             = LiteralCat(0.U(2.W), INT.U, INT.U, INT.X, FMT.VVV, VimacOpcode.vmulh)  // "b00_000_0_001".U(OpTypeWidth.W) // vmulh   vmulhu.vv/vmulhu.vx
    def vmulhsu            = LiteralCat(0.U(2.W), INT.S, INT.U, INT.X, FMT.VVV, VimacOpcode.vmulh)  // "b00_100_0_001".U(OpTypeWidth.W) // vmulh   vmulhsu.vv/vmulhsu.vx
    def vmacc              = LiteralCat(0.U(2.W), INT.S, INT.S, INT.S, FMT.VVV, VimacOpcode.vmacc)  // "b00_111_0_010".U(OpTypeWidth.W) // vmacc   vmacc.vv/vmacc.vx
    def vwmaccu            = LiteralCat(0.U(2.W), INT.U, INT.U, INT.U, FMT.VVW, VimacOpcode.vmacc)  // "b00_000_1_010".U(OpTypeWidth.W) // vmacc   vwmaccu.vv/vwmaccu.vx
    def vwmacc             = LiteralCat(0.U(2.W), INT.S, INT.S, INT.S, FMT.VVW, VimacOpcode.vmacc)  // "b00_111_1_010".U(OpTypeWidth.W) // vmacc   vwmacc.vv/vwmacc.vx
    def vwmaccsu           = LiteralCat(0.U(2.W), INT.U, INT.S, INT.S, FMT.VVW, VimacOpcode.vmacc)  // "b00_011_1_010".U(OpTypeWidth.W) // vmacc   vwmaccsu.vv/vwmaccsu.vx
    def vwmaccus           = LiteralCat(0.U(2.W), INT.S, INT.U, INT.S, FMT.VVW, VimacOpcode.vmacc)  // "b00_101_1_010".U(OpTypeWidth.W) // vmacc   vwmaccus.vx
    def vnmsac             = LiteralCat(0.U(2.W), INT.S, INT.S, INT.S, FMT.VVV, VimacOpcode.vnmsac) // "b00_111_0_011".U(OpTypeWidth.W) // vnmsac  vnmsac.vv/vnmsac.vx
    def vmadd              = LiteralCat(0.U(2.W), INT.S, INT.S, INT.S, FMT.VVV, VimacOpcode.vmadd)  // "b00_111_0_100".U(OpTypeWidth.W) // vmadd   vmadd.vv/vmadd.vx
    def vnmsub             = LiteralCat(0.U(2.W), INT.S, INT.S, INT.S, FMT.VVV, VimacOpcode.vnmsub) // "b00_111_0_101".U(OpTypeWidth.W) // vnmsub  vnmsub.vv/vnmsub.vx
    def vsmul              = LiteralCat(0.U(2.W), INT.S, INT.S, INT.X, FMT.VVV, VimacOpcode.vsmul)  // "b00_110_0_110".U(OpTypeWidth.W) // vsmul   vsmul.vv/vsmul.vx

    def getOpcode(fuOpType: UInt) : UInt = fuOpType(2, 0)
    def getFormat(fuOpType: UInt) : UInt = fuOpType(3)

    def vs2Sign(fuOpType: UInt) = fuOpType(6)
    def vs1Sign(fuOpType: UInt) = fuOpType(5)
    def vdSign (fuOpType: UInt) = fuOpType(4)

    def getSrcVdType(fuOpType: UInt, sew: UInt) = {
      val isWiden = fuOpType(3)
      val vs2Sign = fuOpType(6)
      val vs1Sign = fuOpType(5)
      val vdSign  = fuOpType(4)
      val vs2Type = Cat(0.U(1.W), vs2Sign, sew(1,0))
      val vs1Type = Cat(0.U(1.W), vs1Sign, sew(1,0))
      val vdType  = Cat(0.U(1.W), vdSign , sew(1,0) + isWiden)
      val format  = Cat(vs2Type, vs1Type, vdType)
      format
    }
  }

  object VfaluType {
    // isWiden:1bit opcode:5bits
    def dummy = BitPat.Y(OpTypeWidth) // exu not implemented
    //                                  opb_isWiden,res_isWiden, opcode
    def vfadd     = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fadd)
    def vfwadd    = LiteralCat(0.U(1.W), 0.U(1.W), 1.U(1.W), VfaddOpCode.fadd)
    def vfwadd_w  = LiteralCat(0.U(1.W), 1.U(1.W), 1.U(1.W), VfaddOpCode.fadd)
    def vfsub     = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fsub)
    def vfwsub    = LiteralCat(0.U(1.W), 0.U(1.W), 1.U(1.W), VfaddOpCode.fsub)
    def vfwsub_w  = LiteralCat(0.U(1.W), 1.U(1.W), 1.U(1.W), VfaddOpCode.fsub)
    def vfmin     = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fmin)
    def vfmax     = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fmax)
    def vfmerge   = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fmerge)
    def vfmv      = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fmove)
    def vfsgnj    = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fsgnj)
    def vfsgnjn   = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fsgnjn)
    def vfsgnjx   = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fsgnjx)
    def vfeq      = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.feq)
    def vfne      = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fne)
    def vflt      = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.flt)
    def vfle      = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fle)
    def vfgt      = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fgt)
    def vfge      = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fge)
    def vfclass   = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fclass)
    def vfmv_f_s  = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fmv_f_s)
    def vfmv_s_f  = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fmv_s_f)
    def vfredusum = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fsum_ure)
    def vfredmax  = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fmax_re)
    def vfredmin  = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fmin_re)
    def vfredosum = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fsum_ore)
    def vfwredosum= LiteralCat(0.U(1.W), 0.U(1.W), 1.U(1.W), VfaddOpCode.fsum_ore)
    def fminm     = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fminm)
    def fmaxm     = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fmaxm)
    def fleq      = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fleq)
    def fltq      = LiteralCat(0.U(1.W), 0.U(1.W), 0.U(1.W), VfaddOpCode.fltq)
  }

  object VfaddOpCode {
    def dummy    = "b11111".U(5.W)
    def fadd     = "b00000".U(5.W)
    def fsub     = "b00001".U(5.W)
    def fmin     = "b00010".U(5.W)
    def fmax     = "b00011".U(5.W)
    def fmerge   = "b00100".U(5.W)
    def fmove    = "b00101".U(5.W)
    def fsgnj    = "b00110".U(5.W)
    def fsgnjn   = "b00111".U(5.W)
    def fsgnjx   = "b01000".U(5.W)
    def feq      = "b01001".U(5.W)
    def fne      = "b01010".U(5.W)
    def flt      = "b01011".U(5.W)
    def fle      = "b01100".U(5.W)
    def fgt      = "b01101".U(5.W)
    def fge      = "b01110".U(5.W)
    def fclass   = "b01111".U(5.W)
    def fmv_f_s  = "b10001".U(5.W)
    def fmv_s_f  = "b10010".U(5.W)
    def fsum_ure = "b11010".U(5.W) // unordered
    def fmin_re  = "b10100".U(5.W)
    def fmax_re  = "b10101".U(5.W)
    def fsum_ore = "b10110".U(5.W) // ordered
    def fminm    = "b11110".U(5.W)
    def fmaxm    = "b10011".U(5.W)
    def fleq     = "b11100".U(5.W)
    def fltq     = "b11011".U(5.W)
  }

  object VfmaType{
    // isWiden:1bit opcode:5bits
    def dummy = BitPat.Y(OpTypeWidth) // exu not implemented
    //                                 res_isWiden, opcode
    def vfmul     = LiteralCat(0.U(3.W), 0.U(1.W), VfmaOpCode.vfmul  )
    def vfmul_w   = LiteralCat(0.U(3.W), 1.U(1.W), VfmaOpCode.vfmul  )
    def vfmacc    = LiteralCat(0.U(3.W), 0.U(1.W), VfmaOpCode.vfmacc )
    def vfmacc_w  = LiteralCat(0.U(3.W), 1.U(1.W), VfmaOpCode.vfmacc )
    def vfnmacc   = LiteralCat(0.U(3.W), 0.U(1.W), VfmaOpCode.vfnmacc)
    def vfnmacc_w = LiteralCat(0.U(3.W), 1.U(1.W), VfmaOpCode.vfnmacc)
    def vfmsac    = LiteralCat(0.U(3.W), 0.U(1.W), VfmaOpCode.vfmsac )
    def vfmsac_w  = LiteralCat(0.U(3.W), 1.U(1.W), VfmaOpCode.vfmsac )
    def vfnmsac   = LiteralCat(0.U(3.W), 0.U(1.W), VfmaOpCode.vfnmsac)
    def vfnmsac_w = LiteralCat(0.U(3.W), 1.U(1.W), VfmaOpCode.vfnmsac)
    def vfmadd    = LiteralCat(0.U(3.W), 0.U(1.W), VfmaOpCode.vfmadd )
    def vfnmadd   = LiteralCat(0.U(3.W), 0.U(1.W), VfmaOpCode.vfnmadd)
    def vfmsub    = LiteralCat(0.U(3.W), 0.U(1.W), VfmaOpCode.vfmsub )
    def vfnmsub   = LiteralCat(0.U(3.W), 0.U(1.W), VfmaOpCode.vfnmsub)
  }

  object VfmaOpCode {
    def dummy   = "b1111".U(4.W)
    def vfmul   = "b0000".U(4.W)
    def vfmacc  = "b0001".U(4.W)
    def vfnmacc = "b0010".U(4.W)
    def vfmsac  = "b0011".U(4.W)
    def vfnmsac = "b0100".U(4.W)
    def vfmadd  = "b0101".U(4.W)
    def vfnmadd = "b0110".U(4.W)
    def vfmsub  = "b0111".U(4.W)
    def vfnmsub = "b1000".U(4.W)
  }

  object VfdivType {
    def dummy= BitPat.Y(OpTypeWidth) // exu not implemented
    def vfdiv    = LiteralCat(0.U(7.W), VfdivOpCode.vfdiv)
    def vfsqrt   = LiteralCat(0.U(7.W), VfdivOpCode.vfsqrt)
  }

  object VfdivOpCode {
    def vfdiv   = "b0".U(1.W)
    def vfsqrt  = "b1".U(1.W)
  }

object VfcvtType {
  def width = 9
  //  f/i(input) ## f/i(output) ## intr's func1
  def vfcvt_xufv        = "b10_000000".U(8.W)
  def vfcvt_xfv         = "b10_000001".U(8.W)
  def vfcvt_fxuv        = "b01_000010".U(8.W)
  def vfcvt_fxv         = "b01_000011".U(8.W)
  def vfcvt_rtz_xufv    = "b10_000110".U(8.W)
  def vfcvt_rtz_xfv     = "b10_000111".U(8.W)
  def vfrsqrt7          = "b11_100000".U(8.W)
  def vfrec7            = "b11_100001".U(8.W)
  def vfwcvt_xufv       = "b10_001000".U(8.W)
  def vfwcvt_xfv        = "b10_001001".U(8.W)
  def vfwcvt_fxuv       = "b01_001010".U(8.W)
  def vfwcvt_fxv        = "b01_001011".U(8.W)
  def vfwcvt_ffv        = "b11_001100".U(8.W)
  def vfwcvt_rtz_xufv   = "b10_001110".U(8.W)
  def vfwcvt_rtz_xfv    = "b10_001111".U(8.W)
  def vfncvt_xufw       = "b10_010000".U(8.W)
  def vfncvt_xfw        = "b10_010001".U(8.W)
  def vfncvt_fxuw       = "b01_010010".U(8.W)
  def vfncvt_fxw        = "b01_010011".U(8.W)
  def vfncvt_ffw        = "b11_010100".U(8.W)
  def vfncvt_rod_ffw    = "b11_010101".U(8.W)
  def vfncvt_rtz_xufw   = "b10_010110".U(8.W)
  def vfncvt_rtz_xfw    = "b10_010111".U(8.W)
  def fcvt_h_s          = "b11_010000".U(8.W)
  def fcvt_s_h          = "b11_001000".U(8.W)
  def fcvt_h_d          = "b11_011000".U(8.W)
  def fcvt_d_h          = "b11_011000".U(8.W)
  def fcvt_w_h          = "b10_001001".U(8.W)
  def fcvt_wu_h         = "b10_001000".U(8.W)
  def fcvt_l_h          = "b10_011001".U(8.W)
  def fcvt_lu_h         = "b10_011000".U(8.W)
  def fround            = "b11_000000".U(8.W)
  def froundnx          = "b11_000100".U(8.W)
  def fcvtmod_w_d     = "b1_10_010001".U(9.W)
}


  object VectorElementFormat {
    def width = 2
    def b = "b00".U(width.W)
    def h = "b01".U(width.W)  // f16
    def w = "b10".U(width.W)  // f32
    def d = "b11".U(width.W)  // f64

    def apply() = UInt(width.W)
  }
  object FaddOpCode {
    def width = 5

    def dummy    = "b11111".U(width.W)
    def fadd     = "b00000".U(width.W)
    def fsub     = "b00001".U(width.W)
    def feq      = "b01001".U(width.W)
    def flt      = "b01011".U(width.W)
    def fle      = "b01100".U(width.W)
    def fmin     = "b00010".U(width.W)
    def fmax     = "b00011".U(width.W)
    def fclass   = "b01111".U(width.W)
    def fsgnj    = "b00110".U(width.W)
    def fsgnjx   = "b01000".U(width.W)
    def fsgnjn   = "b00111".U(width.W)
    def fminm    = "b11110".U(width.W)
    def fmaxm    = "b10011".U(width.W)
    def fleq     = "b11100".U(width.W)
    def fltq     = "b11011".U(width.W)
  }
  object FmaOpCode {
    def width = 4

    def fmul    = "b0000".U(width.W)
    def fmacc   = "b0001".U(width.W)
    def fmsac   = "b0011".U(width.W)
    def fnmacc  = "b0010".U(width.W)
    def fnmsac  = "b0100".U(width.W)
  }
}