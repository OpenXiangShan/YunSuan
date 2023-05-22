import chisel3._
import chisel3.util._
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

    private def literalCat(dataVec: UInt*): UInt = {
      var res = BigInt(0)
      for (data <- dataVec) {
        res <<= data.getWidth
        res |= data.litValue.toInt
      }
      res.U
    }
    private def SINT = "b1".U(1.W)
    private def UINT = "b0".U(1.W)

    // format:2bits   sign:1bit    opcode:6bits
    def dummy       = "b00_0_000000".U(OpTypeWidth.W) // exu not implemented
    def vadd_vv     = literalCat(FMT.VVV  , SINT, VialuOpcode.vadd)   // "b00_0_000000".U(OpTypeWidth.W) // vadd
    def vsub_vv     = literalCat(FMT.VVV  , SINT, VialuOpcode.vsub)   // "b00_0_000001".U(OpTypeWidth.W) // vsub
    def vrsub_vv    = literalCat(FMT.VVV  , SINT, VialuOpcode.vsub)   // "b00_0_000001".U(OpTypeWidth.W) // vsub
    def vwaddu_vv   = literalCat(FMT.VVW  , UINT, VialuOpcode.vadd)   // "b01_0_000000".U(OpTypeWidth.W) // vadd
    def vwsubu_vv   = literalCat(FMT.VVW  , UINT, VialuOpcode.vsub)   // "b01_0_000001".U(OpTypeWidth.W) // vsub
    def vwadd_vv    = literalCat(FMT.VVW  , SINT, VialuOpcode.vadd)   // "b01_1_000000".U(OpTypeWidth.W) // vadd
    def vwsub_vv    = literalCat(FMT.VVW  , SINT, VialuOpcode.vsub)   // "b01_1_000001".U(OpTypeWidth.W) // vsub
    def vwaddu_wv   = literalCat(FMT.WVW  , UINT, VialuOpcode.vadd)   // "b10_0_000000".U(OpTypeWidth.W) // vadd
    def vwsubu_wv   = literalCat(FMT.WVW  , UINT, VialuOpcode.vsub)   // "b10_0_000001".U(OpTypeWidth.W) // vsub
    def vwadd_wv    = literalCat(FMT.WVW  , SINT, VialuOpcode.vadd)   // "b10_1_000000".U(OpTypeWidth.W) // vadd
    def vwsub_wv    = literalCat(FMT.WVW  , SINT, VialuOpcode.vsub)   // "b10_1_000001".U(OpTypeWidth.W) // vsub
    def vzext_vf2   = literalCat(FMT.VF2  , UINT, VialuOpcode.vext)   // "b00_0_000010".U(OpTypeWidth.W) // vext
    def vsext_vf2   = literalCat(FMT.VF2  , SINT, VialuOpcode.vext)   // "b00_1_000010".U(OpTypeWidth.W) // vext
    def vzext_vf4   = literalCat(FMT.VF4  , UINT, VialuOpcode.vext)   // "b01_0_000010".U(OpTypeWidth.W) // vext
    def vsext_vf4   = literalCat(FMT.VF4  , SINT, VialuOpcode.vext)   // "b01_1_000010".U(OpTypeWidth.W) // vext
    def vzext_vf8   = literalCat(FMT.VF8  , UINT, VialuOpcode.vext)   // "b10_0_000010".U(OpTypeWidth.W) // vext
    def vsext_vf8   = literalCat(FMT.VF8  , SINT, VialuOpcode.vext)   // "b10_1_000010".U(OpTypeWidth.W) // vext
    def vadc_vvm    = literalCat(FMT.VVMV , UINT, VialuOpcode.vadc)   // "b00_0_000011".U(OpTypeWidth.W) // vadc
    def vmadc_vvm   = literalCat(FMT.VVMM , UINT, VialuOpcode.vmadc)  // "b01_0_000100".U(OpTypeWidth.W) // vmadc
    def vmadc_vv    = literalCat(FMT.VVM  , UINT, VialuOpcode.vmadc)  // "b10_0_000100".U(OpTypeWidth.W) // vmadc
    def vsbc_vvm    = literalCat(FMT.VVMV , UINT, VialuOpcode.vsbc)   // "b00_0_000101".U(OpTypeWidth.W) // vsbc
    def vmsbc_vvm   = literalCat(FMT.VVMM , UINT, VialuOpcode.vmsbc)  // "b01_0_000110".U(OpTypeWidth.W) // vmsbc
    def vmsbc_vv    = literalCat(FMT.VVM  , UINT, VialuOpcode.vmsbc)  // "b10_0_000110".U(OpTypeWidth.W) // vmsbc
    def vand_vv     = literalCat(FMT.VVV  , UINT, VialuOpcode.vand)   // "b00_0_000111".U(OpTypeWidth.W) // vand
    def vor_vv      = literalCat(FMT.VVV  , UINT, VialuOpcode.vor)    // "b00_0_001011".U(OpTypeWidth.W) // vor
    def vxor_vv     = literalCat(FMT.VVV  , UINT, VialuOpcode.vxor)   // "b00_0_001010".U(OpTypeWidth.W) // vxor
    def vsll_vv     = literalCat(FMT.VVV  , UINT, VialuOpcode.vsll)   // "b00_0_001111".U(OpTypeWidth.W) // vsll
    def vsrl_vv     = literalCat(FMT.VVV  , UINT, VialuOpcode.vsrl)   // "b00_0_010000".U(OpTypeWidth.W) // vsrl
    def vsra_vv     = literalCat(FMT.VVV  , SINT, VialuOpcode.vsra)   // "b00_1_010001".U(OpTypeWidth.W) // vsra
    def vnsrl_wv    = literalCat(FMT.WVV  , UINT, VialuOpcode.vsrl)   // "b11_0_010000".U(OpTypeWidth.W) // vsrl
    def vnsra_wv    = literalCat(FMT.WVV  , SINT, VialuOpcode.vsra)   // "b11_1_010001".U(OpTypeWidth.W) // vsra
    def vmseq_vv    = literalCat(FMT.VVM  , UINT, VialuOpcode.vmseq)  // "b01_0_010010".U(OpTypeWidth.W) // vmseq
    def vmsne_vv    = literalCat(FMT.VVM  , UINT, VialuOpcode.vmsne)  // "b01_0_010011".U(OpTypeWidth.W) // vmsne
    def vmsltu_vv   = literalCat(FMT.VVM  , UINT, VialuOpcode.vmslt)  // "b01_0_010100".U(OpTypeWidth.W) // vmslt
    def vmslt_vv    = literalCat(FMT.VVM  , SINT, VialuOpcode.vmslt)  // "b01_1_010100".U(OpTypeWidth.W) // vmslt
    def vmsleu_vv   = literalCat(FMT.VVM  , UINT, VialuOpcode.vmsle)  // "b01_0_010101".U(OpTypeWidth.W) // vmsle
    def vmsle_vv    = literalCat(FMT.VVM  , SINT, VialuOpcode.vmsle)  // "b01_1_010101".U(OpTypeWidth.W) // vmsle
    def vmsgtu_vv   = literalCat(FMT.VVM  , UINT, VialuOpcode.vmsgt)  // "b01_0_010110".U(OpTypeWidth.W) // vmsgt
    def vmsgt_vv    = literalCat(FMT.VVM  , SINT, VialuOpcode.vmsgt)  // "b01_1_010110".U(OpTypeWidth.W) // vmsgt
    def vminu_vv    = literalCat(FMT.VVV  , UINT, VialuOpcode.vmin)   // "b00_0_010111".U(OpTypeWidth.W) // vmin
    def vmin_vv     = literalCat(FMT.VVV  , SINT, VialuOpcode.vmin)   // "b00_1_010111".U(OpTypeWidth.W) // vmin
    def vmaxu_vv    = literalCat(FMT.VVV  , UINT, VialuOpcode.vmax)   // "b00_0_011000".U(OpTypeWidth.W) // vmax
    def vmax_vv     = literalCat(FMT.VVV  , SINT, VialuOpcode.vmax)   // "b00_1_011000".U(OpTypeWidth.W) // vmax
    def vmerge_vvm  = literalCat(FMT.VVMV , UINT, VialuOpcode.vmerge) // "b00_0_011001".U(OpTypeWidth.W) // vmerge
    def vmv_v_v     = literalCat(FMT.ZVV  , UINT, VialuOpcode.vmv)    // "b00_0_011010".U(OpTypeWidth.W) // vmv
    def vsaddu_vv   = literalCat(FMT.VVV  , UINT, VialuOpcode.vsadd)  // "b00_0_011011".U(OpTypeWidth.W) // vsadd
    def vsadd_vv    = literalCat(FMT.VVV  , SINT, VialuOpcode.vsadd)  // "b00_1_011011".U(OpTypeWidth.W) // vsadd
    def vssubu_vv   = literalCat(FMT.VVV  , UINT, VialuOpcode.vssub)  // "b00_0_011100".U(OpTypeWidth.W) // vssub
    def vssub_vv    = literalCat(FMT.VVV  , SINT, VialuOpcode.vssub)  // "b00_1_011100".U(OpTypeWidth.W) // vssub
    def vaaddu_vv   = literalCat(FMT.VVV  , UINT, VialuOpcode.vaadd)  // "b00_0_011101".U(OpTypeWidth.W) // vaadd
    def vaadd_vv    = literalCat(FMT.VVV  , SINT, VialuOpcode.vaadd)  // "b00_1_011101".U(OpTypeWidth.W) // vaadd
    def vasubu_vv   = literalCat(FMT.VVV  , UINT, VialuOpcode.vasub)  // "b00_0_011110".U(OpTypeWidth.W) // vasub
    def vasub_vv    = literalCat(FMT.VVV  , SINT, VialuOpcode.vasub)  // "b00_1_011110".U(OpTypeWidth.W) // vasub
    def vssrl_vv    = literalCat(FMT.VVV  , UINT, VialuOpcode.vssrl)  // "b00_0_011111".U(OpTypeWidth.W) // vssrl
    def vssra_vv    = literalCat(FMT.VVV  , SINT, VialuOpcode.vssra)  // "b00_1_100000".U(OpTypeWidth.W) // vssra // Todo
    def vnclipu_wv  = literalCat(FMT.WVV  , UINT, VialuOpcode.vssrl)  // "b11_0_011111".U(OpTypeWidth.W) // vssrl ---
    def vnclip_wv   = literalCat(FMT.WVV  , SINT, VialuOpcode.vssra)  // "b11_1_100000".U(OpTypeWidth.W) // vssra
    def vmand_mm    = literalCat(FMT.MMM  , UINT, VialuOpcode.vand)   // "b10_0_000111".U(OpTypeWidth.W) // vand
    def vmnand_mm   = literalCat(FMT.MMM  , UINT, VialuOpcode.vnand)  // "b10_0_001000".U(OpTypeWidth.W) // vnand
    def vmandn_mm   = literalCat(FMT.MMM  , UINT, VialuOpcode.vandn)  // "b10_0_001001".U(OpTypeWidth.W) // vandn
    def vmxor_mm    = literalCat(FMT.MMM  , UINT, VialuOpcode.vxor)   // "b10_0_001010".U(OpTypeWidth.W) // vxor
    def vmor_mm     = literalCat(FMT.MMM  , UINT, VialuOpcode.vor)    // "b10_0_001011".U(OpTypeWidth.W) // vor
    def vmnor_mm    = literalCat(FMT.MMM  , UINT, VialuOpcode.vnor)   // "b10_0_001100".U(OpTypeWidth.W) // vnor
    def vmorn_mm    = literalCat(FMT.MMM  , UINT, VialuOpcode.vorn)   // "b10_0_001101".U(OpTypeWidth.W) // vorn
    def vmxnor_mm   = literalCat(FMT.MMM  , UINT, VialuOpcode.vxnor)  // "b10_0_001110".U(OpTypeWidth.W) // vxnor
    def vmv_s_x     = literalCat(FMT.ZXV  , SINT, VialuOpcode.vmvsx)  // "b00_1_101110".U(OpTypeWidth.W) // vmvsx

    def getOpcode(fuOpType: UInt) : UInt = fuOpType(5, 0)

    def isSigned(fuOpType: UInt) : Bool = fuOpType(6)

    def getFormat(fuOpType: UInt) : UInt = fuOpType(8, 7)

    def needReverse(fuOpType: UInt) = fuOpType === vrsub_vv
    def needClearMask(fuOpType: UInt) = fuOpType === vmadc_vv | fuOpType === vmsbc_vv
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
          (formatOH === "b00".U) -> Cat(Sew, Sew, Sew).asUInt(),
          (formatOH === "b01".U) -> Cat(Sew, Sew, Sew2).asUInt(),
          (formatOH === "b10".U) -> Cat(Sew2, Sew, Sew2).asUInt(),
          (formatOH === "b11".U) -> Cat(Sew, Sew, Sew).asUInt(),
        )
        ),
        Mux(
          (getOpcodeGeneral(fuOpType) === getOpcodeGeneral(vzext_vf2)),
          Mux1H(Seq( // format for vext : 00 22v   01 44v   10 88v
            (formatOH === "b00".U) -> Cat(Sewf2, Sewf2, Sew).asUInt(),
            (formatOH === "b01".U) -> Cat(Sewf4, Sewf4, Sew).asUInt(),
            (formatOH === "b10".U) -> Cat(Sewf8, Sewf8, Sew).asUInt(),
          )
          ),
          Mux1H(Seq( // format for general opcode : 00 vvv/0xv   01 vvm   10 mmm   11 wvv
            (formatOH === "b00".U) -> Cat(Sew, Sew, Sew).asUInt(),
            (formatOH === "b01".U) -> Cat(Sew, Sew, Mask).asUInt(),
            (formatOH === "b10".U) -> Cat(Mask, Mask, Mask).asUInt(),
            (formatOH === "b11".U) -> Cat(Sew2, Sew, Sew).asUInt(),
          )
          )
        )
      )
      format
    }
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

    def getLmulVmvnr(fuOpType: UInt) = Cat(0.U(1.W), fuOpType(4,3))
    def isVmvnr(fuOpType: UInt) = fuOpType(2,0).andR() && (!fuOpType(7,5).orR())
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
      val format = Cat(uSew, srcType1, uSew).asUInt()
      format
    }
    def notNeedSew(fuOpType: UInt) = fuOpType(7)
  }

  object VimacType {

    // sign:3bits(vs2,vs1,vd) isWiden:1bit opcode:3bits
    def dummy              = "b11111111".U(OpTypeWidth.W) // exu not implemented
    def vmul               = "b0_110_0_000".U(OpTypeWidth.W) // vmul    vmul.vv/vmul.vx
    def vwmul              = "b0_110_1_000".U(OpTypeWidth.W) // vmul    vwmul.vv/vwmul.vx
    def vwmulu             = "b0_000_1_000".U(OpTypeWidth.W) // vmul    vwmulu.vv/vwmulu.vx
    def vwmulsu            = "b0_100_1_000".U(OpTypeWidth.W) // vmul    vwmulsu.vv/vwmulsu.vx
    def vmulh              = "b0_110_0_001".U(OpTypeWidth.W) // vmulh   vmulh.vv/vmulh.vx
    def vmulhu             = "b0_000_0_001".U(OpTypeWidth.W) // vmulh   vmulhu.vv/vmulhu.vx
    def vmulhsu            = "b0_100_0_001".U(OpTypeWidth.W) // vmulh   vmulhsu.vv/vmulhsu.vx
    def vmacc              = "b0_110_0_010".U(OpTypeWidth.W) // vmacc   vmacc.vv/vmacc.vx
    def vwmaccu            = "b0_000_1_010".U(OpTypeWidth.W) // vmacc   vwmaccu.vv/vwmaccu.vx
    def vwmacc             = "b0_110_1_010".U(OpTypeWidth.W) // vmacc   vwmacc.vv/vwmacc.vx
    def vwmaccsu           = "b0_010_1_010".U(OpTypeWidth.W) // vmacc   vwmaccsu.vv/vwmaccsu.vx
    def vwmaccus           = "b0_100_1_010".U(OpTypeWidth.W) // vmacc   vwmaccus.vx
    def vnmsac             = "b0_110_0_011".U(OpTypeWidth.W) // vnmsac  vnmsac.vv/vnmsac.vx
    def vmadd              = "b0_011_0_100".U(OpTypeWidth.W) // vmadd   vmadd.vv/vmadd.vx
    def vnmsub             = "b0_011_0_101".U(OpTypeWidth.W) // vnmsub  vnmsub.vv/vnmsub.vx
    def vsmul              = "b0_110_0_110".U(OpTypeWidth.W) // vsmul   vsmul.vv/vsmul.vx

    def getOpcode(fuOpType: UInt) = Cat(0.U(3.W), fuOpType(2,0))
    def getSrcVdType(fuOpType: UInt, sew: UInt) = {
      val isWiden = fuOpType(3)
      val vs2Sign = fuOpType(6)
      val vs1Sign = fuOpType(5)
      val vdSign  = fuOpType(4)
      val vs2Type = Cat(0.U(1.W), vs2Sign, sew(1,0))
      val vs1Type = Cat(0.U(1.W), vs1Sign, sew(1,0))
      val vdType  = Cat(0.U(1.W), vdSign , (sew(1,0)+isWiden))
      val format = Cat(vs2Type, vs1Type, vdType).asUInt()
      format
    }
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