
package yunsuan.vectortest.mac

import chiseltest._
import chisel3.util._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import yunsuan.vector._
import yunsuan.vectortest._
import chiseltest.WriteVcdAnnotation
import yunsuan.vectortest.dataType._
import yunsuan.vector.mac._

class VIMac64bInput extends Bundle {
  val info = new VIFuInfo
  val srcType = Vec(2, UInt(4.W))
  val vdType  = UInt(4.W)
  val vs1 = UInt(64.W)
  val vs2 = UInt(64.W)
  val oldVd = UInt(64.W)
  val highHalf = Bool()
  val isMacc = Bool()
  val isSub = Bool()
  val widen = Bool()
  val isFixP = Bool()
}
class VIMac64bOutput extends Bundle {
  val vd = UInt(64.W)  // !!!!!!! 64
  val vxsat = Bool()
}

class VIMac64bWrapper extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VIMac64bInput))
    val out = Decoupled(new VIMac64bOutput)
  })

  val vIMac = Module(new VIMac64b)
  vIMac.io.fire := io.in.valid
  vIMac.io.info := io.in.bits.info
  vIMac.io.srcType := io.in.bits.srcType
  vIMac.io.vdType := io.in.bits.vdType
  vIMac.io.vs1 := io.in.bits.vs1
  vIMac.io.vs2 := io.in.bits.vs2
  vIMac.io.oldVd := io.in.bits.oldVd
  vIMac.io.highHalf := io.in.bits.highHalf
  vIMac.io.isMacc := io.in.bits.isMacc
  vIMac.io.isSub := io.in.bits.isSub
  vIMac.io.widen := io.in.bits.widen
  vIMac.io.isFixP := io.in.bits.isFixP

  io.out.bits.vd := vIMac.io.vd
  io.out.bits.vxsat := vIMac.io.vxsat

  io.out.valid := RegNext(RegNext(io.in.valid))
  io.in.ready := io.out.ready
}


trait VIMac64bBehavior {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

  val vmul = IMac64bCtrlBundle(s64, s64, s64)
  val vwmul = IMac64bCtrlBundle(widen = true)
  val vsmul = IMac64bCtrlBundle(isFixP = true)
  val vmacc = IMac64bCtrlBundle(isMacc = true)

  def vIMacTest0(): Unit = {
    it should "pass the single-width multiply" in {
      test(new VIMac64bWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarnessIMac.test_init_64b(dut)

        //----- Input gen -----
        val inputSeq = Seq(
          // vmul sew=64
          genVIMac64bInput(SrcBundle("h27d275bd27d274c0", "h0369c8cd0369c7d0"), vmul),
          genVIMac64bInput(SrcBundle("h4b17dd3e4b17dc40", "h26af304e26af2f50"), vmul.copy(highHalf=true)),
          genVIMac64bInput(SrcBundle("hdcba90fedcba9000", "hb851e40eb851e310"), vmul.copy(highHalf=true)),
          genVIMac64bInput(SrcBundle("hdcba90fedcba9000", "hb851e40eb851e310"), vmul.copy(u64, u64, u64, highHalf=true)),
          genVIMac64bInput(SrcBundle("h12344ef812344df8", "hedcba207edcba108"), vmul.copy(u64, u64, u64, highHalf=true)),
          genVIMac64bInput(SrcBundle("hdcba90fedcba9000", "hb851e40eb851e310"), vmul.copy(s64, s64, u64, highHalf=true)),
          // sew=8,16,32
          genVIMac64bInput(SrcBundle("hc962f76fc962f6c0", "ha4fa4a7fa4fa49d0"), vmul.copy(s8, s8, s8)),
          genVIMac64bInput(SrcBundle("hdb974de7db974d38", "hb72ea0f7b72ea048"), vmul.copy(s8, s8, s8)),
          genVIMac64bInput(SrcBundle("hae147507ae147440", "h89abc81789abc750"), vmul.copy(s16, s16, s16)),
          genVIMac64bInput(SrcBundle("hc048cb7fc048cab8", "h9be01e8f9be01dc8"), vmul.copy(s16, s16, s16)),
          genVIMac64bInput(SrcBundle("hb17e4464b17e4380", "h8d1597748d159690"), vmul.copy(s32, s32, s32)),
          genVIMac64bInput(SrcBundle("hc3b29adcc3b299f8", "h9f49edec9f49ed08"), vmul.copy(s32, s32, s32)),
          // vmulh/vmulhu/vmulhsu
          genVIMac64bInput(SrcBundle("hcdf00d2bcdf00c80", "ha987603ba9875f90"), vmul.copy(s8, s8, s8, highHalf=true)),
          genVIMac64bInput(SrcBundle("h51eb7f4051eb7e78", "h2d82d2502d82d188"), vmul.copy(s16, s16, s16, highHalf=true)),
          genVIMac64bInput(SrcBundle("h6af375626af37480", "h468ac872468ac790"), vmul.copy(s32, s32, s32, highHalf=true)),
          genVIMac64bInput(SrcBundle("h5d4c33b65d4c32b8", "h38e386c638e385c8"), vmul.copy(s64, s64, s64, highHalf=true)),
          genVIMac64bInput(SrcBundle("hb851e666b851e5b8", "h93e9397693e938c8"), vmul.copy(u8, u8, u8, highHalf=true)),
          genVIMac64bInput(SrcBundle("h62fc904962fc8f80", "h3e93e3593e93e290"), vmul.copy(u16, u16, u16, highHalf=true)),
          genVIMac64bInput(SrcBundle("h0eca7f9b0eca7eb8", "hea61d2aaea61d1c8"), vmul.copy(u32, u32, u32, highHalf=true)),
          genVIMac64bInput(SrcBundle("hd82d7b42d82d7a40", "hb3c4ce52b3c4cd50"), vmul.copy(u64, u64, u64, highHalf=true)),
          genVIMac64bInput(SrcBundle("h7e4b12b17e4b1200", "h59e265c159e26510"), vmul.copy(s8, s8, u8, highHalf=true)),
          genVIMac64bInput(SrcBundle("h2ea617bf2ea616f8", "h0a3d6acf0a3d6a08"), vmul.copy(s16, s16, u16, highHalf=true)),
          genVIMac64bInput(SrcBundle("hb60b5a20b60b5940", "h91a2ad3091a2ac50"), vmul.copy(s32, s32, u32, highHalf=true)),
          genVIMac64bInput(SrcBundle("hea61d1baea61d0b8", "hc5f924cac5f923c8"), vmul.copy(s64, s64, u64, highHalf=true)),
        )

        //----- Output expectation -----
        val outputSeq = Seq(
          // vmul sew=64
          genVIMac64bOutput("he80ffe1516801c00"), // vmul 6229
          genVIMac64bOutput("h0b58ee52e2d4e518"), // vmulh 6257
          genVIMac64bOutput("h09e03ed454811904"), // vmulh 6271
          genVIMac64bOutput("h9eecb3e1e98d8c14"), // vmulh 6271
          genVIMac64bOutput("h10e8e81bf1e0e7ff"), // vmulh 6299
          genVIMac64bOutput("he69acfd3313ba904"), // vmulsh 6271
          // sew=8,16,32
          genVIMac64bOutput("hc4b46611c4b42600"), // vmul 4325
          genVIMac64bOutput("h8d2220e18d2220c0"), // vmul 4325
          genVIMac64bOutput("hfb5cfba1fb5c1400"), // vmul 4913
          genVIMac64bOutput("hd7008df1d70037c0"), // vmul 4913
          genVIMac64bOutput("h97c1f950dc12f800"), // vmul 5627
          genVIMac64bOutput("h8ec06ed065d767c0"), // vmul 5627
          // vmulh/vmulhu/vmulhsu
          genVIMac64bOutput("h1107040911070438"), // vmulh 4227
          genVIMac64bOutput("h0e8fe94a0e8fe90b"), // vmulh 4927
          genVIMac64bOutput("h1d788d131d788c77"), // vmulh 5571
          genVIMac64bOutput("h14bb96ffdfda702e"), // vmulh 6257
          genVIMac64bOutput("h6949332f6949328f"), // vmulhu 4297
          genVIMac64bOutput("h1831802218317eff"), // vmulhu 4955
          genVIMac64bOutput("h0d8abf890d8abead"), // vmulhu 5585
          genVIMac64bOutput("h97cdfe3c8be4920c"), // vmulhu 6369
          genVIMac64bOutput("h2b4207c42b420700"), // vmulhsu 4367
          genVIMac64bOutput("h01dd09e801dd0983"), // vmulhsu 4899
          genVIMac64bOutput("hd5ed6f32d5ed6ef4"), // vmulhsu 5529
          genVIMac64bOutput("hef483c6e772141b8"), // vmulhsu 6369
        )

        fork {
          dut.io.in.enqueueSeq(inputSeq)
        }.fork {
          dut.io.out.expectDequeueSeq(outputSeq)
        }.join()
        dut.clock.step(1)
      }
    }
  }

  def vIMacTest1(): Unit = {
    it should "pass the widen (11.12) and fixed-point (12.3)." in {
      test(new VIMac64bWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarnessIMac.test_init_64b(dut)

        //----- Input gen -----
        val inputSeq = Seq(
          // widen (11.12)
          genVIMac64bInput(SrcBundle("h51eb837851eb8340", "h2d82d6882d82d650"), vwmul.copy(s16, s8, s8)),
          genVIMac64bInput(SrcBundle("h51eb837851eb8340", "h2d82d6882d82d650"), vwmul.copy(s16, s8, s8, uopIdx=1)),
          genVIMac64bInput(SrcBundle("ha1907d7aa1907d38", "h7d27d08a7d27d048"), vwmul.copy(s32, s16, s16, uopIdx=0)),
          genVIMac64bInput(SrcBundle("ha1907d7aa1907d38", "h7d27d08a7d27d048"), vwmul.copy(s32, s16, s16, uopIdx=1)),
          genVIMac64bInput(SrcBundle("h2a1905c32a190578", "h05b058d305b05888"), vwmul.copy(s64, s32, s32, uopIdx=0)),
          genVIMac64bInput(SrcBundle("h2a1905c32a190578", "h05b058d305b05888"), vwmul.copy(s64, s32, s32, uopIdx=1)),
          genVIMac64bInput(SrcBundle("h51eb837851eb8340", "h2d82d6882d82d650"), vwmul.copy(u16, u8, u8)),
          genVIMac64bInput(SrcBundle("h51eb837851eb8340", "h2d82d6882d82d650"), vwmul.copy(u16, u8, u8, uopIdx=1)),
          genVIMac64bInput(SrcBundle("hb4e81961b4e81920", "h907f6c71907f6c30"), vwmul.copy(u32, u16, u16, uopIdx=0)),
          genVIMac64bInput(SrcBundle("hb4e81961b4e81920", "h907f6c71907f6c30"), vwmul.copy(u32, u16, u16, uopIdx=1)),
          genVIMac64bInput(SrcBundle("h17e4af4b17e4af00", "hf37c025af37c0210"), vwmul.copy(u64, u32, u32, uopIdx=0)),
          genVIMac64bInput(SrcBundle("h17e4af4b17e4af00", "hf37c025af37c0210"), vwmul.copy(u64, u32, u32, uopIdx=1)),
          genVIMac64bInput(SrcBundle("h89abcc4f89abcc18", "h65431f5f65431f28"), vwmul.copy(s16, s8, u8)),
          genVIMac64bInput(SrcBundle("h89abcc4f89abcc18", "h65431f5f65431f28"), vwmul.copy(s16, s8, u8, uopIdx=1)),
          genVIMac64bInput(SrcBundle("hb4e81961b4e81920", "h907f6c71907f6c30"), vwmul.copy(s32, s16, u16, uopIdx=0)),
          genVIMac64bInput(SrcBundle("hb4e81961b4e81920", "h907f6c71907f6c30"), vwmul.copy(s32, s16, u16, uopIdx=1)),
          genVIMac64bInput(SrcBundle("h17e4af4b17e4af00", "hf37c025af37c0210"), vwmul.copy(s64, s32, u32, uopIdx=0)),
          genVIMac64bInput(SrcBundle("h17e4af4b17e4af00", "hf37c025af37c0210"), vwmul.copy(s64, s32, u32, uopIdx=1)),
          // fixed-point (12.3)
          genVIMac64bInput(SrcBundle("hd3a06b9ed3a06b68", "haf37beaeaf37be78"), vsmul.copy(s8, s8, s8)),
          genVIMac64bInput(SrcBundle("h89abcc4f89abcc18", "h65431f5f65431f28"), vsmul.copy(s8, s8, s8)),
          genVIMac64bInput(SrcBundle("h93e93cbe93e93c80", "h6f808fce6f808f90"), vsmul.copy(s16, s16, s16)),
          genVIMac64bInput(SrcBundle("h49f49d6f49f49d30", "h258bf07f258bf040"), vsmul.copy(s16, s16, s16)),
          genVIMac64bInput(SrcBundle("h1c71c5071c71c4c0", "hf8091816f80917d0"), vsmul.copy(s32, s32, s32)),
          genVIMac64bInput(SrcBundle("h2ea61b7f2ea61b38", "h0a3d6e8f0a3d6e48"), vsmul.copy(s32, s32, s32)),
          genVIMac64bInput(SrcBundle("ha4fa4d4fa4fa4d00", "h8091a05f8091a010"), vsmul.copy(s64, s64, s64)),
          genVIMac64bInput(SrcBundle("hb72ea3c7b72ea378", "h92c5f6d792c5f688"), vsmul.copy(s64, s64, s64)),
        )

        //----- Output expectation -----
        val outputSeq = Seq(
          // widen (11.12)
          genVIMac64bOutput("h0e3d0a5614821400"), // vwmul 1091
          genVIMac64bOutput("h0e3d0a561482c7c0"), // vwmul 1091
          genVIMac64bOutput("hd1d4ecf0e8a8b7c0"), // vwmul 1259
          genVIMac64bOutput("hd1d4ecf0e8bcc3c4"), // vwmul 1259
          genVIMac64bOutput("h00ef7cde11ac27c0"), // vwmul 1399
          genVIMac64bOutput("h00ef7cec11aac7b9"), // vwmul 1399
          genVIMac64bOutput("h0e3d77566d821400"), // vwmulu 1091
          genVIMac64bOutput("h0e3d77566d823fc0"), // vwmulu 1091
          genVIMac64bOutput("h661c3f180a9e3600"), // vwmulu 1245
          genVIMac64bOutput("h661c3f180ac01fd1"), // vwmulu 1245
          genVIMac64bOutput("h16b9a5130ba8f000"), // vwmulu 1399
          genVIMac64bOutput("h16b9a5614918365e"), // vwmulu 1399
          genVIMac64bOutput("hd10de9c1f9b403c0"), // vwmulsu 1077
          genVIMac64bOutput("hd10de9c1f9b41d51"), // vwmulsu 1077
          genVIMac64bOutput("hd59d3f180a9e3600"), // vwmulsu 1245
          genVIMac64bOutput("hd59d3f180ac01fd1"), // vwmulsu 1245
          genVIMac64bOutput("h16b9a5130ba8f000"), // vwmulsu 1399
          genVIMac64bOutput("h16b9a5614918365e"), // vwmulsu 1399
          // fixed-point (12.3)
          genVIMac64bOutput("h1cd7c93f1cd7c962"), // vsmul 1077
          genVIMac64bOutput("ha2d4f33ba2d4f308"), // vsmul 1091
          genVIMac64bOutput("ha1d8cac2a1d8cadb"), // vsmul 1231
          genVIMac64bOutput("h15b10bf015b10c29"), // vsmul 1245
          genVIMac64bOutput("hfe3ae90afe3ae8fe"), // vsmul 1399
          genVIMac64bOutput("h03bb5d9a03bb5d7a"), // vsmul 1399
          genVIMac64bOutput("h5a9e2430e63a44e9"), // vsmul 1567
          genVIMac64bOutput("h3e234c9b6f8f5b28"), // vsmul 1567
        )

        fork {
          dut.io.in.enqueueSeq(inputSeq)
        }.fork {
          dut.io.out.expectDequeueSeq(outputSeq)
        }.join()
        dut.clock.step(1)
      }
    }
  }

  def vIMacTest2(): Unit = {
    it should "pass the madd (11.13) and widen madd (11.14)." in {
      test(new VIMac64bWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarnessIMac.test_init_64b(dut)

        //----- Input gen -----
        val inputSeq = Seq(
          // vmacc/vnmsac/vmadd/vnmsub
          genVIMac64bInput(SrcBundle("hcdf010ebcdf010c0", "ha98763fba98763d0", "h851eb70b851eb6e0"), vmacc.copy(s8, s8, s8)),
          genVIMac64bInput(SrcBundle("h71c71b2471c71af8", "h4d5e6e344d5e6e08", "h28f5c14428f5c118"), vmacc.copy(s8, s8, s8)),
          genVIMac64bInput(SrcBundle("ha1907df2a1907dc0", "h7d27d1027d27d0d0", "h58bf241258bf23e0"), vmacc.copy(s16, s16, s16)),
          genVIMac64bInput(SrcBundle("h06d39eba06d39e80", "he26af1c9e26af190", "hbe0244d9be0244a0"), vmacc.copy(s32, s32, s32)),
          genVIMac64bInput(SrcBundle("hc962fab7c962fa78", "ha4fa4dc7a4fa4d88", "h8091a0d78091a098"), vmacc.copy(s64, s64, s64)),
          genVIMac64bInput(SrcBundle("h5f92c4ac5f92c480", "h3b2a17bc3b2a1790", "h16c16acc16c16aa0"), vmacc.copy(s8, s8, s8, isSub=true)),
          genVIMac64bInput(SrcBundle("he0246763e0246738", "hbbbbba73bbbbba48", "h97530d8397530d58"), vmacc.copy(s8, s8, s8, isSub=true)),
          genVIMac64bInput(SrcBundle("h333331b333333180", "h0eca84c30eca8490", "hea61d7d2ea61d7a0"), vmacc.copy(s16, s16, s16, isSub=true)),
          genVIMac64bInput(SrcBundle("h4567882b456787f8", "h20fedb3b20fedb08", "hfc962e4afc962e18"), vmacc.copy(s16, s16, s16, isSub=true)),
          genVIMac64bInput(SrcBundle("he38e3738e38e3700", "hbf258a48bf258a10", "h9abcdd589abcdd20"), vmacc.copy(s32, s32, s32, isSub=true)),
          genVIMac64bInput(SrcBundle("hf5c28db0f5c28d78", "hd159e0c0d159e088", "hacf133d0acf13398"), vmacc.copy(s32, s32, s32, isSub=true)),
          genVIMac64bInput(SrcBundle("h48d1580048d157c0", "h2468ab102468aad0", "hfffffe1ffffffde0"), vmacc.copy(s64, s64, s64, isSub=true)),
          genVIMac64bInput(SrcBundle("h5b05ae785b05ae38", "h369d0188369d0148", "h1234549812345458"), vmacc.copy(s64, s64, s64, isSub=true)),
          genVIMac64bInput(SrcBundle("hf37c034af37c0320", "h17e4b03b17e4b010", "h3c4d5d2b3c4d5d00"), vmacc.copy(s8, s8, s8)),
          genVIMac64bInput(SrcBundle("h58bf241258bf23e0", "h7d27d1027d27d0d0", "ha1907df2a1907dc0"), vmacc.copy(s16, s16, s16)),
          genVIMac64bInput(SrcBundle("hd0369b51d0369b18", "hf49f4841f49f4808", "h1907f5321907f4f8"), vmacc.copy(s32, s32, s32)),
          genVIMac64bInput(SrcBundle("hfffffe1ffffffde0", "h2468ab102468aad0", "h48d1580048d157c0"), vmacc.copy(s64, s64, s64)),
          genVIMac64bInput(SrcBundle("h76b8e5970a0c1c73", "ha32272c77541d581", "hcf8bfff7e0778e8f"), vmacc.copy(s8, s8, s8, isSub=true)),
          genVIMac64bInput(SrcBundle("h122a53c081482424", "h3e93e0f0ec7ddd32", "h6afd6e2157b39640"), vmacc.copy(s8, s8, s8, isSub=true)),
          genVIMac64bInput(SrcBundle("h490d30136fc03386", "h1ca3a2e3048a7a78", "hc3d088822e1f085c"), vmacc.copy(u16, u8, u8, widen=true, uopIdx=0)),
          genVIMac64bInput(SrcBundle("h490d30136fc03386", "h1ca3a2e3048a7a78", "hda054f1a63b9e4e3"), vmacc.copy(u16, u8, u8, widen=true, uopIdx=1)),
          genVIMac64bInput(SrcBundle("h8fda095ea8643014", "h63707c2e3d2e7706", "h0a9d61cd66c304ea"), vmacc.copy(s32, s16, s16, widen=true, uopIdx=0)),
          genVIMac64bInput(SrcBundle("h8fda095ea8643014", "h63707c2e3d2e7706", "h20d228659c5de171"), vmacc.copy(s32, s16, s16, widen=true, uopIdx=1)),
          genVIMac64bInput(SrcBundle("h271cf20587ccd0cc", "hfab364d51c9717be", "ha1e04a74462ba5a2"), vmacc.copy(s16, u8, s8, widen=true, uopIdx=0)),
          genVIMac64bInput(SrcBundle("h271cf20587ccd0cc", "hfab364d51c9717be", "hb815110c7bc68229"), vmacc.copy(s16, u8, s8, widen=true, uopIdx=1)),
          genVIMac64bInput(SrcBundle("hb1ca476c905792ce", "h8560ba3c2521d9c0", "h2c8d9fdb4eb667a4"), 
                        vmacc.copy(s32, u16, s16, widen=true, uopIdx=0)),
          genVIMac64bInput(SrcBundle("hb1ca476c905792ce", "h8560ba3c2521d9c0", "h42c266738451442b"), 
                        vmacc.copy(s32, u16, s16, widen=true, uopIdx=1)),
          genVIMac64bInput(SrcBundle("h3c779cd398e254d0", "h100e0fa32dac9bc2", "hb73af542574129a6"), 
                        vmacc.copy(s64, u32, s32, widen=true, uopIdx=0)),
          genVIMac64bInput(SrcBundle("h3c779cd398e254d0", "h100e0fa32dac9bc2", "hcd6fbbda8cdc062d"), 
                        vmacc.copy(s64, u32, s32, widen=true, uopIdx=1)),
          genVIMac64bInput(SrcBundle("h1634c698359adc99", "h1212121212121212", "h6f07e0f90c064eb5"), 
                        vmacc.copy(s16, s8, u8, widen=true, uopIdx=0)),
          genVIMac64bInput(SrcBundle("h1634c698359adc99", "h1212121212121212", "h853ca79141a12b3c"), 
                        vmacc.copy(s16, s8, u8, widen=true, uopIdx=1)),
          genVIMac64bInput(SrcBundle("h2c698d306b35b920", "h0012001200120012", "h9b716e29773c07c3"), 
                        vmacc.copy(s32, s16, u16, widen=true, uopIdx=0)),
          genVIMac64bInput(SrcBundle("h2c698d306b35b920", "h0012001200120012", "hb1a634c1acd6e44a"), 
                        vmacc.copy(s32, s16, u16, widen=true, uopIdx=1)),
        )

        //----- Output expectation -----
        val outputSeq = Seq(
          // vmacc/vnmsac/vmadd/vnmsub
          genVIMac64bOutput("hdaaee774daaee6e0"), // vmacc 1077
          genVIMac64bOutput("h25075b942507edd8"), // vmacc 1091
          genVIMac64bOutput("h45afb1f645af4fe0"), // vmacc 1245
          genVIMac64bOutput("h3f9afee36e41eca0"), // vmacc 1427
          genVIMac64bOutput("ha184f872bbacc858"), // vmacc 1567
          genVIMac64bOutput("h31cdce7c31cdcea0"), // vnmsac 1091
          genVIMac64bOutput("hf707370af7073798"), // vnmsac 1077
          genVIMac64bOutput("hba23b079ba23ffa0"), // vnmsac 1259
          genVIMac64bOutput("h406403614064c658"), // vnmsac 1259
          genVIMac64bOutput("h2de12598c5336d20"), // vnmsac 1399
          genVIMac64bOutput("h2cdcefd0bb150bd8"), // vnmsac 1399
          genVIMac64bOutput("hfe6a8c272ba331e0"), // vnmsac 1581
          genVIMac64bOutput("h06deff67ca951c98"), // vnmsac 1581
          genVIMac64bOutput("h11bd6d3911bd6d00"), // vmadd 1063
          genVIMac64bOutput("h69a9781669a9a3c0"), // vmadd 1245
          genVIMac64bOutput("hfde32cc32a438db8"), // vmadd 1427
          genVIMac64bOutput("ha6258fb2ea665dc0"), // vmadd 1581
          genVIMac64bOutput("had1b05964e6b429c"), // vnmsub 1063
          genVIMac64bOutput("h0edfce216b8b8238"), // vnmsub 1077
          genVIMac64bOutput("hc58cf002466d472c"), // vwmaccu 1077
          genVIMac64bOutput("he20157618219f5bc"), // vwmaccu 1077
          genVIMac64bOutput("hf5ad77c57d1d7162"), // vwmaccu 1259
          genVIMac64bOutput("hf54265c5a0e91855"), // vwmaccu 1259
          genVIMac64bOutput("hb0a4f6c858db710a"), // vwmaccsu 1091
          genVIMac64bOutput("hb72b08a0da4e8152"), // vwmaccsu 1091
          genVIMac64bOutput("h417cce1238c72024"), // vwmaccsu 1245
          genVIMac64bOutput("hed99043370da797b"), // vwmaccsu 1245
          genVIMac64bOutput("hd281d340c7df5f46"), // vwmaccsu 1399
          genVIMac64bOutput("hd13a87e3d2bf3d86"), // vwmaccsu 1399
          genVIMac64bOutput("h72c1d9cd097e4777"), // vwmaccus 1161
          genVIMac64bOutput("h86c8ab393d8d23ec"), // vwmaccus 1161
          genVIMac64bOutput("h9b78f7e377370c03"), // vwmaccus 1315
          genVIMac64bOutput("hb1a95423acced1aa"), // vwmaccus 1315
        )

        fork {
          dut.io.in.enqueueSeq(inputSeq)
        }.fork {
          dut.io.out.expectDequeueSeq(outputSeq)
        }.join()
        dut.clock.step(1)
      }
    }
  }


}

class VIMac64bSpec extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VIMac64bBehavior {
  behavior of "IMac64b test"
  it should behave like vIMacTest0()
  it should behave like vIMacTest1()
  it should behave like vIMacTest2()
}
