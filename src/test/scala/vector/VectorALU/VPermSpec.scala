
package yunsuan.vectortest.perm

import chiseltest._
import chisel3.util._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import yunsuan.vector._
import yunsuan.vectortest._
import chiseltest.WriteVcdAnnotation
import yunsuan.vectortest.dataType._
import yunsuan.vector.perm._

trait VPermBehavior {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>
  val vslideup     = CtrlBundle(opcode = 0) 
  val vslidedn     = CtrlBundle(opcode = 1) 
  val vslide1up    = CtrlBundle(opcode = 2) 
  val vslide1dn    = CtrlBundle(opcode = 3) 
  val vrgather     = CtrlBundle(opcode = 4) 
  val vrgather_vx  = CtrlBundle(opcode = 5) 
  val vcompress    = CtrlBundle(opcode = 6) 
  val vmvnr        = CtrlBundle(opcode = 7) 
 
  def vPermTest(): Unit = {
    it should "pass the permutation test" in {
      test(new VPermWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // test(new VIMacWrapper) { dut =>
        TestHarnessPerm.test_init(dut)

        //----- Input gen -----
        val inputSeq = Seq(

        // vcomprss lmul=1
        genVPermInput(SrcBundle("h0008060f0206ff0001070008060f0206", "h7ff8f8030105ffff7f7f7ff8f8030105",  "h8344c0aa5b76129530ac75dc32808aa4", "h0"), vcompress.copy(u8, u8, u8, vm = true, ta = false, vl=16)),
        // vcomprss lmul=2
        genVPermInput(SrcBundle("hc4d5e6dbc4d5e6d8b2a19063b2a19060", "h7c048cfb7c048cf869d0368369d03680",  "h579be00b579be0084567899345678990", "h0"), vcompress.copy(u8, u8, mask, vm = true, ta = false, ma=false, vlmul=1, vl=32)),
        genVPermInput(SrcBundle("hc4d5e6dbc4d5e6d8b2a19063b2a19060", "h7c048cfb7c048cf869d0368369d03680",  "h579be00b579be0084567899345678990", "h0"), vcompress.copy(u8, u8, mask, vm = true, ta = false, ma=false, vlmul=1, vl=32, uopIdx = 1)),
        genVPermInput(SrcBundle("he93e93cbe93e93c8d70a3d53d70a3d50", "h5007c048cfb7c048cf869d0368369d0",  "h579be00b579be008456789e6dbd5e6b2", "h5"), vcompress.copy(u8, u8, mask, vm = true, ta = false, ma=false, vlmul=1, vl=32, uopIdx = 2)),
        genVPermInput(SrcBundle("he93e93cbe93e93c8d70a3d53d70a3d50", "h7c048cfb7c048cf869d0368369d03680",  "h7c048cfb7c048cf869d0368369d03680", "h5"), vcompress.copy(u8, u8, mask, vm = true, ta = false, ma=false, vlmul=1, vl=32, uopIdx = 3)),
        // vslide1up lmul=1
        genVPermInput(SrcBundle("h6d3a06d06d3a06d75b05b0585b05b05f", "h07070707070707070707070707070707",  "h48d159e048d159e7369d0368369d036f", "h2468acf02468acf7123456781234567f"), vslide1up.copy(u8, u8, u8, vm = false, ta = false, ma = false, vl=16)),
        // vslide1up lmul=2 vl =16
        genVPermInput(SrcBundle("h91a2b3c091a2b3c97f6e5d487f6e5d51", "h04040404040404040404040404040404",  "h48d159e048d159e9369d0368369d0371", "h2468acf02468acf91234567812345681"), vslide1up.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=16)),
        genVPermInput(SrcBundle("hb60b60b0b60b60b9a3d70a38a3d70a41", "h91a2b3c091a2b3c97f6e5d487f6e5d51",  "h6d3a06d06d3a06d95b05b0585b05b061", "h2468acf02468acf91234567812345681"), vslide1up.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=16, uopIdx = 1)),
        // vslide1up lmul=2 vl =32
        genVPermInput(SrcBundle("h91a2b3c091a2b3c47f6e5d487f6e5d4c", "h03030303030303030303030303030303",  "h48d159e048d159e4369d0368369d036c", "h2468acf02468acf4123456781234567c"), vslide1up.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=32)),
        genVPermInput(SrcBundle("hb60b60b0b60b60b4a3d70a38a3d70a3c", "h91a2b3c091a2b3c47f6e5d487f6e5d4c",  "h6d3a06d06d3a06d45b05b0585b05b05c", "h2468acf02468acf4123456781234567c"), vslide1up.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=32, uopIdx = 1)),
        // vslideup lmul=2 vl =16
        genVPermInput(SrcBundle("h91a2b3c091a2b3c97f6e5d487f6e5d51", "h4",  "h48d159e048d159e9369d0368369d0371", "h2468acf02468acf91234567812345681"), vslideup.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=16)),
        genVPermInput(SrcBundle("h91a2b3c091a2b3c97f6e5d487f6e5d51", "h4",  "h6d3a06d06d3a06d95b05b0585b05b061", "h2468acf02468acf91234567812345681"), vslideup.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=16, uopIdx = 1)),
        genVPermInput(SrcBundle("hb60b60b0b60b60b9a3d70a38a3d70a41", "h4",  "h6d3a06d06d3a06d95b05b0585b05b061", "h2468acf02468acf91234567812345681"), vslideup.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=16, uopIdx = 2)),
        // vslidedn lmul=2 vl =16
        genVPermInput(SrcBundle("hb60b60b0b60b60b9a3d70a38a3d70a41", "h4",  "h48d159e048d159e9369d0368369d0371", "h2468acf02468acf91234567812345681"), vslidedn.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=16)),
        genVPermInput(SrcBundle("h91a2b3c091a2b3c97f6e5d487f6e5d51", "h4",  "h48d75941480000e9009d0368369d0300", "h2468acf02468acf91234567812345681"), vslidedn.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=16, uopIdx = 1)),
        genVPermInput(SrcBundle("hb60b60b0b60b60b9a3d70a38a3d70a41", "h4",  "h6d3a06d06d3a06d95b05b0585b05b061", "h2468acf02468acf91234567812345681"), vslidedn.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=16, uopIdx = 2)),
         // vslide1dn lmul=2 vl =16
         genVPermInput(SrcBundle("h91a2b3c091a2b3c97f6e5d487f6e5d51", "hb60b60b0b60b60b9a3d70a38a3d70a41",  "h48d159e048d159e9369d0368369d0371", "h2468acf02468acf91234567812345681"), vslide1dn.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=16)),
         genVPermInput(SrcBundle("h91a2b3c091a2b3c97f6e5d487f6e5d51", "h04040404040404040404040404040404",  "h489159b34891a2e9c99d0368369d035d", "h2468acf02468acf91234567812345681"), vslide1dn.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=16, uopIdx = 1)),
         genVPermInput(SrcBundle("hb60b60b0b60b60b9a3d70a38a3d70a41", "h04040404040404040404040404040404",  "h6d3a06d06d3a06d95b05b0585b05b061", "h2468acf02468acf91234567812345681"), vslide1dn.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=16, uopIdx = 2)),
         // vrgather lmul=2 vl =16
         genVPermInput(SrcBundle("hacf134c0acf134a89abcde489abcde30", "h641fdae0641fdac851eb846851eb8450",  "h1b4e81001b4e80e8091a2a88091a2a70", "hf6e5d40ff6e5d3f8e4b17d97e4b17d80"), vrgather.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=16)),
         genVPermInput(SrcBundle("hd159e1b0d159e198bf258b38bf258b20", "h641fdae0641fdac851eb846851eb8450",  "h1b00000000008000001a2a88091a2a70", "hf6e5d40ff6e5d3f8e4b17d97e4b17d80"), vrgather.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=16, uopIdx = 1)),
         genVPermInput(SrcBundle("hacf134c0acf134a89abcde489abcde30", "h888887d0888887b87654315876543140",  "h3fb72df03fb72dd82d82d7782d82d760", "hf6e5d40ff6e5d3f8e4b17d97e4b17d80"), vrgather.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=16, uopIdx = 2)),
         genVPermInput(SrcBundle("hd159e1b0d159e198bf258b38bf258b20", "h888887d0888887b87654315876543140",  "h3fb72df03fb72dd82d82d7782d82d760", "hf6e5d40ff6e5d3f8e4b17d97e4b17d80"), vrgather.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=16, uopIdx = 3)),
         // vrgather16 lmul=2 vl =16
         genVPermInput(SrcBundle("hd5e6eefcd5e6edc8c3b29884c3b29750", "h8d15951c8d1593e87ae13ea47ae13d70",  "h44443b3c44443a08320fe4c4320fe390", "h1fdb8e4c1fdb8d180da737d40da736a0"), vrgather.copy(u8, u8, u16, vm = false, ta = false, ma = false, vlmul = 1, vl=16)),
         genVPermInput(SrcBundle("hd5e6eefcd5e6edc8c3b29884c3b29750", "hb17e420cb17e40d89f49eb949f49ea60",  "h44443b3c44443a08000f00c4320fe390", "h1fdb8e4c1fdb8d180da737d40da736a0"), vrgather.copy(u8, u8, u16, vm = false, ta = false, ma = false, vlmul = 1, vl=16, uopIdx = 1)),
         genVPermInput(SrcBundle("hfa4f9becfa4f9ab8e81b4574e81b4440", "h8d15951c8d1593e87ae13ea47ae13d70",  "h4444000044000008000f00c4320fe390", "h1fdb8e4c1fdb8d180da737d40da736a0"), vrgather.copy(u8, u8, u16, vm = false, ta = false, ma = false, vlmul = 1, vl=16, uopIdx = 2)),
         genVPermInput(SrcBundle("hfa4f9becfa4f9ab8e81b4574e81b4440", "hb17e420cb17e40d89f49eb949f49ea60",  "h4444000044000008000f00c4320fe390", "h1fdb8e4c1fdb8d180da737d40da736a0"), vrgather.copy(u8, u8, u16, vm = false, ta = false, ma = false, vlmul = 1, vl=16, uopIdx = 3)),
         genVPermInput(SrcBundle("hd5e6eefcd5e6edc8c3b29884c3b29750", "h8d15951c8d1593e87ae13ea47ae13d70",  "h68ace82c68ace6f8567891b456789080", "h1fdb8e4c1fdb8d180da737d40da736a0"), vrgather.copy(u8, u8, u16, vm = false, ta = false, ma = false, vlmul = 1, vl=16, uopIdx = 4)),
         genVPermInput(SrcBundle("hd5e6eefcd5e6edc8c3b29884c3b29750", "hb17e420cb17e40d89f49eb949f49ea60",  "h68ace82c68ace6f8567891b456789080", "h1fdb8e4c1fdb8d180da737d40da736a0"), vrgather.copy(u8, u8, u16, vm = false, ta = false, ma = false, vlmul = 1, vl=16, uopIdx = 5)),
         genVPermInput(SrcBundle("hfa4f9becfa4f9ab8e81b4574e81b4440", "h8d15951c8d1593e87ae13ea47ae13d70",  "h68ace82c68ace6f8567891b456789080", "h1fdb8e4c1fdb8d180da737d40da736a0"), vrgather.copy(u8, u8, u16, vm = false, ta = false, ma = false, vlmul = 1, vl=16, uopIdx = 6)),
         genVPermInput(SrcBundle("hfa4f9becfa4f9ab8e81b4574e81b4440", "hb17e420cb17e40d89f49eb949f49ea60",  "h68ace82c68ace6f8567891b456789080", "h1fdb8e4c1fdb8d180da737d40da736a0"), vrgather.copy(u8, u8, u16, vm = false, ta = false, ma = false, vlmul = 1, vl=16, uopIdx = 7)),
         // vrgather_vx, lmul=2 vl=16
         genVPermInput(SrcBundle("h91a2b3c091a2b3c37f6e5d487f6e5d4b", "h3",  "h48d159e048d159e3369d0368369d036b", "h2468acf02468acf3123456781234567b"), vrgather_vx.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=16)),
         genVPermInput(SrcBundle("hb60b60b0b60b60b3a3d70a38a3d70a3b", "h3",  "h487f597f487f7fe3367f7f7f7f9d7f7f", "h2468acf02468acf3123456781234567b"), vrgather_vx.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=16, uopIdx = 1)),
         genVPermInput(SrcBundle("h91a2b3c091a2b3c37f6e5d487f6e5d4b", "h3",  "h6d3a06d06d3a06d35b05b0585b05b05b", "h2468acf02468acf3123456781234567b"), vrgather_vx.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=16, uopIdx = 2)),
         genVPermInput(SrcBundle("hb60b60b0b60b60b3a3d70a38a3d70a3b", "h3",  "h6d3a06d06d3a06d35b05b0585b05b05b", "h2468acf02468acf3123456781234567b"), vrgather_vx.copy(u8, u8, u8, vm = false, ta = false, ma = false, vlmul = 1, vl=16, uopIdx = 3)),
         // vmv2r
         genVPermInput(SrcBundle("h48d159e048d159e3369d0368369d036b", "h0", "h0", "h0"), vmvnr.copy()),
         genVPermInput(SrcBundle("h48d159e048d159e3369d0368369d0361", "h0", "h0", "h0"), vmvnr.copy(uopIdx = 1)),
 
        )

        //----- Output expectation -----
        val outputSeq = Seq(
        // vcompress, lmul=1
        genVAluOutput("h8344c0aa5b76129530ac75dc32000f06"),
        // vcompress, lmul=2
        genVAluOutput("h579be00b579be008456789e6dbd5e6b2"),
        genVAluOutput("h5007c048cfb7c048cf869d0368369d0"),
        genVAluOutput("h579be00b3e93e9c8d70a53e6dbd5e6b2"),
        genVAluOutput("h7c048cfb7c048cf869d0368369d03680"),
        // vslide1up, lmul=1
        genVAluOutput("h4806596d4806d7e736b0585b05b05f07"),
        // vslide1up, lmul=2 vl=16
        genVAluOutput("h48b3599148b3c9e96e9d0368369d0304"),
        genVAluOutput("h6d3a06d06d3a06d95b05b0585b05b061"),
        // vslide1up, lmul=2 vl=32
        genVAluOutput("h48b3599148b3c4e4365d487f6e5d036c"),
        genVAluOutput("h6d3a06b66d3ab4d45b0538a35b0ab05c"),
        // vslideup, lmul=2 vl=16
        genVAluOutput("h48a259c9486e5de97f9d0368369d0371"),
        genVAluOutput("h6d3a06d06d3a06d95b05b0585b05b061"),
        genVAluOutput("h6d3a06d06d3a06d95b05b0585b05b061"),
        // vslidedn, lmul=2 vl=16
        genVAluOutput("h48d75941480000e9009d0368369d0300"),
        genVAluOutput("h48d7594148a2b3e9919d0368369d0348"),
        genVAluOutput("h6d3a06d06d3a06d95b05b0585b05b061"),
        // vslide1dn, lmul=2 vl=16
        genVAluOutput("h489159b34891a2e9c99d0368369d035d"),
        genVAluOutput("h489159b34891a2e9c99d0368369d035d"),
        genVAluOutput("h6d3a06d06d3a06d95b05b0585b05b061"),
         // vrgather, lmul=2 vl=16
        genVAluOutput("h1b00000000008000001a2a88091a2a70"),
        genVAluOutput("h1bd1000000d18000001a2a88091a2a70"),
        genVAluOutput("h3fb72df03fb72dd82d82d7782d82d760"),
        genVAluOutput("h3fb72df03fb72dd82d82d7782d82d760"),
        // vrgather16, lmul=2 vl=16
        genVAluOutput("h44443b3c44443a08000f00c4320fe390"),
        genVAluOutput("h4444000044000008000f00c4320fe390"),
        genVAluOutput("h4444000044000008000f00c4320fe390"),
        genVAluOutput("h4444000044000008000f00c4320fe390"),
        genVAluOutput("h68ace82c68ace6f8567891b456789080"),
        genVAluOutput("h68ace82c68ace6f8567891b456789080"),
        genVAluOutput("h68ace82c68ace6f8567891b456789080"),
        genVAluOutput("h68ace82c68ace6f8567891b456789080"),
        // vrgather_vx, lmul=2 vl=16
        genVAluOutput("h487f597f487f7fe3367f7f7f7f9d7f7f"),
        genVAluOutput("h487f597f487f7fe3367f7f7f7f9d7f7f"),
        genVAluOutput("h6d3a06d06d3a06d35b05b0585b05b05b"),
        genVAluOutput("h6d3a06d06d3a06d35b05b0585b05b05b"),
        // vmv2r
        genVAluOutput("h48d159e048d159e3369d0368369d036b"),
        genVAluOutput("h48d159e048d159e3369d0368369d0361"),

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

class VPermSpec extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VPermBehavior {
  behavior of "Permutation test"
  it should behave like vPermTest() 
}
