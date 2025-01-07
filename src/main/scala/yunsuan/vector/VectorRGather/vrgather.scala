package yunsuan.vector

import chisel3._
import chisel3.util._
import yunsuan.util._
import yunsuan.vector._

trait VParameter {
  val VLEN       : Int = 2048
  val VLMUL      : Int = 8  // vlmul max
}

// valid only for VLEN = 2048
class Vrgather() extends Module with VParameter {
  val XLEN = 64
  val io = IO(new Bundle {
    val vs1 = Input(UInt(VLEN.W))   //index data
    val vs2 = Input(UInt(VLEN.W))   //table data
    val res_vd = Output(UInt(VLEN.W))

    val sew       = Input(UInt(2.W))

    val vm        = Input(Bool())         // 0: masked, 1: unmasked
    val ta        = Input(Bool())         // 0: undisturbed, 1: agnostic
    val ma        = Input(Bool())         // 0: undisturbed, 1: agnostic

    val vstart    = Input(UInt(7.W))      // 0-127
    val vl        = Input(UInt(8.W))      // 0-128
    val vlmul     = Input(UInt(3.W))
  })

  // latency  = 1
  // width max = 64
  // val width = Wire(UInt(6.W))
  // width := (1.U << io.sew) << 3.U

  val vrgather_e8  = Module(new VRGather_with_sew(SEW = 0))
  val vrgather_e16 = Module(new VRGather_with_sew(SEW = 1))


  vrgather_e8.io.vs1 := io.vs1
  vrgather_e8.io.vs2 := io.vs2
  // vrgather_e8.io.vstart := io.vstart
  
  vrgather_e16.io.vs1 := io.vs1
  vrgather_e16.io.vs2 := io.vs2
  // vrgather_e16.io.vstart := io.vstart

  io.res_vd := LookupTreeDefault(io.sew, 0.U(VLEN.W), List(
      0.U -> vrgather_e8.io.res_data,
      1.U -> vrgather_e16.io.res_data
  ) )
}

// vlmul = 0
class VRGather_with_sew(
  val SEW: Int = 1
  ) 
  extends Module with VParameter{
  val XLEN = 8 << SEW
  val VLMAX = VLMUL * VLEN / XLEN
  val io = IO(new Bundle {
    val vs1 = Input(UInt(VLEN.W))
    val vs2 = Input(UInt(VLEN.W))
    val res_data = Output(UInt(VLEN.W))

    // val vstart    = Input(UInt(7.W))
    // val vl        = Input(UInt(8.W))
    // val vm        = Input(Bool())
    // val ta        = Input(Bool())
    // val ma        = Input(Bool())
  })

    val index = Wire(Vec(VLEN / XLEN, UInt(VLMAX.W)))
    val lut   = Wire(Vec(VLEN / XLEN, UInt(XLEN.W)))

    for (n <- 0 until (VLEN / XLEN)) {
      index(n) := 1.U << io.vs1((n + 1) * XLEN - 1, n * XLEN) 
      lut(n)   := io.vs2((n + 1) * XLEN - 1, n * XLEN)    
    }
    // ASCII Chart for VRGather_with_sew result calculation:
    //
    // vs1: [element N-1][element N-2]...[element 0]
    //      |<- XLEN  ->||<- XLEN  ->|...|<- XLEN->|
    //
    // vs2: [element N-1][element N-2]...[element 0]
    //      |<- XLEN  ->||<- XLEN  ->|...|<- XLEN->|
    //
    // index(n): 1.U << vs1[n]  => One-hot vector of size VLMAX
    // lut(n):   vs2[n]         => XLEN-bit value
    //
    // res(n) calculation:
    //   for each element n:
    //     for each possible index m:
    //       Fill(XLEN, index(n)(m)) & lut(m)  => XLEN-bit mask from index
    //     reduce with OR to get final value selected by mask
    //
    // Final result:
    //   io.res_data = [res(N-1)][res(N-2)]...[res(0)]
    //                  |<-XLEN->| |<-XLEN->| ... |<-XLEN->|

    // Example
    //
    // Input vs1 (indices): [3, 0, 2, 1] = 0x03000201
    // Input vs2 (data):    [A, B, C, D] = 0x0A0B0C0D
    //
    // vs2(lut): [A][B][C][D]
    //    index: |0||1||2||3|
    //    width: |8||8||8||8|
    //
    // index(0) = 1 << 3 = 00001000 (selects element 3)
    // index(1) = 1 << 0 = 00000001 (selects element 0)
    // index(2) = 1 << 2 = 00000100 (selects element 2)
    // index(3) = 1 << 1 = 00000010 (selects element 1)
    //
    // res(0) = vs2[3] = D
    // res(1) = vs2[0] = A
    // res(2) = vs2[2] = C
    // res(3) = vs2[1] = B
    //
    // Final result: [D][A][C][B] = 0x0D0A0C0B
    //
    // More detailed:
    //
    // Fill(XLEN, index(0)(0)) = 00000000 (mask for element 0)
    // Fill(XLEN, index(0)(1)) = 00000000 (mask for element 1)
    // Fill(XLEN, index(0)(2)) = 00000000 (mask for element 2)
    // Fill(XLEN, index(0)(3)) = 11111111 (mask for element 3)
    //
    // Fill(XLEN, index(0)(0)) & lut(0) = 00000000 & 0A = 00
    // Fill(XLEN, index(0)(1)) & lut(1) = 00000000 & 0B = 00
    // Fill(XLEN, index(0)(2)) & lut(2) = 00000000 & 0C = 00
    // Fill(XLEN, index(0)(3)) & lut(3) = 11111111 & 0D = 0D
    //
    // {00, 00, 00, 0D}.reduce(_ | _) = 0D
    //

    val res = RegInit(VecInit(Seq.fill(VLEN / XLEN)(0.U(XLEN.W))))

    for (n <- 0 until (VLEN / XLEN)) {
      res(n) := Cat((0 until VLEN / XLEN).map { m =>  Fill(XLEN, index(n)(m)) & lut(m)}.reduce(_ | _))
    }

    io.res_data := Cat((0 until VLEN/XLEN).map(i => res(i)).reverse)


}