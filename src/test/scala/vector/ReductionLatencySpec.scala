package yunsuan.vector

import org.scalatest.funspec.AnyFunSpec
import yunsuan.encoding.Opcode.{Latency, Opcodes}

class ReductionLatencySpec extends AnyFunSpec {
  it("encodes the full fixed latency of FP reduction operations") {
    val red = Opcodes.VFRedOpcode
    assert(red.getLat(red.vfredmax_fp32) == 1)
    assert(red.getLat(red.vfredusum_fp32) == 7)
    assert(red.getLat(red.vfwredosum_fp16) == 17)
    assert(Latency().getWidth == Latency.width)
    assert((BigInt(1) << Latency().getWidth) > 17)
  }
}
