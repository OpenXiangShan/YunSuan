package yunsuan.fpu

trait FloatParams {
  val totalWidth: Int
  private val widthSeq = Seq(16, 32, 64)
  assert(widthSeq.contains(totalWidth))
  private val exponentWidthSeq  = Seq(5, 8, 11)
  private val shiftBitsWidthSeq = Seq(4, 5, 6)
  private val idx = widthSeq.indexOf(totalWidth)
  val rmWidth = 3
  val flagsWidth = 5
  val signWidth = 1
  val exponentWidth = exponentWidthSeq(idx)
  val significandWidth = totalWidth - signWidth - exponentWidth
  val decimalWidth = significandWidth + 1
  val exponentBias = (1 << (exponentWidth - 1)) - 1
  val shiftBitsWidth = shiftBitsWidthSeq(idx)

  val RNE = "b000"
  val RTZ = "b001"
  val RDN = "b010"
  val RUP = "b011"
  val RMM = "b100"
  val cNaN = "b0" + "1" * exponentWidth + "1" + "0" * (significandWidth - 1)
  val infNoSign = "b" + "1" * exponentWidth + "0" * significandWidth
  val maxNormal = "b" + "1" * (exponentWidth - 1) + "0" + "1" * significandWidth
}