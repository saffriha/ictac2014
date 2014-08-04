package gaganalysis

// This file describes the data structures used by the intraprocedural side
// effect analysis.

case class SEFlowObject(val reads: GAG, val writes: GAG) {
  def join(fo: SEFlowObject) = { reads join fo.reads; writes join fo.writes }
  def clear() = { reads set GAG.bottom; writes set GAG.bottom }
}
object SEFlowObject {
  def bottom = SEFlowObject(GAG.bottom, GAG.bottom)
}

case class SESummary(val reads: GAG, val writes: GAG)