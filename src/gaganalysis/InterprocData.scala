package gaganalysis

// This file describes the data structures used in the interprocedural analysis.

case class MethodSummary(val retObj: GAG,
                         val ptsFO: PtsFlowObject,
                         val seFO: SEFlowObject) {
  def reads = seFO.reads
  def writes = seFO.writes
  def ptsEnv = ptsFO.env
  def ptsMap = ptsFO.map
}

object MethodSummary {
  def bottom = MethodSummary(GAG.bottom,
                             PtsFlowObject.bottom,
                             SEFlowObject.bottom)
}