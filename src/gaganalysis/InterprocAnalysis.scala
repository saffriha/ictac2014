package gaganalysis

import scala.collection.mutable.HashMap
import scala.collection.mutable.Map

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable.Graph
import soot.Scene
import soot.SootMethod
import soot.toolkits.graph.BriefUnitGraph
import soot.toolkits.graph.UnitGraph
import sootproxy.ScalaSceneTransformer

// This file describes the interprocedural part of our analysis.

class InterprocAnalysis extends ScalaSceneTransformer {
  val summaries: Map[SootMethod, MethodSummary] = HashMap()
  
  // This method get's called from soot to query our analysis.
  // At this point the call graph and certain other structures relating the
  // whole program have been already constructed and can be accessed using
  // soot's Scene.v() Java-style singleton.
  override def internalTransform(phaseName: String) = {
    // Create an SCC Graph of soot's call graph.
    Log.v.Timer.sccGraphConstruction.start
    val sccGraph: Graph[SCC, DiEdge] =
      SCCCallGraph.fromSoot(Scene.v.getCallGraph, Scene.v.getMainMethod)
    Log.v.Timer.sccGraphConstruction.stop
      
    // Initialize the method summaries of all methods to bottom.
    val methods = sccGraph.nodes.toList.flatten(_.nodes.toList) sortBy { _.getSignature }
    methods map { summaries.put(_, MethodSummary.bottom) }

    // Traverse the SCC graph bottom-up and compute for each SCC the
    // fixpoint of method summaries for the contained methods.
    SCCCallGraph.topologicalTraversal(sccGraph, onSCC)
    
    def onSCC(scc: SCC): Unit = {
      while (true) {
        var changed = false
        Log.v.Timer.interprocAnalysis.start
        for (m <- scc.nodes) {
          if (!m.hasActiveBody) println("Method " + m.getSignature()
                                      + " has no active body. Skipping method.")
          else {
            println("Current method: " + m.getSignature()) // + ", Body: " + m.getActiveBody)
            Log.v.Timer.intraprocAnalysis.start
            Log.v.onStartIntraproc(m)
            
            val body = m.getActiveBody
            val summary = summaries.get(m).get
            val cfg: UnitGraph = new BriefUnitGraph(body)

            // Apply Points-to Analysis.
            Log.v.Timer.ptsAnalysis.start
            val ptsAnalysis = new IntraprocPtsAnalysis(cfg, body, summaries)
            Log.v.Timer.ptsAnalysis.stop

            // Apply Side Effect Analysis.
            Log.v.Timer.seAnalysis.start
            val seAnalysis = new IntraprocSEAnalysis(cfg, body, ptsAnalysis, summaries)
            Log.v.Timer.seAnalysis.stop
            
            // Combine analysis results to new method summary.
            val newSummary = MethodSummary(
              ptsAnalysis.getReturnObj,
              ptsAnalysis.getReturnFO,
              seAnalysis.getSummary)
                
            // If the summary has changed, update the summary and queue another
            // iteration of analyses for the current SCC.
            if (summary != newSummary) {
              summaries.put(m, newSummary)
              changed = true
            }
            
            Log.v.Timer.intraprocAnalysis.stop
          }
        }
        Log.v.Timer.interprocAnalysis.stop
        if (!changed) return
      }
    }
    
    Log.v.Timer.total.stop
    Log.v.finished(summaries)
  }
}