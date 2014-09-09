package gaganalysis

import scalax.collection.mutable.Graph
import scala.collection.mutable.Set
import SCCGraph._
import soot.SootMethod
import soot.jimple.toolkits.callgraph.CallGraph
import soot.jimple.toolkits.annotation.purity.SootMethodFilter
import java.util.LinkedList
import soot.jimple.toolkits.annotation.purity.DirectedCallGraph
import soot.toolkits.graph.DirectedGraph
import scala.collection.mutable.Queue

// This file describes how to create the SCC graph of the soot call graph and
// two methods providing graph traversal.

object SCCCallGraph {
  def fromSoot(cg: CallGraph,
               rootMethod: SootMethod): SCCGraph = {
    val filter = new SootMethodFilter() {
      override def want(m: SootMethod): Boolean =
             (m.getName != "<clinit>")
//            && !m.isJavaLibraryMethod()
//              && !m.getName().equals("equals")
//              && !m.getName().equals("hashCode")
//              && !m.getName().equals("compareTo")
//              && !m.getName().equals("toString")
    }
    
    val headList = new LinkedList[SootMethod];
    headList add rootMethod
    
    val dcg = new DirectedCallGraph(cg, filter, headList.iterator, false)
    SCCGraph.fromSootGraph(dcg.asInstanceOf[DirectedGraph[SootMethod]])
  }
  
  def topologicalTraversal(g: SCCGraph, f: SCC => Unit) = {
    val unprocessed = g.nodes.clone
    while (!unprocessed.isEmpty)
      unprocessed
        . filter { scc => (scc.outNeighbors intersect unprocessed).isEmpty }
        . map    { scc => f(scc); unprocessed -= scc }
  }
  
  def depthFirstTraversal(g: SCCGraph,
                          onEntry: SCC => Unit,
                          onExit:  SCC => Unit) = {
    def f(n: g.NodeT): Unit = { onEntry(n); n.outNeighbors map f; onExit(n) }
    g.nodes filter { _.inDegree == 0 } map f
  }
}