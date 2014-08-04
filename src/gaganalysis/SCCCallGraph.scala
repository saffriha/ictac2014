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
               rootMethod: SootMethod): SCCGraph[SootMethod] = {
    val filter = new SootMethodFilter() {
      override def want(m: SootMethod): Boolean =
             !m.getName.equals("<clinit>")
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
  
  def breadthFirstBackTraversal(g: SCCGraph[SootMethod],
                                f: SCC[SootMethod] => Unit) = {
    val exits = g.nodes filter { _.outDegree == 0 }
    println("Exits: " + exits)
    println("Graph: " + g)
    val visited = Set[g.NodeT]()
    val q = Queue() ++= exits
    while (!q.isEmpty) {
      val n = q.dequeue
      f(n)
      q ++= (n.inNeighbors filter { !visited.contains(_) })
      visited ++= n.inNeighbors
    }
  }
  
  def depthFirstTraversal(g: SCCGraph[SootMethod],
                          onEntry: SCC[SootMethod] => Unit,
                          onExit:  SCC[SootMethod] => Unit) = {
    def f(n: g.NodeT): Unit = { onEntry(n); n.outNeighbors map f; onExit(n) }
    g.nodes filter { _.inDegree == 0 } map f
  }
}