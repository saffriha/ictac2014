package gaganalysis

import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.mutable.HashMap
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable.Graph
import soot.toolkits.graph.DirectedGraph
import soot.toolkits.graph.StronglyConnectedComponentsFast
import soot.SootMethod

// This file describes how to create the SCC graph of an directed graph from
// the soot framework.

case class SCC(val nodes: List[SootMethod])

object SCCGraph {
  type SCCGraph = Graph[SCC, DiEdge]
  
  def fromSootGraph(g: DirectedGraph[SootMethod]): Graph[SCC, DiEdge] = {
    // Calculate SCCs.
	val components: List[SCC] =
	  new StronglyConnectedComponentsFast(g).getComponents . toList
	    . map {x => SCC(x.toList sortBy {_.getSignature}) }
	
	// Map each node to its SCC node.
	val sccMap = HashMap[SootMethod, SCC]()
    for { scc <- components; n <- scc.nodes }
      sccMap.put(n, scc)
      
    // Collect SCC graph nodes and edges.
    var sccNodes: Set[SCC] = Set()
    var sccEdges: Set[DiEdge[SCC]] = Set()
    for { srcSCC  <- components;
          srcNode <- srcSCC.nodes;
          tgtNode <- g.getSuccsOf(srcNode);
          tgtSCC  <- sccMap.get(tgtNode);
          if srcSCC != tgtSCC }
        { sccEdges += DiEdge(srcSCC, tgtSCC)
          sccNodes += srcSCC
          sccNodes += tgtSCC }
    
    // Create SCC graph
    Graph.from(sccNodes.toList.asInstanceOf[Iterable[AnyRef]],
               sccEdges.toList.asInstanceOf[Iterable[DiEdge[AnyRef]]])
      . asInstanceOf[Graph[SCC, DiEdge]]
  }
}