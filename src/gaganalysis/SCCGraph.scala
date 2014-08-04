package gaganalysis

import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.mutable.HashMap

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable.Graph
import soot.toolkits.graph.DirectedGraph
import soot.toolkits.graph.StronglyConnectedComponentsFast

// This file describes how to create the SCC graph of an directed graph from
// the soot framework.

case class SCC[N](val nodes: Set[N])

object SCCGraph {
  type SCCGraph[N] = Graph[SCC[N], DiEdge]
  
  def fromSootGraph[N](g: DirectedGraph[N]): Graph[SCC[N], DiEdge] = {
    // Calculate SCCs.
	val components: Set[SCC[N]] =
	  new StronglyConnectedComponentsFast[N](g).getComponents
	    . toSet . map {x: java.util.List[N] => SCC(x.toSet) }
	
	// Map each node to its SCC node.
	val sccMap = HashMap[N, SCC[N]]()
    for { scc <- components; n <- scc.nodes }
      sccMap.put(n, scc)
      
    // Collect SCC graph nodes and edges.
    var sccNodes: Set[SCC[N]] = Set()
    var sccEdges: Set[DiEdge[SCC[N]]] = Set()
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
      . asInstanceOf[Graph[SCC[N], DiEdge]]
  }
}