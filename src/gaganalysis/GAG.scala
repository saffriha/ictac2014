package gaganalysis

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Map
import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import scala.reflect.runtime.universe

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.Param
import scalax.collection.GraphPredef.anyToNode
import scalax.collection.GraphPredef.edgeSetToSeq
import scalax.collection.GraphPredef.nodeSetToSeq
import scalax.collection.GraphPredef.predicateToNodePredicate
import scalax.collection.mutable.Graph
import scalax.collection.mutable.Graph.empty$default$2
import scalaz.Scalaz
import Scalaz._

// This file defines the abstract domain of Generalized Access Graphs (GAGs),
// which we use to model the heap in both the intraprocedural points-to and
// side effect analyses.

trait Node {
  def id(): Any
  def pp(): Any
  override def toString(): String = id + "@" + pp
}

case class NodeImpl(val _id: Any, val _pp: Any) extends Node {
  override def id() = _id
  override def pp() = _pp
}

case class GAG(val graph: Graph[Node, DiEdge],
               val roots: Set[Node],
               val exits: Set[Node]) {
  type Edge = (Node, Node)
  type NodeT = graph.NodeT
  
  def copy(): GAG = new GAG(graph.clone, roots.clone, exits.clone)

  def analysis[FO](inToOut: (Node, FO) => FO,
                   join: (FO, FO) => FO,
                   bottom: => FO,
                   startFrom: Iterable[Node]
  ): (Map[Node, FO], Map[Node, FO]) = {
    def bottomMap: Map[Node, FO] =
      HashMap() ++= (nodes map {n => (n.value, bottom)})
    val init = (bottomMap, bottomMap)
    analysis(inToOut, join, init, startFrom)
  }
  
  def analysis[FO](inToOut: ((Node, FO) => FO),
                   join: (FO, FO) => FO,
                   init: (Map[Node, FO], Map[Node, FO]),
                   startFrom: Iterable[Node]
  ): (Map[Node, FO], Map[Node, FO]) = {
    val q = Queue() ++= startFrom
    val initIn = init._1
    val initOut = init._2
    val in: Map[Node, FO] = HashMap()
    val out: Map[Node, FO] = HashMap()
    while (!q.isEmpty) {
      val n = q.dequeue
      val predOuts = preds(n) map { p => out.get(p) | initOut.get(p).get }
      val newIn = (predOuts reduceOption join) | initIn.get(n).get
      in.put(n, newIn)
      val newOut = inToOut(n, newIn)
      (out get n) match {
        case Some(oldOut) if oldOut == newOut => { }
        case _ =>
          out.put(n, newOut)
          q ++= succs(n) map { _.value } filter { n => !(q contains n) }
      }
    }
    (in, out)
  }
  
  // Accessors
  def nodes = graph.nodes
  def edges = graph.edges
  def rootsT: Set[NodeT] = roots map graph.get
  def exitsT: Set[NodeT] = exits map graph.get
  def preds(n: Node): Iterable[NodeT] = graph.get(n).inNeighbors
  def succs(n: Node): Iterable[NodeT] = graph.get(n).outNeighbors
  def hasRoot(n: Node): Boolean = roots contains n
  def hasExit(n: Node): Boolean = exits contains n
  def hasNode(n: Node): Boolean = nodes contains n
  def hasEdge(e: Edge): Boolean = edges contains e
  def hasEdge(e: DiEdge[Node]): Boolean = edges contains e
  
  // FIXME: find better names to diff between bool and this mutators

  // Boolean Mutators
  // Return true if the operation mutated the AccesseGraph instance.
  def add(n: Node): Boolean = graph add n
  def add(e: Edge): Boolean = add(DiEdge(e._1, e._2))
  def add(e: DiEdge[Node]): Boolean = graph add e
  def addNodes(ns: Iterable[Node]): Unit = ns map add
  def addEdges(es: Iterable[Edge]): Unit = es map add
  
  def sub(n: Node): Boolean = { roots -= n; exits -= n; graph.remove(n) }
  def sub(e: Edge): Boolean = sub(DiEdge(e._1, e._2))
  def sub(e: DiEdge[Node]): Boolean = graph remove e
  def subNodes(ns: Iterable[Node]): Boolean = ns.foldLeft(false)(_ || sub(_))
  def subEdges(es: Iterable[Edge]): Boolean = es.foldLeft(false)(_ || sub(_))
  
  def setRoot(n: Node): Boolean = { add(n); roots add n }
  def setRoots(ns: Iterable[Node]): Boolean = ns.foldLeft(false)(_ || setRoot(_))
  def unsetRoot(n: Node): Boolean = roots remove n
  def unsetRoots(ns: Iterable[Node]): Boolean = ns.foldLeft(false)(_ || unsetRoot(_))
  
  def setExit(n: Node): Boolean = { add(n); exits add n }
  def setExits(ns: Iterable[Node]): Boolean = ns.foldLeft(false)(_ || setExit(_))
  def unsetExit(n: Node): Boolean = exits remove n
  def unsetExits(ns: Iterable[Node]): Boolean = ns.foldLeft(false)(_ || unsetExit(_))
  
  // This Mutators
  def +=(n: Node) = { add(n); this }
  def +=(e: Edge) = { add(e); this }
  def +=(e: DiEdge[Node]) = { add(e); this }
  
  def -=(n: Node): GAG = { sub(n); this }
  def -=(e: Edge): GAG = { sub(e); this }
  def -=(e: DiEdge[Node]): GAG = { sub(e); this }
  
  def clear(): GAG = { graph.clear; roots.clear; exits.clear; this }
  def set(g: GAG): GAG = { clear; this join g }
  
  def join(g: GAG): GAG = {
    graph ++= g.graph
    roots ++= g.roots
    exits ++= g.exits
    this
  }
  def join(gs: GAG*): GAG = { gs map join; this }
  def join(gs: Iterable[GAG]): GAG = { gs map join; this }
  def union(g: GAG): GAG = join(g)
  def +=(g: GAG): GAG = join(g)
  
  def concat(rg: GAG): GAG = {
    graph ++= rg.graph
    addEdges(for (s <- exits; t <- rg.roots) yield (s, t))
    exits.clear
    exits ++= rg.exits
    this
  }
  def *=(rg: GAG): GAG = concat(rg)
  
  // Pure Operators
  def +(n: Node): GAG = copy += n
  def +(e: Edge): GAG = copy += e
  def +(e: DiEdge[Node]): GAG = copy += e
  
  def -(n: Node): GAG = copy -= n
  def -(e: Edge): GAG = copy -= e
  def -(e: DiEdge[Node]): GAG = copy -= e
  
  def +(g: GAG): GAG = copy += g

  def *(rg: GAG): GAG = copy *= rg 

  // Functor
  def map[B, That](f: (Param[Node, DiEdge]) ⇒ B)(implicit bf: CanBuildFrom[Graph[Node, DiEdge], B, That]): That = {
    def f2(n: Node): B = f(n)
    roots map f2
    exits map f2
    graph map f
  }

  def isGreaterEqThan(g: GAG) = g isSmallerEqThan this
  def isSmallerEqThan(g: GAG) =
    (graph subsetOf g.graph) &&
    (roots subsetOf g.roots) &&
    (exits subsetOf g.exits)
  def <=(g: GAG): Boolean = isSmallerEqThan(g)
  def >=(g: GAG): Boolean = isGreaterEqThan(g)

  def splitExits(): (GAG, List[Node]) = {
    val g = copy
    exits map g.unsetExit
    exits flatMap g.preds map g.setExit
    g.cleanup
    (g, exits.clone.asInstanceOf)
  }

  def cleanup() = graph.nodes filter onPathFromRootToExit

  def correspondingNodes(ag: GAG, ns2: Iterable[Node]):
      Iterable[Set[Node]] =
    ns2 map (correspondingNodes(ag).get(_).getOrElse(Set()))

  def correspondingNodes(ag: GAG): Map[Node, Set[Node]] = {
    val cns: Map[Node, Set[Node]] = new HashMap
    nodes map (cns.put(_, new HashSet))

    var nodeStack: List[(Node, Node)] = nil

    for { r1 <- this.roots;
          r2 <- ag.roots;
          if r1.id == r2.id }
        { cns.get(r1).get.add(r2)
          nodeStack :+ (r1, r2) }

    while (!nodeStack.isEmpty) {
      val addMap: Map[Node, Set[Node]] = new HashMap

      val (n1, n2) = nodeStack.head
      nodeStack = nodeStack.tail

      for (
        s1 <- this.succs(n1);
        s2 <- ag.succs(n2);
        if (s1.id == s2.id) && !(cns.get(s1).get contains s2)
      ) {
        addMap.getOrElseUpdate(s1, new HashSet) add s2
        nodeStack :+ (s1, s2);
      }

      for ((n, ss) <- addMap; s <- ss)
        cns.get(n).get.add(s)
    }
    cns
  }

  def remainderAt(newEntry: Node): GAG = remainderAt(List(newEntry))
  def remainderAt(newEntries: Iterable[Node]): GAG = {
    val ag = copy()
    roots map (ag unsetRoot _)
    newEntries map (ag setRoot _)
    ag.cleanup
    ag
  }

  // Print compact, structurally complete string representation of the GAG by
  // listing paths of a spanning tree.
  override def toString: String = {
    def nodeToString(n: NodeT) =
      n.value.id + "@" + n.value.pp + 
      (if (hasRoot(n) || hasExit(n)) ":" else "") +
      (if (hasRoot(n)) "R" else "") +
      (if (hasExit(n)) "E" else "")
      
    "GAG { " +
      spanningLists . map { "[ " + _ . map { nodeToString }
                                     . reduceOption {_ + " → " + _}
                                     . getOrElse("") + " ]" } 
                    . reduceOption {_ + ",\n  " + _}
                    . getOrElse("") + 
    " }"
  }

  def spanningLists(): List[List[NodeT]] = {
    var paths: List[List[NodeT]] = nil
    var path: List[NodeT] = nil
    val marked: Set[(NodeT, NodeT)] = Set()
    var queue: List[NodeT] = rootsT.toList
    def isUnmarked(e: (NodeT, NodeT)) = !(marked contains e)
    def outEdges(n: NodeT) = n.outNeighbors map { (n, _) }
    while (queue != nil) {
      val n = queue.head
      queue = queue.tail
      path = path :+ n
      outEdges(n).filter(isUnmarked).toList match {
        case List() => { paths = paths :+ path; path = List[NodeT]() }
        case e :: es => {
          marked add e;
          if (es != nil)
            queue = n :: (queue filter {_ != n})
          queue = e._2 :: (queue filter {_ != e._2})
        }
      }
    }
    paths
  }

  def hasPathFromTo(src: Node, tgt: Node): Boolean =
    !((graph get src) pathTo (graph get tgt)).isEmpty
  def hasPathFromRoot(n: Node): Boolean =
    !roots.filter(hasPathFromTo(_, n)).isEmpty
  def hasPathToExit(n: Node): Boolean =
    !exits.filter(hasPathFromTo(n, _)).isEmpty
  def onPathFromRootToExit(n: Node): Boolean =
    hasPathFromRoot(n) && hasPathToExit(n)

  def disjoint(ag: GAG): Boolean =
    correspondingNodes(ag, ag.exits)
      . filter { ns => !(exits intersect ns).isEmpty }
      . isEmpty

  def hasEmptyLang(): Boolean =
    roots.filter(hasPathToExit).isEmpty

  // TODO: optimized AG version where cleanup state is cached and automatically
  // managed
  def hasFiniteLang(): Boolean = { var g = copy; g.cleanup; g.graph.isCyclic }
  
//  def reachedNodes(startFrom: List[NodeT],
//                   succs: NodeT => Iterable[NodeT]): Set[NodeT] =
//    worklistT[Set[NodeT]](startWith = Set(),
//      startFrom = startFrom,
//      continueOn = state => node => state add node,
//      succs = succs)
//
//  def worklistT[T](startWith: T,
//                   startFrom: List[NodeT],
//                   continueOn: T => NodeT => Boolean,
//                   succs: NodeT => Iterable[NodeT]): T =
//    { var v = startWith; worklist(startFrom, continueOn(v), succs); v }
//  // FIXME: no idea what comes back here
//
//  def worklist(startFrom: List[NodeT],
//               continueOn: NodeT => Boolean,
//               succs: NodeT => Iterable[NodeT]) = {
//    var ns = startFrom
//    while (!ns.isEmpty) {
//      val n = ns.head
//      ns = ns.tail
//      if (continueOn(n)) ns :+ succs(n)
//    }
//  }

  //    worklistT [List[List[(NodeT,NodeT)]]] (
  //        startWith  = nil,
  //        startFrom  = entriesT.toList,
  //        continueOn = state => node => {
  //          val curPath = state.head
  //          if ((curPath contains node) ||
  //            node.outNeighbors.isEmpty) {
  //            state :+ nil
  //            false
  //          }
  //          else {
  //            curPath :+ node
  //            true
  //          }
  //        },
  //        succs      = _.outNeighbors
  //      )

  //  def replace(nOld: Node, nNew: Node) = {
  //    if (entries contains nOld) { entries -= nOld; entries += nNew}
  //    if (exits contains nOld) { exits -= nOld; exits += nNew}
  //    if (nodes contains nOld) { graph -= nOld; graph += nNew}
  //    graph map { case n => if (n == nOld) nNew else n }
  //  }


  //  def containsWord(idWord: Iterable[Any]) = {
  //    if (idWord.isEmpty) false;
  //    
  //    var w = idWord
  //    var stack : List[(Iterable[Any],Node)] = nil
  //    stack :+ (entries filter ({_.id == w.head})) map ((w.tail, _))
  //    for ((w1,n) <- stack) {
  //      stack :+ (outNeighbors(n) filter ({_.id == w1.head})) map ((w1.tail, _))
  //    }
  //    false
  //  }

}

object GAG {
  def emptyLang(): GAG =
    new GAG(Graph.empty, new HashSet, new HashSet)

  def bottom() = emptyLang
  
  def fromSets(nodes: Set[Node],
               edges: Set[(Node, Node)],
               roots: Set[Node],
               exits: Set[Node]) : GAG =
    new GAG(Graph.from(nodes, edges map {e => DiEdge(e._1, e._2)}), roots, exits)
  
  def fromNode(id: Any, pp: Any): GAG = fromNode(new NodeImpl(id, pp))
  def fromNode(n: Node): GAG =
    new GAG(
      Graph.from(List(n), nil),
      HashSet() += n,
      HashSet() += n)

  def fromPath(path: Node*): GAG = fromPath(path.toList)
  def fromPath(path: Iterable[Node]): GAG = {
    if (path.isEmpty) bottom
    else new GAG(
      Graph.from(path, (path.init, path.tail).zipped map (DiEdge(_, _))),
      HashSet() += path.head,
      HashSet() += path.last)
  }

  def fromPaths(paths: Iterable[Node]*): GAG =
    fromPaths(paths.toList)
  def fromPaths(paths: Iterable[Iterable[Node]]): GAG =
    paths.foldLeft(bottom)((ag, p) => { ag join fromPath(p); ag })
    
  def join(gags: Iterable[GAG]) : GAG = gags.foldLeft(bottom)(_ += _)
}
