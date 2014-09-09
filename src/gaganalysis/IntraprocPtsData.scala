package gaganalysis

import scala.collection.mutable.Map
import scalaz._
import scalaz.Scalaz._
import soot.Body
import soot.SootMethod
import soot.toolkits.graph.DirectedGraph
import soot.toolkits.scalar.ForwardFlowAnalysis
import scala.collection.mutable.HashMap

// This file describes the data structures used by the intraprocedural
// points-to analysis.

case class Ref(val base: GAG, val field: Field) {
  def mayAlias(r: Ref): Boolean = (field.id == r.field.id) &&
                                  !(base disjoint r.base)
  def <=(r: Ref): Boolean = (field == r.field) &&
                            (base <= r.base)
}

// Common super class for the points-to and variable environment.
abstract class GAGMap[K](map: Map[K, GAG]) {
  def put(k: K, v: GAG): Option[GAG] = map.put(k, v)
  def put(m: GAGMap[K]): Unit = m.entries map { e => put(e._1, e._2) }
  def get(k: K): Option[GAG] = map.get(k)
  def getUnsafe(k: K): GAG = get(k).get
  def entries(): List[(K, GAG)] = map.toList
  def join(k: K, v: GAG): Unit = 
    map.get(k) match {
      case Some(v2) => v2 join v
      case None     => map.put(k, v)
    }
  def join(m: GAGMap[K]): Unit = m.entries map { e => this.join(e._1, e._2) }
  def clear = map.clear
}

case class PtsMap(map: Map[Ref, GAG]) extends GAGMap[Ref](map) {
  def resolve(r: Ref): GAG = 
    GAG.join(entries . filter { _._1 mayAlias r } . map { _._2 })
      . join(r.base * GAG.fromNode(r.field.id, r.field.pp))
      
  def compact(): Unit = {
    val e = entries
    clear
    e map (join _).tupled
  }
  
  override def join(r1: Ref, or1: GAG): Unit =
    PtsMap.compactingMode match {
      case PtsCompactingMode.None => super.join(r1, or1)
      case PtsCompactingMode.Precise =>
        for { (r2@Ref(ol2, f2), or2) <- map } 
          if (r1 <= r2 && or1 <= or2) {
            put(r2, or2)
            return
          }
          else if (r2 <= r1 && or2 <= or1) {
            map remove r2
            put(r1, or1)
            return
          }
        put(r1, or1)
      case PtsCompactingMode.Conservative =>
        for { (r2@Ref(ol2, f2), or2) <- map } 
          if (r1 <= r2) {
            put(r2, or1 + or2)
            return
          }
          else if (r2 <= r1) {
            map.remove(r2);
            put(r1, or1 + or2);
            return
          }
        put(r1, or1)
    }
}

object PtsMap {
  def bottom() = PtsMap(HashMap())
  var compactingMode: PtsCompactingMode = PtsCompactingMode.None
}

trait PtsCompactingMode
object PtsCompactingMode {
  case object None extends PtsCompactingMode
  case object Conservative extends PtsCompactingMode
  case object Precise extends PtsCompactingMode
}

case class PtsEnv(map: Map[VarID, GAG]) extends GAGMap[VarID](map) {
  override def get(k: VarID): Option[GAG] = super.get(k) match {
    case Some(g) => Some(g)
    case None    => k match {
      case v : GlobalID => Some(GAG.fromNode(v, GlobalPP(v)))
      case _ => None
    }
  }
}
object PtsEnv { def bottom() = PtsEnv(HashMap()) }

case class PtsFlowObject(val env: PtsEnv, val map: PtsMap) {
  def join(fo: PtsFlowObject) = { env join fo.env ; map join fo.map }
  def clear() = { env.clear; map.clear }
  def objs(fa: FieldAccess): GAG = 
    map.resolve(Ref(env.getUnsafe(fa.base.id), fa.field))
  def refs(fa: FieldAccess): Ref =
    Ref(env.getUnsafe(fa.base.id), fa.field)
  def get(v: VarID): Option[GAG] = env get v
  def get(g: Ref)  : Option[GAG] = map get g
}
object PtsFlowObject { def bottom() = PtsFlowObject(PtsEnv.bottom, PtsMap.bottom) }

case class PtsSummary(val retFO: PtsFlowObject, val retObj: GAG)
