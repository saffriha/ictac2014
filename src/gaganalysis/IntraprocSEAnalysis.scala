package gaganalysis

import scala.collection.mutable.Map
import scalaz._
import scalaz.Scalaz._
import soot.Body
import soot.SootMethod
import soot.toolkits.graph.DirectedGraph
import soot.toolkits.scalar.ForwardFlowAnalysis
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions.collectionAsScalaIterable
import soot.jimple.ReturnStmt
import soot.jimple.InstanceInvokeExpr
import soot.Scene
import java.util.TreeMap

// This file describes the intraprocedural side effect analysis.

class IntraprocSEAnalysis(graph: DirectedGraph[soot.Unit],
                          body: Body,
                          pts: IntraprocPtsAnalysis,
                          summaries: Map[SootMethod, MethodSummary])
  extends ForwardFlowAnalysis[soot.Unit, SEFlowObject](graph) {

  super.doAnalysis
  
  // Each control flow node has initially no side effects.
  override def newInitialFlow   = SEFlowObject.bottom
  override def entryInitialFlow = SEFlowObject.bottom
  
  // On joining CFG edges the in-object of the common target node
  // is computed as the join of the out-objects of the source nodes.
  override def merge(in1: SEFlowObject, in2: SEFlowObject,
                     out: SEFlowObject): Unit = {
    out.clear
    out.join(in1)
    out.join(in2)
//    Log.curSELog.onJoin(in1, in2, out)
  }

  // Compute out-objects from in-objects.
  override def flowThrough(in: SEFlowObject,
                           unit: soot.Unit,
                           out: SEFlowObject) = {
//    Log.Timer.seCfgNode.start
    
    val gen      = SEFlowObject.bottom
    val killWith = SEFlowObject.bottom
    
    val ptsIn = pts getFlowBefore unit

    val stmt = unit.asInstanceOf[soot.jimple.Stmt]
    Stmt.fromJimple(stmt) match {
      case None    =>
        println("Failed converting jimple stmt " + stmt +
                " of type" + stmt.getClass.getName + ".")
      case Some(e) => e match {
        case Assign(l, r, _)   => gen join genWrite(l)
                                  gen join genRead(r)
        case If(c)             => gen join genRead(c)
        case Invoke(c)         => gen join handleCall(c)
        case Return(e, _)      => gen join genRead(e)
        case Nop(_)            => {}
      }
    }

    def handleCall(c: Call): SEFlowObject = {
	  val effects =
	    for { m <- Analysis.calleesOf(unit);
              if m.hasActiveBody() && (m.getName equals c.method.getName) }
        yield {
          val calleeSum = summaries.get(m).get
          SEFlowObject(Analysis.transo(c, ptsIn, calleeSum.reads),
                       Analysis.transo(c, ptsIn, calleeSum.writes))
        }
	  effects.foldLeft(SEFlowObject.bottom) { (l, r) => l join r; l }
    }
    
    def genWrite(e: Expr): SEFlowObject = e match {
        case xa   @ FieldAccess(_,_,_) => SEFlowObject(
          reads  = GAG.bottom,
          writes = Analysis.pIn(ptsIn, xa.base.id) * GAG.fromNode(xa.field.id, xa.field.pp))
        case _                         => SEFlowObject.bottom
      }

    def genRead(e: Expr): SEFlowObject = e match {
        case ya   @ FieldAccess(_,_,_) => SEFlowObject(
          reads  = Analysis.pIn(ptsIn, ya.base.id) * GAG.fromNode(ya.field.id, ya.field.pp),
          writes = GAG.bottom)
        case call @ Call(_,_,_,_)      => handleCall(call)
        case _                         => SEFlowObject.bottom
      }
    
    out.clear
    
    out join in
    out join gen
    
//    Log.curSELog.onNode(stmt, gen)
//    Log.Timer.seCfgNode.stop
  }

  // Describes how the contents of one flow object can be assigned to another.
  override def copy(source: SEFlowObject, dest: SEFlowObject) = {
    dest.clear
    dest.join(source)
  }

  def getSummary(): SEFlowObject = graph.getTails
    . foldLeft (SEFlowObject.bottom)
               { (fo, stmt) => fo join getFlowAfter(stmt); fo }
}
