package gaganalysis

import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.mutable.Map

import scalaz.Scalaz._
import soot.Body
import soot.SootMethod
import soot.toolkits.graph.DirectedGraph
import soot.toolkits.scalar.ForwardFlowAnalysis

// This file describes the intraprocedural points-to analysis.

class IntraprocPtsAnalysis(graph: DirectedGraph[soot.Unit],
                           body: Body,
                           summaries: Map[SootMethod, MethodSummary])
  extends ForwardFlowAnalysis[soot.Unit, PtsFlowObject](graph) {
  super.doAnalysis

  // Each control flow node has initially the bottom points-to and variable
  // environment.
  override def newInitialFlow   = PtsFlowObject.bottom
  override def entryInitialFlow = PtsFlowObject.bottom
  
  // On joining CFG edges the in-object of the common target node
  // is computed as the join of the out-objects of the source nodes.
  override def merge(src1Out: PtsFlowObject, src2Out: PtsFlowObject,
                     dstIn: PtsFlowObject): Unit = {
    dstIn.clear
    dstIn.join(src1Out)
    dstIn.join(src2Out)
//    Log.curPtsLog.onJoin(in1, in2, out)
  }

  // Compute out-objects from in-objects.
  override def flowThrough(in: PtsFlowObject,
                           unit: soot.Unit,
                           out: PtsFlowObject) = {
//    Log.Timer.ptsCfgNode.start
//    println("Stmt: " + unit)
    
    val gen      = PtsFlowObject.bottom
    val killWith = PtsFlowObject.bottom
    
    val stmt = unit.asInstanceOf[soot.jimple.Stmt]
    Stmt.fromJimple(stmt) match {
      case None    =>
//        println("Ignored jimple stmt " + stmt +
//                " of type" + stmt.getClass.getName + ".")
      case Some(e) => e match {
        case Invoke(c)       => handleCall(c)
        case Return(e, _)    => {}
        case If(_)           => {}
        case Nop(_)          => {}
        case Assign(l, r, JRef) =>
          val lhsVarOrRef: Either[VarID, Ref] = l match {
            case x    @ Local(_,_)         => Left(x.id)
            case x    @ Global(_,_)        => Left(x.id)
            case xa   @ FieldAccess(_,_,_) => Right(Analysis.refs(in, xa))
          }
          val rhsObj: GAG = r match {
            case y    @ Local(_,_)         => Analysis.pIn(in, y.id)
            case y    @ Formal(id,pp)      => GAG.fromNode(id, pp)
            case ya   @ FieldAccess(_,_,_) => Analysis.objs(in, ya)
            case newp @ Alloc(id,pp)       => GAG.fromNode(id, pp)
            case call @ Call(_,_,_,_)      => handleCall(call)
            case nul  @ Null               => GAG.fromNode(nul.id, nul.pp)
          }
          lhsVarOrRef.bimap(killWith.env.put(_, rhsObj),
                            gen.map.join(_, rhsObj))
        case Assign(_, r, JVal) =>
          r match {
            case call @ Call(_,_,_,_)      => handleCall(call)
            case _ => {}
          }
      }
    }

    def handleCall(c: Call): GAG = {
	  val objs =
	    for { m <- Analysis.calleesOf(unit);
              if m.hasActiveBody() && (m.getName equals c.method.getName) }
        yield {
          val calleeSum = summaries.get(m).get
          
          for { (ref, obj) <- calleeSum.ptsMap.entries }
            gen.map.join(Analysis.transr(c, in, ref),
                         Analysis.transo(c, in, obj))
            
          for { (v @ GlobalID(_), obj) <- calleeSum.ptsEnv.entries }
            gen.env.join(v,
                         Analysis.transo(c, in, obj))
            
          Analysis.transo(c, in, calleeSum.retObj)
        }
	  GAG.join(objs)
    }
    
    out.clear
    
    out.env put  in.env
    out.env join gen.env
    out.env put  killWith.env
    
    out.map put  in.map
    out.map join gen.map
    out.map put  killWith.map
    
//    Log.curPtsLog.onNode(stmt, gen, killWith)
//    Log.Timer.ptsCfgNode.stop
  }

  // Describes how the contents of one flow object can be assigned to another.
  override def copy(source: PtsFlowObject, dest: PtsFlowObject) = {
    dest.clear
    dest.join(source)
  }

  // Accessors to query the analysis results.
  
  def getSummary() = PtsSummary(getReturnFO, getReturnObj)

  def getReturnFO(): PtsFlowObject =
    graph.getTails . foldLeft (PtsFlowObject.bottom)
                              { (fo, stmt) => fo join getFlowAfter(stmt); fo }
  
  def getReturnObj(): GAG =
    graph.getTails
    . filter { _.isInstanceOf[soot.jimple.ReturnStmt] }
    . map { _.asInstanceOf[soot.jimple.ReturnStmt] }
    . foldLeft (GAG.bottom) { (gag, stmt) => Stmt.fromJimple(stmt) match {
        case Some(Return(Local(id,_), JRef)) =>
          gag join Analysis.pIn(getFlowBefore(stmt), id)
        case _ => gag
      } }
    
}
