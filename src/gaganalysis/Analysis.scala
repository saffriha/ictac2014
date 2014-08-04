package gaganalysis

import soot.Scene
import soot.SootMethod

// This file describes functions common to both intraprocedural analyses.

object Analysis {
  def pIn(in: PtsFlowObject, v: VarID) = in.get(v) match {
    case Some(v) => v
    case None =>
      println("pIn called on a VarID for which it is not defined.")
      println("VarID: " + v + " : " + v.getClass.getName)
      System.exit(1)
      null
    }
  def objs(in: PtsFlowObject, f: FieldAccess): GAG = in.objs(f)
  def refs(in: PtsFlowObject, f: FieldAccess): Ref = in.refs(f)
  def calleesOf(unit: soot.Unit): List[SootMethod] = {
    var methods: List[SootMethod] = List()
    val cg = Scene.v.getCallGraph
    val it = cg edgesOutOf unit
    while (it.hasNext)
      methods = methods :+ it.next.tgt
    methods sortBy { _.getName }
  }
  
  def transr(call: Call, calleeIn: PtsFlowObject, ref: Ref): Ref =
    Ref(transo(call, calleeIn, ref.base), ref.field)
    
  def transo(call: Call, calleeIn: PtsFlowObject, obj: GAG): GAG = {
    object FO {
      def inToOut(n: Node, objIn: GAG): GAG = {
        n.id match {
          case AllocID(_, _) => GAG.fromNode(n)
          case NullID        => GAG.fromNode(Null.id, Null.pp)
          case FormalID(-1)  =>
            Analysis.pIn(calleeIn, call.thisArg.get.asInstanceOf[Local].id)
          case FormalID(i)   => call.args.drop(i).head match {
            case l @ Local(_,_) => Analysis.pIn(calleeIn, l.id)
            case n @ Null       => GAG.fromNode(NullID, NullPP)
          }
          case GlobalID(v)   => Analysis.pIn(calleeIn, GlobalID(v))
          case FieldID(f)    => 
            calleeIn.map resolve Ref(objIn, Field(n.id.asInstanceOf[ID],
                                                  n.pp.asInstanceOf[PP]))
        }
      }
      def join(gag1: GAG, gag2: GAG): GAG = { gag1 join gag2; gag1 }
    }
    val (inX, outX) =
      obj.analysis(FO.inToOut(_,_), FO.join(_,_), GAG.bottom, obj.roots)
      
    val g = GAG.join(outX filter {obj hasExit _._1} map {_._2})
    Log.onTransfer(call, calleeIn, obj, inX, outX, g)
    g
  }

}