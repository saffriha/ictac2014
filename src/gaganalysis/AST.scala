package gaganalysis

import scala.collection.JavaConversions.collectionAsScalaIterable

import scalaz._
import scalaz.Scalaz._
import soot.SootMethod
import soot.Value
import soot.jimple.AbstractJimpleValueSwitch
import soot.jimple.AbstractStmtSwitch
import soot.jimple.AnyNewExpr
import soot.jimple.ArrayRef
import soot.jimple.AssignStmt
import soot.jimple.CastExpr
import soot.jimple.DefinitionStmt
import soot.jimple.DynamicInvokeExpr
import soot.jimple.GotoStmt
import soot.jimple.IdentityStmt
import soot.jimple.IfStmt
import soot.jimple.InstanceFieldRef
import soot.jimple.InstanceInvokeExpr
import soot.jimple.InterfaceInvokeExpr
import soot.jimple.InvokeExpr
import soot.jimple.InvokeStmt
import soot.jimple.NewArrayExpr
import soot.jimple.NewExpr
import soot.jimple.NewMultiArrayExpr
import soot.jimple.NopStmt
import soot.jimple.NullConstant
import soot.jimple.ParameterRef
import soot.jimple.ReturnStmt
import soot.jimple.SpecialInvokeExpr
import soot.jimple.StaticFieldRef
import soot.jimple.StaticInvokeExpr
import soot.jimple.ThisRef
import soot.jimple.VirtualInvokeExpr

// This file defines the syntax, on which we define our analysis.
// We model a subset of Jimple with focus on the properties relevant for our
// analysis and provide transformations from Jimple to this language.
// In the intraprocedural analyses the statement of each control flow graph node
// gets first translated to this language before further inspection.

trait JType
  case object JVal extends JType
  case object JRef extends JType
  
object JType {
  def from(t: soot.Type): JType =
    if (t.isInstanceOf[soot.RefLikeType]) JRef else JVal
}

trait HasID { def id: ID }
trait HasPP { def pp: PP }

trait Stmt
  case class Assign(lhs: Expr, rhs: Expr, t: JType) extends Stmt
  case class If(condition: Expr) extends Stmt
  case class Return(e: Expr, t: JType) extends Stmt
  case class Invoke(c: Call) extends Stmt
  case class Nop(s: String) extends Stmt

abstract class Expr(_pp: PP) extends HasPP { override def pp = _pp }
  case class Binop(e1: Expr, e2: Expr, _pp: PP) extends Expr(_pp)
  case class Constant(s: String) extends Expr(NullPP)
  
  case class Alloc(_id: AllocID, _pp: PP) extends Expr(_pp) with HasID
  { override def id = _id }

  case class Call(method: SootMethod, args: List[Expr],
                  thisArg: Option[Expr], _pp: PP) extends Expr(_pp)
  
  case class FieldAccess(base: VarExpr, field: Field, _pp: PP) extends Expr(_pp)
  
  case object Null extends Expr(NullPP) with HasID
  { override def id = NullID; }
  
  abstract class VarExpr(_id: VarID, _pp: PP) extends Expr(_pp) with HasID
  { override def id = _id; }
  
    case class Local (_id: LocalID, _pp: PP) extends VarExpr(_id, _pp)
    case class Formal(_id: FormalID, _pp: PP) extends VarExpr(_id, _pp)
    case class Global(_id: GlobalID, _pp: PP) extends VarExpr(_id, _pp)

trait ID 
  case class AllocID(t: Type, pp: PP) extends ID
  { override def toString = "new " + t + " @ " + pp }
  case class FieldID(name: String) extends ID
  { override def toString = name}
  case object NullID extends ID
  { override def toString = "null"}
  trait VarID extends ID
    case class LocalID(name: String) extends VarID
    { override def toString = name}
    case class FormalID(index: Int) extends VarID
    { override def toString = if (index == -1) "this" else "param " + index }
    case class GlobalID(name: String) extends VarID
    { override def toString = name}

trait PP
  case object NullPP extends PP
  { override def toString = "null"}
  case class GlobalPP(id: GlobalID) extends PP
  { override def toString = id.toString }
  case class SootValuePP(v: soot.Value) extends PP {
    override def toString = PPPrinter.rep(v)
    PPPrinter.register(v)
  }
    
// Print program points as natural numbers.
object PPPrinter {
  var pps: List[Any] = Nil
  def register(a: Any) = if (!(pps contains a)) pps ::= a
  def rep(a: Any): String =
    pps . zip(pps.indices)
        . find { _._1 equals a }
        . map { _._2.toString }
        . getOrElse("[unregistered program point]")
}

case class Type(name: String)
case class Field(id: ID, pp: PP)


object Stmt extends AbstractStmtSwitch {
  def fromJimple(s: soot.jimple.Stmt): Option[Stmt] = {
    Log.Timer.astcreation time { StmtSwitch.fromJimple(s) }
  }
  
  object StmtSwitch extends AbstractStmtSwitch {
    var res: Option[Stmt] = None
    def fromJimple(s: soot.jimple.Stmt): Option[Stmt] = { s.apply(this); res }
    
    override def caseAssignStmt(stmt: AssignStmt) =
      handleDefinitionStmt(stmt)
    override def caseIdentityStmt(stmt: IdentityStmt) =
      handleDefinitionStmt(stmt)

    def toExprList(r: Expr): List[Expr] = r match {
      case Binop(e1, e2, pp) => toExprList(e1) ++ toExprList(e2)
      case e                 => List(e)
    }
    
    def handleDefinitionStmt(stmt: DefinitionStmt) = {
      val ctor : (Expr, Expr) => Stmt = Assign(_, _, 
        JType.from(stmt.getLeftOp.getType))
//        if (stmt.getLeftOp.getType.isInstanceOf[soot.RefType]) JRef
//        else                                                   JVal)
      
      res = Apply[Option].lift2(ctor) (Expr.fromJimple(stmt.getLeftOp),
                                       Expr.fromJimple(stmt.getRightOp))
    }

    override def caseInvokeStmt(stmt: InvokeStmt) =
      res = Expr.fromJimple(stmt.getInvokeExpr) >>= {
        case c @ Call(_, _, _, _) => some(Invoke(c))
        case _                    => none
      }

    override def caseReturnStmt(stmt: ReturnStmt) = {
      val ctor : Expr => Stmt = Return(_, JType.from(stmt.getOp.getType))
      res = Expr.fromJimple(stmt.getOp) map ctor
    }
    
    override def caseIfStmt(stmt: IfStmt) =
      res = Expr.fromJimple(stmt.getCondition) map If
      
    override def caseNopStmt(stmt: NopStmt) =
      res = Some(Nop("Nop"))
    override def caseGotoStmt(stmt: GotoStmt) =
      res = Some(Nop("Goto"))
    
    
    override def defaultCase(o: Any) =
      if (o.isInstanceOf)
      // ignore JGotoStmt, JNopStmt, JReturnVoidStmt, JRetStmt
      // TODO: JBreakpointStmt, JEnterMonitorStmt, JExitMonitorStmt, JThrowStmt,
      //   JTableSwitchStmt, JLookupSwitchStmt
      // TODO: how are catches handled?
      res = None
  }
}

object Expr {
  def fromJimple(v: Value): Option[Expr] = ExprSwitch.fromJimple(v)
  
  object ExprSwitch extends AbstractJimpleValueSwitch {
    var res: Option[Expr] = None
    def fromJimple(v: Value): Option[Expr] = { v.apply(this); res }

    override def caseDynamicInvokeExpr(v: DynamicInvokeExpr) =
      res = handleInvoke(v)
    override def caseStaticInvokeExpr(v: StaticInvokeExpr) =
      res = handleInvoke(v)
    override def caseSpecialInvokeExpr(v: SpecialInvokeExpr) =
      res = handleInvoke(v)
    override def caseVirtualInvokeExpr(v: VirtualInvokeExpr) =
      res = handleInvoke(v)
    override def caseInterfaceInvokeExpr(v: InterfaceInvokeExpr) =
      res = handleInvoke(v)

    def handleInvoke(e: InvokeExpr): Option[Call] = {
      val mthis: Option[Expr] =
        if (e.isInstanceOf[InstanceInvokeExpr])
          Some(Expr.fromJimple(e.asInstanceOf[InstanceInvokeExpr].getBase).get)
        else None
      e.getArgs.toList
        . map { e =>
            if (e.getType.isInstanceOf[soot.RefLikeType]) Expr.fromJimple(e)
            else some(null) // signal that the argument is of a value type and that we don't need it
          }
        . sequence
        . map { Call(e.getMethod, _, mthis, SootValuePP(e)) }
    }
    
    override def caseInstanceFieldRef(v: InstanceFieldRef) =
      Expr.fromJimple(v.getBase) match
        { case Some(e: VarExpr) => res = handleFieldRef(e, v) }

    override def caseLocal(v: soot.Local) =
      res = some(Local(LocalID(v.getName), SootValuePP(v)))

    override def caseThisRef(v: ThisRef) =
      res = some(Formal(FormalID(-1), SootValuePP(v)))

    override def caseParameterRef(v: ParameterRef) =
      res = some(Formal(FormalID(v.getIndex), SootValuePP(v)))

    override def caseStaticFieldRef(v: StaticFieldRef) =
      res = handleFieldRef(Global(GlobalID(v.getFieldRef.declaringClass.toString),
                                  SootValuePP(v)),
                           v)

    override def caseCastExpr(v: CastExpr) =
      res = Expr.fromJimple(v.getOp)
                             
    def handleFieldRef(base: VarExpr, fr: soot.jimple.FieldRef): Option[Expr] =
      some(FieldAccess(base,
                       Field(FieldID(fr.getField.toString), SootValuePP(fr)),
                       SootValuePP(fr)))
      
    override def caseNullConstant(v: NullConstant) =
      res = some(Null)
      
    override def caseArrayRef(v: ArrayRef) =
      res = Expr.fromJimple(v.getBase)
      
    override def caseNewExpr(v: NewExpr) = handleAlloc(v)
    override def caseNewArrayExpr(v: NewArrayExpr) = handleAlloc(v)
    override def caseNewMultiArrayExpr(v: NewMultiArrayExpr) = handleAlloc(v)
    def handleAlloc(v: AnyNewExpr) =
      res = some(Alloc(AllocID(Type(v.getType.toString), SootValuePP(v)), SootValuePP(v)))
    
    override def defaultCase(v: Any) =
      res = v match {
        case _ =>
//          println("Failed Expr Creation for: " + v +
//                  " of type " + v.getClass.getName)
          none
      }
  }
}