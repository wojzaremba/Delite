package ppl.dsl.optiml

import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common.DSLOpsExp
import scala.virtualization.lms.internal.ScalaGenBase
import scala.virtualization.lms.common.{VariablesExp, Variables}

trait Vector[T]

trait VectorOps extends DSLType with Variables {

  object Vector {
    def apply[A : Manifest](len: Rep[Int], is_row : Rep[Boolean] = true) : Rep[Vector[A]] = vector_new(len, is_row)
    def zeros(len: Rep[Int]) : Rep[Vector[Double]] = vector_obj_zeros(len)
  }

  implicit def repVecToRepVecOps[A:Manifest](x: Rep[Vector[A]]) = new vecRepCls(x)
  implicit def vecToRepVecOps[A:Manifest](x: Vector[A]) = new vecRepCls(x)
  implicit def varToRepVecOps[A:Manifest](x: Var[Vector[A]]) : vecRepCls[A]

  // could convert to infix, but apply doesn't work with it anyways yet
  class vecRepCls[A:Manifest](x: Rep[Vector[A]]) {
    def apply(n: Rep[Int]) = vector_apply(x, n)
    def update(n: Rep[Int], y: Rep[A]) = vector_update(x,n,y)
    def length = vector_length(x)
    def toBoolean(implicit conv: Rep[A] => Rep[Boolean]) = vector_toboolean(x)
    def +(y: Rep[Vector[A]])(implicit n: Numeric[A]) = vector_plus(x,y)
    def -(y: Rep[Vector[A]])(implicit n: Numeric[A]) = vector_minus(x,y)
    def *(y: Rep[Vector[A]])(implicit n: Numeric[A]) = vector_times(x,y)
    def /(y: Rep[A])(implicit f: Fractional[A]) = vector_divide(x,y)
    def outer(y: Rep[Vector[A]])(implicit n: Numeric[A]) = vector_outer(x,y)
    def trans  = vector_trans(x)
    def pprint = vector_pprint(x)
    def is_row = vector_is_row(x)
 
    def +=(y: Rep[A]) = vector_plusequals(x,y)
  }

  // object defs
  def vector_obj_zeros(len: Rep[Int]): Rep[Vector[Double]]

  // class defs
  def vector_apply[A:Manifest](x: Rep[Vector[A]], n: Rep[Int]): Rep[A]
  def vector_update[A:Manifest](x: Rep[Vector[A]], n: Rep[Int], y: Rep[A]): Rep[Unit]
  def vector_length[A:Manifest](x: Rep[Vector[A]]): Rep[Int]
  def vector_plusequals[A:Manifest](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]
  def vector_is_row[A:Manifest](x: Rep[Vector[A]]): Rep[Boolean]
  def vector_toboolean[A](x: Rep[Vector[A]])(implicit conv: Rep[A] => Rep[Boolean], mA: Manifest[A]): Rep[Vector[Boolean]]
  def vector_plus[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_minus[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_times[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_divide[A:Manifest:Fractional](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]
  def vector_trans[A:Manifest](x: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_outer[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Matrix[A]]
  def vector_pprint[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]

  // impl defs
  def vector_new[A:Manifest](len: Rep[Int], is_row: Rep[Boolean]) : Rep[Vector[A]]
}

trait VectorOpsExp extends VectorOps with VariablesExp with DSLOpsExp { this: VectorImplOps =>
  implicit def varToRepVecOps[A:Manifest](x: Var[Vector[A]]) = new vecRepCls(readVar(x))

  // implemented via method on real data structure
  case class VectorApply[A:Manifest](x: Exp[Vector[A]], n: Exp[Int]) extends Def[A]
  case class VectorUpdate[A:Manifest](x: Exp[Vector[A]], n: Exp[Int], y: Exp[A]) extends Def[Unit]
  case class VectorLength[A:Manifest](x: Exp[Vector[A]]) extends Def[Int]
  case class VectorPlusEquals[A:Manifest](x: Exp[Vector[A]], y: Exp[A]) extends Def[Vector[A]]
  case class VectorIsRow[A:Manifest](x: Exp[Vector[A]]) extends Def[Boolean]

  // implemented via kernel embedding
  case class VectorObjectZeros(len: Exp[Int])
    extends DSLOp(reifyEffects(vector_obj_zeros_impl(len)))

  case class VectorToBoolean[A](x: Exp[Vector[A]])(implicit conv: Exp[A] => Exp[Boolean], mA: Manifest[A])
    extends DSLOp(reifyEffects(vector_toboolean_impl[A](x,conv)))

  case class VectorPlus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]])
    extends DSLOp(reifyEffects(vector_plus_impl[A](x,y)))

  case class VectorMinus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]])
    extends DSLOp(reifyEffects(vector_minus_impl[A](x,y)))

  case class VectorDivide[A:Manifest:Fractional](x: Exp[Vector[A]], y: Exp[A])
    extends DSLOp(reifyEffects(vector_divide_impl[A](x,y)))

  case class VectorOuter[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]])
    extends DSLOp(reifyEffects(vector_outer_impl[A](x,y)))
  
  case class VectorPPrint[A:Manifest](x: Exp[Vector[A]])
    extends DSLOp(reifyEffects(vector_pprint_impl[A](x)))

  case class VectorTrans[A:Manifest](x: Exp[Vector[A]])
      extends DSLOp(reifyEffects(vector_trans_impl[A](x)))

  case class VectorTimes[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]])
      extends Def[Vector[A]]

  case class VectorNew[A:Manifest](len: Exp[Int], is_row: Exp[Boolean])
    extends DSLOp(reifyEffects(vector_new_impl[A](len, is_row)))


  def vector_apply[A:Manifest](x: Exp[Vector[A]], n: Exp[Int]) = VectorApply(x, n)
  def vector_update[A:Manifest](x: Exp[Vector[A]], n: Exp[Int], y: Exp[A]) = reflectMutation(VectorUpdate(x,n,y))
  def vector_length[A:Manifest](x: Exp[Vector[A]]) = VectorLength(x)
  def vector_plusequals[A:Manifest](x: Exp[Vector[A]], y: Exp[A]) = reflectMutation(VectorPlusEquals(x, y))
  def vector_is_row[A:Manifest](x: Exp[Vector[A]]) = VectorIsRow(x)

  def vector_obj_zeros(len: Exp[Int]) = reflectEffect(VectorObjectZeros(len))
  def vector_toboolean[A](x: Exp[Vector[A]])(implicit conv: Exp[A] => Exp[Boolean], mA: Manifest[A]) = VectorToBoolean(x)
  def vector_plus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorPlus(x, y)
  def vector_minus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorMinus(x, y)
  def vector_times[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorTimes(x, y)
  def vector_divide[A:Manifest:Fractional](x: Exp[Vector[A]], y: Exp[A]) = VectorDivide(x, y)
  def vector_trans[A:Manifest](x: Exp[Vector[A]]) = VectorTrans(x)
  def vector_outer[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorOuter(x, y)
  def vector_pprint[A:Manifest](x: Exp[Vector[A]]) = reflectEffect(VectorPPrint(x))

  def vector_new[A:Manifest](len: Exp[Int], is_row: Exp[Boolean]) = reflectEffect(VectorNew[A](len, is_row))
}

/**
 * Optimizations for composite VectorOps operations.
 */

trait VectorOpsExpOpt extends VectorOpsExp { this: VectorImplOps =>
  override def vector_plus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x, y) match {
    // (TB + TD) == T(B + D)
    case (Def(VectorTimes(a, b)), Def(VectorTimes(c, d))) if (a == c) => VectorTimes[A](a.asInstanceOf[Exp[Vector[A]]], VectorPlus[A](b.asInstanceOf[Exp[Vector[A]]],d.asInstanceOf[Exp[Vector[A]]]))
    // ...
    case _ => super.vector_plus(x, y)
  }

  override def vector_times[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x, y) match {
    case _ => super.vector_times(x, y)
  }
}

trait ScalaGenVectorOps extends ScalaGenBase {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case VectorApply(x, n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
    case VectorUpdate(x,n,y) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(y))
    case VectorLength(x)    => emitValDef(sym, quote(x) + ".length")
    case VectorIsRow(x)     => emitValDef(sym, quote(x) + ".is_row")
    case VectorPlusEquals(x,y) => emitValDef(sym, quote(x) + " += " + quote(y))

    case _ => super.emitNode(sym, rhs)
  }
}
