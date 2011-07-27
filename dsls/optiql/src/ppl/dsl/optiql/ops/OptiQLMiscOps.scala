package ppl.dsl.optiql.ops

import scala.virtualization.lms.common.{ScalaGenEffect, EffectExp, Base}
import java.io.PrintWriter
import ppl.dsl.optiql.{OptiQLExp,OptiQL}

trait OptiQLMiscOps extends Base {  this : OptiQL =>

  def tic(deps: Rep[Any]*) = optiql_profile_start(deps)
  def toc(deps: Rep[Any]*) = optiql_profile_stop(deps)

  def optiql_profile_start(deps: Seq[Rep[Any]]): Rep[Unit]
  def optiql_profile_stop(deps: Seq[Rep[Any]]): Rep[Unit]

}

trait OptiQLMiscOpsExp extends OptiQLMiscOps with EffectExp { this : OptiQLExp =>

  case class OptiQLProfileStart(deps: List[Exp[Any]]) extends Def[Unit]
  case class OptiQLProfileStop(deps: List[Exp[Any]]) extends Def[Unit]

  def optiql_profile_start(deps: Seq[Rep[Any]]): Rep[Unit] = reflectEffect(OptiQLProfileStart(deps.toList))
  def optiql_profile_stop(deps: Seq[Rep[Any]]): Rep[Unit] =  reflectEffect(OptiQLProfileStop(deps.toList))
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case Reflect(OptiQLProfileStart(deps), u, es) => reflectMirrored(Reflect(OptiQLProfileStart(f(deps)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(OptiQLProfileStop(deps), u, es) => reflectMirrored(Reflect(OptiQLProfileStop(f(deps)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]  
}

trait ScalaGenOptiQLMiscOps extends ScalaGenEffect {
  val IR:OptiQLMiscOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {

    case OptiQLProfileStart(deps) => emitValDef(sym, "ppl.delite.runtime.profiler.PerformanceTimer.start(\"app\", false)")
    case OptiQLProfileStop(deps) => emitValDef(sym, "ppl.delite.runtime.profiler.PerformanceTimer.stop(\"app\", false)")
    case _ => super.emitNode(sym,rhs)
  }

}