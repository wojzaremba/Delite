package epfl.mdarrays.staged

import virtualization.lms.common._
import java.io.PrintWriter

trait TimedSupport extends Base {
  def timed[A: Manifest](op: Rep[A], name: String = null, enabled: Boolean = false): Rep[A]
}

trait TimedSupportExp extends BaseExp with TimedSupport {

  case class Timed[A: Manifest](op: Exp[A])(val name: String) extends Def[A]

  def timed[A: Manifest](op: Exp[A], name: String = null, enabled: Boolean = false): Exp[A] = enabled match {
    case true => Timed(op)(name)
    case false => op
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    // we want it drawn inside
    case Timed(op) => freqCold(op)
    case _ => super.symsFreq(e)
  }
}

trait ScalaGenTimedSupport extends ScalaGenBase {
  val IR: TimedSupportExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter): Unit = {
    rhs match {
      case t: Timed[_] =>
        val name = if (t.name != null) t.name else quote(sym)
        stream.println("/**** STARTING " + name + " ****/")
        stream.println("val " + quote(sym) + ": " + remap(sym.typeManifest) + " = {")
        stream.println("var timer" + sym.id + " = - System.currentTimeMillis")
        stream.println("println(\"started operation \\\"" + name + "\\\"\")")
        emitBlock(t.op)
        stream.println("timer" + sym.id + " += System.currentTimeMillis")
        stream.println("println(\"finished operation \\\"" + name + "\\\" in \" + timer" + sym.id + " + \"ms\")")
        stream.println(quote(t.op))
        stream.println("}")
        stream.println("/**** FINISHED " + name + " ****/")
      case _ =>
        super.emitNode(sym, rhs)
    }
  }
}
