package epfl.mdarrays.staged
import virtualization.lms.common._
import java.io.PrintWriter
import virtualization.lms.internal.GenerationFailedException

trait IntegerSupport extends Base {
  // Integer arithmetic
  def infix_<(o1: Rep[Int], o2: Rep[Int]): Rep[Boolean]
  def infix_===(o1: Rep[Int], o2: Rep[Int]): Rep[Boolean]
  def infix_-(o1: Rep[Int], o2: Rep[Int]): Rep[Int]
  def int(o: Int): Rep[Int]
}

trait IntegerSupportExp extends BaseExp with IntegerSupport {
  // Integer arithmetic
  case class IntegerMinus(o1: Exp[Int], o2: Exp[Int]) extends Def[Int]
  case class IntegerLt(o1: Exp[Int], o2: Exp[Int]) extends Def[Boolean]
  case class IntegerEqeqeq(o1: Exp[Int], o2: Exp[Int]) extends Def[Boolean]
  case class IntConst(o: Int) extends Def[Int]

  def infix_<(o1: Exp[Int], o2: Exp[Int]): Exp[Boolean] = IntegerLt(o1, o2)
  def infix_===(o1: Exp[Int], o2: Exp[Int]): Exp[Boolean] = IntegerEqeqeq(o1, o2)
  def infix_-(o1: Exp[Int], o2: Exp[Int]): Exp[Int] = IntegerMinus(o1, o2)
  def int(o: Int): Exp[Int] = IntConst(o)
}

trait ScalaGenIntegerSupport extends ScalaGenBase {
  val IR: IntegerSupportExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter): Unit = {
    rhs match {
      case IntConst(value) =>
        stream.println("val " + quote(sym) + ": Int = " + value)
      case IntegerEqeqeq(o1, o2) =>
        stream.println("val " + quote(sym) + ": Boolean = " + quote(o1) + " == " + quote(o2))
      case IntegerLt(o1, o2) =>
        stream.println("val " + quote(sym) + ": Boolean = " + quote(o1) + " < " + quote(o2))
      case IntegerMinus(o1, o2) =>
        stream.println("val " + quote(sym) + ": Int = " + quote(o1) + " - " + quote(o2))
      case _ =>
        super.emitNode(sym, rhs)
    }
  }
}
