package example.profiling


import scala.virtualization.lms.common.{ScalaGenBase, Base, EffectExp}
import java.io.PrintWriter
import example.profiling.datastruct.scala.ProfileArray


// this is the abstract interface of our profiling methods
trait ProfileOps extends Base {
  def profile(n: Rep[Int]) = new ProfileOpsCls(n)

  // syntax
  class ProfileOpsCls(n: Rep[Int]) {
    def times(func: =>Rep[Any]) = profile_body(n, func)
  }
 
  // implementation
  protected def profile_body(n: Rep[Int], func: =>Rep[Any]): Rep[ProfileArray]
}


trait ProfileOpsExp extends ProfileOps with EffectExp {
  case class Profile(n: Exp[Int], body: Exp[Any]) extends Def[ProfileArray]
  
  protected def profile_body(n: Rep[Int], func: =>Exp[Any]) = reflectEffect(Profile(n, reifyEffects(func)))

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Profile(n, body) => effectSyms(body)
    case _ => super.boundSyms(e)
  }
  
}


trait ScalaGenProfileOps extends ScalaGenBase {
  val IR: ProfileOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = 
    rhs match {
      // insert instrumentation code around function body
      case Profile(n, body) => 
        stream.println("val " + quote(sym) + " = {")
        stream.println("val out = new ProfileArray(" + quote(n) + ")")
        stream.println("var i = 0")
        stream.println("while (i < " + quote(n) + ") {")
        stream.println("  val start = System.currentTimeMillis()")
        emitBlock(body)
        stream.println("  val end = System.currentTimeMillis()")
        stream.println("  val duration = (end - start)/1000f ")
        stream.println("  out.dcUpdate(i, duration)")
        stream.println("  i += 1")
        stream.println("}")
        stream.println("out")
        stream.println("}")

      case _ => super.emitNode(sym, rhs)
    }
}


trait ScalaGenProfileArrayOps extends ScalaGenBase {
  val IR: ProfileArrayOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) =
    rhs match {
      case ReportMedian(x) =>
        val a = quote(x)
        val size = a + ".dcSize"
        stream.println("val " + quote(sym) + " = {")
        stream.println("val d = new Array[Double]("+size+")")
        stream.println("System.arraycopy("+a+"._data, 0, d, 0, "+size+")")
        stream.println("scala.util.Sorting.quickSort(d)")
        stream.println("d(Math.ceil("+size+"/2).asInstanceOf[Int])")
        stream.println("}")
      case ProfileLength(x) => emitValDef(sym, quote(x) + ".dcSize")
      case _ => super.emitNode(sym, rhs)
    }  
}
