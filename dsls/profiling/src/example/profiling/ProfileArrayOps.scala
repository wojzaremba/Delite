package example.profiling


import scala.virtualization.lms.common.{NumericOpsExp, FractionalOpsExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.DeliteOpsExp
import java.io.PrintWriter
import example.profiling.datastruct.scala.ProfileArray


trait ProfileArrayOps extends Base {
  // a simple way of enumerating choices in our syntax
  class Reporter
  object average extends Reporter
  object median extends Reporter
  
  // add report and length methods to Rep[ProfileArray]
  def infix_report(x: Rep[ProfileArray], y: Reporter) = profile_report(x, y)
  def infix_length(x: Rep[ProfileArray]) = profile_length(x)
  
  // implementation
  def profile_report(x: Rep[ProfileArray], y: Reporter): Rep[Double]
  def profile_length(x: Rep[ProfileArray]): Rep[Int]
}


trait ProfileArrayOpsExp extends ProfileArrayOps with NumericOpsExp
with FractionalOpsExp with DeliteOpsExp {
  
  // a Delite parallel operation! was it really that easy?
  case class ReportSum(in: Exp[ProfileArray]) 
  extends DeliteOpReduce[Double] {
    val zero = Const(0.0)
    val size = in.size
    val func = (a: Exp[Double], b: Exp[Double]) => a + b
  }
  
  // median is a little trickier, let's just be sequential
  case class ReportMedian(in: Exp[ProfileArray]) extends Def[Double]
  
  // length needs to reference the underlying data structure
  case class ProfileLength(in: Exp[ProfileArray]) extends Def[Int]
  
  def profile_report(x: Exp[ProfileArray], y: Reporter) = y match {
    case this.average => ReportSum(x) / x.length   // inline
    case this.median => ReportMedian(x)
    case _ => throw new IllegalArgumentException("unknown report type")
  }
  
  def profile_length(x: Exp[ProfileArray]) = ProfileLength(x)
}
