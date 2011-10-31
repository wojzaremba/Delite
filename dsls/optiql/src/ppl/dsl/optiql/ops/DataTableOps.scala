package ppl.dsl.optiql.ops

import scala.virtualization.lms.common._
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import scala.reflect.SourceContext
import ppl.dsl.optiql._
import java.io.PrintWriter
import ppl.delite.framework.ops.DeliteCollection

trait DataTableOps extends Base { this: OptiQL =>

  abstract class DataTable[TSource] extends Struct with DeliteCollection[TSource]

  // Lifting and Interface Injection
  implicit def dataTableRepToDataTableRepOps[T:Manifest](d: Rep[DataTable[T]]) = new DataTableRepOps(d)

  object DataTable {
    def apply[T:Manifest](): Rep[DataTable[T]] = dataTableObjectApply()
	  def apply[T:Manifest](initSize: Rep[Int]): Rep[DataTable[T]] = dataTableObjectApply(initSize)
  }

  class DataTableRepOps[T:Manifest](t:Rep[DataTable[T]]) {
    def apply(i: Rep[Int]): Rep[T] = dataTableApply(t, i)
  }


  def infix_size[T:Manifest](t:Rep[DataTable[T]]): Rep[Int] = dataTableSize(t)
  def infix_printAsTable[T:Manifest](t: Rep[DataTable[T]], max_rows: Rep[Int] = unit(100)): Rep[Unit] = dataTablePrintAsTable(t, max_rows)




  //implementation method defintions
  def dataTableApply[T:Manifest](t: Rep[DataTable[T]], i: Rep[Int]): Rep[T]
  def dataTableObjectApply[T:Manifest](): Rep[DataTable[T]]
  def dataTableObjectApply[T:Manifest](initSize: Rep[Int]): Rep[DataTable[T]]
  def dataTableSize[T:Manifest](t: Rep[DataTable[T]]): Rep[Int]
  def dataTablePrintAsTable[T:Manifest](t: Rep[DataTable[T]], max_rows: Rep[Int]): Rep[Unit]



}

trait DataTableOpsExp extends DataTableOps with DeliteCollectionOpsExp with BaseFatExp { this: DataTableOpsExp with OptiQLExp =>

  case class DataTablePrintAsTable[T](t: Rep[DataTable[T]], max_rows: Rep[Int]) extends Def[Unit]

  private def infix_data[T:Manifest](t: Exp[DataTable[T]]) = field[DeliteArray[T]](t, "data")  
  def dataTableApply[T:Manifest](t: Exp[DataTable[T]], i: Exp[Int]): Exp[T] = darray_apply(t.data, i)
  def dataTableObjectApply[T:Manifest](): Exp[DataTable[T]] = struct[DataTable[T]]("data" -> DeliteArray(unit(0)))
  def dataTableObjectApply[T:Manifest](initSize: Exp[Int]): Exp[DataTable[T]] = struct[DataTable[T]]("data" -> DeliteArray(initSize))
  def dataTableSize[T:Manifest](t: Exp[DataTable[T]]): Exp[Int] = darray_length(t.data)
  def dataTablePrintAsTable[T:Manifest](t: Exp[DataTable[T]], max_rows: Rep[Int]): Exp[Unit] = reflectEffect(DataTablePrintAsTable(t, max_rows))

  private def ifDataTable[T:Manifest, R](x: Exp[DeliteCollection[T]])(then: Exp[DataTable[T]] => R)(orElse: => R): R = {
    if (x.Type.erasure == classOf[DataTable[T]]) then(x.asInstanceOf[Exp[DataTable[T]]]) else orElse
  }

  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]]) = ifDataTable(x)(dataTableSize(_))(super.dc_size(x))
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int]) = ifDataTable(x)(dataTableApply(_,n))(super.dc_apply(x,n))
  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A]) = ifDataTable(x)(t => darray_update(t.data,n,y))(super.dc_update(x,n,y))

}

trait ScalaGenDataTableOps extends ScalaGenFat {
  val IR: DataTableOpsExp with OptiQLExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case DataTablePrintAsTable(t,max_rows) => emitValDef(sym, quote(t) + ".printAsTable(" +  quote(max_rows) + ")")
    case _ => super.emitNode(sym, rhs)
  }

}
