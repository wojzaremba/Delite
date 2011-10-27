package ppl.delite.framework.datastructures

import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.common.{Base, EffectExp, StructExp, StructExpOptCommon, StructFatExpOptCommon, ScalaGenEffect}
import java.io.PrintWriter
import scala.reflect.SourceContext

trait DeliteArrayOps extends Base {

  trait DeliteArray[T] // TBD: extends DeliteCollection or not?

  object DeliteArray {
    def apply[T:Manifest](length: Rep[Int]) = darray_new(length)
  }
  
  implicit def repDArrayToDArrayOps[T:Manifest](da: Rep[DeliteArray[T]]) = new DeliteArrayOpsCls(da)
  
  class DeliteArrayOpsCls[T:Manifest](da: Rep[DeliteArray[T]]) {
    def length: Rep[Int] = darray_length(da)
    def apply(i: Rep[Int]): Rep[T] = darray_apply(da,i)
    def update(i: Rep[Int], x: Rep[T]): Rep[Unit] = darray_update(da,i,x)
  }
    
  def darray_new[T:Manifest](length: Rep[Int]): Rep[DeliteArray[T]]
  def darray_length[T:Manifest](da: Rep[DeliteArray[T]]): Rep[Int]
  def darray_apply[T:Manifest](da: Rep[DeliteArray[T]], i: Rep[Int]): Rep[T]
  def darray_update[T:Manifest](da: Rep[DeliteArray[T]], i: Rep[Int], x: Rep[T]): Rep[Unit]
    
}

trait DeliteArrayOpsExp extends DeliteArrayOps with StructExp with EffectExp {
  this: DeliteOpsExp =>
  
  case class DeliteArrayNew[T:Manifest](length: Exp[Int]) extends Def[DeliteArray[T]]
  case class DeliteArrayLength[T:Manifest](da: Exp[DeliteArray[T]]) extends Def[Int]
  case class DeliteArrayApply[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int]) extends Def[T]
  case class DeliteArrayUpdate[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int], x: Exp[T]) extends Def[Unit]
  
  def darray_new[T:Manifest](length: Exp[Int]) = reflectMutable(DeliteArrayNew[T](length))
  def darray_length[T:Manifest](da: Exp[DeliteArray[T]]) = reflectPure(DeliteArrayLength[T](da))
  def darray_apply[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int]) = reflectPure(DeliteArrayApply[T](da,i))
  def darray_update[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int], x: Exp[T]) = reflectWrite(da)(DeliteArrayUpdate[T](da,i,x))
  
  def reflectPure[T](x: Def[T]) = x
}

trait DeliteArrayOpsExpOpt extends DeliteArrayOpsExp with StructExpOptCommon {
  this: DeliteOpsExp =>

  override def field[T:Manifest](struct: Rep[Struct], index: String): Rep[T] = struct match {
    //case Def(m: DeliteOpMapLike[_,_]) =>
    //  val alloc = m.body.asInstanceOf[DeliteCollectElem[_,_]].alloc //TODO: need to reflect the op's writes to the alloc so op isn't DCE'd
    //  field(alloc, index)
    case _ => super.field[T](struct, index)
  }

  //forwarders to appease the manifest craziness in the pattern-matching below
  private def dnew[T:Manifest](length: Exp[Int]): Rep[DeliteArray[T]] = darray_new(length)
  private def dlength[T:Manifest](da: Exp[DeliteArray[T]]): Rep[Int] = darray_length(da)
  private def dapply[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int]): Rep[T] = darray_apply(da,i)
  private def dupdate[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int], x: Exp[T]): Rep[Unit] = darray_update(da,i,x)

  private def darrayManifest(arg: Manifest[Any]) = new Manifest[DeliteArray[Any]] {
    val erasure = classOf[DeliteArray[Any]]
    override val typeArguments = List(arg)
  }

  override def darray_length[T:Manifest](da: Exp[DeliteArray[T]]) = da match {
    case Def(Struct(tag, elems:Map[String,Exp[DeliteArray[Any]]])) =>
      dlength(elems.head._2)(elems.head._2.Type.typeArguments(0).asInstanceOf[Manifest[Any]])
    case _ => structType match {
      case Some(elems) => dlength(field[DeliteArray[Any]](da.asInstanceOf[Exp[Struct]],elems.head._1)(darrayManifest(elems.head._2)))(elems.head._2)
      case None => super.darray_length(da)
    }
  }
  
  override def darray_apply[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int]) = da match {
    case Def(Struct(tag, elems:Map[String,Exp[DeliteArray[Any]]])) =>
      struct[T](elems.map(p=>(p._1, dapply(p._2,i)(p._2.Type.typeArguments(0).asInstanceOf[Manifest[Any]]))))
    case _ => structType match {
      case Some(elems) => struct[T](elems.map(p=>(p._1, dapply(field[DeliteArray[Any]](da.asInstanceOf[Exp[Struct]],p._1)(darrayManifest(p._2)),i)(p._2))))
      case None => super.darray_apply(da,i)
    }
  }

  override def darray_update[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int], x: Exp[T]) = da match {
    case Def(Struct(tag, elems:Map[String,Exp[DeliteArray[Any]]])) =>
      elems.foreach(p=>dupdate(p._2,i,field[T](x.asInstanceOf[Exp[Struct]],p._1))(p._2.Type.typeArguments(0).asInstanceOf[Manifest[Any]]))
    case _ => structType match {
      case Some(elems) => elems.foreach(p=>dupdate(field[DeliteArray[Any]](da.asInstanceOf[Exp[Struct]],p._1)(darrayManifest(p._2)),i,field[T](x.asInstanceOf[Exp[Struct]],p._1))(p._2))
      case None => super.darray_update(da,i,x)
    }
  }

  override def darray_new[T:Manifest](length: Exp[Int]) = structType match {
    case Some(elems) => struct[DeliteArray[T]](elems.map(p=>(p._1, dnew(length)(p._2))))
    case None => super.darray_new(length)
  }

}

trait DeliteArrayFatExp extends DeliteArrayOpsExpOpt with StructFatExpOptCommon {
  this: DeliteOpsExp =>
}

trait ScalaGenDeliteArrayOps extends ScalaGenEffect {
  val IR: DeliteArrayOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case DeliteArrayNew(length) =>
      emitValDef(sym, "new Array[" + remap(sym.Type.typeArguments(0)) + "](" + quote(length) + ")")
    case DeliteArrayLength(da) =>
      emitValDef(sym, quote(da) + ".length")
    case DeliteArrayApply(da, idx) =>
      emitValDef(sym, quote(da) + "(" + quote(idx) + ")")
    case DeliteArrayUpdate(da, idx, x) =>
      emitValDef(sym, quote(da) + "(" + quote(idx) + ") = " + quote(x))
    case _ => super.emitNode(sym, rhs)
  }
  
  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DeliteArray" => m.typeArguments(0) match {
      case s if s <:< manifest[Struct] => structName(m)
      case arg => "Array[" + remap(arg) + "]"
    }
    case _ => super.remap(m)
  }
}
