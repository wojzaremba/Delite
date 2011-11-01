package ppl.dsl.assignment2

import virtualization.lms.common.{Variables, VariablesExp, BaseFatExp, StructExp}
import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._

/**
 * Operations
 */

trait VectorOps extends Variables {
  this: SimpleVector =>

  abstract class Vector[A] extends Struct with DeliteCollection[A] //making this a trait (interface only) causes subtyping problems :(

  //syntax
  def Vector[A:Manifest](length: Rep[Int]) = vectorNew(length)
  def infix_+[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]) = vectorPlus(x,y)
  def infix_+[A](x: Rep[Vector[A]], y: Rep[A])(implicit m: Manifest[A], n: Numeric[A], o: Overloaded1) = vectorPlusScalar(x,y)
  
  def infix_*[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[A]) = vectorScalarTimes(x,y)
  def infix_sum[A:Manifest:Numeric](x: Rep[Vector[A]]) = vectorSum(x)
  def infix_filter[A:Manifest](x: Rep[Vector[A]]) = vectorFilter(x)

  implicit def repToVecOps[A:Manifest](x: Rep[Vector[A]]) = new VecOpsCls(x)
  class VecOpsCls[A:Manifest](x: Rep[Vector[A]]) {
    def map[B:Manifest](f: Rep[A] => Rep[B]): Rep[Vector[B]] = vectorMap(x,f)
    def apply(idx: Rep[Int]) = vectorApply(x, idx)
    def update(idx: Rep[Int], value: Rep[A]) = vectorUpdate(x, idx, value)
  }

  def infix_length[A:Manifest](x: Rep[Vector[A]]) = vectorLength(x)
  def infix_isRow[A:Manifest](x: Rep[Vector[A]]) = vectorIsRow(x)
  def infix_pprint[A:Manifest](x: Rep[Vector[A]]) = vectorPrint(x)
  def infix_mtrans[A:Manifest](x: Rep[Vector[A]]) = vectorMTrans(x)

  //operations
  def vectorNew[A:Manifest](length: Rep[Int]): Rep[Vector[A]]
  def vectorPlus[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vectorPlusScalar[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]
  def vectorScalarTimes[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]
  def vectorSum[A:Manifest:Numeric](x: Rep[Vector[A]]): Rep[A]
  def vectorFilter[A:Manifest](x: Rep[Vector[A]]): Rep[Vector[A]]
  def vectorMap[A:Manifest,B:Manifest](x: Rep[Vector[A]], f: Rep[A] => Rep[B]): Rep[Vector[B]]
  
  def vectorLength[A:Manifest](x: Rep[Vector[A]]): Rep[Int]
  def vectorApply[A:Manifest](x: Rep[Vector[A]], idx: Rep[Int]): Rep[A]
  def vectorUpdate[A:Manifest](x: Rep[Vector[A]], idx: Rep[Int], value: Rep[A]): Rep[Unit]
  def vectorIsRow[A:Manifest](x: Rep[Vector[A]]): Rep[Boolean]

  def vectorPrint[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]
  def vectorMTrans[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]
}

trait VectorOpsExp extends VectorOps with VariablesExp with BaseFatExp with DeliteCollectionOpsExp {
  this: SimpleVectorExp =>
    
  //implemented via kernel embedding (sequential)
  case class PPrint[A:Manifest](x: Exp[Vector[A]], print: Exp[Unit])
    extends DeliteOpSingleTask(print)

  case class MTrans[A:Manifest](x: Exp[Vector[A]]) extends DeliteOpSingleTask(reifyEffectsHere(println(x))) //FIXME: var update crashes compiler
  
  //implemented via Delite ops
  case class VectorPlus[A:Manifest:Numeric](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpZipWith[A,A,A,Vector[A]] {

    def alloc = Vector[A](inA.length) // ignored
    override def allocWithArray = data => vectorWithArray(data)
    val size = copyTransformedOrElse(_.size)(inA.length)
    
    def func = (a,b) => a + b
  }
  
  abstract class VectorMap[A:Manifest:Numeric](in: Exp[Vector[A]]) extends DeliteOpMap[A,A,Vector[A]] {
    def alloc = Vector[A](in.length) // not used
    override def allocWithArray = data => vectorWithArray(data)
    val size = copyTransformedOrElse(_.size)(in.length)
  }

  case class VectorGenericMap[A:Manifest,B:Manifest](in: Exp[Vector[A]], f: Exp[A] => Exp[B]) extends DeliteOpMap[A,B,Vector[B]] {
    def alloc = Vector[B](in.length)
    override def allocWithArray = data => vectorWithArray(data)
    val size = copyTransformedOrElse(_.size)(in.length)
    def func = f
  }

  case class VectorPlusScalar[A:Manifest:Numeric](in: Exp[Vector[A]], s: Exp[A]) extends VectorMap[A](in) {
    def func = e => e + s
  }
  
  case class VectorTimesScalar[A:Manifest:Numeric](in: Exp[Vector[A]], s: Exp[A]) extends VectorMap[A](in) {
    def func = e => e * s
  }
  
  case class VectorSum[A:Manifest:Numeric](in: Exp[Vector[A]]) extends DeliteOpReduce[A] {
    val size = copyTransformedOrElse(_.size)(in.length)
    val zero = unit(0).asInstanceOfL[A]
    def func = (a,b) => a + b
  }
  
  case class VectorFilter[A:Manifest](in: Exp[Vector[A]]) extends DeliteOpFilter[A,A,Vector[A]] {
    val size = copyTransformedOrElse(_.size)(in.length)
    def alloc = Vector[A](0) // not used
    override def allocWithArray = data => vectorWithArray(data)
    
    def func = a => a
    def cond = a => v > 4
  }

  /**
   * Struct ops
   */
  //def vectorNew[A:Manifest](length: Exp[Int]) = struct[Vector[A]]("data"->DeliteArray[A](length), "isRow"->var_new(unit(false)).e) //appears to generate correctly, but a var update throws npe in type-checker
  def vectorNew[A:Manifest](length: Exp[Int]) = struct[Vector[A]]("data" -> DeliteArray[A](length))
  def vectorWithArray[A:Manifest](data: Exp[DeliteArray[A]]) = struct[Vector[A]]("data"->data)
  private def infix_data[A:Manifest](x: Exp[Vector[A]]) = field[DeliteArray[A]](x, "data")

  //def length[A:Manifest](x: Exp[Vector[A]]) = x.data.length
  def vectorLength[A:Manifest](x: Exp[Vector[A]]) = darray_length(x.data)
  def vectorApply[A:Manifest](x: Exp[Vector[A]], idx: Exp[Int]) = darray_apply(x.data, idx)
  def vectorUpdate[A:Manifest](x: Exp[Vector[A]], idx: Exp[Int], value: Exp[A]) = darray_update(x.data, idx, value)
  def vectorIsRow[A:Manifest](x: Exp[Vector[A]]) = sys.error("isRow removed") /*readVar(vfield[Boolean](x, "isRow"))*/

  /**
   * Vector Ops
   */
  def vectorPlus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorPlus(x,y)
  def vectorPlusScalar[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[A]) = VectorPlusScalar(x,y)
  def vectorScalarTimes[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[A]) = VectorTimesScalar(x, y)
  def vectorSum[A:Manifest:Numeric](x: Exp[Vector[A]]) = VectorSum(x)
  def vectorFilter[A:Manifest](x: Exp[Vector[A]]) = VectorFilter(x)
  def vectorMap[A:Manifest,B:Manifest](x: Exp[Vector[A]], f: Exp[A] => Exp[B]) = VectorGenericMap(x, f)

  def vectorPrint[A:Manifest](x: Exp[Vector[A]]) = reflectEffect(PPrint(x, reifyEffectsHere(pprint_impl(x))))
  def vectorMTrans[A:Manifest](x: Exp[Vector[A]]) = reflectEffect(MTrans(x))

  /**
   * DeliteCollection Ops
   */
  private def isSubType(x: java.lang.Class[_], cls: java.lang.Class[_]): Boolean = {
    if ((x == cls) || x.getInterfaces.contains(cls)) true
    else if (x.getSuperclass == null) false
    else isSubType(x.getSuperclass, cls)
  }

  private def ifVector[A:Manifest, R](x: Exp[DeliteCollection[A]])(then: Exp[Vector[A]] => R)(orElse: => R): R = {
    if (isSubType(x.Type.erasure, classOf[Vector[A]])) then(x.asInstanceOf[Exp[Vector[A]]]) else orElse
  }

  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]]): Exp[Int] = ifVector(x)(vectorLength(_))(super.dc_size(x))
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], idx: Exp[Int]): Exp[A] = ifVector(x)(vectorApply(_, idx))(super.dc_apply(x, idx))
  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], idx: Exp[Int], value: Exp[A]): Exp[Unit] = ifVector(x)(v => reifyEffectsHere(darray_update(v.data, idx, value)))(super.dc_update(x,idx,value))
  
}

/**
 * Implementation using kernel embedding
 */
trait VectorImplOps { this: SimpleVector =>
  def pprint_impl[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]
}

trait VectorImplOpsStandard extends VectorImplOps {
  this: SimpleVectorCompiler with SimpleVectorLift =>

  def pprint_impl[A:Manifest](x: Rep[Vector[A]]) = {
    print("[ ")
    for (i <- 0 until x.length) {
      print(x(i)); print(" ")
    }
    print("]\\n")
  }

}
