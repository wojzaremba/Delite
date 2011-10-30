package ppl.delite.framework.collections



import ppl.delite.framework.DeliteApplication
import scala.virtualization.lms.common._
import ppl.delite.framework.datastruct.scala._
import ppl.delite.framework.datastruct.scala.DeliteCollection
import java.io.PrintWriter



trait HashMapOps extends MapOps {
self: ArraySeqOps with ArraySeqEmitting =>
  
  /* ctors */
  object HashMap {
    def apply[K: Manifest, V: Manifest]() = hashmap_obj_new[K, V]()
    def range(n: Rep[Int]) = hashmap_obj_new_range(n)
  }
  
  /* lifting */
  implicit def hashmaprep2traversableops[K: Manifest, V: Manifest](t: Rep[HashMap[K, V]]) = new TraversableClsOps[(K, V), HashMap[K, V]](t)
  
  /* object defs */
  def hashmap_obj_new[K: Manifest, V: Manifest](): Rep[HashMap[K, V]]
  def hashmap_obj_new_range(n: Rep[Int]): Rep[HashMap[Int, Int]]
  
  /* class interface defs */
  
  /* implicit rules */
  implicit def hashMapCanBuild[K: Manifest, V: Manifest, P: Manifest, Q: Manifest]: CanBuild[HashMap[K, V], (P, Q), HashMap[P, Q]]
  
}


trait HashMapOpsExp extends MapOpsExp with HashMapOps {
self: HashMapOpsExp with HashMapEmitting with ArraySeqOps with ArraySeqEmitting with HashMultiMapEmitting =>
  
  /* nodes */
  case class HashMapNew[K, V]()(val mV: Manifest[HashMapImpl[K, V]]) extends Def[HashMap[K, V]]
  case class HashMapNewRange(n: Exp[Int])(val mV: Manifest[HashMapImpl[Int, Int]]) extends Def[HashMap[Int, Int]]
  
  /* lifting */
  
  /* class interface */
  
  /* implicit rules */
  implicit def hashMapCanBuild[K: Manifest, V: Manifest, P: Manifest, Q: Manifest]: CanBuild[HashMap[K, V], (P, Q), HashMap[P, Q]] = new CanBuild[HashMap[K, V], (P, Q), HashMap[P, Q]] {
    def alloc(source: Exp[HashMap[K, V]]) = HashMap[P, Q]()
    def emptyAlloc(source: Exp[HashMap[K, V]]) = HashMap[P, Q]().asInstanceOf[Def[HashMap[P, Q]]]
    def emitterFactory(source: Exp[HashMap[K, V]]) = hashMapEmitterFactory[K, V]
    def noPrealloc = true
  }
  
  def hashmap_obj_new[K: Manifest, V: Manifest](): Exp[HashMap[K, V]] = reflectMutable(HashMapNew[K, V]()(manifest[HashMapImpl[K, V]]))
  def hashmap_obj_new_range(n: Exp[Int]) = reflectMutable(HashMapNewRange(n)(manifest[HashMapImpl[Int, Int]]))
  
}


trait ScalaGenHashMapOps extends ScalaGenMapOps {
//self: ScalaGenHashMapOps =>
  
  val IR: HashMapOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case v @ HashMapNew() => emitValDef(sym, "new " + remap(v.mV) + "()")
    case v @ HashMapNewRange(n) =>
      emitValDef(sym, "HashMapImpl.range(%s)".format(quote(n)))
    case _ => super.emitNode(sym, rhs)
  }
  
}





