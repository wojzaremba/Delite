package ppl.delite.framework.collections



import ppl.delite.framework.DeliteApplication
import scala.virtualization.lms.common._
import ppl.delite.framework.datastruct.scala._
import ppl.delite.framework.datastruct.scala.DeliteCollection
import java.io.PrintWriter



trait MapOps extends TraversableOps {
self: ArraySeqOps with ArraySeqEmitting =>
  
  /* ctors */
  object Map {
  }
  
  /* lifting */
  implicit def maprep2traversableops[K: Manifest, V: Manifest](t: Rep[Map[K, V]]) = new TraversableClsOps[(K, V), Map[K, V]](t)
  implicit def maprep2mapops[K: Manifest, V: Manifest](m: Rep[Map[K, V]]) = new MapClsOps[K, V, Map[K, V]](m)
  
  class MapClsOps[K: Manifest, V: Manifest, Coll <: Map[K, V]: Manifest](m: Rep[Coll]) {
    def get(key: Rep[K]) = map_get[K, V, Coll](m, key)
  }
  
  /* class interface defs */
  def map_get[K: Manifest, V: Manifest, Coll <: Map[K, V]: Manifest](m: Rep[Coll], key: Rep[K]): Rep[V]
  
  /* implicit rules */
  implicit def mapCanBuild[K: Manifest, V: Manifest, P: Manifest, Q: Manifest]: CanBuild[Map[K, V], (P, Q), Map[P, Q]]
  
}


trait MapOpsExp extends TraversableOpsExp {
self: HashMapOpsExp with HashMapEmitting with HashMultiMapEmitting =>
  
  /* nodes */
  case class MapGet[K: Manifest, V: Manifest, Coll <: Map[K, V]: Manifest](m: Exp[Coll], key: Exp[K]) extends Def[V]
  
  /* class interface */
  def map_get[K: Manifest, V: Manifest, Coll <: Map[K, V]: Manifest](m: Exp[Coll], key: Exp[K]): Exp[V] = MapGet[K, V, Coll](m, key)
  
  /* implicit rules */
  implicit def mapCanBuild[K: Manifest, V: Manifest, P: Manifest, Q: Manifest]: CanBuild[Map[K, V], (P, Q), Map[P, Q]] = new CanBuild[Map[K, V], (P, Q), Map[P, Q]] {
    def alloc(source: Exp[Map[K, V]]) = HashMap.apply[P, Q]()
    def emptyAlloc(source: Exp[Map[K, V]]) = HashMap[P, Q]()
    def emitterFactory(source: Exp[Map[K, V]]) = hashMapEmitterFactory[K, V]
    def noPrealloc = true
  }
  
}


trait ScalaGenMapOps extends ScalaGenTraversableOps {
self: ScalaGenHashMapOps =>
  
  val IR: MapOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case MapGet(m, key) => emitValDef(sym, quote(m) + ".get(" + quote(key) + ")")
    case _ => super.emitNode(sym, rhs)
  }
  
}

