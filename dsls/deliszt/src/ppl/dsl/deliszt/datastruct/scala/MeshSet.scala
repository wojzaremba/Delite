package ppl.dsl.deliszt.datastruct.scala

import ppl.delite.framework.datastruct.scala.DeliteCollection

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait MeshSet extends DeliteCollection[Int] with Traversable[Int] {
  def apply(i : Int) : Int

  def dcApply(idx: Int) = apply(idx)
  def dcUpdate(idx: Int, x: Int) = {} // Read only, bitches
  def dcSize : Int = size
  
  def foreach[U](f: Int => U) : Unit = {
     var i = 0
     while(i < size) {
        f(this(i))
        i += 1
     }
  }

  def contains(i : Int) : Boolean = throw new Exception("Only implemented for BoundarySetRangeImpl")

  //wz: just for small collections, not sure if it's good idea
  /*def map[U](f: Int => U) : Traversable[U] = {
    val arr = new Array[U](size)
    var i = 0
    while(i < size) {
      arr(i) = f(this(i))
      i += 1
    }
    arr
  } */

}
