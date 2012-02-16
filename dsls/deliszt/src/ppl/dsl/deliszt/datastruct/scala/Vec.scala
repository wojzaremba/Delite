package ppl.dsl.deliszt.datastruct.scala

import ppl.dsl.optila.datastruct.scala.DenseVector
import ppl.dsl.deliszt.datastruct.scala.MetaInteger._


/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 03/15/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

//have to be refatored to faster, specialized version
class Vec[N <: IntM : Manifest : MVal, T : Manifest](var v:T*) extends DenseVector[T](0, true) {
  def x = apply(0)
  def y = apply(1)
  def z = apply(2)
  def w = apply(3)
  
  _length = MIntDepth[N]
  _data = v.toArray
  

  def foreach[U](f: T => U) = {
    var i = 0
    while(i < _length) {
       f(this(i))
       i += 1
    }
  }

  override def Clone = { 
    new Vec[N, T](_data.toSeq : _*)
  }

}
