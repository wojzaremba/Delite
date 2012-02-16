package ppl.dsl.deliszt.datastruct.scala

import ppl.dsl.deliszt.datastruct.scala.MetaInteger._
import ppl.dsl.deliszt.datastruct.scala._

object VecFieldImpl {
  
  def ofCell[N <: IntM : Manifest : MVal, T : Manifest](m : Mesh = Mesh.mesh) : Field[Vec[N, T]] = {
      new VecFieldImpl[N, T](new Array[T](MIntDepth[N]*m.ncells))
  }
  
  def ofEdge[N <: IntM : Manifest : MVal, T : Manifest](m : Mesh = Mesh.mesh) : Field[Vec[N, T]] = {
      new VecFieldImpl[N, T](new Array[T](MIntDepth[N]*m.nedges))
  }
  
  def ofFace[N <: IntM : Manifest : MVal, T : Manifest](m : Mesh = Mesh.mesh) : Field[Vec[N, T]] = {
      new VecFieldImpl[N, T](new Array[T](MIntDepth[N]*m.nfaces))
  }
  
  def ofVertex[N <: IntM : Manifest : MVal, T : Manifest](m : Mesh = Mesh.mesh) : Field[Vec[N, T]] = {
      new VecFieldImpl[N, T](new Array[T](MIntDepth[N]*m.nvertices))
  }

  def cellWithConst[N <: IntM : Manifest : MVal, T : Manifest](v: Vec[N, T], m : Mesh = Mesh.mesh) : Field[Vec[N, T]] = {
      val f = VecFieldImpl.ofCell[N, T](m)
      f.fill(v)
      f
  }
  
  def edgeWithConst[N <: IntM : Manifest : MVal, T : Manifest](v: Vec[N, T], m : Mesh = Mesh.mesh) : Field[Vec[N, T]] = {
      val f = VecFieldImpl.ofEdge[N, T](m)
      f.fill(v)
      f
  }
  
  def faceWithConst[N <: IntM : Manifest : MVal, T : Manifest](v: Vec[N, T], m : Mesh = Mesh.mesh) : Field[Vec[N, T]] = {
      val f = VecFieldImpl.ofFace[N, T](m)
      f.fill(v)
      f
  }
  
  def vertexWithConst[N <: IntM : Manifest : MVal, T : Manifest](v: Vec[N, T], m : Mesh = Mesh.mesh) : Field[Vec[N, T]] = {
      val f = VecFieldImpl.ofVertex[N, T](m)
      f.fill(v)
      f
  }
}


class VecFieldImpl[N <: IntM : Manifest : MVal, T : Manifest](val data: Array[T]) extends Field[Vec[N, T]] {

  def size = data.length / MIntDepth[N]

  def apply(idx: Int) = {
    new Vec[N, T](data.slice(Mesh.internal(idx)*MIntDepth[N], MIntDepth[N]).toSeq : _*)
  }
  
  def raw_apply(idx: Int, elem: Int) = {
    val offset = idx*MIntDepth[N]
    data(offset+elem)
  }

  def update(idx: Int, x: Vec[N, T]) = {
    val offset = idx*MIntDepth[N]
    var i = 0
    while(i < MIntDepth[N]) {
      data(offset + i) = x(i)
      i += 1
    }
  }

  def raw_update(idx: Int, elem: Int, v: T) = {
    val offset = Mesh.internal(idx)*MIntDepth[N]
    data(offset+elem) = v
  }
  
  def fill(v: Vec[N, T]) {
    var i = 0
    while(i < data.length) {
      data(i) = v(i % MIntDepth[N])
      i += 1
    }
  }
}
  
/*class VecViewImpl[N <: IntM : Manifest : MVal, T : Manifest](val data: Array[T], val idx: Int) extends Vec[N, T] {

  override def apply(n: Int) = {
    data(idx + n)
  }
  override def update(n: Int, v: T) = {
    data(idx + n) = v
  }
  
  override def foreach[U](f: T => U) = {
    var i = 0
    while (i < _length) {
      f(data(idx + i))
      i += 1
    }
  }
}*/
