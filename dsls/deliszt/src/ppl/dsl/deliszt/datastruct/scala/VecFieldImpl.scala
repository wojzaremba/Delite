package ppl.dsl.deliszt.datastruct.scala

import ppl.dsl.deliszt.datastruct.scala.MetaInteger._
import ppl.dsl.deliszt.datastruct.scala._

import ppl.dsl.deliszt.datastruct.scala.MetaInteger._
import ppl.dsl.optila.datastruct.scala.VectorView
import ppl.dsl.optila.datastruct.scala.DenseVector

object VecFieldImpl {
  
  def ofCell[T : Manifest](veclen : Int, m : Mesh = Mesh.mesh) : VecFieldImpl[T] = {
      new VecFieldImpl[T](new Array[T](veclen*m.ncells), veclen)
  }
  
  def ofEdge[T : Manifest](veclen : Int, m : Mesh = Mesh.mesh) : VecFieldImpl[T] = {
      new VecFieldImpl[T](new Array[T](veclen*m.nedges), veclen)
  }
  
  def ofFace[T : Manifest](veclen : Int, m : Mesh = Mesh.mesh) : VecFieldImpl[T] = {
      new VecFieldImpl[T](new Array[T](veclen*m.nfaces), veclen)
  }
  
  def ofVertex[T : Manifest](veclen : Int, m : Mesh = Mesh.mesh) : VecFieldImpl[T] = {
      new VecFieldImpl[T](new Array[T](veclen*m.nvertices), veclen)
  }

  def cellWithConst[T : Manifest](v: VectorView[T], m : Mesh = Mesh.mesh) : VecFieldImpl[T] = {
      val f = VecFieldImpl.ofCell[T](v.length, m)
      f.fill(v)
      f
  }
  
  def edgeWithConst[T : Manifest](v: VectorView[T], m : Mesh = Mesh.mesh) : VecFieldImpl[T] = {
      val f = VecFieldImpl.ofEdge[T](v.length, m)
      f.fill(v)
      f
  }
  
  def faceWithConst[T : Manifest](v: VectorView[T], m : Mesh = Mesh.mesh) : VecFieldImpl[T] = {
      val f = VecFieldImpl.ofFace[T](v.length, m)
      f.fill(v)
      f
  }
  
  def vertexWithConst[T : Manifest](v: VectorView[T], m : Mesh = Mesh.mesh): VecFieldImpl[T] = {
      val f = VecFieldImpl.ofVertex[T](v.length, m)
      f.fill(v)
      f
  }

//  def cellWithConst[T : Manifest](v: DenseVector[T], m : Mesh = Mesh.mesh) : VecFieldImpl[T] = {
//    val f = VecFieldImpl.ofCell[T](v._length, m)
//    f.fill(v)
//    f
//  }
//
//  def edgeWithConst[T : Manifest](v: DenseVector[T], m : Mesh = Mesh.mesh) : VecFieldImpl[T] = {
//    val f = VecFieldImpl.ofEdge[T](v._length, m)
//    f.fill(v)
//    f
//  }
//
//  def faceWithConst[T : Manifest](v: DenseVector[T], m : Mesh = Mesh.mesh) : VecFieldImpl[T] = {
//    val f = VecFieldImpl.ofFace[T](v._length, m)
//    f.fill(v)
//    f
//  }
//
//  def vertexWithConst[T : Manifest](v: DenseVector[T], m : Mesh = Mesh.mesh) : VecFieldImpl[T] = {
//    val f = VecFieldImpl.ofVertex[T](v._length, m)
//    f.fill(v)
//    f
//  }
}


class VecFieldImpl[T : Manifest](val data: Array[T], veclen : Int) extends Field[VectorView[T]] {

  def size = data.length / veclen

  def apply(idx: Int) = {
    new VectorView[T](data, Mesh.internal(idx)*veclen, 1, veclen, true)
  }
  
  def raw_apply(idx: Int, elem: Int) = {
    val offset = idx*veclen
    data(offset+elem)
  }

  def update(idx: Int, x: VectorView[T]) = {
    val offset = idx*veclen
    var i = 0
    while(i < veclen) {
      data(offset + i) = x(i)
      i += 1
    }
  }

  def update(idx: Int, x: DenseVector[T]) = {
    val offset = idx*veclen
    var i = 0
    while(i < veclen) {
      data(offset + i) = x._data(i)
      i += 1
    }
  }

  def raw_update(idx: Int, elem: Int, v: T) = {
    val offset = Mesh.internal(idx)*veclen
    data(offset+elem) = v
  }
  
  def fill(v: VectorView[T]) {
    var i = 0
    while(i < data.length) {
      data(i) = v(i % veclen)
      i += 1
    }
  }

  def fill(v: DenseVector[T]) {
    var i = 0
    while(i < data.length) {
      data(i) = v._data(i % veclen)
      i += 1
    }
  }
  
}

