package ppl.dsl.deliszt.datastruct.scala


/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object FieldImpl {

  def ofCell[T:Manifest](m : Mesh = Mesh.mesh) : Field[T] = {
    new FieldImpl[T](new Array[T](m.ncells))
  }
  
  def ofEdge[T:Manifest](m : Mesh = Mesh.mesh) : Field[T] = {
    new FieldImpl[T](new Array[T](m.nedges))
  }
  
  def ofFace[T:Manifest](m : Mesh = Mesh.mesh) : Field[T] = {
    new FieldImpl[T](new Array[T](m.nfaces))
  }
  
  def ofVertex[T:Manifest](m : Mesh = Mesh.mesh) : Field[T] = {
    new FieldImpl[T](new Array[T](m.nvertices))
  }
  
  def cellWithConst[T:Manifest](v: T, m : Mesh = Mesh.mesh) : Field[T] = {
    val f = FieldImpl.ofCell[T](m)
    f.fill(v)
    f
  }
  
  def edgeWithConst[T:Manifest](v: T, m : Mesh = Mesh.mesh) : Field[T] = {
    val f = FieldImpl.ofEdge[T](m)
    f.fill(v)
    f
  }
  
  def faceWithConst[T:Manifest](v: T, m : Mesh = Mesh.mesh) : Field[T] = {
    val f = FieldImpl.ofFace[T](m)
    f.fill(v)
    f
  }
  
  def vertexWithConst[T:Manifest](v: T, m : Mesh = Mesh.mesh) : Field[T] = {
    val f = FieldImpl.ofVertex[T](m)
    f.fill(v)
    f
  }
}

class FieldImpl[T: Manifest](val data : Array[T]) extends Field[T] {
  def apply(idx: Int) = data(idx)
  def update(idx: Int, x: T) = {
    data(idx) = x
  }
  def size = data.length
  
  def fill(v: T) = {  
    if(classManifest[T] <:< classManifest[Copyable]) {
      var i = 0
      while(i < size) {
        data(i) = v.asInstanceOf[Copyable].copy.asInstanceOf[T]
        i += 1
      }
    }
    else {
      var i = 0
      while(i < size) {
        data(i) = v
        i += 1
      }
    }
  }

  override def toString() = "FieldImpl " + data.toList.toString


}
