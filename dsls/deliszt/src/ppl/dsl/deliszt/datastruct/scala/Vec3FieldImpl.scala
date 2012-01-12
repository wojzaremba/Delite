package ppl.dsl.deliszt.datastruct.scala

object Vec3FieldImpl {
  
  def ofCell(m : Mesh = Mesh.mesh) : Vec3FieldImpl = {
      new Vec3FieldImpl(m.ncells, new Array[Double](3*m.ncells))
  }
  
  def ofEdge(m : Mesh = Mesh.mesh) : Vec3FieldImpl = {
      new Vec3FieldImpl(m.nedges, new Array[Double](3*m.nedges))
  }
  
  def ofFace(m : Mesh = Mesh.mesh) : Vec3FieldImpl = {
      new Vec3FieldImpl(m.nfaces, new Array[Double](3*m.nfaces))
  }
  
  def ofVertex(m : Mesh = Mesh.mesh) : Vec3FieldImpl = {
      new Vec3FieldImpl(m.nvertices, new Array[Double](3*m.nvertices))
  }

  def cellWithConst[T:ClassManifest](v: T, m : Mesh = Mesh.mesh) : Field[T] = {
      val f = Vec3FieldImpl.ofCell(m)
      f.fill(v.asInstanceOf[Vec[Double]])
      f.asInstanceOf[Field[T]]
  }
  
  def edgeWithConst[T:ClassManifest](v: T, m : Mesh = Mesh.mesh) : Field[T] = {
      val f = Vec3FieldImpl.ofEdge(m)
      f.fill(v.asInstanceOf[Vec[Double]])
      f.asInstanceOf[Field[T]]
  }
  
  def faceWithConst[T:ClassManifest](v: T, m : Mesh = Mesh.mesh) : Field[T] = {
      val f = Vec3FieldImpl.ofFace(m)
      f.fill(v.asInstanceOf[Vec[Double]])
      f.asInstanceOf[Field[T]]
  }
  
  def vertexWithConst[T:ClassManifest](v: T, m : Mesh = Mesh.mesh) : Field[T] = {
      val f = Vec3FieldImpl.ofVertex(m)
      f.fill(v.asInstanceOf[Vec[Double]])
      f.asInstanceOf[Field[T]]
  }
}


class Vec3FieldImpl(val numVec:Int, val data: Array[Double]) extends Field[Vec[Double]] {
  def apply(idx: Int) = {
    new Vec3ViewImpl(data, Mesh.internal(idx)*3)
  }
  
  override def raw_apply(idx: Int, elem: Int) = {
    val offset = idx*3//Mesh.internal(idx)*3
    data(offset+elem)
  }

  def update(idx: Int, x: Vec[Double]) = {
    val offset = idx*3//Mesh.internal(idx)*3
    data(offset) = x(0)
    data(offset+1) = x(1)
    data(offset+2) = x(2)
  }

  override def raw_update(idx: Int, elem: Int, v: Double) = {
    val offset = Mesh.internal(idx)*3
    data(offset+elem) = v
  }
  
  def size = numVec

  def fill(v: Vec[Double]) {
    var i = 0
    while(i < 3*size) {
      data(i) = v(0)
      data(i+1) = v(1)
      data(i+2) = v(2)
      i += 3
    }
  }
}
  
class Vec3ViewImpl(val data: Array[Double], val idx: Int) extends Vec[Double] {
  
  def apply(n: Int) = {
    data(idx + n)
  }
  def update(n: Int, v: Double) = {
    data(idx + n) = v
  }
  override val size = 3
  
  def cloneL = {
    new Vec3Impl[Double](data(idx), data(idx+1), data(idx+2))
  }
  
  override def foreach[U](f: Double => U) = {
    var i = 0
    while (i < size) {
      f(data(idx + i))
      i += 1
    }
  }
}
