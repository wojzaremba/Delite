package ppl.dsl.deliszt.datastruct.scala

import ppl.dsl.optila.datastruct.scala.VectorView

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object LabelFieldImpl {
  def ofCell[T:Manifest](mesh: Mesh, url: String) : Field[T] = {
    val data = mesh.labelCell[T](url)._1
    new FieldImpl[T](data)
  }
  
  def ofEdge[T:Manifest](mesh: Mesh, url: String) : Field[T] = {
    val data = mesh.labelEdge[T](url)._1
    new FieldImpl[T](data)
  }
  
  def ofFace[T:Manifest](mesh: Mesh, url: String) : Field[T] = {
    val data = mesh.labelFace[T](url)._1
    new FieldImpl[T](data)
  }
  
  def ofVertex[T:Manifest](mesh: Mesh, url: String) : Field[T] = {
    val data = mesh.labelVertex[T](url)._1
    new FieldImpl[T](data)
  }

  def ofVertexVec[T:Manifest](mesh: Mesh, url: String) : Field[VectorView[T]] = {
    val data = mesh.labelVertex[T](url)
    new VecFieldImpl[T](data._1, data._2)
  }
}
