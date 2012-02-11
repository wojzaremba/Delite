package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object LabelFieldImpl {
  def ofCell[T:ClassManifest](mesh: Mesh, url: String) : Field[T] = {  
    val data = mesh.cellData.getGenericArray[T](url)
    new FieldImpl[T](data)
  }
  
  def ofEdge[T:ClassManifest](mesh: Mesh, url: String) : Field[T] = {  
    val data = mesh.edgeData.getGenericArray[T](url)
    new FieldImpl[T](data)
  }
  
  def ofFace[T:ClassManifest](mesh: Mesh, url: String) : Field[T] = {  
    val data = mesh.faceData.getGenericArray[T](url) 
    new FieldImpl[T](data)
  }
  
  def ofVertex[T:ClassManifest](mesh: Mesh, url: String) : Field[T] = {  
    val data = mesh.vertexData.getGenericArray[T](url)
    new FieldImpl[T](data)
  }
}
