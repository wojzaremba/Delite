package ppl.dsl.deliszt

import ppl.delite.framework.datastruct.scala.DeliteCollection

/**
 * DeLiszt compiler types
 */

//////////////////
// DeLiszt

trait Vec[N<:IntM,T] extends DeliteCollection[T]
trait Mat[R<:IntM,C<:IntM,T] extends DeliteCollection[T]

trait VecView[N<:IntM,T] extends Vec[N,T]
trait MatRow[C<:IntM,T] extends VecView[C,T]
trait MatCol[R<:IntM,T] extends VecView[R,T]

// Mesh objects
trait MeshObj {
  val id : Int
}

object MeshObj {
  implicit def MeshObjToInt(m : MeshObj) : Int = m.id
  implicit def MeshObjSeqToIntSeq[T <: MeshObj](m : Seq[T]) : Seq[Int] = for (t <- m) yield t.id
}

trait Mesh extends MeshObj
case class Cell(override val id : Int) extends MeshObj
case class Edge(override val id : Int, head : Vertex, tail : Vertex) extends MeshObj
case class Face(override val id : Int) extends MeshObj
case class Vertex(override val id : Int) extends MeshObj

// Mesh set
trait MeshSet[MO <: MeshObj] extends DeliteCollection[MO]
trait BoundarySet[MO<:MeshObj] extends MeshSet[MO]

// Fields
trait CRS
trait Field[+MO<:MeshObj,T] extends DeliteCollection[T]
trait LabelField[MO<:MeshObj,T] extends DeliteCollection[T]


//helpful for IO operations
trait SyncedFile