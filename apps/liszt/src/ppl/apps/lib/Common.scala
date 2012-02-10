package ppl.apps.liszt.lib

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

/**
 * User: woj.zaremba
 * Date: 01/12/12
 */

trait Common {
  this: Libs =>

  var m: ExtendedMesh = null
  var pos : Rep[Field[Vertex, Vec[_3, Float]]] = null

  import scala.collection.{Seq => USeq}

  implicit def toMesh(e: ExtendedMesh): Rep[Mesh] = e.mesh

  // implicit def toVertex(e : ExtendedVertex): Rep[Vertex] = e.vertex

  implicit def toFace(e: ExtendedFace): Rep[Face] = e.face

  implicit def toCell(e: Tetrahedron): Rep[Cell] = e.cell

  implicit def repVertexToVec(v: Rep[Vertex]) : Rep[Vec[_3, Float]] = pos(v)

  implicit def repVertexToVecOps(v: Rep[Vertex]) : vecOpsCls[_3, Float] = repVecToVecOps[_3, Float](repVertexToVec(v))

  implicit def toExtendedEdge(e: Rep[Edge]): ExtendedEdge = ExtendedEdge(e, head(e), tail(e))

  implicit def toExtendedFace(e: Rep[Face]): ExtendedFace = ExtendedFace(e)

  /* implicit def toExtendedVertex(v : Rep[Vertex]): ExtendedVertex = {
    val pos = FieldWithLabel[Vertex,Vec[_3,Float]]("position", mesh(v))
    val p = pos(v)
    ExtendedVertex(v, p.x, p.y, p.z, p)
  }*/

  implicit def toFunc1[T: Numeric : Manifest](f: Function3[Rep[Float], Rep[Float], Rep[Float], Rep[T]]): Function1[Rep[Vertex], Rep[T]] =
    (vec: Rep[Vertex]) => f(vec.x, vec.y, vec.z)

  implicit def toFunc2[T: Numeric : Manifest](f: Function3[Rep[Float], Rep[Float], Rep[Float], Tuple2[Rep[T], Rep[T]]]): USeq[Function1[Rep[Vertex], Rep[T]]] =
    USeq((vec: Rep[Vertex]) => f(vec.x, vec.y, vec.z)._1, (vec: Rep[Vertex]) => f(vec.x, vec.y, vec.z)._2)

  implicit def toFunc3[T: Numeric : Manifest](f: Function3[Rep[Float], Rep[Float], Rep[Float], Tuple3[Rep[T], Rep[T], Rep[T]]]): USeq[Function1[Rep[Vertex], Rep[T]]] =
    USeq((vec: Rep[Vertex]) => f(vec.x, vec.y, vec.z)._1, (vec: Rep[Vertex]) => f(vec.x, vec.y, vec.z)._2, (vec: Rep[Vertex]) => f(vec.x, vec.y, vec.z)._3)

  var det_ : Rep[Field[Cell, Float]] = null

  implicit def toTetrahedron(cell: Rep[Cell]): Tetrahedron = Tetrahedron(cell)


  case class ExtendedMesh(mesh: Rep[Mesh], boundary: Rep[MeshSet[Vertex]] = null, inside: Rep[MeshSet[Vertex]] = null)

  // case class ExtendedVertex(vertex : Rep[Vertex], x : Rep[Float], y : Rep[Float], z : Rep[Float], pos : Rep[Vec[_3,Float]])

  case class ExtendedEdge(edge: Rep[Edge], head: Rep[Vertex], tail: Rep[Vertex])

  case class ExtendedFace(face: Rep[Face]) {
    def det() : Rep[Float] = {
      val a = vertex(face, 1) - vertex(face, 0)
      val b = vertex(face, 2) - vertex(face, 0)
      val r = (a.x * b.y - a.y * b.x)
      if (r < 0) -r else r
    }
  }

  case class Tetrahedron(cell: Rep[Cell]) {
    def det() : Rep[Float] = {
      val a = vertex(cell, 1) - vertex(cell, 0)
      val b = vertex(cell, 2) - vertex(cell, 0)
      val c = vertex(cell, 3) - vertex(cell, 0)
      val r = (a.x * b.y * c.z + b.x * c.y * a.z + c.x * a.y * b.z -
        (a.x * c.y * b.z + b.x * a.y * c.z + c.x * b.y * a.z))
      if (r < 0) -r else r
    }
  }

}
