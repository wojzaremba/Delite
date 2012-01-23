package ppl.apps.liszt.lib

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

/**
 * User: woj.zaremba
 * Date: 01/12/12
 */

trait Common {
  this:Libs =>

  implicit def toMesh(e : ExtendedMesh): Rep[Mesh] = e.mesh
  
  implicit def toVertex(e : ExtendedVertex): Rep[Vertex] = e.vertex
  
  implicit def toCell(e : ExtendedCell): Rep[Cell] = e.cell
  
  implicit def toExtendedVertex(v : Rep[Vertex]): ExtendedVertex = {
    val pos = FieldWithLabel[Vertex,Vec[_3,Double]]("position", mesh(v))
    val p = pos(v)
    ExtendedVertex(v, p.x, p.y, p.z, p)
  }

  implicit def toFunc1[T : Numeric : Manifest](f: Function3[Rep[Double], Rep[Double], Rep[Double], Rep[T]]) : Function1[Rep[Vertex], Rep[T]] = 
    (vec: Rep[Vertex]) => f(vec.x, vec.y, vec.z)
    
  implicit def toFunc2[T : Numeric : Manifest](f: Function3[Rep[Double], Rep[Double], Rep[Double], Tuple2[Rep[T], Rep[T]]]) : Seq[Function1[Rep[Vertex], Rep[T]]] = 
    Seq((vec: Rep[Vertex]) => f(vec.x, vec.y, vec.z)._1, (vec: Rep[Vertex]) => f(vec.x, vec.y, vec.z)._2)
  
  implicit def toFunc3[T : Numeric : Manifest](f: Function3[Rep[Double], Rep[Double], Rep[Double], Tuple3[Rep[T], Rep[T], Rep[T]]]) : Seq[Function1[Rep[Vertex], Rep[T]]] = 
    Seq((vec: Rep[Vertex]) => f(vec.x, vec.y, vec.z)._1, (vec: Rep[Vertex]) => f(vec.x, vec.y, vec.z)._2, (vec: Rep[Vertex]) => f(vec.x, vec.y, vec.z)._3)
  
  var det_ : Rep[Field[Cell,Double]] = null

  implicit def toExtendedCell(cell : Rep[Cell]): ExtendedCell = {
    //wz : I know - ugly, wrong, works only for tetrahedron and only for single mesh
    det_ =  FieldWithConst[Cell, Double](0, mesh(cell))
    for {
      a0 <- vertices(cell)
      a1 <- vertices(cell)
      a2 <- vertices(cell)
      a3 <- vertices(cell)
    }   
    if (ID(a0) < ID(a1) && ID(a1) < ID(a2) && ID(a2) < ID(a3)) {
        val a = a1.pos - a0.pos
        val b = a2.pos - a0.pos
        val c = a3.pos - a0.pos
        det_(cell) = abs(a.x * b.y * c.z + b.x * c.y * a.z + c.x * a.y * b.z - 
              (a.x * c.y * b.z + b.x * a.y * c.z + c.x * b.y * a.z))
    }    
    ExtendedCell(cell, det_(cell))
  }

  case class ExtendedMesh(mesh : Rep[Mesh], boundary: Rep[MeshSet[Vertex]])
 
  case class ExtendedVertex(vertex : Rep[Vertex], x : Rep[Double], y : Rep[Double], z : Rep[Double], pos : Rep[Vec[_3,Double]])
  
  case class ExtendedCell(cell : Rep[Cell], det:Rep[Double])



}
