package ppl.apps.liszt.dynamicMesh

import ppl.apps.liszt.lib._
import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object FemRunner extends DeLisztApplicationRunner with Fem


trait Fem extends DeLisztApplication with Libs {

  var matrix: Rep[Field[Edge, Double]] = null
  var matrixV: Rep[Field[Vertex, Double]] = null
  var values: Rep[Field[Vertex, Double]] = null
  var m: ExtendedMesh = null
  var dF: Rep[Field[Vertex, Double]] = null

  def f(x: Rep[Double], y: Rep[Double], z: Rep[Double]) =
    x * (1.- x) * y * (1.- y) * z * (1.- z)

  def deltaF(x: Rep[Double], y: Rep[Double], z: Rep[Double]) =
    -(x * (1.- x) * y * (1.- y) + y * (1.- y) * z * (1.- z) + x * (1.- x) * z * (1.- z))

  def integralEdge(cell: Rep[Cell], edge: Rep[Edge]) = {
    val arr = ((Set[Rep[Int]](0, 1, 2, 3) - ID(edge.head)) - ID(edge.tail)).map(vertex(cell, _)).toArray
    val from1 = edge.head - arr(0)
    val to1 = edge.tail - arr(0)
    val b1 = arr(1) - arr(0)
    val v1 = cross(from1, b1)
    val v2 = cross(to1, b1)
    (dot(v1, v2) * cell.vol / dot(v1, from1)) / dot(v2, from1)
  }

  /*def infix_filter(l : List[T], f : T => Rep[Boolean]) = {

  } */

  def test(x:Int):Rep[Boolean] = unit(true)
  
  def integralVertex(a: Rep[Vertex], b: Rep[Vertex], c: Rep[Vertex], d: Rep[Vertex]) = {
    if (m.inside.contains(a)) {
      val a1 = a - d
      val b1 = b - d
      val c1 = c - d
      val v1 = cross(b1, c1)
      val s1 = dot(v1, a1)
      (dot(v1, v1)) / (s1 * s1)
    } else 0.
  }


  //boundary term
  def main(): Unit = {
    m = CubeMesh(2., 1.)
    //m = TetrahedronMesh()
    matrix = FieldWithConst[Edge, Double](0., m)
    matrixV = FieldWithConst[Vertex, Double](0., m)
  /*  values = FieldWithConst[Vertex, Double](0., m)
    dF = FieldWithConst[Vertex, Double](0., m)
    for (v <- vertices(m)) dF(v) = deltaF(v.x, v.y, v.z)
    for (edge <- edges(m))
      for (cell <- cells(edge))
        matrix(edge) += integralEdge(cell, edge)
    */
    for (cell <- cells(m)) {
      val (a, b, c, d) = (vertex(cell, 0), vertex(cell, 1), vertex(cell, 2), vertex(cell,3))
      matrixV(a) += integralVertex(a, b, c, d)*cell.vol
    }

    for (v <- vertices(m)) println(matrixV(v))

 /*  println("Matrix(edge)")
   for (edge <- edges(m)) println("Edge " + ID(edge) + " value = " + matrix(edge))

    println("MatrixV(Vertex)")
    for (v <- vertices(m)) println("Vertex " + ID(v) + " value = " + matrixV(v))

    var error = 100.
    while (error < 0.001) {
      error = 0.
      for (v <- m.inside) {
        var sum = 0.
        for (e <- edges(v)) {
          val e2 = if (ID(e.head) == ID(v)) e.tail else e.head
          sum += matrix(e) * values(e2)
        }
        val old = values(v)
        values(v) = (dF(v) - sum) / matrixV(v)
        error += abs(values(v) - old)
      }
      println("error " + error)
    }

    OutputMesh(m, values)     */
    //OutputMesh(m, f _)
  }
}
