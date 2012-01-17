package ppl.apps.liszt.dynamicMesh

import ppl.apps.liszt.lib._
import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object DynamicMeshRunner extends DeLisztApplicationRunner with DynamicMesh


trait DynamicMesh extends DeLisztApplication with Libs {

  var pos1: Rep[Field[Vertex, Vec[_3, Double]]] = null
  var pos2: Rep[Field[Vertex, Vec[_3, Double]]] = null

  var f: Rep[Field[Edge, Double]] = null
  var vF: Rep[Field[Vertex, Int]] = null
  var fvec: Rep[Field[Edge, Vec[_3, Double]]] = null


  def main(): Unit = {
    val mesh1 = RectangularMesh(15., 5., 5.)
    val mesh2 = SquareMesh(0.75, 0.75)

    pos1 = FieldWithLabel[Vertex, Vec[_3, Double]]("position", mesh1)
    pos2 = FieldWithLabel[Vertex, Vec[_3, Double]]("position", mesh2)

    for (v1 <- vertices(mesh1)) {
      println("vertices(mesh1) : " + ID(v1) + " " + pos1(v1))
      for (e2 <- edges(mesh2)) {
        println("  edges(mesh2) : " + ID(e2))
        for (v2 <- vertices(e2)) {
          println("    vertices(e2) from mesh2 : " + ID(v2) + " " + pos2(v2))
        }
      }
    }

    val pos = pos1
    f = FieldWithConst[Edge, Double](5.0, mesh1)
    fvec = FieldWithConst[Edge, Vec[_3, Double]](Vec(0.0, 0.0, 0.0), mesh1)
    vF = FieldWithConst[Vertex, Int](0, mesh1)

    for (e <- edges(mesh1)) {
      f(e) = pos(head(e)).y + pos(tail(e)).y
      fvec(e) = pos(head(e)) + pos(tail(e))
    }

    for (e <- edges(mesh1)) {
      println(pos1(head(e)) + " " + pos1(tail(e)) + " " + f(e) + " " + fvec(e))
    }
    for (v <- vertices(mesh1)) {
      vF(v) = (pos1(v).x * 10.).toInt
    }
    OutputMesh(mesh1, vF)

  }
}
