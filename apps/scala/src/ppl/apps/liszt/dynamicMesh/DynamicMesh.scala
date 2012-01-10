package ppl.apps.liszt.laplace

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._
import ppl.dsl.deliszt.lib._

object DynamicMeshRunner extends DeLisztApplicationRunner with DynamicMesh


trait DynamicMesh extends DeLisztApplication with SimpleMesh {

  var pos1 : Rep[Field[Vertex,Vec[_3,Double]]] = null
  var pos2 : Rep[Field[Vertex,Vec[_3,Double]]] = null

  def main(): Unit = {
    val mesh1 = SquareMesh(0.75, 0.75)
    val mesh2 = RectangularMesh(10., 5., 5.)

    pos1 = FieldWithLabel[Vertex,Vec[_3,Double]]("position", mesh1)
    pos2 = FieldWithLabel[Vertex,Vec[_3,Double]]("position", mesh2)
    
    for (v1 <- vertices(mesh1)) {
      println("vertices(mesh1) : " + ID(v1) + " " + pos1(v1))
      for (e2 <- edges(mesh2)) {
        println("  edges(mesh2) : " + ID(e2))
          for (v2 <- vertices(e2)) {
            println("    vertices(e2) from mesh2 : "  + ID(v2) + " " + pos2(v2))
          }
      }
    }
  }
}
