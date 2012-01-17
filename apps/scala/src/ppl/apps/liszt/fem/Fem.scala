package ppl.apps.liszt.dynamicMesh

import ppl.apps.liszt.lib._
import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object FemRunner extends DeLisztApplicationRunner with Fem


trait Fem extends DeLisztApplication with Libs {

  var vF: Rep[Field[Vertex, Int]] = null
  var pos1: Rep[Field[Vertex, Vec[_3, Double]]] = null

  def main(): Unit = {
    val mesh1 = CuboidMesh(5., 5., 5., 5.)
    pos1 = FieldWithLabel[Vertex, Vec[_3, Double]]("position", mesh1)
    for (f1 <-faces(mesh1)) {
      for (v1 <-vertices(f1)) {
        print(pos1(v1) + " ")
      }
      println()
    }


    vF = FieldWithConst[Vertex, Int](50, mesh1)
/*    for (v <- mesh1.boundary) 
      vF(v) = 250*/
//    OutputMesh(mesh1, vF, vF)

  }
}
