package ppl.apps.liszt.dynamicMesh

import ppl.apps.liszt.lib._
import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object FemRunner extends DeLisztApplicationRunner with Fem


trait Fem extends DeLisztApplication with Libs {

  var vF: Rep[Field[Vertex, Int]] = null
 // var boundary: Rep[MeshSet[Vertex]] = null

  def main(): Unit = {
/*val builder = new MeshBuilder
builder.addCell(Vertex(0), Vertex(1), Vertex(2), Vertex(3))
builder.setPosition(Vertex(0), 0., 0., 0.)
builder.setPosition(Vertex(1), 1., 0., 0.)
builder.setPosition(Vertex(2), 0., 1., 0.)
builder.setPosition(Vertex(3), 0., 0., 1.)
 builder.setBoundarySet("boundary", Vertex(2))
 builder.setBoundarySet("boundary", Vertex(1))
val mesh1 = builder.build
    boundary = BoundarySet[Vertex]("boundary", mesh1)
*/

    val mesh1 = CubeMesh(3., 1.)
    vF = FieldWithConst[Vertex, Int](0, mesh1)
    for (v <- mesh1.boundary) {
	vF(v) = 255
    }
    OutputMesh(mesh1, vF, vF, vF)

  }
}
