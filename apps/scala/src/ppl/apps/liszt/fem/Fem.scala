package ppl.apps.liszt.dynamicMesh

import ppl.apps.liszt.lib._
import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object FemRunner extends DeLisztApplicationRunner with Fem


trait Fem extends DeLisztApplication with Libs {
  
  var vF: Rep[Field[Vertex, Int]] = null
  var m : ExtendedMesh = null

  def f(x : Rep[Double], y: Rep[Double], z : Rep[Double]) : Tuple3[Rep[Double], Rep[Double], Rep[Double]] =
    (100.*x*x*x, 200.*y*y*y, 100.*sin(z))

/*  def deltaF(x : Double, y: Double, z : Double) =
    2 + 2*3*y - sin(z)
*/

  def init() {
    m = CubeMesh(1., 0.1)
  }

  def main(): Unit = {
    init()
    OutputMesh(m, f _)

//for (c <- cells(m)) println(ID(c) + "  " + c.det)


  //  for (c <- cells(m)) println(detF(c))
  //  vF = FieldWithConst[Vertex, Int](0, m)
    /*for (v <- mesh1.boundary) {
	vF(v) = 255
    }*/

  }
}
