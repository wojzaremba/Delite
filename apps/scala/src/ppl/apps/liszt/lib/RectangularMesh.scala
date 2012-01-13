package ppl.apps.liszt.lib

import ppl.dsl.deliszt._

/**
 * Implementation of class for dynamic creation of rectangular mesh
 * User: woj.zaremba
 * Date: 12/30/11
 * Time: 10:52 AM
 */

trait SimpleMesh extends DeLisztApplication {

  object SquareMesh {
    def apply(side: Double, step: Double): Rep[Mesh] = RectangularMesh(side, side, step)
  }

  object RectangularMesh {
    def apply(x: Double, y: Double, step: Double): Rep[Mesh] = {
      val xs = (x / step).toInt + 1
      val ys = (y / step).toInt + 1
      val builder = new MeshBuilder(xs * ys, 3 * (xs - 1) * (ys - 1) + xs + ys - 2, 2 * (xs - 1) * (ys - 1), 2)
      var i: Int = 0
      var j: Int = 0
      while (j < ys) {
        i = 0
        while (i < xs) {
          builder.setVertexPosition(i + j * xs, i * step, j * step, 0d)

          if (i < (xs - 1) && j < (ys - 1)) {
            val a = builder.addEdgeByVertex(i + j * xs, 1 + i + j * xs)
            val b = builder.addEdgeByVertex(1 + i + j * xs, 1 + i + (j + 1) * xs)
            val c = builder.addEdgeByVertex(1 + i + (j + 1) * xs, i + j * xs)
            builder.addFaceByEdge(a, b, c)

            val d = builder.addEdgeByVertex(i + j * xs, i + (j + 1) * xs)
            val e = builder.addEdgeByVertex(i + (j + 1) * xs, 1 + i + (j + 1) * xs)
            builder.addFaceByEdge(d, e, c)
          }
          i += 1
        }
        j += 1
      }
      builder.build()
    }
  }

}
