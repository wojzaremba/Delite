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
      val builder = new MeshBuilder
      var i: Int = 0
      var j: Int = 0
      while (j < ys) {
        i = 0
        while (i < xs) {
          builder.setVertexPosition(i + j * xs, i * step, j * step, 0.)

          if (i < (xs - 1) && j < (ys - 1)) {
            val a = Vertex(i + j * xs)
            val b = Vertex(1 + i + j * xs)
            val c = Vertex(i + (j + 1) * xs)
            val d = Vertex(1 + i + (j + 1) * xs)

            builder.addFaceByVertex(a, b, d)
            builder.addFaceByVertex(a, d, c)
          }
          i += 1
        }
        j += 1
      }
      builder.build()
    }
  }
  
  object CubeMesh {
    def apply(side: Double, step: Double): Rep[Mesh] = CuboidMesh(side, side, side, step)
  }

  object CuboidMesh {
    def apply(x: Double, y: Double, z: Double, step: Double): Rep[Mesh] = {
      val xs = (x / step).toInt + 1
      val ys = (y / step).toInt + 1
      val zs = (z / step).toInt + 1
      val builder = new MeshBuilder
      var i: Int = 0
      var j: Int = 0
      var k: Int = 0
      while (k < zs) {
        j = 0
        while (j < ys) {
          i = 0
          while (i < xs) {
            builder.setVertexPosition(i + j * xs + k * xs * ys, i * step, j * step, k * step)

            if (i < (xs - 1) && j < (ys - 1) && k < (zs - 1)) {
              val a0 = Vertex(i + j * xs + k * xs * ys)
              val b0 = Vertex(1 + i + j * xs + k * xs * ys)
              val c0 = Vertex(i + (j + 1) * xs + k * xs * ys)
              val d0 = Vertex(1 + i + (j + 1) * xs + k * xs * ys)

              val a1 = Vertex(i + j * xs + (k + 1) * xs * ys)
              val b1 = Vertex(1 + i + j * xs + (k + 1) * xs * ys)
              val c1 = Vertex(i + (j + 1) * xs + (k + 1) * xs * ys)
              val d1 = Vertex(1 + i + (j + 1) * xs + (k + 1) * xs * ys)

              builder.addCellByVertex(a0, b0, d0, b1)
              builder.addCellByVertex(a0, c0, d0, b1)
              builder.addCellByVertex(c0, d0, d1, b1)

              builder.addCellByVertex(d1, c1, a1, c0)
              builder.addCellByVertex(d1, b1, a1, c0)
              builder.addCellByVertex(b1, a1, a0, c0)
            }
            i += 1
          }
          j += 1
        }
        k += 1
      }
      builder.build()
    }
  }

}
