package ppl.apps.liszt.lib

import ppl.dsl.deliszt._

/**
 * Implementation of class for dynamic creation of rectangular mesh
 * User: woj.zaremba
 * Date: 12/30/11
 * Time: 10:52 AM
 */

trait SimpleMesh extends DeLisztApplication {

  implicit def toMesh(e : ExtendedMesh): Rep[Mesh] = e.mesh

  case class ExtendedMesh(mesh : Rep[Mesh], boundary: Rep[MeshSet[Vertex]])

  object SquareMesh {
    def apply(side: Double, step: Double): Rep[Mesh] = RectangularMesh(side, side, step)
  }

  object RectangularMesh {
    def apply(x: Double, y: Double, step: Double): ExtendedMesh = {
      val xs = (x / step).toInt + 1
      val ys = (y / step).toInt + 1
      val builder = new MeshBuilder
      var i: Int = 0
      var j: Int = 0
      val one = 1
      while (j < ys) {
        i = 0
        while (i < xs) {
          val v = Vertex(i + j * xs)
          //XXX : wz : inequalities due to problem with ==
          if ((i < 1) || (j < 1) || (i > xs-2) || (j > ys - 2))
            builder.setBoundarySet("boundary", v)
          builder.setVertexPosition(v, i * step, j * step, 0.)
          if ((i < (xs - 1)) && (j < (ys - 1))) {
            val a = Vertex(i + j * xs)
            val b = Vertex(1 + i + j * xs)
            val c = Vertex(i + (j + 1) * xs)
            val d = Vertex(1 + i + (j + 1) * xs)

            builder.addFace(a, b, d)
            builder.addFace(a, d, c)
          }
          i += 1
        }
        j += 1
      }
      val mesh = builder.build()
      val boundary = BoundarySet[Vertex]("boundary", mesh)
      ExtendedMesh(mesh, boundary)
    }
  }
  
  object CubeMesh {
    def apply(side: Double, step: Double): Rep[Mesh] = CuboidMesh(side, side, side, step)
  }

  object CuboidMesh {
    def apply(x: Double, y: Double, z: Double, step: Double): ExtendedMesh = {
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
            val v = Vertex(i + j * xs + k * xs * ys)
            builder.setVertexPosition(v, i * step, j * step, k * step)
            if ((i < 1) || (j < 1) || (k < 1) || (i > xs-2) || (j > ys - 2) || (k > zs - 2))
              builder.setBoundarySet("boundary", v)
            if (i < (xs - 1) && j < (ys - 1) && k < (zs - 1)) {
              val a0 = Vertex(i + j * xs + k * xs * ys)
              val b0 = Vertex(1 + i + j * xs + k * xs * ys)
              val c0 = Vertex(i + (j + 1) * xs + k * xs * ys)
              val d0 = Vertex(1 + i + (j + 1) * xs + k * xs * ys)

              val a1 = Vertex(i + j * xs + (k + 1) * xs * ys)
              val b1 = Vertex(1 + i + j * xs + (k + 1) * xs * ys)
              val c1 = Vertex(i + (j + 1) * xs + (k + 1) * xs * ys)
              val d1 = Vertex(1 + i + (j + 1) * xs + (k + 1) * xs * ys)

              builder.addCell(a0, b0, d0, b1)
              builder.addCell(a0, c0, d0, b1)
              builder.addCell(c0, d0, d1, b1)

              builder.addCell(d1, c1, a1, c0)
              builder.addCell(d1, b1, a1, c0)
              builder.addCell(b1, a1, a0, c0)
            }
            i += 1
          }
          j += 1
        }
        k += 1
      }
      val mesh = builder.build()
      val boundary = BoundarySet[Vertex]("boundary", mesh)
      ExtendedMesh(mesh, boundary)
    }
  }

}
