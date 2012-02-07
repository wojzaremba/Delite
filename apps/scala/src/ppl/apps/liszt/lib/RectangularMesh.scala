package ppl.apps.liszt.lib

import ppl.dsl.deliszt._

/**
 * Implementation of class for dynamic creation of rectangular mesh
 * User: woj.zaremba
 * Date: 12/30/11
 * Time: 10:52 AM
 */

trait SimpleMesh {
  this: Libs =>

  object SquareMesh {
    def apply(side: Float, step: Float): ExtendedMesh = RectangularMesh(side, side, step)
  }

  object RectangularMesh {
    def apply(x: Float, y: Float, step: Float): ExtendedMesh = {
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
          if ((i < 1) || (j < 1) || (i > xs-2) || (j > ys - 2))
            builder.setBoundarySet("boundary", v)
          else
            builder.setBoundarySet("inside", v)
          builder.setPosition(v, i * step, j * step, 0.f)
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
      val inside = BoundarySet[Vertex]("inside", mesh)
      ExtendedMesh(mesh, boundary, inside)
    }
  }
  
  object CubeMesh {
    def apply(side: Float, step: Float): ExtendedMesh = CuboidMesh(side, side, side, step)
  }

  object CuboidMesh {
    def apply(x: Float, y: Float, z: Float, step: Float): ExtendedMesh = {
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
		val id = i + j * xs + k * xs * ys
            val v = Vertex(i + j * xs + k * xs * ys)
            builder.setPosition(v, i * step, j * step, k * step)
            if ((i < 1) || (j < 1) || (k < 1) || (i > xs-2) || (j > ys - 2) || (k > zs - 2))
              builder.setBoundarySet("boundary", v)
            else
              builder.setBoundarySet("inside", v)
            if (i < (xs - 1) && j < (ys - 1) && k < (zs - 1)) {
              val a0 = Vertex(i + j * xs + k * xs * ys)
              val b0 = Vertex(1 + i + j * xs + k * xs * ys)
              val c0 = Vertex(i + (j + 1) * xs + k * xs * ys)
              val d0 = Vertex(1 + i + (j + 1) * xs + k * xs * ys)

              val a1 = Vertex(i + j * xs + (k + 1) * xs * ys)
              val b1 = Vertex(1 + i + j * xs + (k + 1) * xs * ys)
              val c1 = Vertex(i + (j + 1) * xs + (k + 1) * xs * ys)
              val d1 = Vertex(1 + i + (j + 1) * xs + (k + 1) * xs * ys)

              builder.addCell(a0, b0, d1, c0)
              builder.addCell(b0, d0, d1, c0)
              builder.addCell(a0, b0, b1, d1)
              
              builder.addCell(d1, c1, b1, a0)
              builder.addCell(c1, a1, b1, a0)
              builder.addCell(d1, c1, a0, c0)
            }
            i += 1
          }
          j += 1
        }
        k += 1
      }
      val mesh = builder.build()
      val boundary = BoundarySet[Vertex]("boundary", mesh)
      val inside = BoundarySet[Vertex]("inside", mesh)
      ExtendedMesh(mesh, boundary, inside)
    }
  }

  object TetrahedronMesh {
    def apply(): ExtendedMesh = {
      val builder = new MeshBuilder
      builder.addCell(Vertex(0), Vertex(1), Vertex(2), Vertex(3))
      builder.setPosition(Vertex(0), 0.f, 0.f, 0.f)
      builder.setPosition(Vertex(1), 1.f, 0.f, 0.f)
      builder.setPosition(Vertex(2), 0.f, 1.f, 0.f)
      builder.setPosition(Vertex(3), 0.f, 0.f, 1.f)
      builder.setBoundarySet("boundary", Vertex(0))
      val mesh = builder.build()
      val boundary = BoundarySet[Vertex]("boundary", mesh)
      ExtendedMesh(mesh, boundary)
    }
  }



}



