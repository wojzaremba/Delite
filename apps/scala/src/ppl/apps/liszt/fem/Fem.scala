package ppl.apps.liszt.dynamicMesh

import ppl.apps.liszt.lib._
import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object FemRunner extends DeLisztApplicationRunner with Fem


trait Fem extends DeLisztApplication with Libs {

  var matrixE: Rep[Field[Edge, Float]] = null
  var matrixV: Rep[Field[Vertex, Float]] = null

  var xk: Rep[Field[Vertex, Float]] = null
  var pk: Rep[Field[Vertex, Float]] = null
  var rk: Rep[Field[Vertex, Float]] = null

  var dF: Rep[Field[Vertex, Float]] = null
  val sizex : Float = 10.f
  val sizey : Float = 10.f

  def f(x: Rep[Float], y: Rep[Float], z: Rep[Float]) : Rep[Float] =
    x*(sizex - x) * y * y * (sizey - y) * 10.f

  def deltaF(x: Rep[Float], y: Rep[Float]) : Rep[Float] =
    y * y * (sizey - y) * 20.f + x * (sizey - x) * (6.f * y - 2.f * sizey) * 10.f

  def lamda(v : Rep[Vertex], face : Rep[Face]) : Rep[Vec[_3, Float]] = {
    val set = meshset_filter(vertices(face), {(x : Rep[Vertex]) => ID(x) != ID(v)} )
    val vec = cross((set(1) - set(0)), Vec(0.f, 0.f, 1.f)) / face.det
    if (ID(v) == ID(vertex(face, 1)))
      -vec
    else
      vec
  }

  def integralEdge(edge : Rep[Edge], face: Rep[Face]) = {
    val a = head(edge)
    val b = tail(edge)
    if (m.inside.contains(a) || m.inside.contains(b)) {
      dot(lamda(a, face), lamda(b, face)) / 2.f
    } else 0.f
  }

  def integralVertex(a: Rep[Vertex], face: Rep[Face]) = {
    if (m.inside.contains(a)) {
      dot(lamda(a, face), lamda(a, face)) / 2.f
    } else 0.f
  }

  def printV(mat : Rep[Field[Vertex, Float]]) {
    println("Vertex Vector : ")
    for (v <- vertices(m)) 
      println(ID(v) + " " + mat(v))    
  }

  def printE(mat : Rep[Field[Edge, Float]]) {
    println("Matrix : ")
    for (e <- edges(m))
      println(ID(head(e)) + " " + ID(tail(e)) + " " + mat(e))
  }
  

  def main(): Unit = {
    m = RectangularMesh(sizex, sizey, 1.f)
    pos = FieldWithLabel[Vertex, Vec[_3, Float]]("position", m)
    //m = TetrahedronMesh()

    matrixE = FieldWithConst[Edge, Float](0.f, m)
    matrixV = FieldWithConst[Vertex, Float](0.f, m)
    xk = FieldWithConst[Vertex, Float](0.f, m)
    pk = FieldWithConst[Vertex, Float](0.f, m)
    rk = FieldWithConst[Vertex, Float](0.f, m)
    dF = FieldWithConst[Vertex, Float](0.f, m)
    for (v <- vertices(m)) dF(v) = deltaF(v.x, v.y)

    for (face <- faces(m)) {
      for (v <- vertices(face)) {
        matrixV(v) += integralVertex(v, face)
      }
      for (e <- edges(face))
        matrixE(e) += integralEdge(e, face)
    }
    
    printE(matrixE)
    println()
    printV(matrixV)
    OutputMesh.freeFem(m, "freefem.mesh")

    var result : Rep[Float] = 0.f
   /* for (i <- 1 until 10) {
      result += i
      println("iii " + i)
    }*/
    println("result " + result)
    
  /*
    for (v <- m.inside) {
      rk(v) = dF(v)
      pk(v) = rk(v)
    }



    for (iter <- 0 until 1000) {
      for (v <- m.inside) {
        values2(v) = dF(v)
        for (e <- edges(v)) {
          val e2 = if (ID(e.head) == ID(v)) e.tail else e.head
          values2(v) -= (matrixE(e) * values(e2))
        }
      }
      for (v <- m.inside) {
        values(v) = values2(v) / matrixV(v)
      }
    }
    OutputMesh(m, "values.ply", values)
    OutputMesh(m, "f.ply", f _)*/


  }
}
