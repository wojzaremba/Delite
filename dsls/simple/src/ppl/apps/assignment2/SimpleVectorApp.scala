package ppl.apps.assignment2

import ppl.dsl.assignment2.{SimpleVectorApplicationRunner, SimpleVectorApplication}

object SimpleVectorAppRunner extends SimpleVectorApplicationRunner with SimpleVectorApp

trait SimpleVectorApp extends SimpleVectorApplication {

  abstract class Complex extends Struct
  def Complex(real: Rep[Double], imaginary: Rep[Double]) = struct[Complex]("re"->real, "imag"->imaginary)
  def infix_re(c: Rep[Complex]) = field[Double](c, "re")
  def infix_imag(c: Rep[Complex]) = field[Double](c, "imag")

  def main() {

    //val x = Vector[Int](10)
    //x.map(e => e + 1)

    val myComplex = Complex(0,0) //FIXME: Array[T] can be allocated before T, and then we don't know that T is a Struct at Array creation

    //val v1 = Vector[Complex](10)
    //v1(0) = Complex(0,0)
    //v1(1) = Complex(1,-1)
    //v1(0).re = -1
    //v1(1).imag = -2
    //println(v1(1))
    //println(v1(0))
    //println(v1.length)

    val v1 = Vector[Complex](10).map(c => Complex(0, 0))
    val v2 = v1.map(c => Complex(c.re + 1, c.imag - 1))
    val v3 = v2.filter
    v3.pprint


    //val x = Vector[Int](10) + 1
    //val y = Vector[Int](10) + 2

    //val z = x + y
    //z.pprint

    /*val x = Vector[Int](10)
    x.update(0,0)
    x.update(1,1)
    val zero = x.apply(0)
    x.update(0,10)
    val ten = x.apply(0)
    x.update(0,20)
    val one = x.apply(1)

    println(zero)
    println(ten)
    println(one)*/

    //val f = z.filter
    //f.pprint
    
    //val res = z.sum
    //println(res)
  }
}
