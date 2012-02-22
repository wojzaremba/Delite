package ppl.tests.scalatest.dsl.deliszt

import ppl.dsl.deliszt._
import ppl.tests.scalatest._

object DoubleArithRunner extends DeliteTestRunner with DeLisztApplicationRunner with DoubleArith

trait DoubleArith extends DeliteTestModule with DeLisztApplication {

  def main() {
    /*var a = 1.f
    var b: Double = a.ToDouble
    b += 1
    a += b.toFloat
    collect(a == 3)
    collect(b == 2)
    mkReport*/
  }
}