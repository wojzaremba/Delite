package ppl.tests.scalatest.dsl.deliszt

import ppl.dsl.deliszt._
import ppl.tests.scalatest._

object BoolFieldsRunner extends DeliteTestRunner with DeLisztApplicationRunner with BoolFields

trait BoolFields extends DeliteTestModule with DeLisztApplication {
  def main(): Unit = {
    val iField = FieldWithConst[Cell, Boolean](false)
    for (c <- cells(mesh)) {
      iField(c) = true
    }
    for (c <- cells(mesh)) {
      collect(iField(c))
    }
    mkReport
  }
}