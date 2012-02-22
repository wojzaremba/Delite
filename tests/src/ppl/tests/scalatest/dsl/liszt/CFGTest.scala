package ppl.tests.scalatest.dsl.deliszt

import ppl.dsl.deliszt._
import ppl.tests.scalatest._

object CFGTestRunner extends DeliteTestRunner with DeLisztApplicationRunner with CFGTest

trait CFGTest extends DeliteTestModule with DeLisztApplication {
  def testfn(): Int = {
    val a = 5
    while (a < 5) {
      collect(false)
    }
    if (a < 5) {
      if (a < 3) {
        return a
      }
    } else {
      return a
    }
    return a
  }

  def main(): Unit = {
    collect(testfn() == 5)
    mkReport
  }
}

