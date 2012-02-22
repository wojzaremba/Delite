package ppl.tests.scalatest.dsl.deliszt

import ppl.dsl.deliszt._
import ppl.tests.scalatest._

object EmptyRunner extends DeliteTestRunner with DeLisztApplicationRunner with Empty

trait Empty extends DeliteTestModule with DeLisztApplication {
 def main(): Unit = {
   collect(true)
   mkReport
 }
}