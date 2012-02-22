package ppl.tests.scalatest.dsl.deliszt

import ppl.dsl.deliszt._
import ppl.tests.scalatest._

object BoolGlobalsRunner extends DeliteTestRunner with DeLisztApplicationRunner with BoolGlobals

trait BoolGlobals extends DeliteTestModule with DeLisztApplication {
	var foo = false
	def main() {
		for(c <- cells(mesh)) {
			foo = foo || true
		}
		collect(foo)
    mkReport
	}
}