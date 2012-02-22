package ppl.tests.scalatest.dsl.deliszt

import ppl.dsl.deliszt._
import ppl.tests.scalatest._

object ArithRunner extends DeliteTestRunner with DeLisztApplicationRunner with Arith

trait Arith extends DeliteTestModule with DeLisztApplication {
  def main(): Unit = {
    val a = 1 + 1
    val b = 1. + 1.
    val c = 1 - 1
    val d = 1. - 1.
    val e = 2 * 3
    val f = 2. * 3.
    val g = 3. / 4.
    val h = 6 % 8
    val i = 1 == 2
    val j = 1 < 2
    val k = 1 > 2
    val l = 2 <= 2
    val m = 2 >= 3
    val n = 2 min 3
    val o = 2 max 3
    val p = 2 << 3
    val q = 8 >> 1
    val r = 2 | 1
    val s = 2 & 1
    val t = 2 ^ 1
    val u = false || true
    val v = false && true
    collect(a == 2)
    collect(b == 2)
    collect(c == 0)
    collect(d == 0)
    collect(e == 6)
    collect(f == 6)
    collect(g == 0.75)
    collect(h == 6)
    collect(i == false)
    collect(j == true)
    collect(k == false)
    collect(l == true)
    collect(m == false)
    collect(n == 2)
    collect(o == 3)
    collect(p == 16)
    collect(q == 4)
    collect(r == 3)
    collect(s == 0)
    collect(t == 3)
    collect(u == true)
    collect(v == false)
    mkReport
  }
}

class ArithSuite extends DeliteSuite {
  def testArith() { compileAndTest(ArithRunner) }
}