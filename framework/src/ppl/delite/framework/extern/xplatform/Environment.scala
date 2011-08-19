package ppl.delite.framework.extern.xplatform

import scala.util.matching._
import scala.util.matching.Regex._

object `package` {
  implicit def string2environmentRichString(s: String) = new EnvironmentRichString(s)
  class EnvironmentRichString(s: String) {
    def expandEnvironmentVars = {
      def expandVar(m: Match) = {
        val k = m.group(1)
        val v = System.getenv(k)
        if (v == null) sys.error("environment variable " + k + " is not set")
        v
      }

      var result = s
      result = """\$(\w+)""".r.replaceAllIn(result, expandVar _)
      result = """\$\{(\w+?)\}""".r.replaceAllIn(result, expandVar _)
      result = """%(\w+?)%""".r.replaceAllIn(result, expandVar _)
      result
    }
  }
}

