package ppl.dsl.deliszt.datastruct.scala

import util.parsing.json.JSON
import io.Source
import java.io.File

//woj.zaremba : Not best place for this class, however it is necessary during runtime

object LogSettings {
  val log_rules: Map[String, Set[String] => Set[String]] =
    Map(
      "none" -> {
        (x: Set[String]) =>
          Set.empty[String]
      },
      "default" -> {
        (x: Set[String]) =>
          x ++ List("MeshBuilder") ++ List("MeshLoader")
      }
    )
  var enabled_logs: Set[String] = Set.empty[String]
  val rules = JSON.parseFull(Source.fromFile(new File("liszt.cfg")).mkString).get.asInstanceOf[Map[String, Any]].getOrElse("log", "default") match {
    case s: String => List(s)
    case rs: List[_] => rs.map(_.asInstanceOf[String])
  }
  setLevel(rules)
  println("log: log enabled for %s".format(enabled_logs))


  def setLevel(rules: List[String]) {
    for (rule <- rules) {
      enabled_logs = log_rules.get(rule) match {
        case Some(x) => x(enabled_logs)
        case None => enabled_logs + rule
      }
    }
  }

  def enabled(s: String): Boolean = {
    println("Checking enabled: %s in %s".format(s, enabled_logs))
    if (rules.contains("all")) {
      true
    } else {
      enabled_logs.contains(s)
    }
  }
}

class Log(unit: String) {
  private lazy val unit_enabled = LogSettings.enabled(unit)

  private def printerr(pred: String, s: Any, args: Seq[Any]) = Console.println(pred.format(s.toString.format(args: _*)))

  def warn(a: Any, args: Any*) = printerr("warning: %s", a, args)

  def log(a: Any, args: Any*) = if (this.logEnabled) printerr("log: %s", a, args)

  def logEnabled: Boolean = unit_enabled

  def error(a: Any, args: Any*): Nothing = {
    printerr("error: %s", a, args)
    throw new Exception(a.toString.format(args: _*))
  }

  //internal error, indicates a bug in the compiler
  def ierror(a: Any, args: Any*): Nothing = {
    printerr("liszt internal compiler error BUGBUG: %s", a, args)
    throw new Exception("error trace")
  }
}
