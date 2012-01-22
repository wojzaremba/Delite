package ppl.dsl.deliszt.datastruct.scala

import util.parsing.json.JSON
import io.Source
import java.io.File

object LogSettings {
  def log_rules(key : String) : (String => Boolean) = {
    key match {
      case "none" => {
        _: String => false
      }
      case "all" => {
        _: String => true
      }
      case _ => {
        x: String => List("MeshBuilder", "MeshLoader").contains(x)
      }
    }
  }
  var enabled_logs: Set[String] = Set.empty[String]

  val rules : List[String] = if ((new File("liszt.cfg")).exists()) {
    JSON.parseFull(Source.fromFile(new File("liszt.cfg")).mkString).get.asInstanceOf[Map[String, Any]].getOrElse("log", "default") match {
      case s: String => List(s)
      case rs: List[_] => rs.map(_.asInstanceOf[String])
    }
  } else {
    List()
  }.asInstanceOf[List[String]] ++
    (System.getenv("LOG") match {
      case null => List()
      case label => List(label.toString)
    }).asInstanceOf[List[String]]

  def enabled(s: String): Boolean = rules.exists((x: String) => log_rules(x)(s))

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
