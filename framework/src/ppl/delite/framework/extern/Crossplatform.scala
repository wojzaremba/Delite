package ppl.delite.framework.extern

trait Crossplatform {
  var os : String = null // todo. make this an enum or a case class
  if (System.getProperty("os.name").startsWith("Windows")) os = "windows"
  else if (System.getProperty("os.name").startsWith("Linux")) os = "linux"
  else if (System.getProperty("os.name").startsWith("Mac OS X")) os = "mac"
  if (os == null) sys.error("operating system %s is not supported".format(System.getProperty("os.name")))

  val windows = os == "windows"
  val linux = os == "linux"
  val mac = os == "mac"
  val unix = !windows

  val shellScriptHeader = if (unix) "#!/usr/bin/env bash" else ""
  val extShellScript = if (unix) ".sh" else ".bat"
  val extSharedLib = Map("windows" -> "dll", "linux" -> "so", "mac" -> "dylib")(os)

  class ShellCommand(command: String, args: List[String]) {
    def this(command:String, args: String*) = this(command, args.toList)

    def wrap(arg: String): String = {
      var s: String = arg
      
      if (windows) {
        s = s.replace("\\\"", "\0")
        s = s.replace("\"", "\\\"")
        s = s.replace("\0", "\\\\\\\"")
        if (s.endsWith("\\")) s += "\\"
        "\"%s\"".format(s)
      } else {
        s = s.replace("\\\'", "\0")
        s = s.replace("\'", "\\\'")
        s = s.replace("\0", "\\\\\\\'")
        if (s.endsWith("\\")) s += "\\"
        "\'%s\'".format(s)
      }
    }

    override def toString() = {
      "%s %s".format(command, args.map({arg => wrap(arg)}).mkString(" "))
    }
  }
}
