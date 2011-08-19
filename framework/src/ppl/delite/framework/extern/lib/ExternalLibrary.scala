package ppl.delite.framework.extern.lib

import ppl.delite.framework._
import ppl.delite.framework.extern.xplatform._
import java.io._
import scala.xml._

trait ExternalLibraryCompiler {
  val path: String // machine-dependent path to compiler executable, sourced or calculated from XML configuration by trait implementors
  val init: String = "" // machine-dependent environment initializer, sourced or calculated from XML configuration by trait implementors
  val args: List[String] // machine-dependent compilation args, sourced or calculated from XML configuration by trait implementors
  val output: List[String] // templates of compiler parameters that allows us to specify the destination (e.g. List("-o", "%s") for Unix or List("/Fe:%s") for Windows)
}

trait ExternalLibrary extends Crossplatform {
  //val target: Target
  val libName: String
  lazy val name = "lib" + libName // generated scala interface for this library will use this as its object name 
  val ext: String // native file extension (can this ever be anything besides .c or .cpp??)
  val header: String = "" // an optional header to be included at the top of every generated call for this lib
  val compiler: ExternalLibraryCompiler 

  lazy val config = loadConfig(configFile)
  val configFile: String // name of file, will always be searched for inside extern/src/ppl/delite/extern/lib/config
  def loadConfig(f: String) = {
    // parse XML, return configuration
    val configFile = new File(Config.homeDir, "/framework/src/ppl/delite/framework/extern/lib/config/".replace("/", File.separator) + f)
    if (!configFile.exists) throw new FileNotFoundException("could not load library configuration: " + configFile)    
    XML.loadFile(configFile)
  }
  
  def compile(src: String, destDir: String) {
    val srcFile = new File(src)
    if (!srcFile.exists) throw new FileNotFoundException("source file does not exist: " + src)

    val script = File.createTempFile(name + "-compiler", extShellScript)
    val stream = new PrintWriter(script)
    stream.println(shellScriptHeader)

    val buildPath = new File(Config.buildDir + File.separator + "scala" + File.separator + "kernels" + File.separator)
    stream.println(new ShellCommand("cd", buildPath.getAbsolutePath))

    val outputFile = new File(destDir + File.separator + name + extSharedLib)
    val outputArgs = compiler.output.map({s => s.format(outputFile.getAbsolutePath)})
    val compilerArgs: List[String] = compiler.args ++ outputArgs :+ srcFile.getAbsolutePath
    // println("--external compiler args: " + ((compiler.path +: compilerArgs) mkString ","))
    stream.println(compiler.init)
    stream.println(new ShellCommand(compiler.path, compilerArgs))

    stream.close
    script.setExecutable(true)
    val process = Runtime.getRuntime().exec(script.getAbsolutePath, null, buildPath)
    process.waitFor
    checkError(process)
  }

  private def checkError(process: Process) {
    var errors = io.Source.fromInputStream(process.getErrorStream).getLines.toList

    // unfortunately Mac linker produces warnings even if we provide "-w" option to the compiler
    // i guess, that's because icc calls libtool and libtool calls whatever it wishes, including ld
    // however, libtool doesn't seem to expose an option to disable warnings, so it just swallows our "-w"
    // that's why here we're manually filtering linker warnings
    if (mac) errors = errors filter { line => !line.startsWith("ld: warning:") }

    if (errors.length != 0) {
      println(errors mkString System.getProperty("line.separator"))
      println()
      sys.error("%s compilation failed".format(name))
    }
  }
}
