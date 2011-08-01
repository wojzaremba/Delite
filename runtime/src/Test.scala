import javax.tools.ToolProvider

/*
* Pervasive Parallelism Laboratory (PPL)
* Stanford University
*
*/
 
/**
 * @author Kevin J. Brown
 */
 
object Test {

  def main(args: Array[String]) {
    val compiler = ToolProvider.getSystemJavaCompiler
    val success = compiler.getTask(null, null, null, null, null, null).call()
    println(success)
  }

  private def set(x: Int) {
    //
  }
}
