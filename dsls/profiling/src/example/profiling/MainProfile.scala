package example.profiling


import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import ppl.delite.framework._
import ppl.delite.framework.codegen._
import ppl.delite.framework.ops._
import codegen.delite.overrides._
import codegen.scala.TargetScala
import java.io.File
import example.profiling.datastruct.scala.ProfileArray
                   

trait ProfileApplication extends Profile with ProfileLift {
  var args: Rep[Array[String]]
  def main(): Unit
}


/* Application packages */
trait ProfileApplicationRunner extends ProfileApplication 
with DeliteApplication with ProfileExp


trait ProfileLift extends LiftScala { // allow apps to use all of Scala
this: Profile =>
}


/* IR packages */
trait Profile extends ScalaOpsPkg with ProfileOps with ProfileArrayOps


trait ProfileExp extends Profile with ScalaOpsPkgExp with ProfileOpsExp
with ProfileArrayOpsExp with DeliteOpsExp with VariantsOpsExp 
with DeliteAllOverridesExp {
this: DeliteApplication with ProfileApplication with ProfileExp =>

  def getCodeGenPkg(t: Target{val IR: ProfileExp.this.type}): GenericFatCodegen{val IR: ProfileExp.this.type} = {
    t match {
      case _:TargetScala => new ProfileCodeGenScala {
        val IR: ProfileExp.this.type = ProfileExp.this
      }
      case _ => throw new IllegalArgumentException("unsupported target: " + t)
    }
  }
}


/* Code generator packages */
trait ProfileCodeGenBase extends GenericFatCodegen with codegen.Utils {
  val IR: DeliteApplication with ProfileExp
  override def initialDefs = IR.deliteGenerator.availableDefs
                                        
  def dsmap(s: String) = s.replaceAll("example.profiling.datastruct", "generated")
  
  override def remap[A](m: Manifest[A]): String = dsmap(super.remap(m))
  
  override def emitDataStructures(path: String) {
    val s = File.separator
    val dsRoot = Config.homeDir + s+"dsls"+s+"profiling"+s+"src"+s+
                 "example"+s+"profiling"+s+"datastruct"+s + this.toString

    copyDataStructures(dsRoot, path, dsmap)
  }
}


trait ProfileCodeGenScala extends ProfileCodeGenBase with ScalaCodeGenPkg 
with ScalaGenDeliteOps with ScalaGenProfileOps with ScalaGenProfileArrayOps
with ScalaGenVariantsOps with ScalaGenDeliteCollectionOps 
with DeliteScalaGenAllOverrides {
      
  val IR: DeliteApplication with ProfileExp

}


