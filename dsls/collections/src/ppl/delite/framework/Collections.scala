package ppl.delite.framework.collections



import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import ppl.delite.framework._
import ppl.delite.framework.codegen._
import ppl.delite.framework.ops._
import codegen.delite.overrides._
import codegen.scala.TargetScala
import java.io.File



trait CollectionsOps
extends TraversableOps
with SeqOps
with ArraySeqOps
with MapOps
with HashMapOps
with ArraySeqEmitting
with HashMapEmitting
with HashMultiMapEmitting


trait CollectionsOpsExp
extends TraversableOpsExp
with SeqOpsExp
with ArraySeqOpsExp
with ArraySeqEmitting
with MapOpsExp
with HashMapOpsExp
with HashMapEmitting
with HashMultiMapEmitting


trait ScalaGenCollectionsOps
extends ScalaGenTraversableOps
with ScalaGenSeqOps
with ScalaGenArraySeqOps
with ScalaGenMapOps
with ScalaGenHashMapOps
{
  val IR: CollectionsOpsExp
}



trait CollScalaOpsPkg extends Base
    with ImplicitOps with NumericOps with FractionalOps with OrderingOps with StringOps
    with RangeOps with IOOps with ArrayOps with BooleanOps with PrimitiveOps with MiscOps
    with Equal with IfThenElse with Variables with While with TupleOps with ListOps
    with MathOps with CastingOps with ObjectOps
    with SynchronizedArrayBufferOps


trait CollScalaOpsPkgExp extends CollScalaOpsPkg
    with ImplicitOpsExp with NumericOpsExp with FractionalOpsExp with OrderingOpsExp with StringOpsExp
    with RangeOpsExp with IOOpsExp with ArrayOpsExp with BooleanOpsExp with PrimitiveOpsExp with MiscOpsExp
    with FunctionsExp with EqualExp with IfThenElseExp with VariablesExp with WhileExp with TupleOpsExp with ListOpsExp
    with DSLOpsExp with MathOpsExp with CastingOpsExp with ObjectOpsExp
    with SynchronizedArrayBufferOpsExp


trait CollScalaCodeGenPkg extends ScalaGenImplicitOps with ScalaGenNumericOps with ScalaGenFractionalOps with ScalaGenOrderingOps
    with ScalaGenStringOps with ScalaGenRangeOps with ScalaGenIOOps with ScalaGenArrayOps with ScalaGenBooleanOps
    with ScalaGenPrimitiveOps with ScalaGenMiscOps with ScalaGenFunctions with ScalaGenEqual with ScalaGenIfThenElse
    with ScalaGenVariables with ScalaGenWhile with ScalaGenTupleOps with ScalaGenListOps
    with ScalaGenDSLOps with ScalaGenMathOps with ScalaGenCastingOps
    with ScalaGenObjectOps
    with ScalaGenSynchronizedArrayBufferOps
{ val IR: CollScalaOpsPkgExp  }



trait Collections extends CollScalaOpsPkg with CollectionsOps with TupleOps


trait CollectionsExp
extends Collections
with CollScalaOpsPkgExp
with CollectionsOpsExp
with TupleOpsExp
with DeliteOpsExp
with VariantsOpsExp
with DeliteAllOverridesExp {
_: DeliteApplication with CollectionsApplication with CollectionsExp =>
  
  def getCodeGenPkg(t: Target { val IR: CollectionsExp.this.type }): GenericFatCodegen { val IR: CollectionsExp.this.type } = {
    t match {
      case _: TargetScala => new CollectionsCodeGenScala {
        val IR: CollectionsExp.this.type = CollectionsExp.this
      }
      case _ => throw new IllegalArgumentException("unsupported target")
    }
  }
  
}


trait CollectionsCodeGenBase extends GenericFatCodegen with codegen.Utils {
  val IR: DeliteApplication with CollectionsExp
  
  override def initialDefs = IR.deliteGenerator.availableDefs
  
  def dsmap(s: String) = s.replaceAll("ppl.delite.framework.datastruct.scala", "generated.scala")
  
  override def remap[A](m: Manifest[A]) = dsmap(super.remap(m))
  
  override def emitDataStructures(path: String) {
    val sep = File.separator
    val relpath = "dsls collections src ppl delite framework collections datastruct".split(" ")
    val dsRoot = Seq(Config.homeDir) ++ relpath ++ Seq(this.toString)
    copyDataStructures(dsRoot.mkString(sep), path, dsmap)
  }
}


trait CollectionsCodeGenScala
extends CollectionsCodeGenBase
with CollScalaCodeGenPkg
with ScalaGenDeliteOps
with ScalaGenCollectionsOps
with ScalaGenVariantsOps
with ScalaGenDeliteCollectionOps 
with DeliteScalaGenAllOverrides {
  val IR: DeliteApplication with CollectionsExp with DeliteAllOverridesExp
}


trait CollectionsLift extends LiftAll with LiftVariables with LiftEquals {
_: Collections =>
}


trait CollectionsApplication extends Collections with CollectionsLift


trait CollectionsApplicationRunner extends CollectionsApplication with DeliteApplication with CollectionsExp
