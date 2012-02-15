package ppl.dsl.deliszt

import datastruct.{CudaGenDataStruct,CGenDataStruct}
import extern.{DeLisztCudaGenExternal, DeLisztScalaGenExternal}
import java.io._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenerationFailedException}
import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.analysis.TraversalAnalysis
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import ppl.delite.framework.codegen.c.TargetC
import ppl.delite.framework.codegen.delite.overrides.{DeliteCudaGenAllOverrides, DeliteCGenAllOverrides, DeliteScalaGenAllOverrides, DeliteAllOverridesExp}
import ppl.delite.framework.ops._
import scala.util.matching.Regex

import ppl.dsl.optila.{OptiLAApplication}
import ppl.dsl.optila.{OptiLAScalaOpsPkg, OptiLAScalaOpsPkgExp, OptiLA, OptiLAExp, OptiLACompiler, OptiLALift, OptiLAUtilities}
import ppl.dsl.optila.{OptiLAScalaCodeGenPkg, OptiLACudaCodeGenPkg, OptiLAOpenCLCodeGenPkg, OptiLACCodeGenPkg, OptiLACodeGenBase, OptiLACodeGenScala, OptiLACodeGenCuda, OptiLACodeGenOpenCL, OptiLACodeGenC}

import ppl.dsl.deliszt.field._
import ppl.dsl.deliszt.intm._
import ppl.dsl.deliszt.mesh._
import ppl.dsl.deliszt.meshset._

import ppl.dsl.deliszt.analysis.{DeLisztCodeGenAnalysis, LoopColoringOpt, LoopColoringOpsExp, ScalaGenLoopColoringOps}

trait DeLisztApplicationRunner extends DeLisztApplication with DeliteApplication with DeLisztExp {
  override def liftedMain(x: Rep[Array[String]]) = {
    _init(x)
    this.args = x
    val y = main()
    this.args = null
    unit(y)
  }

  //FIXME
  //override val deliteGenerator = new DeliteCodeGenPkg with LoopColoringOpt { val IR: DeLisztApplicationRunner.this.type = DeLisztApplicationRunner.this;
    //                                                                        val generators = DeLisztApplicationRunner.this.generators }
}

trait DeLisztApplication extends OptiLAApplication with DeLiszt with DeLisztLift with DeLisztLibrary

trait DeLisztLibrary {
  this: DeLisztApplication =>
}

trait DeLisztLift extends OptiLALift {
  this: DeLiszt =>
}

trait DeLisztScalaOpsPkg extends OptiLAScalaOpsPkg

trait DeLisztScalaOpsPkgExp extends OptiLAScalaOpsPkgExp with DeLisztScalaOpsPkg

trait DeLisztScalaCodeGenPkg extends OptiLAScalaCodeGenPkg
  { val IR: DeLisztScalaOpsPkgExp  }

trait DeLisztCudaCodeGenPkg extends OptiLACudaCodeGenPkg
  { val IR: DeLisztScalaOpsPkgExp  }

trait DeLisztCCodeGenPkg extends OptiLACCodeGenPkg
  { val IR: DeLisztScalaOpsPkgExp  }

/**
 * This the trait that every DeLiszt application must extend.
 */
trait DeLiszt extends DeLisztScalaOpsPkg with OptiLA with LanguageOps with MeshBuilderOps
  with MeshPrivateOps with MeshSetOps
  with IntMOps
  with SeqOps
  with MathOps
  with FieldOps with OrderingOps {
  this: DeLisztApplication =>
}

// these ops are only available to the compiler (they are restricted from application use)
trait DeLisztCompiler extends OptiLACompiler with DeLiszt with ListOps {  // FieldPrivateOps, MeshPrivateOps
  this: DeLisztApplication with DeLisztExp =>
}

/**
 * These are the corresponding IR nodes for DeLiszt.
 */
trait DeLisztExp extends OptiLAExp with DeLisztCompiler with DeLisztScalaOpsPkgExp with LanguageOpsExpOpt with MeshBuilderOpsExp
  with LanguageImplOpsStandard
  with MeshSetOpsExp
  with MeshPrivateOpsExp
  with OrderingOpsExp
  with MathOpsExp
  with IntMOpsExp
  with LoopColoringOpsExp
  with DeliteOpsExp with VariantsOpsExp with DeliteAllOverridesExp
  with FieldOpsExpOpt with FieldImplOpsStandard {

  this: DeliteApplication with DeLisztApplication with DeLisztExp => // can't be DeLisztApplication right now because code generators depend on stuff inside DeliteApplication (via IR)

  override def getCodeGenPkg(t: Target{val IR: DeLisztExp.this.type}) : GenericFatCodegen{val IR: DeLisztExp.this.type} = {
    t match {
      case _:TargetScala => new DeLisztCodeGenScala{val IR: DeLisztExp.this.type = DeLisztExp.this}
      case _:TargetCuda => new DeLisztCodeGenCuda{val IR: DeLisztExp.this.type = DeLisztExp.this}
      case _:TargetC => new DeLisztCodeGenC{val IR: DeLisztExp.this.type = DeLisztExp.this} 
      case _ => throw new RuntimeException("DeLiszt does not support this target")
    }
  }

  //FIXME
  //override lazy val analyses = scala.collection.immutable.List(new DeLisztCodeGenAnalysis{val IR: DeLisztExp.this.type = DeLisztExp.this})
}


/**
 * DeLiszt code generators
 */
trait DeLisztCodeGenBase extends OptiLACodeGenBase {

  val IR: DeliteApplication with DeLisztExp
  override def initialDefs = IR.deliteGenerator.availableDefs
  
  
  def genSpec2(f: File, outPath: String) = {}

  override def emitDataStructures(path: String) {
    val s = File.separator
    val dsRoot = Config.homeDir + s+"dsls"+s+"deliszt"+s+"src"+s+"ppl"+s+"dsl"+s+"deliszt"+s+"datastruct"+s + this.toString

    val dsDir = new File(dsRoot)
    if (!dsDir.exists) return
    val outDir = new File(path)
    outDir.mkdirs()

    for {
	f <- dsDir.listFiles
	if f.getName()(0) != '.' //don't care about hidden files
        }
    {
      if(f.getName.indexOf(".") > -1) {
        if (specialize contains (f.getName.substring(0, f.getName.indexOf(".")))) {
          genSpec(f, path)
        }
        if (specialize2 contains (f.getName.substring(0, f.getName.indexOf(".")))) {
          genSpec2(f, path)
        }
      }
      val outFile = path + f.getName
      val out = new BufferedWriter(new FileWriter(outFile))
      for (line <- scala.io.Source.fromFile(f).getLines) {
        out.write(dsmap(line) + "\n")
      }
      out.close()
    }
    
    super.emitDataStructures(path)
  }
}

trait DeLisztCodeGenScala extends OptiLACodeGenScala with DeLisztCodeGenBase with DeLisztScalaCodeGenPkg with ScalaGenDeliteOps with ScalaGenLanguageOps
with ScalaGenMeshBuilderOps with ScalaGenVariantsOps with ScalaGenDeliteCollectionOps
  with ScalaGenFieldOps with ScalaGenIntMOps with ScalaGenMeshPrivateOps with ScalaGenMeshSetOps
  with ScalaGenLoopColoringOps /*with LoopColoringOpt*/ // LoopColoringOpt only needed here for debugging (it's mixed into DeLiszt's DeliteCodeGenPkg)
  with DeliteScalaGenAllOverrides with DeLisztScalaGenExternal { //with ScalaGenMLInputReaderOps {
  
  val IR: DeliteApplication with DeLisztExp

  override val specialize = Set("FieldImpl", "LabelFieldImpl", "Vec3Impl", "VecImpl", "VecViewImpl")

  override def genSpec(f: File, dsOut: String) {
    for (s <- List("Double","Int","Float","Long","Boolean")) {
      val outFile = dsOut + s + f.getName
      val out = new BufferedWriter(new FileWriter(outFile))
      for (line <- scala.io.Source.fromFile(f).getLines) {
        out.write(specmap(line, s) + "\n")
      }
      out.close()
    }
  }

  override def genSpec2(f: File, dsOut: String) {
    for (s1 <- List("Double","Int","Float","Long","Boolean")) {
   	  for (s2 <- List("Double","Int","Float","Long","Boolean")) {
        val outFile = dsOut + s1 + s2 + f.getName
        val out = new BufferedWriter(new FileWriter(outFile))
        for (line <- scala.io.Source.fromFile(f).getLines) {
          out.write(specmap2(line, s1, s2) + "\n")
        }
        out.close()
	  }
    }
  }

  
  override def specmap(line: String, t: String) : String = {
    var res = line.replaceAll("class ", "class " + t)
    res = res.replaceAll("object ", "object " + t)
    //in case class A[T] (bla : T) we want to generate IntA ( bla : Int) - not IntA[Int](bla:Int), because then [Int] will mean any type
    res = res.replaceAll("\\[\\s*@specialized\\s*T\\s*:\\s*ClassManifest\\s*\\]", "")
    res = res.replaceAll("\\[\\s*T\\s*:\\s*ClassManifest\\s*\\]", "")    
    res = res.replaceAll("\\[\\s*@specialized\\s*T\\s*:\\s*Manifest\\s*\\]", "")
    res = res.replaceAll("\\[\\s*T\\s*:\\s*Manifest\\s*\\]", "")
    //res = res.replaceAll("\\bT:Manifest\\b", t)
    res = res.replaceAll("\\bT\\b", t)
    //res = res.replaceAll("\\bT: ClassManifest, V: ClassManifest\\b", "\\b" + t + ",V\\b")
    val size = t match {
    	case "Double" => 8
	case _ => 4
    }
    res = res.replaceAll("\\/\\*unsafe.UnsafeAccessor.unsafe.getT\\(this, 16 \\+ n\\*UNSAFE_SIZE\\)\\*\\/ v0", "unsafe.UnsafeAccessor.unsafe.get"+t+"\\(this, 16 \\+ n\\*"+size+"\\)")    
    res = res.replaceAll("\\/\\*unsafe.UnsafeAccessor.unsafe.putT\\(this, 16 \\+ n\\*UNSAFE_SIZE, v\\)\\*\\/", "unsafe.UnsafeAccessor.unsafe.put"+t+"\\(this, 16 \\+ n\\*"+size+", v\\)")
    res = res.replaceAll("\\/\\*unsafe.UnsafeAccessor.unsafe.getT\\(this, 16 \\+ idx\\*UNSAFE_SIZE\\)\\*\\/ v00", "unsafe.UnsafeAccessor.unsafe.get"+t+"\\(this, 16 \\+ idx\\*"+size+"\\)")
    res = res.replaceAll("\\/\\*unsafe.UnsafeAccessor.unsafe.putT\\(this, 16 \\+ idx\\*UNSAFE_SIZE, x\\)\\*\\/","unsafe.UnsafeAccessor.unsafe.put"+t+"\\(this, 16 \\+ idx\\*"+size+", x\\)")  
    parmap(res)
  }

  def specmap2(line: String, t1: String, t2: String) : String = {
    var res = line.replaceAll("object ", "object " + t1 + t2)
    res = res.replaceAll("@specialized T: ClassManifest", t1)
    res = res.replaceAll("@specialized L: ClassManifest", t2)
    res = res.replaceAll("T:Manifest", t1)
    res = res.replaceAll("L:Manifest", t2)
    res = res.replaceAll("\\bT\\b", t1)
    res = res.replaceAll("\\bL\\b", t2)
    parmap(res)
  }
  

  override def remap(s: String) = parmap(s)
  override def remap[A](m: Manifest[A]): String = {
    var res = super.remap(m)
    res = res.replaceAllLiterally("package$", "")
    parmap(res)
  }

  override def parmap(line: String): String = {
    var res = line
    
    val moSub = (m: Regex.Match) => {
      "[" + m.group(1) + "]"
    }    
    
    // Vec, Mat, Field, anything with that final parameter of some value type        
    for(s <- List("Vec", "Mat", "Field")) {
      val expr = ("\\b" + s + "\\[.*,\\s*([^\\s]+)\\s*\\]").r  
      res = expr.replaceAllIn(res, m => s + moSub(m))
    }

    
    // MeshSet
    val meshSetExpr = ("MeshSet\\[.+\\]").r  
    res = meshSetExpr.replaceAllIn(res, m => "MeshSet")
      
    // MeshObject types
    for(s <- List("Cell", "Edge", "Face", "Vertex")) {
      val expr = ("(ppl\\.dsl\\.deliszt|generated\\.scala)\\." + s + "\\b").r  
      res = expr.replaceAllIn(res, "Int")
    }
    
    // Replace fields with just flat arrays
    /* val fieldExpr = ("(ppl\\.dsl\\.deliszt|generated\\.scala)\\.Field\\b").r  
    res = fieldExpr.replaceAllIn(res, "Array") */
    
    for(tpe1 <- List("Int","Long","Double","Float","Boolean")) {
      val parSub = (m: Regex.Match) => {
        val rest = (m.group(2) + m.group(4)).replaceAll("""^\s+""", "")
        val fun = if(m.group(1) != null) m.group(1) else ""
        if(rest.length > 0) {
          fun + "[" + rest + "]"
        }
        else {
          fun
        }
      }


      for (s <- specialize) {
        // Object apply, methods, new
        val expr = ("\\b" + s + "(\\.\\w+)?\\[(.*?)(,\\s*)?\\b" + tpe1 + "\\b(.*?)\\]\\(").r  
        res = expr.replaceAllIn(res, m => tpe1 + s + parSub(m) + "(")
      }
      
      val parSub2 = (m: Regex.Match) => {
        val rest = (m.group(1) + m.group(3)).replaceAll("""^\s+""", "")
        if(rest.length > 0) {
          "[" + rest + "]"
        }
        else {
          ""
        }
      }
      
      // Map methods defs (like in the companion)
      val expr = ("\\[(.*?)(,\\s*)?\\b" + tpe1 + "\\s*:\\s*\\w+?\\b(.*?)\\]\\(").r
      res = expr.replaceAllIn(res, m => parSub2(m) + "(")
        
      for(tpe2 <- List("Int","Long","Double","Float","Boolean")) {
        for (s <- specialize2) {
          // should probably parse and trim whitespace, this is fragile
          res = res.replaceAll(s+"\\["+tpe1+","+tpe2+"\\]", tpe1+tpe2+s)
          res = res.replaceAll(s+"\\["+tpe1+", "+tpe2+"\\]", tpe1+tpe2+s)
        }
      }
    }
    
    dsmap(res)
  }

  override def dsmap(line: String) : String = {
    var res = line.replaceAll("ppl.dsl.deliszt.datastruct", "generated")
    res = res.replaceAll("ppl.delite.framework.datastruct", "generated")
    res = res.replaceAll("ppl.dsl.deliszt", "generated.scala")    
    super.dsmap(res)
  }
}

trait DeLisztCodeGenCuda extends OptiLACodeGenCuda with DeLisztCodeGenBase with DeLisztCudaCodeGenPkg with CudaGenDeliteOps with CudaGenLanguageOps
  with CudaGenVariantsOps with CudaGenDeliteCollectionOps
  with CudaGenFieldOps with CudaGenIntMOps with CudaGenMeshPrivateOps with CudaGenMeshSetOps
  with CudaGenDataStruct with DeliteCudaGenAllOverrides with DeLisztCudaGenExternal
{
  val IR: DeliteApplication with DeLisztExp
  import IR._

  def isVecType[A](m: Manifest[A]) = {
    if (m.toString.startsWith("ppl.dsl.deliszt.Vec")) true
    else false
  }

  def isMatType[A](m: Manifest[A]) = {
    if (m.toString.startsWith("ppl.dsl.deliszt.Mat")) true
    else false
  }

  def getVecSize[A](m: Manifest[A]):Int = {
    val startsWith = m.toString.split("\\[")
    startsWith(0) match {
      case "ppl.dsl.deliszt.Succ" => getVecSize(m.typeArguments(0)) + 1
      case "ppl.dsl.deliszt.Zero" => 0
      case _ => throw new GenerationFailedException("CudaGen: Unknown Vec/Mat Type: " + m.toString)
    }
  }

  // Maps the scala type to cuda type
  override def remap[A](m: Manifest[A]) : String = {

    val startsWith = m.toString.split("\\[")
    startsWith(0) match {
      case "ppl.dsl.deliszt.Mesh" => "Mesh"
      case "ppl.dsl.deliszt.Cell" => "Cell"
      case "ppl.dsl.deliszt.Face" => "Face"
      case "ppl.dsl.deliszt.Vertex" => "Vertex"
      case "ppl.dsl.deliszt.Edge" => "Edge"
      case "ppl.dsl.deliszt.MeshObj" => "MeshObj"
      case "ppl.dsl.deliszt.Vec" => "Vec<" + remap(m.typeArguments(1)) + "," + getVecSize(m.typeArguments(0)) + ">" //TODO: Is nested Vec/Mat type supported on Liszt?
      case "ppl.dsl.deliszt.Mat" => "Mat<" + remap(m.typeArguments(2)) + "," + getVecSize(m.typeArguments(0)) + "," + getVecSize(m.typeArguments(1)) + ">" 
      case "ppl.dsl.deliszt.MatRow" => "MatRow<" + remap(m.typeArguments(1)) + "," + getVecSize(m.typeArguments(0)) + ">"
      case "ppl.dsl.deliszt.MatCol" => "MatCol<" + remap(m.typeArguments(1)) + "," + getVecSize(m.typeArguments(0)) + ">"
      case "ppl.dsl.deliszt.VecImpl" => "Vec<" + remap(m.typeArguments(1)) + "," + getVecSize(m.typeArguments(0)) + ">"
      case "ppl.dsl.deliszt.MatImpl" => "Mat<" + remap(m.typeArguments(2)) + "," + getVecSize(m.typeArguments(0)) + "," + getVecSize(m.typeArguments(1)) + ">"
      case "ppl.dsl.deliszt.MeshSet" => "MeshSet<" + remap(m.typeArguments(0)) + ">"
      case "ppl.dsl.deliszt.BoundarySet" => "BoundarySet<" + remap(m.typeArguments(0)) + ">"
      case "ppl.dsl.deliszt.Field" if (isPrimitiveType(m.typeArguments(1))) => "Field<" + remap(m.typeArguments(1)) + ">"
      case "ppl.dsl.deliszt.Field" if (isVecType(m.typeArguments(1))) => "VecField<" + remap(m.typeArguments(1).typeArguments(1)) + "," + getVecSize(m.typeArguments(1).typeArguments(0)) + ">"
      case "ppl.dsl.deliszt.Field" if (isMatType(m.typeArguments(1))) => "MatField<" + remap(m.typeArguments(1).typeArguments(2)) + "," + getVecSize(m.typeArguments(1).typeArguments(0)) + "," + getVecSize(m.typeArguments(1).typeArguments(1)) + ">"
      case "scala.collection.immutable.List" => "CudaArrayList<" + remap(m.typeArguments(0)) + ">"  //TODO: Remove this
      case _ => super.remap(m)
    }
  }

  override def isObjectType[T](m: Manifest[T]) : Boolean = remap(m) match {
    case "Cell" | "Face" | "Vertex" | "Edge" => true
    case "MeshSet<Cell>" | "MeshSet<Face>" | "MeshSet<Edge>" | "MeshSet<Vertex>" => true
    case "BoundarySet<Cell>" | "BoundarySet<Face>" | "BoundarySet<Edge>" | "BoundarySet<Vertex>" => true
    case "Field<int>" | "Field<long>" | "Field<float>" | "Field<bool>" | "Field<double>" => true
    case "Vec<int,3>" | "Vec<long,3>" | "Vec<float,3>" | "Vec<bool,3>" | "Vec<double,3>" => true
    case "Vec<int,4>" | "Vec<long,4>" | "Vec<float,4>" | "Vec<bool,4>" | "Vec<double,4>" => true
    case "Mat<int,3,3>" | "Mat<long,3,3>" | "Mat<float,3,3>" | "Mat<bool,3,3>"| "Mat<double,3,3>" => true
    case "VecField<int,3>" | "VecField<long,3>" | "VecField<float,3>" | "VecField<bool,3>" | "VecField<double,3>" => true
    case "MatField<int,3,3>" | "MatField<long,3,3>" | "MatField<float,3,3>" | "MatField<bool,3,3>" | "MatField<double,3,3>" => true
    case "Mesh" => true
    case "CudaArrayList<Cell>" | "CudaArrayList<Face>" | "CudaArrayList<Edge>" | "CudaArrayList<Vertex>" => true //TODO: Remove this

    case _ => super.isObjectType(m)
  }

  override def copyInputHtoD(sym: Sym[Any]) : String = remap(sym.Type) match {
    case "Cell" | "Face" | "Vertex" | "Edge" => "//copy\n"
    case "MeshSet<Cell>" | "MeshSet<Face>" | "MeshSet<Edge>" | "MeshSet<Vertex>" => MeshSetCopyInputHtoD(sym, sym.Type.typeArguments(0))
    case "BoundarySet<Cell>" | "BoundarySet<Face>" | "BoundarySet<Edge>" | "BoundarySet<Vertex>" => MeshSetCopyInputHtoD(sym, sym.Type.typeArguments(0))
    case "Field<int>" | "Field<long>" | "Field<float>" | "Field<bool>" | "Field<double>" => FieldCopyInputHtoD(sym, sym.Type.typeArguments(1))
    case "Vec<int,3>" | "Vec<long,3>" | "Vec<float,3>" | "Vec<bool,3>" | "Vec<double,3>" => VecCopyInputHtoD(sym, sym.Type.typeArguments(1), 3)
    case "Vec<int,4>" | "Vec<long,4>" | "Vec<float,4>" | "Vec<bool,4>" | "Vec<double,4>" => VecCopyInputHtoD(sym, sym.Type.typeArguments(1), 4)
    case "Mat<int,3,3>" | "Mat<long,3,3>" | "Mat<float,3,3>" | "Mat<bool,3,3>" | "Mat<double,3,3>" => MatCopyInputHtoD(sym, sym.Type.typeArguments(2), 3, 3)
    case "VecField<int,3>" | "VecField<long,3>" | "VecField<float,3>" | "VecField<bool,3>" | "VecField<double,3>" => VecFieldCopyInputHtoD(sym, sym.Type.typeArguments(1).typeArguments(1), 3)
    case "MatField<int,3,3>" | "MatField<long,3,3>" | "MatField<float,3,3>" | "MatField<bool,3,3>" | "MatField<double,3,3>" => MatFieldCopyInputHtoD(sym, sym.Type.typeArguments(1).typeArguments(2), 3, 3)
    case "Mesh" => MeshCopyInputHtoD(sym)
    case "CudaArrayList<Cell>" | "CudaArrayList<Face>" | "CudaArrayList<Edge>" | "CudaArrayList<Vertex>" => "return new CudaArrayList<int>();\n" //TODO: Remove this
    case _ => super.copyInputHtoD(sym)
  }

  override def copyOutputDtoH(sym: Sym[Any]) : String = remap(sym.Type) match {
    case "Cell" | "Face" | "Vertex" | "Edge" => "//copy\n"
    case "MeshSet<Cell>" | "MeshSet<Face>" | "MeshSet<Edge>" | "MeshSet<Vertex>" => MeshSetCopyOutputDtoH(sym, sym.Type.typeArguments(0))
    case "BoundarySet<Cell>" | "BoundarySet<Face>" | "BoundarySet<Edge>" | "BoundarySet<Vertex>" => MeshSetCopyOutputDtoH(sym, sym.Type.typeArguments(0))
    case "Field<int>" | "Field<long>" | "Field<float>" | "Field<bool>" | "Field<double>" => FieldCopyOutputDtoH(sym, sym.Type.typeArguments(1))
    case "Vec<int,3>" | "Vec<long,3>" | "Vec<float,3>" | "Vec<bool,3>" | "Vec<double,3>" => VecCopyOutputDtoH(sym, sym.Type.typeArguments(1))
    case "Vec<int,4>" | "Vec<long,4>" | "Vec<float,4>" | "Vec<bool,4>" | "Vec<double,4>" => VecCopyOutputDtoH(sym, sym.Type.typeArguments(1))
    case "Mat<int,3,3>" | "Mat<long,3,3>" | "Mat<float,3,3>" | "Mat<bool,3,3>" | "Mat<double,3,3>" => MatCopyOutputDtoH(sym, sym.Type.typeArguments(2))
    case "VecField<int,3>" | "VecField<long,3>" | "VecField<float,3>" | "VecField<bool,3>" | "VecField<double,3>" => VecFieldCopyOutputDtoH(sym, sym.Type.typeArguments(1).typeArguments(1), 3)
    case "MatField<int,3>" | "MatField<long,3>" | "MatField<float,3>" | "MatField<bool,3>" | "MatField<double,3>" => MatFieldCopyOutputDtoH(sym, sym.Type.typeArguments(1).typeArguments(2), 3, 3)
    case "Mesh" => MeshCopyOutputDtoH(sym)
    case "CudaArrayList<Cell>" | "CudaArrayList<Face>" | "CudaArrayList<Edge>" | "CudaArrayList<Vertex>" => "//copy\n" //TODO: Remove this
    case _ => super.copyOutputDtoH(sym)
  }

  override def copyMutableInputDtoH(sym: Sym[Any]) : String = remap(sym.Type) match {
    case "Cell" | "Face" | "Vertex" | "Edge" => "//copy\n"
    case "MeshSet<Cell>" | "MeshSet<Face>" | "MeshSet<Edge>" | "MeshSet<Vertex>" => MeshSetCopyMutableInputDtoH(sym, sym.Type.typeArguments(0))
    case "BoundarySet<Cell>" | "BoundarySet<Face>" | "BoundarySet<Edge>" | "BoundarySet<Vertex>" => MeshSetCopyMutableInputDtoH(sym, sym.Type.typeArguments(0))
    case "Field<int>" | "Field<long>" | "Field<float>" | "Field<bool>" | "Field<double>" => FieldCopyMutableInputDtoH(sym, sym.Type.typeArguments(1))
    case "Vec<int,3>" | "Vec<long,3>" | "Vec<float,3>" | "Vec<bool,3>" | "Vec<double,3>" => VecCopyMutableInputDtoH(sym, sym.Type.typeArguments(1))
    case "Vec<int,4>" | "Vec<long,4>" | "Vec<float,4>" | "Vec<bool,4>" | "Vec<double,4>" => VecCopyMutableInputDtoH(sym, sym.Type.typeArguments(1))
    case "Mat<int,3,3>" | "Mat<long,3,3>" | "Mat<float,3,3>" | "Mat<bool,3,3>" | "Mat<double,3,3>" => MatCopyMutableInputDtoH(sym, sym.Type.typeArguments(2))
    case "VecField<int,3>" | "VecField<long,3>" | "VecField<float,3>" | "VecField<bool,3>" | "VecField<double,3>" => VecFieldCopyMutableInputDtoH(sym, sym.Type.typeArguments(1).typeArguments(1), 3)
    case "MatField<int,3,3>" | "MatField<long,3,3>" | "MatField<float,3,3>" | "MatField<bool,3,3>" | "MatField<double,3,3>" => MatFieldCopyMutableInputDtoH(sym, sym.Type.typeArguments(1).typeArguments(2), 3, 3)
    case "Mesh" => MeshCopyMutableInputDtoH(sym)
    case "CudaArrayList<Cell>" | "CudaArrayList<Face>" | "CudaArrayList<Edge>" | "CudaArrayList<Vertex>" => "//copy\n" //TODO: Remove this
    case _ => super.copyMutableInputDtoH(sym)
  }

  override def getDSLHeaders: String = {
    val out = new StringBuilder
    out.append("#include <assert.h>\n")
    out.append("#include <float.h>\n")
    out.append("#include \"CudaArrayList.h\"\n")  //TODO: Remove this
    out.append("#include \"BitReverse.h\"\n")
    out.append("#include \"CRS.h\"\n")
    out.append("#include \"Field.h\"\n")
    out.append("#include \"Mesh.h\"\n")
    out.append("#include \"MeshObj.h\"\n")
    out.append("#include \"MeshSet.h\"\n")
    out.append("#include \"MatImpl.h\"\n")
    out.append("#include \"VecImpl.h\"\n")
    out.toString
  }
}

trait DeLisztCodeGenC extends OptiLACodeGenC with DeLisztCodeGenBase with DeLisztCCodeGenPkg with CGenDeliteOps with CGenDataStruct
  with DeliteCGenAllOverrides 
{
  val IR: DeliteApplication with DeLisztExp
  import IR._
 
 /*//defined only for GPU code generator
  * override def isObjectType[T](m: Manifest[T]) : Boolean = {
    if (m.erasure == classOf[Variable[Any]]) true
    else super.isObjectType(m)
  }
  
  override def copyInputHtoD(sym: Sym[Any]) : String = remap(sym.Type) match {
    case "int" | "long" | "float" | "double" | "bool" => refCopyInputHtoD(sym)
    case _ => super.copyInputHtoD(sym)
  }

  override def copyOutputDtoH(sym: Sym[Any]) : String = remap(sym.Type) match {
    case "int" | "long" | "float" | "double" | "bool" => refCopyOutputDtoH(sym)
    case _ => super.copyInputHtoD(sym)
  }

  override def copyMutableInputDtoH(sym: Sym[Any]) : String = remap(sym.Type) match {
    case "int" | "long" | "float" | "double" | "bool" => refCopyMutableInputDtoH(sym)
    case _ => super.copyInputHtoD(sym)
  }*/

}
