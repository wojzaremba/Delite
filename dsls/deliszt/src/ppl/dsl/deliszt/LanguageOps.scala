package ppl.dsl.deliszt

import java.io.PrintWriter
import reflect.Manifest

import scala.virtualization.lms.internal.{GenericFatCodegen, GenerationFailedException}
import scala.virtualization.lms.common._

/* Machinery provided by DeLiszt itself (language features and control structures).
 *
 * author: Michael Wu (mikemwu@stanford.edu)
 * created: Mar 14, 2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait LanguageOps extends Base { this: DeLiszt with MathOps =>
  def _init(args: Rep[Array[String]]) : Unit

  //should be part of LMS
  def infix_toInt[T : Numeric:Manifest](d: Rep[T]) : Rep[Int]

  //SyncedFile should be part of Delite - common mechanism to deal with distributed file (current implementation is faked)
  def SyncedFile(name: Rep[String]) : Rep[SyncedFile]
  def infix_write(f: Rep[SyncedFile], as: Rep[Any]*) : Unit
  def infix_writeln(f: Rep[SyncedFile], as: Rep[Any]*) : Unit
  def infix_close(f: Rep[SyncedFile]) : Unit

  def Print(as: Rep[Any]*) : Unit
  //Boundary set without mesh parameter reference to default mesh from cfg file (to keep programs working)
  def BoundarySet[MO<:Cell:Manifest](name: Rep[String])(implicit ev : MO =:= Cell) : Rep[MeshSet[Cell]]
  def BoundarySet[MO<:Edge:Manifest](name: Rep[String])(implicit ev : MO =:= Edge, o: Overloaded1) : Rep[MeshSet[Edge]]
  def BoundarySet[MO<:Face:Manifest](name: Rep[String])(implicit ev : MO =:= Face, o: Overloaded2) : Rep[MeshSet[Face]]
  def BoundarySet[MO<:Vertex:Manifest](name: Rep[String])(implicit ev : MO =:= Vertex, o: Overloaded3) : Rep[MeshSet[Vertex]]

  def infix_contains[MO<:MeshObj:Manifest](b : Rep[MeshSet[MO]], e : Rep[MO]) : Rep[Boolean]

  def BoundarySet[MO<:Cell:Manifest](name: Rep[String], m : Rep[Mesh])(implicit ev : MO =:= Cell, o: Overloaded4) : Rep[MeshSet[Cell]]
  def BoundarySet[MO<:Edge:Manifest](name: Rep[String], m : Rep[Mesh])(implicit ev : MO =:= Edge, o: Overloaded5) : Rep[MeshSet[Edge]]
  def BoundarySet[MO<:Face:Manifest](name: Rep[String], m : Rep[Mesh])(implicit ev : MO =:= Face, o: Overloaded6) : Rep[MeshSet[Face]]
  def BoundarySet[MO<:Vertex:Manifest](name: Rep[String], m : Rep[Mesh])(implicit ev : MO =:= Vertex, o: Overloaded7) : Rep[MeshSet[Vertex]]

  def mesh: Rep[Mesh]

  def vertices(e: Rep[Mesh])(implicit x: Overloaded1) : Rep[MeshSet[Vertex]]
  def vertices(e: Rep[Vertex])(implicit x: Overloaded2) : Rep[MeshSet[Vertex]]
  def vertices(e: Rep[Edge])(implicit x: Overloaded3) : Rep[MeshSet[Vertex]]
  def vertices(e: Rep[Face])(implicit x: Overloaded4) : Rep[MeshSet[Vertex]]
  def vertices(e: Rep[Cell])(implicit x: Overloaded5) : Rep[MeshSet[Vertex]]

  def verticesCCW(e: Rep[Face]) : Rep[MeshSet[Vertex]]
  def verticesCW(e: Rep[Face]) : Rep[MeshSet[Vertex]]
  
  def vertex(e: Rep[Cell], i: Rep[Int]) : Rep[Vertex]

  def cells(e: Rep[Mesh])(implicit x: Overloaded1) : Rep[MeshSet[Cell]]
  def cells(e: Rep[Vertex])(implicit x: Overloaded2) : Rep[MeshSet[Cell]]
  def cells(e: Rep[Edge])(implicit x: Overloaded3) : Rep[MeshSet[Cell]]
  def cells(e: Rep[Face])(implicit x: Overloaded4) : Rep[MeshSet[Cell]]
  def cells(e: Rep[Cell])(implicit x: Overloaded5) : Rep[MeshSet[Cell]]

  def cellsCCW(e: Rep[Edge]) : Rep[MeshSet[Cell]]
  def cellsCW(e: Rep[Edge]) : Rep[MeshSet[Cell]]

  def edges(e: Rep[Mesh])(implicit x: Overloaded1) : Rep[MeshSet[Edge]]
  def edges(e: Rep[Vertex])(implicit x: Overloaded2) : Rep[MeshSet[Edge]]
  def edges(e: Rep[Face])(implicit x: Overloaded3) : Rep[MeshSet[Edge]]
  def edges(e: Rep[Cell])(implicit x: Overloaded4) : Rep[MeshSet[Edge]]

  def edgesCCW(e: Rep[Face]) : Rep[MeshSet[Edge]]
  def edgesCW(e: Rep[Face]) : Rep[MeshSet[Edge]]

  def faces(e: Rep[Mesh])(implicit x: Overloaded1) : Rep[MeshSet[Face]]
  def faces(e: Rep[Vertex])(implicit x: Overloaded2) : Rep[MeshSet[Face]]
  def faces(e: Rep[Edge])(implicit x: Overloaded3) : Rep[MeshSet[Face]]
  def faces(e: Rep[Cell])(implicit x: Overloaded4) : Rep[MeshSet[Face]]

  def mesh(e: Rep[MeshObj]) : Rep[Mesh]

  def facesCCW(e: Rep[Edge]) : Rep[MeshSet[Face]]
  def facesCW(e: Rep[Edge]) : Rep[MeshSet[Face]]
  
  def face(e: Rep[Edge], i: Rep[Int]) : Rep[Face]

  def head(e: Rep[Edge]) : Rep[Vertex]
  def tail(e: Rep[Edge]) : Rep[Vertex]

  def inside(e: Rep[Face]) : Rep[Cell]
  def outside(e: Rep[Face]) : Rep[Cell]

  def flip(e: Rep[Edge])(implicit x: Overloaded1) : Rep[Edge]
  def flip(e: Rep[Face])(implicit x: Overloaded2) : Rep[Face]

  def towards(e: Rep[Edge],v: Rep[Vertex])(implicit x: Overloaded1) : Rep[Edge]
  def towards(e: Rep[Face],c: Rep[Cell])(implicit x: Overloaded2) : Rep[Face]
  def size[MO<:MeshObj:Manifest](s: Rep[MeshSet[MO]]) : Rep[Int]

  def ID[MO<:MeshObj:Manifest](x: Rep[MO]) : Rep[Int]

  def MATH_PI(): Rep[Double]
  def MIN_FLOAT(): Rep[Float]
  def MAX_FLOAT(): Rep[Float]
  def MIN_DOUBLE(): Rep[Double]
  def MAX_DOUBLE(): Rep[Double]
  def min[A:Manifest:Numeric](x: Rep[A], y: Rep[A]) = math_min(x, y)
  def max[A:Manifest:Numeric](x: Rep[A], y: Rep[A]) = math_max(x, y)
  def sqrt(a: Rep[Double]) = math_sqrt(a)
  def sqrtf(a: Rep[Float]) = math_sqrt(a).asInstanceOfL[Float]
  def exp(a: Rep[Double]) = math_exp(a)
  def expf(a: Rep[Float]) = math_exp(a).asInstanceOfL[Float]
  def sin(a: Rep[Double]) = math_sin(a)
  def sinf(a: Rep[Float]) = math_sin(a).asInstanceOfL[Float]
  def cos(a: Rep[Double]) = math_cos(a)
  def cosf(a: Rep[Float]) = math_cos(a).asInstanceOfL[Float]
  def acos(a: Rep[Double]) = math_acos(a)
  def acosf(a: Rep[Float]) = math_acos(a).asInstanceOfL[Float]
  def atan2(a: Rep[Double], b: Rep[Double]) = math_atan2(a,b)
  def atan2f(a: Rep[Float], b: Rep[Float]) = math_atan2(a,b).asInstanceOfL[Float]
  def pow(a: Rep[Double], b: Rep[Double]) = math_pow(a,b)
  def powf(a: Rep[Float], b: Rep[Float]) = math_pow(a,b).asInstanceOfL[Float]
  def abs(a: Rep[Double]) = math_abs(a)
  def fabs(a: Rep[Float]) = math_abs(a).asInstanceOfL[Float]
  
  def wall_time() : Rep[Double]
  def processor_time() : Rep[Double]
}

trait LanguageOpsExp extends LanguageOps with BaseFatExp with EffectExp {
  this: LanguageImplOps with DeLisztExp =>



  case class NumericToInt[T:Numeric:Manifest](e: Exp[T]) extends Def[Int]

  /******* Ops *********/
  case class DeLisztInit(args: Exp[Array[String]]) extends Def[Unit]
  case class DeLisztLoadCfgMesh(args: Exp[Array[String]]) extends Def[Mesh]
  case class DeLisztFile(name: Exp[String]) extends Def[SyncedFile]
  case class DeLisztCloseFile(file: Exp[SyncedFile]) extends Def[Unit]
  case class DeLisztWrite(file : Exp[SyncedFile], as : Exp[Any]) extends Def[Unit]
  case class DeLisztPrint(as: Seq[Exp[Any]])(block: Exp[Unit]) // stupid limitation...
    extends DeliteOpSingleTask(block)

  case class DeLisztBoundarySetContains[MO <: MeshObj : Manifest](b : Exp[MeshSet[MO]], e : Exp[MO]) extends Def[Boolean]

  case class DeLisztBoundarySetCells(name: Exp[String], mesh : Exp[Mesh]) extends Def[MeshSet[Cell]]
  case class DeLisztBoundarySetEdges(name: Exp[String], mesh : Exp[Mesh]) extends Def[MeshSet[Edge]]
  case class DeLisztBoundarySetFaces(name: Exp[String], mesh : Exp[Mesh]) extends Def[MeshSet[Face]]
  case class DeLisztBoundarySetVertices(name: Exp[String], mesh : Exp[Mesh]) extends Def[MeshSet[Vertex]]

  case class DeLisztMesh(mesh: Exp[Mesh]) extends Def[Mesh]

  case class DeLisztVerticesCell(val e: Exp[Int], mesh : Exp[Mesh]) extends Def[MeshSet[Vertex]]
  case class DeLisztVerticesEdge(val e: Exp[Int], mesh : Exp[Mesh]) extends Def[MeshSet[Vertex]]
  case class DeLisztVerticesFace(val e: Exp[Int], mesh : Exp[Mesh]) extends Def[MeshSet[Vertex]]
  case class DeLisztVerticesVertex(val e: Exp[Int], mesh : Exp[Mesh]) extends Def[MeshSet[Vertex]]
  case class DeLisztVerticesMesh(mesh : Exp[Mesh]) extends Def[MeshSet[Vertex]]

  
  case class DeLisztCtov(val mesh: Exp[Mesh]) extends Def[CRS]
  case class DeLisztEtov(val mesh: Exp[Mesh]) extends Def[CRS]
  case class DeLisztFtov(val mesh: Exp[Mesh]) extends Def[CRS]
  case class DeLisztVtov(val mesh: Exp[Mesh]) extends Def[CRS]
  
  case class DeLisztVertex(e: Exp[Int], i: Exp[Int], mesh : Exp[Mesh]) extends Def[Vertex]
  
  case class DeLisztFaceVerticesCCW(e: Exp[Face], mesh : Exp[Mesh]) extends Def[MeshSet[Vertex]]
  case class DeLisztFaceVerticesCW(e: Exp[Face], mesh : Exp[Mesh]) extends Def[MeshSet[Vertex]]

  case class DeLisztCellsCell(val e: Exp[Int], mesh : Exp[Mesh]) extends Def[MeshSet[Cell]]
  case class DeLisztCellsEdge(val e: Exp[Int], mesh : Exp[Mesh]) extends Def[MeshSet[Cell]]
  case class DeLisztCellsFace(val e: Exp[Int], mesh : Exp[Mesh]) extends Def[MeshSet[Cell]]
  case class DeLisztCellsVertex(val e: Exp[Int], mesh : Exp[Mesh]) extends Def[MeshSet[Cell]]
  case class DeLisztCellsMesh(mesh : Exp[Mesh]) extends Def[MeshSet[Cell]]
  
  case class DeLisztCtoc(val mesh: Exp[Mesh]) extends Def[CRS]
  case class DeLisztEtoc(val mesh: Exp[Mesh]) extends Def[CRS]
  case class DeLisztFtoc(val mesh: Exp[Mesh]) extends Def[CRS]
  case class DeLisztVtoc(val mesh: Exp[Mesh]) extends Def[CRS]
  
  case class DeLisztEdgeCellsCCW(e: Exp[Edge], mesh : Exp[Mesh]) extends Def[MeshSet[Cell]]
  case class DeLisztEdgeCellsCW(e: Exp[Edge], mesh : Exp[Mesh]) extends Def[MeshSet[Cell]]

  case class DeLisztEdgesCell(val e: Exp[Int], mesh : Exp[Mesh]) extends Def[MeshSet[Edge]]
  case class DeLisztEdgesFace(val e: Exp[Int], mesh : Exp[Mesh]) extends Def[MeshSet[Edge]]
  case class DeLisztEdgesVertex(val e: Exp[Int], mesh : Exp[Mesh]) extends Def[MeshSet[Edge]]
  case class DeLisztEdgesMesh(mesh : Exp[Mesh]) extends Def[MeshSet[Edge]]
  
  case class DeLisztCtoe(val mesh: Exp[Mesh]) extends Def[CRS]
  case class DeLisztFtoe(val mesh: Exp[Mesh]) extends Def[CRS]
  case class DeLisztVtoe(val mesh: Exp[Mesh]) extends Def[CRS]

  case class DeLisztFacesCell(val e: Exp[Int], mesh : Exp[Mesh]) extends Def[MeshSet[Face]]
  case class DeLisztFacesEdge(val e: Exp[Int], mesh : Exp[Mesh]) extends Def[MeshSet[Face]]
  case class DeLisztFacesVertex(val e: Exp[Int], mesh : Exp[Mesh]) extends Def[MeshSet[Face]]
  case class DeLisztFacesMesh(mesh : Exp[Mesh]) extends Def[MeshSet[Face]]
  
  case class DeLisztCtof(val mesh: Exp[Mesh]) extends Def[CRS]
  case class DeLisztEtof(val mesh: Exp[Mesh]) extends Def[CRS]
  case class DeLisztVtof(val mesh: Exp[Mesh]) extends Def[CRS]

  case class DeLisztEdgeFacesCCW(e: Exp[Edge], mesh : Exp[Mesh]) extends Def[MeshSet[Face]]
  case class DeLisztEdgeFacesCW(e: Exp[Edge], mesh : Exp[Mesh]) extends Def[MeshSet[Face]]
  
  case class DeLisztFaceEdgesCCW(e: Exp[Face], mesh : Exp[Mesh]) extends Def[MeshSet[Edge]]
  case class DeLisztFaceEdgesCW(e: Exp[Face], mesh : Exp[Mesh]) extends Def[MeshSet[Edge]]
  
  case class DeLisztEdgeHead(e: Exp[Edge], mesh : Exp[Mesh]) extends Def[Vertex]
  case class DeLisztEdgeTail(e: Exp[Edge], mesh : Exp[Mesh]) extends Def[Vertex]

  case class DeLisztFaceInside(e: Exp[Face], mesh : Exp[Mesh]) extends Def[Cell]
  case class DeLisztFaceOutside(e: Exp[Face], mesh : Exp[Mesh]) extends Def[Cell]
  
  case class DeLisztFace(e: Exp[Int], i: Exp[Int], mesh : Exp[Mesh]) extends Def[Face]
  
  case class DeLisztFlipEdge(e: Exp[Edge]) extends Def[Edge]
  case class DeLisztFlipFace(e: Exp[Face]) extends Def[Face]

  case class DeLisztTowardsEdgeVertex(e: Exp[Edge], v: Exp[Vertex], mesh : Exp[Mesh]) extends Def[Edge]
  case class DeLisztTowardsFaceCell(e: Exp[Face], c: Exp[Cell], mesh : Exp[Mesh]) extends Def[Face]

  case class DeLisztSize[MO<:MeshObj:Manifest](s: Exp[MeshSet[MO]]) extends Def[Int]
  
  case class DeLisztID[MO<:MeshObj:Manifest](x: Exp[MO]) extends Def[Int]

  case class MinFloat() extends Def[Float]
  case class MaxFloat() extends Def[Float]
  case class MinDouble() extends Def[Double]
  case class MaxDouble() extends Def[Double]
  case class WallTime() extends Def[Double]
  case class ProcessorTime() extends Def[Double]

  def findMesh[MO <: MeshObj](e: Exp[MO]) : Exp[Mesh] = {
    val tp = findDefinition(e.asInstanceOf[Sym[_]]).get
    tp.rhs match {
      case DeliteCollectionApply(x, n) => {
        val tp2 = findDefinition(x.asInstanceOf[Sym[_]]).get
        tp2.rhs match {
          case m:{val mesh : Exp[Mesh]} => m.mesh
          case _ => throw new Exception("Can't find corresponding mesh " + tp2.rhs.toString() + " " )
        }
      }
      case DeliteIfThenElse(_, a, b, _) => {
        val m1 = findMesh(a.asInstanceOf[Exp[MeshObj]])
        val m2 = findMesh(b.asInstanceOf[Exp[MeshObj]])
        if (m1.asInstanceOf[Sym[_]].id != m2.asInstanceOf[Sym[_]].id) throw new Exception("both if branches should reference to same mesh") else m1
      }
      case m:{val mesh : Exp[Mesh]} => m.mesh
      case DeLisztFlipEdge(e) => findMesh(e)
      case DeLisztFlipFace(f) => findMesh(f)
      case _ => throw new Exception("Can't find corresponding mesh (can't find DeliteCollectionApply node) " + tp.rhs.toString)
    }
  }

  /******* Language functions *********/

  def _init(args: Exp[Array[String]]) = {
    reflectEffect(DeLisztInit(args))
    findOrCreateDefinition(DeLisztLoadCfgMesh(args))
  }

  def infix_toInt[T : Numeric : Manifest](d: Exp[T]) = reflectPure(NumericToInt(d))

  def SyncedFile(name: Exp[String]) = reflectMutable(DeLisztFile(name))
  def infix_write(f : Rep[SyncedFile], as: Exp[Any]*) = for (a <- as) reflectWrite(f)(DeLisztWrite(f, a))
  def infix_writeln(f : Rep[SyncedFile], as: Exp[Any]*) = {
    infix_write(f, as:_*)
    infix_write(f, Const("""\n"""))
  }
  def infix_close(f : Rep[SyncedFile]) = reflectWrite(f)(DeLisztCloseFile(f))

  def Print(as: Exp[Any]*) = reflectEffect(DeLisztPrint(as)(reifyEffectsHere(print_impl(as))))

  //I had to copy this lines, because of abstract definition
  def BoundarySet[MO<:Cell:Manifest](name: Exp[String])(implicit ev : MO =:= Cell) = BoundarySet[MO](name, mesh)
  def BoundarySet[MO<:Edge:Manifest](name: Exp[String])(implicit ev : MO =:= Edge, o: Overloaded1) = BoundarySet[MO](name, mesh)
  def BoundarySet[MO<:Face:Manifest](name: Exp[String])(implicit ev : MO =:= Face, o: Overloaded2) = BoundarySet[MO](name, mesh)
  def BoundarySet[MO<:Vertex:Manifest](name: Exp[String])(implicit ev : MO =:= Vertex, o: Overloaded3) = BoundarySet[MO](name, mesh)

  def infix_contains[MO<:MeshObj:Manifest](b : Exp[MeshSet[MO]], e : Exp[MO]) = reflectPure(DeLisztBoundarySetContains(b, e))

  def BoundarySet[MO<:Cell:Manifest](name: Exp[String], mesh : Exp[Mesh])(implicit ev : MO =:= Cell, o: Overloaded4) = reflectPure(DeLisztBoundarySetCells(name, mesh))
  def BoundarySet[MO<:Edge:Manifest](name: Exp[String], mesh : Exp[Mesh])(implicit ev : MO =:= Edge, o: Overloaded5) = reflectPure(DeLisztBoundarySetEdges(name, mesh))
  def BoundarySet[MO<:Face:Manifest](name: Exp[String], mesh : Exp[Mesh])(implicit ev : MO =:= Face, o: Overloaded6) = reflectPure(DeLisztBoundarySetFaces(name, mesh))
  def BoundarySet[MO<:Vertex:Manifest](name: Exp[String], mesh : Exp[Mesh])(implicit ev : MO =:= Vertex, o: Overloaded7) = reflectPure(DeLisztBoundarySetVertices(name, mesh))

  def mesh = {
      val m = globalDefs.find(tp => tp.rhs match {
        case DeLisztLoadCfgMesh(_) => true
        case _ => false
      }).get.rhs
      reflectPure(DeLisztMesh(toAtom(m).asInstanceOf[Def[Mesh]]))
  }

  def vertices(e: Exp[Cell])(implicit x: Overloaded5) = reflectPure(DeLisztVerticesCell(ID(e), findMesh(e)))
  def vertices(e: Exp[Edge])(implicit x: Overloaded3) = reflectPure(DeLisztVerticesEdge(ID(e), findMesh(e)))
  def vertices(e: Exp[Face])(implicit x: Overloaded4) = reflectPure(DeLisztVerticesFace(ID(e), findMesh(e)))
  def vertices(e: Exp[Vertex])(implicit x: Overloaded2) = reflectPure(DeLisztVerticesVertex(ID(e), findMesh(e)))
  def vertices(e: Exp[Mesh])(implicit x: Overloaded1) = reflectPure(DeLisztVerticesMesh(e))

  def crs_ctov(mesh: Exp[Mesh]) = reflectPure(DeLisztCtov(mesh))
  def crs_etov(mesh: Exp[Mesh]) = reflectPure(DeLisztEtov(mesh))
  def crs_ftov(mesh: Exp[Mesh]) = reflectPure(DeLisztFtov(mesh))
  def crs_vtov(mesh: Exp[Mesh]) = reflectPure(DeLisztVtov(mesh))

  def verticesCCW(e: Exp[Face]) = reflectPure(DeLisztFaceVerticesCCW(e, findMesh(e)))
  def verticesCW(e: Exp[Face]) = reflectPure(DeLisztFaceVerticesCW(e, findMesh(e)))
  
  def vertex(e: Exp[Cell], i: Exp[Int]) = reflectPure(DeLisztVertex(ID(e),i, findMesh(e)))

  def cells(e: Exp[Cell])(implicit x: Overloaded5) = reflectPure(DeLisztCellsCell(ID(e), findMesh(e)))
  def cells(e: Exp[Edge])(implicit x: Overloaded3) = reflectPure(DeLisztCellsEdge(ID(e), findMesh(e)))
  def cells(e: Exp[Face])(implicit x: Overloaded4) = reflectPure(DeLisztCellsFace(ID(e), findMesh(e)))
  def cells(e: Exp[Vertex])(implicit x: Overloaded2) = reflectPure(DeLisztCellsVertex(ID(e), findMesh(e)))
  def cells(mesh: Exp[Mesh])(implicit x: Overloaded1) = reflectPure(DeLisztCellsMesh(mesh))
  
  def crs_ctoc(mesh: Exp[Mesh]) = reflectPure(DeLisztCtoc(mesh))
  def crs_etoc(mesh: Exp[Mesh]) = reflectPure(DeLisztEtoc(mesh))
  def crs_ftoc(mesh: Exp[Mesh]) = reflectPure(DeLisztFtoc(mesh))
  def crs_vtoc(mesh: Exp[Mesh]) = reflectPure(DeLisztVtoc(mesh))
  
  def cellsCCW(e: Exp[Edge]) = reflectPure(DeLisztEdgeCellsCCW(e, findMesh(e)))
  def cellsCW(e: Exp[Edge]) = reflectPure(DeLisztEdgeCellsCW(e, findMesh(e)))

  def edges(e: Exp[Cell])(implicit x: Overloaded4) = reflectPure(DeLisztEdgesCell(ID(e), findMesh(e)))
  def edges(e: Exp[Face])(implicit x: Overloaded3) = reflectPure(DeLisztEdgesFace(ID(e), findMesh(e)))
  def edges(e: Exp[Vertex])(implicit x: Overloaded2) = reflectPure(DeLisztEdgesVertex(ID(e), findMesh(e)))
  def edges(mesh: Exp[Mesh])(implicit x: Overloaded1) = reflectPure(DeLisztEdgesMesh(mesh))
  
  def crs_ctoe(mesh: Exp[Mesh]) = reflectPure(DeLisztCtoe(mesh))
  def crs_ftoe(mesh: Exp[Mesh]) = reflectPure(DeLisztFtoe(mesh))
  def crs_vtoe(mesh: Exp[Mesh]) = reflectPure(DeLisztVtoe(mesh))

  def edgesCCW(e: Exp[Face]) = reflectPure(DeLisztFaceEdgesCCW(e, findMesh(e)))
  def edgesCW(e: Exp[Face]) = reflectPure(DeLisztFaceEdgesCW(e, findMesh(e)))

  def faces(e: Exp[Edge])(implicit x: Overloaded3) = reflectPure(DeLisztFacesEdge(ID(e), findMesh(e)))
  def faces(e: Exp[Cell])(implicit x: Overloaded4) = reflectPure(DeLisztFacesCell(ID(e), findMesh(e)))
  def faces(e: Exp[Vertex])(implicit x: Overloaded2) = reflectPure(DeLisztFacesVertex(ID(e), findMesh(e)))
  def faces(e: Exp[Mesh])(implicit x: Overloaded1) = reflectPure(DeLisztFacesMesh(e))
  
  def crs_ctof(mesh: Exp[Mesh]) = reflectPure(DeLisztCtof(mesh))
  def crs_etof(mesh: Exp[Mesh]) = reflectPure(DeLisztEtof(mesh))
  def crs_vtof(mesh: Exp[Mesh]) = reflectPure(DeLisztVtof(mesh))

  def facesCCW(e: Exp[Edge]) = reflectPure(DeLisztEdgeFacesCCW(e, findMesh(e)))
  def facesCW(e: Exp[Edge]) = reflectPure(DeLisztEdgeFacesCW(e, findMesh(e)))
  
  def face(e: Exp[Edge], i: Exp[Int]) = reflectPure(DeLisztFace(ID(e), i, findMesh(e)))

  def mesh(e: Exp[MeshObj]) = findMesh(e)

  def head(e: Exp[Edge]) = reflectPure(DeLisztEdgeHead(e, findMesh(e)))
  def tail(e: Exp[Edge]) = reflectPure(DeLisztEdgeTail(e, findMesh(e)))

  def inside(e: Exp[Face]) = reflectPure(DeLisztFaceInside(e, findMesh(e)))
  def outside(e: Exp[Face]) = reflectPure(DeLisztFaceOutside(e, findMesh(e)))

  def flip(e: Exp[Edge])(implicit x: Overloaded1) = reflectPure(DeLisztFlipEdge(e))
  def flip(e: Exp[Face])(implicit x: Overloaded2) = reflectPure(DeLisztFlipFace(e))

  def towards(e: Exp[Edge], v: Exp[Vertex])(implicit x: Overloaded1) = reflectPure(DeLisztTowardsEdgeVertex(e,v, findMesh(e)))
  def towards(e: Exp[Face], c: Exp[Cell])(implicit x: Overloaded2) = reflectPure(DeLisztTowardsFaceCell(e,c, findMesh(e)))
  
  def size[MO<:MeshObj:Manifest](s: Exp[MeshSet[MO]]) = reflectPure(DeLisztSize(s))

  def ID[MO<:MeshObj:Manifest](x: Exp[MO]) = reflectPure(DeLisztID(x))

  def MATH_PI() = reflectPure(MathPi())
  def MIN_FLOAT() = reflectPure(MinFloat())
  def MAX_FLOAT() = reflectPure(MaxFloat())
  def MIN_DOUBLE() = reflectPure(MinDouble())
  def MAX_DOUBLE() = reflectPure(MaxDouble())
  
  def wall_time() = reflectEffect(WallTime())
  def processor_time() = reflectEffect(ProcessorTime())
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {    
    case DeLisztVerticesCell(e, mesh) => reflectPure(DeLisztVerticesCell(f(e),f(mesh)))
    case DeLisztVerticesEdge(e, mesh) => reflectPure(DeLisztVerticesEdge(f(e),f(mesh)))
    case DeLisztVerticesFace(e, mesh) => reflectPure(DeLisztVerticesFace(f(e),f(mesh)))
    case DeLisztVerticesVertex(e, mesh) => reflectPure(DeLisztVerticesVertex(f(e),f(mesh)))
    case DeLisztVerticesMesh(e) => reflectPure(DeLisztVerticesMesh(f(e)))
    case DeLisztVertex(e,i, mesh) => reflectPure(DeLisztVertex(f(e),f(i),f(mesh)))
    case DeLisztFaceVerticesCCW(e, mesh) => reflectPure(DeLisztFaceVerticesCCW(f(e),f(mesh)))
    case DeLisztFaceVerticesCW(e, mesh) => reflectPure(DeLisztFaceVerticesCW(f(e),f(mesh)))
    case DeLisztCellsCell(e, mesh) => reflectPure(DeLisztCellsCell(f(e),f(mesh)))
    case DeLisztCellsEdge(e, mesh) => reflectPure(DeLisztCellsEdge(f(e),f(mesh)))
    case DeLisztCellsFace(e, mesh) => reflectPure(DeLisztCellsFace(f(e),f(mesh)))
    case DeLisztCellsVertex(e, mesh) => reflectPure(DeLisztCellsVertex(f(e),f(mesh)))
    case DeLisztCellsMesh(e) => reflectPure(DeLisztCellsMesh(f(e)))
    case DeLisztEdgeCellsCCW(e, mesh) => reflectPure(DeLisztEdgeCellsCCW(f(e),f(mesh)))
    case DeLisztEdgeCellsCW(e, mesh) => reflectPure(DeLisztEdgeCellsCW(f(e),f(mesh)))
    case DeLisztEdgesCell(e, mesh) => reflectPure(DeLisztEdgesCell(f(e),f(mesh)))
    case DeLisztEdgesFace(e, mesh) => reflectPure(DeLisztEdgesFace(f(e),f(mesh)))
    case DeLisztEdgesVertex(e, mesh) => reflectPure(DeLisztEdgesVertex(f(e),f(mesh)))
    case DeLisztEdgesMesh(e) => reflectPure(DeLisztEdgesMesh(f(e)))
    case DeLisztFacesEdge(e, mesh) => reflectPure(DeLisztFacesEdge(f(e),f(mesh)))
    case DeLisztFacesCell(e, mesh) => reflectPure(DeLisztFacesCell(f(e),f(mesh)))
    case DeLisztFacesVertex(e, mesh) => reflectPure(DeLisztFacesVertex(f(e),f(mesh)))
    case DeLisztFacesMesh(e) => reflectPure(DeLisztFacesMesh(f(e)))
    case DeLisztFaceInside(e, mesh) => reflectPure(DeLisztFaceInside(f(e),f(mesh)))
    case DeLisztFaceOutside(e, mesh) => reflectPure(DeLisztFaceOutside(f(e),f(mesh)))
    case DeLisztEdgeFacesCCW(e, mesh) => reflectPure(DeLisztEdgeFacesCCW(f(e),f(mesh)))
    case DeLisztEdgeFacesCW(e, mesh) => reflectPure(DeLisztEdgeFacesCW(f(e),f(mesh)))
    case DeLisztFaceEdgesCCW(e, mesh) => reflectPure(DeLisztFaceEdgesCCW(f(e),f(mesh)))
    case DeLisztFaceEdgesCW(e, mesh) => reflectPure(DeLisztFaceEdgesCW(f(e),f(mesh)))
    case DeLisztEdgeHead(e, mesh) => reflectPure(DeLisztEdgeHead(f(e),f(mesh)))
    case DeLisztEdgeTail(e, mesh) => reflectPure(DeLisztEdgeTail(f(e),f(mesh)))
    case DeLisztFace(e,i, mesh) => reflectPure(DeLisztFace(f(e),f(i),f(mesh)))
    case DeLisztFlipEdge(e) => reflectPure(DeLisztFlipEdge(f(e)))
    case DeLisztFlipFace(e) => reflectPure(DeLisztFlipFace(f(e)))
    case DeLisztTowardsEdgeVertex(e,v, mesh) => reflectPure(DeLisztTowardsEdgeVertex(f(e),f(v),f(mesh)))
    case DeLisztTowardsFaceCell(e,c, mesh) => reflectPure(DeLisztTowardsFaceCell(f(e),f(c),f(mesh)))
    case DeLisztSize(s) => size(f(s))    
    case DeLisztID(e) => ID(f(e))
    case Reflect(e@DeLisztPrint(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeLisztPrint(f(x))(f(e.block)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(WallTime(), u, es) => reflectMirrored(Reflect(WallTime(), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??  
}

trait LanguageOpsExpOpt extends LanguageOpsExp {
  this: DeLisztExp =>
  override def ID[MO<:MeshObj:Manifest](x: Exp[MO]) = x match {
    case Def(DeLisztFlipEdge(e)) => super.ID(e)
    case Def(DeLisztFlipFace(e)) => super.ID(e)
    case _ => super.ID(x)
  }
}

trait ScalaGenLanguageOps extends ScalaGenBase {
  val IR: LanguageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case DeLisztInit(args) => emitValDef(sym, "Liszt.init(" + quote(args) + ")")
      case DeLisztLoadCfgMesh(args) => emitValDef(sym, "Liszt.load(" + quote(args) + ")")
      case DeLisztMesh(mesh) => emitValDef(sym, quote(mesh))

      case NumericToInt(e) => emitValDef(sym, quote(e) + ".toInt" )

      //woj.zaremba : This file is faulty implement - currently do not take care of concurent accesss (it is mine temporary impl.)
      case DeLisztFile(name) => emitValDef(sym, "new SyncedFile(" + quote(name) + ")" )
      case DeLisztWrite(f, str) => emitValDef(sym, quote(f) + ".write(" + quote(str) + ")" )

      case DeLisztCloseFile(f) => emitValDef(sym, quote(f) + ".close()" )

      case DeLisztBoundarySetContains(b, e) => emitValDef(sym, quote(b) + ".contains(" + quote(e) + ")")

      case DeLisztBoundarySetCells(name, mesh) => emitValDef(sym, quote(mesh) + ".boundarySetCells(" + quote(name) + ")")
      case DeLisztBoundarySetEdges(name, mesh) => emitValDef(sym, quote(mesh) + ".boundarySetEdges(" + quote(name) + ")")
      case DeLisztBoundarySetFaces(name, mesh) => emitValDef(sym, quote(mesh) + ".boundarySetFaces(" + quote(name) + ")")
      case DeLisztBoundarySetVertices(name, mesh) => emitValDef(sym, quote(mesh) + ".boundarySetVertices(" + quote(name) + ")")

      case DeLisztCellsCell(e, mesh) => emitValDef(sym, quote(mesh) + ".cellsCell(" + quote(e) + ")")
      case DeLisztCellsEdge(e, mesh) => emitValDef(sym, quote(mesh) + ".cellsEdge(" + quote(e) + ")")
      case DeLisztCellsFace(e, mesh) => emitValDef(sym, quote(mesh) + ".cellsFace(" + quote(e) + ")")
      case DeLisztCellsVertex(e, mesh) => emitValDef(sym, quote(mesh) + ".cellsVertex(" + quote(e) + ")")
      case DeLisztCellsMesh(e) => emitValDef(sym, quote(e) + ".cellsMesh")
      
      case DeLisztCtoc(mesh) => emitValDef(sym, quote(mesh) + ".ctoc")
      case DeLisztEtoc(mesh) => emitValDef(sym, quote(mesh) + ".etoc")
      case DeLisztFtoc(mesh) => emitValDef(sym, quote(mesh) + ".ftoc")
      case DeLisztVtoc(mesh) => emitValDef(sym, quote(mesh) + ".vtoc")
      
      case DeLisztEdgeCellsCCW(e, mesh) => emitValDef(sym, quote(mesh) + ".cellsCCW(" + quote(e) + ")")
      case DeLisztEdgeCellsCW(e, mesh) => emitValDef(sym, quote(mesh) + ".cellsCW(" + quote(e) + ")")
      case DeLisztFaceInside(e, mesh) => emitValDef(sym, quote(mesh) + ".inside(" + quote(e) + ")")
      case DeLisztFaceOutside(e, mesh) => emitValDef(sym, quote(mesh) + ".outside(" + quote(e) + ")")

      case DeLisztEdgesCell(e, mesh) => emitValDef(sym, quote(mesh) + ".edgesCell(" + quote(e) + ")")
      case DeLisztEdgesFace(e, mesh) => emitValDef(sym, quote(mesh) + ".edgesFace(" + quote(e) + ")")
      case DeLisztEdgesVertex(e, mesh) => emitValDef(sym, quote(mesh) + ".edgesVertex(" + quote(e) + ")")
      case DeLisztEdgesMesh(e) => emitValDef(sym, quote(e) + ".edgesMesh")
      
      case DeLisztCtoe(mesh) => emitValDef(sym, quote(mesh) + ".ctoe")
      case DeLisztFtoe(mesh) => emitValDef(sym, quote(mesh) + ".ftoe")
      case DeLisztVtoe(mesh) => emitValDef(sym, quote(mesh) + ".vtoe")
      
      case DeLisztEdgeHead(e, mesh) => emitValDef(sym, quote(mesh) + ".head(" + quote(e) + ")")
      case DeLisztEdgeTail(e, mesh) => emitValDef(sym, quote(mesh) + ".tail(" + quote(e) + ")")
      
      case DeLisztEdgeFacesCCW(e, mesh) => emitValDef(sym, quote(mesh) + ".facesCCW(" + quote(e) + ")")
      case DeLisztEdgeFacesCW(e, mesh) => emitValDef(sym, quote(mesh) + ".facesCW(" + quote(e) + ")")

      case DeLisztFacesCell(e, mesh) => emitValDef(sym, quote(mesh) + ".facesCell(" + quote(e) + ")")
      case DeLisztFacesEdge(e, mesh) => emitValDef(sym, quote(mesh) + ".facesEdge(" + quote(e) + ")")
      case DeLisztFacesVertex(e, mesh) => emitValDef(sym, quote(mesh) + ".facesVertex(" + quote(e) + ")")
      case DeLisztFacesMesh(e) => emitValDef(sym, quote(e) + ".facesMesh")
      
      case DeLisztCtof(mesh) => emitValDef(sym, quote(mesh) + ".ctof")
      case DeLisztEtof(mesh) => emitValDef(sym, quote(mesh) + ".etof")
      case DeLisztVtof(mesh) => emitValDef(sym, quote(mesh) + ".vtof")
      
      case DeLisztFaceEdgesCCW(e, mesh) => emitValDef(sym, quote(mesh) + ".edgesCCW(" + quote(e) + ")")
      case DeLisztFaceEdgesCW(e, mesh) => emitValDef(sym, quote(mesh) + ".edgesCW(" + quote(e) + ")")
      case DeLisztFace(e,i, mesh) => emitValDef(sym, quote(mesh) + ".face(" + quote(e) + "," + quote(i) + ")")

      case DeLisztVerticesCell(e, mesh) => emitValDef(sym, quote(mesh) + ".verticesCell(" + quote(e) + ")")
      case DeLisztVerticesEdge(e, mesh) => emitValDef(sym, quote(mesh) + ".verticesEdge(" + quote(e) + ")")
      case DeLisztVerticesFace(e, mesh) => emitValDef(sym, quote(mesh) + ".verticesFace(" + quote(e) + ")")
      case DeLisztVerticesVertex(e, mesh) => emitValDef(sym, quote(mesh) + ".verticesVertex(" + quote(e) + ")")
      case DeLisztVerticesMesh(e) => emitValDef(sym, quote(e) + ".verticesMesh")

      case DeLisztCtov(mesh) => emitValDef(sym, quote(mesh) + ".ctov")
      case DeLisztEtov(mesh) => emitValDef(sym, quote(mesh) + ".etov")
      case DeLisztFtov(mesh) => emitValDef(sym, quote(mesh) + ".ftov")
      case DeLisztVtov(mesh) => emitValDef(sym, quote(mesh) + ".vtov")
      
      case DeLisztFaceVerticesCCW(e, mesh) => emitValDef(sym, quote(mesh) + ".verticesCCW(" + quote(e) + ")")
      case DeLisztFaceVerticesCW(e, mesh) => emitValDef(sym, quote(mesh) + ".verticesCW(" + quote(e) + ")")
      case DeLisztVertex(e,i, mesh) => emitValDef(sym, quote(mesh) + ".vertex(" + quote(e) + "," + quote(i) + ")")

      case DeLisztFlipEdge(e) => emitValDef(sym, "generated.scala.Mesh.flip(" + quote(e) + ")")
      case DeLisztFlipFace(e) => emitValDef(sym, "generated.scala.Mesh.flip(" + quote(e) + ")")
      
      case DeLisztTowardsEdgeVertex(e,v, mesh) => emitValDef(sym, quote(mesh) + ".towardsEdgeVertex(" + quote(e) + "," + quote(v) + ")")
      case DeLisztTowardsFaceCell(e,c, mesh) => emitValDef(sym, quote(mesh) + ".towardsFaceCell(" + quote(e) + "," + quote(c) + ")")
      
      case DeLisztID(x) => emitValDef(sym, "generated.scala.Mesh.internal(" + quote(x) + ")")
      
      case DeLisztSize(s) => emitValDef(sym, quote(s) + ".size")

      case MinFloat() => emitValDef(sym, "scala.Float.MinValue")
      case MaxFloat() => emitValDef(sym, "scala.Float.MaxValue")
      case MinDouble() => emitValDef(sym, "scala.Double.MinValue")
      case MaxDouble() => emitValDef(sym, "scala.Double.MaxValue")
      case ProcessorTime() => emitValDef(sym, "generated.scala.Global.processor_time")
      case WallTime() => emitValDef(sym, "generated.scala.Global.wall_time")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait CudaGenLanguageOps extends CudaGenBase {
  val IR: LanguageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    //TODO: Use mesh symbol
    case DeLisztCellsCell(e, mesh) => emitValDef(sym, quote(mesh) + ".cellsCell(" + quote(e) + ")")
    case DeLisztCellsEdge(e, mesh) => emitValDef(sym, quote(mesh) + ".cellsEdge(" + quote(e) + ")")
    case DeLisztCellsFace(e, mesh) => emitValDef(sym, quote(mesh) + ".cellsFace(" + quote(e) + ")")
    case DeLisztCellsVertex(e, mesh) => emitValDef(sym, quote(mesh) + ".cellsVertex(" + quote(e) + ")")
    case DeLisztCellsMesh(e) => emitValDef(sym, quote(e) + ".cellsMesh")

    case DeLisztCtoc(mesh) => emitValDef(sym, quote(mesh) + ".ctoc")
    case DeLisztEtoc(mesh) => emitValDef(sym, quote(mesh) + ".etoc")
    case DeLisztFtoc(mesh) => emitValDef(sym, quote(mesh) + ".ftoc")
    case DeLisztVtoc(mesh) => emitValDef(sym, quote(mesh) + ".vtoc")

    case DeLisztEdgeCellsCCW(e, mesh) => emitValDef(sym, quote(mesh) + ".cellsCCW(" + quote(e) + ")")
    case DeLisztEdgeCellsCW(e, mesh) => emitValDef(sym, quote(mesh) + ".cellsCW(" + quote(e) + ")")
    case DeLisztFaceInside(e, mesh) => emitValDef(sym, quote(mesh) + ".inside(" + quote(e) + ")")
    case DeLisztFaceOutside(e, mesh) => emitValDef(sym, quote(mesh) + ".outside(" + quote(e) + ")")

    case DeLisztEdgesCell(e, mesh) => emitValDef(sym, quote(mesh) + ".edgesCell(" + quote(e) + ")")
    case DeLisztEdgesFace(e, mesh) => emitValDef(sym, quote(mesh) + ".edgesFace(" + quote(e) + ")")
    case DeLisztEdgesVertex(e, mesh) => emitValDef(sym, quote(mesh) + ".edgesVertex(" + quote(e) + ")")
    case DeLisztEdgesMesh(e) => emitValDef(sym, quote(e) + ".edgesMesh")

    case DeLisztCtoe(mesh) => emitValDef(sym, quote(mesh) + ".ctoe")
    case DeLisztFtoe(mesh) => emitValDef(sym, quote(mesh) + ".ftoe")
    case DeLisztVtoe(mesh) => emitValDef(sym, quote(mesh) + ".vtoe")

    case DeLisztEdgeHead(e, mesh) => emitValDef(sym, quote(mesh) + ".head(" + quote(e) + ")")
    case DeLisztEdgeTail(e, mesh) => emitValDef(sym, quote(mesh) + ".tail(" + quote(e) + ")")

    case DeLisztEdgeFacesCCW(e, mesh) => emitValDef(sym, quote(mesh) + ".facesCCW(" + quote(e) + ")")
    case DeLisztEdgeFacesCW(e, mesh) => emitValDef(sym, quote(mesh) + ".facesCW(" + quote(e) + ")")

    case DeLisztFacesCell(e, mesh) => emitValDef(sym, quote(mesh) + ".facesCell(" + quote(e) + ")")
    case DeLisztFacesEdge(e, mesh) => emitValDef(sym, quote(mesh) + ".facesEdge(" + quote(e) + ")")
    case DeLisztFacesVertex(e, mesh) => emitValDef(sym, quote(mesh) + ".facesVertex(" + quote(e) + ")")
    case DeLisztFacesMesh(e) => emitValDef(sym, quote(e) + ".facesMesh")

    case DeLisztCtof(mesh) => emitValDef(sym, quote(mesh) + ".ctof")
    case DeLisztEtof(mesh) => emitValDef(sym, quote(mesh) + ".etof")
    case DeLisztVtof(mesh) => emitValDef(sym, quote(mesh) + ".vtof")

    case DeLisztFaceEdgesCCW(e, mesh) => emitValDef(sym, quote(mesh) + ".edgesCCW(" + quote(e) + ")")
    case DeLisztFaceEdgesCW(e, mesh) => emitValDef(sym, quote(mesh) + ".edgesCW(" + quote(e) + ")")
    case DeLisztFace(e,i, mesh) => emitValDef(sym, quote(mesh) + ".face(" + quote(e) + "," + quote(i) + ")")

    case DeLisztVerticesCell(e, mesh) => emitValDef(sym, quote(mesh) + ".verticesCell(" + quote(e) + ")")
    case DeLisztVerticesEdge(e, mesh) => emitValDef(sym, quote(mesh) + ".verticesEdge(" + quote(e) + ")")
    case DeLisztVerticesFace(e, mesh) => emitValDef(sym, quote(mesh) + ".verticesFace(" + quote(e) + ")")
    case DeLisztVerticesVertex(e, mesh) => emitValDef(sym, quote(mesh) + ".verticesVertex(" + quote(e) + ")")
    case DeLisztVerticesMesh(e) => emitValDef(sym, quote(e) + ".verticesMesh()")

    case DeLisztCtov(mesh) => emitValDef(sym, quote(mesh) + ".ctov")
    case DeLisztEtov(mesh) => emitValDef(sym, quote(mesh) + ".etov")
    case DeLisztFtov(mesh) => emitValDef(sym, quote(mesh) + ".ftov")
    case DeLisztVtov(mesh) => emitValDef(sym, quote(mesh) + ".vtov")

    case DeLisztFaceVerticesCCW(e, mesh) => emitValDef(sym, quote(mesh) + ".verticesCCW(" + quote(e) + ")")
    case DeLisztFaceVerticesCW(e, mesh) => emitValDef(sym, quote(mesh) + ".verticesCW(" + quote(e) + ")")
    case DeLisztVertex(e,i, mesh) => emitValDef(sym, quote(mesh) + ".vertex(" + quote(e) + "," + quote(i) + ")")

    case DeLisztFlipEdge(e) => emitValDef(sym, "flip(" + quote(e) + ")")
    case DeLisztFlipFace(e) => emitValDef(sym, "flip(" + quote(e) + ")")

    case DeLisztTowardsEdgeVertex(e,v, mesh) => emitValDef(sym, quote(mesh) + ".towardsEdgeVertex(" + quote(e) + "," + quote(v) + ")")
    case DeLisztTowardsFaceCell(e,c, mesh) => emitValDef(sym, quote(mesh) + ".towardsFaceCell(" + quote(e) + "," + quote(c) + ")")

    case DeLisztID(x) => emitValDef(sym, "internal(" + quote(x) + ")")

    //TODO: Why is this node here?
    case DeLisztSize(s) => emitValDef(sym, quote(s) + ".dcSize()")

    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenLanguageOps extends CGenBase {
  val IR: LanguageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

