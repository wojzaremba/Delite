package ppl.dsl.deliszt.meshset

import java.io.PrintWriter
import ppl.dsl.deliszt._

import ppl.delite.framework.DSLType
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.dsl.deliszt.{DeLisztExp, DeLiszt}

import ppl.dsl.deliszt.datastruct.scala.Mesh

trait MeshSetOps extends DSLType with Variables {
  this: DeLiszt =>

  implicit def repMeshSetToMeshSetOps[MO <: MeshObj:Manifest](x: Rep[MeshSet[MO]]) = new meshSetOpsCls[MO](x)
  implicit def varToMeshSetOps[MO <: MeshObj:Manifest](x: Var[MeshSet[MO]]) = new meshSetOpsCls[MO](readVar(x))

  /**
   * This class defines the public interface for the Field[T] class.
   */

  class meshSetOpsCls[MO<:MeshObj:Manifest](x: Rep[MeshSet[MO]]) {
    def foreach(block: Rep[MO] => Rep[Unit]) = meshset_foreach(x, block)
    def filter(block: Rep[MO] => Rep[Boolean]) = meshset_filter(x, block)
    def mapReduce[A:Manifest:Arith](block: Rep[MO] => Rep[A]) = meshset_mapReduce(x, block)    
    def apply(i : Rep[Int]) = meshset_apply(x, i)
    def update(i : Rep[Int], e : Rep[MO]) = meshset_update(x, i, e)
  }


  def meshset_foreach[MO<:MeshObj:Manifest](x: Rep[MeshSet[MO]], block: Rep[MO] => Rep[Unit]) : Rep[Unit]
  def meshset_filter[MO<:MeshObj:Manifest](x: Rep[MeshSet[MO]], block: Rep[MO] => Rep[Boolean]) : Rep[MeshSet[MO]]
  def meshset_mapReduce[MO<:MeshObj:Manifest, A:Manifest:Arith](x: Rep[MeshSet[MO]], block: Rep[MO] => Rep[A]) : Rep[A]

  def meshset_new[MO<:MeshObj:Manifest](size : Rep[Int]) : Rep[MeshSet[MO]]
  def meshset_apply[MO<:MeshObj:Manifest](x: Rep[MeshSet[MO]], i : Rep[Int]) : Rep[MO]
  def meshset_update[MO<:MeshObj:Manifest](x: Rep[MeshSet[MO]], i : Rep[Int], e : Rep[MO]) : Rep[Unit]
}

trait MeshSetOpsExp extends MeshSetOps with VariablesExp with BaseFatExp {
  this: DeLisztExp =>
  
  //wz: I've got to merge it with mergeset
  case class MeshSetNew2[MO<:MeshObj:Manifest](size : Exp[Int]) extends Def[MeshSet[MO]]
  case class MeshSetApply[MO<:MeshObj:Manifest](x: Exp[MeshSet[MO]], i : Exp[Int]) extends Def[MO]
  case class MeshSetUpdate[MO<:MeshObj:Manifest](x: Exp[MeshSet[MO]], i : Exp[Int], e : Rep[MO]) extends Def[Unit]
  
  
  ////////////////////////////////
  // implemented via delite ops

  def meshset_new[MO<:MeshObj:Manifest](size : Exp[Int]) = reflectMutable(MeshSetNew2[MO](size))
  def meshset_apply[MO<:MeshObj:Manifest](x: Exp[MeshSet[MO]], i : Exp[Int]) = reflectPure(MeshSetApply(x, i))
  def meshset_update[MO<:MeshObj:Manifest](x: Exp[MeshSet[MO]], i : Exp[Int], e : Exp[MO]) = reflectWrite(x)(MeshSetUpdate(x, i, e))

  case class MeshSetForeach[MO<:MeshObj:Manifest](x: Exp[MeshSet[MO]], func: Exp[MO] => Exp[Unit])
    extends DeliteOpForeach[MO] {

    def sync = n => List()
    val in = copyTransformedOrElse(_.in)(x)
    val size = copyTransformedOrElse(_.size)(x.size)
  }

  case class MeshSetFilter[MO<:MeshObj:Manifest](in: Exp[MeshSet[MO]], mesh : Exp[Mesh],   cond: Exp[MO] => Exp[Boolean])
    extends DeliteOpFilter[MO, MO, MeshSet[MO]] {

    val size = in.size
    def func = e => e
    def alloc = meshset_new(in.size)

    def m = manifest[MO]

  }
  
  case class MeshSetMapReduce[MO<:MeshObj:Manifest, A:Manifest:Arith](in: Exp[MeshSet[MO]], map : Rep[MO] => Rep[A])
    extends DeliteOpMapReduce[MO, A] {
    def reduce = (a, b) => a + b
    def m = manifest[A]
    def a = implicitly[Arith[A]]   
    val size = copyTransformedOrElse(_.size)(in.length)
    val zero = a.empty
  }  

  // e comes in as an internal id of a face
  case class NestedMeshSetForeach[MO<:MeshObj:Manifest](in: Exp[MeshSet[MO]], crs: Exp[CRS], e: Exp[Int], block: Exp[MO] => Exp[Unit]) extends Def[Unit] {
    def mm = manifest[MO]
    val mo = fresh[MO]
    
    val body = reifyEffects(block(mo))
  }
  
  // e comes in as an internal id of a face
  case class DirectedNestedMeshSetForeach[E<:MeshObj:Manifest,MO<:MeshObj:Manifest](in: Exp[MeshSet[MO]], crs: Exp[CRS], dir: Int, e: Exp[E], block: Exp[MO] => Exp[Unit]) extends Def[Unit] {
    def mm = manifest[MO]
    val mo = fresh[MO]
    
    val eid = ID(e)
    val body = reifyEffects(block(mo))
  }
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    // DOES NOT USE IN, only the index
    case f@NestedMeshSetForeach(m, crs, e, fn) => f.mo :: syms(crs):::syms(e):::syms(f.body)
    case f@DirectedNestedMeshSetForeach(m, crs, dir, e, fn) => f.mo :: syms(crs):::syms(e):::syms(f.eid):::syms(f.body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case f@NestedMeshSetForeach(m, crs, e, fn) => f.mo :: effectSyms(crs):::effectSyms(e):::effectSyms(f.body)
    case f@DirectedNestedMeshSetForeach(m, crs, dir, e, fn) => f.mo :: effectSyms(crs):::effectSyms(e):::effectSyms(f.eid):::effectSyms(f.body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case f@NestedMeshSetForeach(m, crs, e, fn) => (f.mo,1.0) :: freqNormal(crs) ++ freqNormal(e) ++ freqHot(f.body)
    case f@DirectedNestedMeshSetForeach(m, crs, dir, e, fn) => (f.mo,1.0) :: freqNormal(crs) ++ freqNormal(e) ++ freqNormal(f.eid) ++ freqHot(f.body)
    case _ => super.symsFreq(e)
  }
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    // Doesn't work wtf
    // case f@NestedMeshSetForeach(crs,e,func) => reflectPure(NestedMeshSetForeach(f(crs),f(e),f(func))(f.m))(mtype(manifest[A]))
    case Reflect(e@NestedMeshSetForeach(m,crs,i,func), u, es) => reflectMirrored(Reflect(NestedMeshSetForeach(f(m),f(crs),f(i),f(func)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MeshSetForeach(a,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MeshSetForeach(f(a),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MeshSetFilter(a,b,c), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MeshSetFilter(f(a),f(b),f(c)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    //case Reflect(e@MeshSetMapReduce(a,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MeshSetMapReduce(f(a),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
  
  /////////////////////
  // object interface

  /////////////////////
  // class interface
  
  def meshset_foreach[MO<:MeshObj:Manifest](x: Exp[MeshSet[MO]], block: Exp[MO] => Exp[Unit]) : Exp[Unit] = {
    val t = MeshSetForeach(x,block)
    x match {
      // Why do I have to cast? I hate you
      /*
      case Def(DeLisztCellsCell(e,m)) => nms_foreach(x, crs_ctoc(m), e, block)
      case Def(DeLisztCellsEdge(e,m)) => nms_foreach(x, crs_etoc(m), e, block)
      case Def(DeLisztCellsFace(e,m)) => nms_foreach(x, crs_ftoc(m), e, block)
      case Def(DeLisztCellsVertex(e,m)) => nms_foreach(x, crs_vtoc(m), e, block)
      case Def(DeLisztVerticesCell(e,m)) => nms_foreach(x, crs_ctov(m), e, block)
      case Def(DeLisztVerticesEdge(e,m)) => nms_foreach(x, crs_etov(m), e, block)
      case Def(DeLisztVerticesFace(e,m)) => nms_foreach(x, crs_ftov(m), e, block)
      case Def(DeLisztVerticesVertex(e,m)) => nms_foreach(x, crs_vtov(m), e, block)
      case Def(DeLisztEdgesCell(e,m)) => nms_foreach(x, crs_ctoe(m), e, block)
      case Def(DeLisztEdgesFace(e,m)) => nms_foreach(x, crs_ftoe(m), e, block)
      case Def(DeLisztEdgesVertex(e,m)) => nms_foreach(x, crs_vtoe(m), e, block)
      case Def(DeLisztFacesEdge(e,m)) => nms_foreach(x, crs_etof(m), e, block)
      case Def(DeLisztFacesCell(e,m)) => nms_foreach(x, crs_ctof(m), e, block)
      case Def(DeLisztFacesVertex(e,m)) => nms_foreach(x, crs_vtof(m), e, block)
      case Def(DeLisztEdgeFacesCCW(e,m)) => dnms_foreach(x, crs_etof(m), Mesh.HEAD, e, block)
      case Def(DeLisztEdgeFacesCW(e,m)) => dnms_foreach(x, crs_etof(m), Mesh.TAIL, e, block)
      case Def(DeLisztFaceEdgesCCW(e,m)) => dnms_foreach(x, crs_ftoe(m), Mesh.OUTSIDE, e, block)
      case Def(DeLisztFaceEdgesCW(e,m)) => dnms_foreach(x, crs_ftoe(m), Mesh.INSIDE, e, block)
      case Def(DeLisztEdgeCellsCCW(e,m)) => dnms_foreach(x, crs_etof(m), Mesh.HEAD, e, block)
      case Def(DeLisztEdgeCellsCW(e,m)) => dnms_foreach(x, crs_etof(m), Mesh.TAIL, e, block)
      case Def(DeLisztFaceVerticesCCW(e,m)) => dnms_foreach(x, crs_ftov(m), Mesh.OUTSIDE, e, block)
      case Def(DeLisztFaceVerticesCW(e,m)) => dnms_foreach(x, crs_ftov(m), Mesh.INSIDE, e, block)
      */
      case _ => reflectEffect(t, summarizeEffects(t.body.asInstanceOf[DeliteForeachElem[MO]].func).star)
    }
  }
  
  def meshset_mapReduce[MO<:MeshObj:Manifest, A:Manifest:Arith](x: Rep[MeshSet[MO]], block: Rep[MO] => Rep[A]) = {
    val t = MeshSetMapReduce(x, block) 
    reflectEffect(t, summarizeEffects(t.body).star)
  }

  def meshset_filter[MO<:MeshObj:Manifest](x: Exp[MeshSet[MO]], block: Exp[MO] => Exp[Boolean]) : Rep[MeshSet[MO]] = {
    val tp2 = findDefinition(x.asInstanceOf[Sym[_]]).get
    val mesh = tp2.rhs match {
      case m:{val mesh : Exp[Mesh]} => m.mesh
      case _ => throw new Exception("Can't find corresponding mesh " + tp2.rhs.toString() + " " )
    }
    val t = MeshSetFilter(x, mesh, block)
    reflectEffect(t, summarizeEffects(t.body).star)
  }
  
  def nms_foreach[MO<:MeshObj:Manifest](x: Exp[MeshSet[MO]], crs: Exp[CRS], e: Exp[Int], block: Exp[MO] => Exp[Unit]) : Exp[Unit] = {
    val t = NestedMeshSetForeach(x, crs, e, block)
    
    reflectEffect(t, summarizeEffects(t.body).star)
  }
  
  def dnms_foreach[E<:MeshObj:Manifest,MO<:MeshObj:Manifest](x: Exp[MeshSet[MO]], crs: Exp[CRS], dir: Int, e: Exp[E], block: Exp[MO] => Exp[Unit]) : Exp[Unit] = {
    val t = DirectedNestedMeshSetForeach(x, crs, dir, e, block)
    
    reflectEffect(t, summarizeEffects(t.body).star)
  }
}

trait MeshSetOpsExpOpt extends MeshSetOpsExp {
  this: DeLisztExp =>
}

trait ScalaGenMeshSetOps extends ScalaGenBase {
  val IR: MeshSetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {

      case MeshSetNew2(size) => emitValDef(sym, "new MeshSetImpl2(" + quote(size) + ")" )
      case MeshSetApply(x, i) => emitValDef(sym, quote(x) + "(" + quote(i) + ")" )
      case MeshSetUpdate(x, i, e) => emitValDef(sym, "{" + quote(x) + "(" + quote(i) + ") = " + quote(e) + "}")


      case f@NestedMeshSetForeach(m,crs,i,body) => {
        stream.println("val " + quote(sym) + " = { // Begin nested foreach " + sym.id)
          stream.println("var i = " + quote(crs) + ".row(" + quote(i) + ")")
          stream.println("val end = " + quote(crs) + ".row(" + quote(i) + "+1)")
          stream.println("while (i < end) {")
            stream.println("val " + quote(f.mo) + " = " + quote(crs) + ".values(i)") 
            emitBlock(f.body)
            stream.println("i += 1")
            stream.print(quote(getBlockResult(f.body)))
        stream.println("}} // End nested foreach " + sym.id)
      }
      
      case f@DirectedNestedMeshSetForeach(m,crs,dir,i,body) => {
        stream.println("val " + quote(sym) + " = { // Begin directed foreach " + sym.id)
          stream.println("if((" + quote(i) + " >>> generated.scala.Mesh.SHIFT) == " + dir + ") {")
            stream.println("var i = " + quote(crs) + ".row(" + quote(f.eid) + ")")
            stream.println("val end = " + quote(crs) + ".row(" + quote(f.eid) + "+1)")
            stream.println("while (i < end) {")
              stream.println("val " + quote(f.mo) + " = " + quote(crs) + ".values(i)") 
              emitBlock(f.body)
              stream.println("i += 1")
              stream.print(quote(getBlockResult(f.body)))
          stream.println("}} else {")
            stream.println("var i = " + quote(crs) + ".row(" + quote(f.eid) + "+1)-1")
            stream.println("val end = " + quote(crs) + ".row(" + quote(f.eid) + ")-1")
            stream.println("while (i > end) {")
              stream.println("val " + quote(f.mo) + " = " + quote(crs) + ".values(i) ^ generated.scala.Mesh.DMASK") 
              emitBlock(f.body)
              stream.println("i -= 1")
              stream.print(quote(getBlockResult(f.body)))
        stream.println("}}} // End directed foreach " + sym.id)
      }
    
      // these are the ops that call through to the underlying real data structure
      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenMeshSetOps extends CudaGenBase {
  val IR: MeshSetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenMeshSetOps extends CGenBase {
  val IR: MeshSetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
