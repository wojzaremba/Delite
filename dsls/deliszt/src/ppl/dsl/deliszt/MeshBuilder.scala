package ppl.dsl.deliszt

import datastruct.scala.BoundaryRange._
import datastruct.scala.LabelData._
import datastruct.scala.Mesh._
import datastruct.scala.MeshSize._
import datastruct.scala.{BoundaryRange, LabelData, CRSImpl, Log}
import reflect.Manifest
import scala.virtualization.lms.internal.{GenericFatCodegen, GenerationFailedException}
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import ppl.delite.framework.ops._
import scala.specialized
import java.io.PrintWriter
import collection.mutable.Map

/**
 * dynamic mesh creation
 *
 * User: wojto
 * Date: 12/31/11
 * Time: 1:35 PM
 */


trait MeshBuilderOps extends Base with OverloadHack {
  this: DeLiszt =>

  object Log extends Log("MeshBuilder")

  object FromFileMesh {
    def apply(filename: String): Rep[Mesh] = meshGetFromFile(filename)
  }

  class MeshBuilder() {
    val meshId: Int = MeshBuilder.incId()

    newMeshBuilder(meshId)

    def addEdge(v1: Vertex, v2: Vertex): Edge = meshAddEdgeByVertex(meshId, v1, v2)

    def addFace(ids: Vertex*): Face = meshAddFaceByVertex(meshId, ids: _*)

    def addFace(ids: Edge*)(implicit o: Overloaded1): Face = meshAddFaceByEdge(meshId, ids: _*)

    def addCell(ids: Vertex*): Cell = meshAddCellByVertex(meshId, ids: _*)

    def addCell(ids: Face*)(implicit o: Overloaded1): Cell = meshAddCellByFace(meshId, ids: _*)

    def setPosition(v: Int, x: Float, y: Float, z: Float) = setData("position", Vertex(v), x, y, z)

    def setData[T:Manifest](name: String, v: Vertex, value: T*) = meshSetData(meshId, name, v, value : _*)

    def setBoundarySet(name: String, v: Vertex) = meshSetBoundarySet(meshId, name, v)

    def build(): Rep[Mesh] = meshBuild(meshId)
  }

  private object MeshBuilder {
    var maxId : Int = 0
    def incId() : Int = {
     maxId = maxId + 1
     maxId
    } 
  }

  def newMeshBuilder(meshId: Int): Unit

  def meshAddEdgeByVertex(meshId: Int, id1: Vertex, id2: Vertex): Edge

  def meshAddFaceByVertex(meshId: Int, ids: Vertex*): Face

  def meshAddFaceByEdge(meshId: Int, ids: Edge*): Face

  def meshAddCellByVertex(meshId: Int, ids: Vertex*): Cell

  def meshAddCellByFace(meshId: Int, ids: Face*): Cell

  def meshSetData[T:Manifest](meshId: Int, name: String, id: Vertex, value: T*): Unit

  def meshSetBoundarySet(meshId: Int, name: String, v: Vertex): Unit

  def meshBuild(meshId: Int): Rep[Mesh]

  def meshGetFromFile(filename: String): Rep[Mesh]
}


trait MeshBuilderOpsExp extends MeshBuilderOps with BaseFatExp with EffectExp {
  this: DeLisztExp =>

  import scala.collection.mutable.Map
  import scala.collection.SortedSet
  import ppl.dsl.deliszt.datastruct.scala._

  case class DeLisztMeshBuild(m: Mesh) extends Def[ppl.dsl.deliszt.Mesh]

  case class DeLisztMeshPath(filename: String) extends Def[ppl.dsl.deliszt.Mesh]

  val meshMap = Map.empty[Int, MeshSkeleton]

  def getMesh(id: Int): MeshSkeleton = {
    if (!meshMap.contains(id)) {
      throw new Exception("mesh not defined for id " + id)
    }
    meshMap(id)
  }

  def newMeshBuilder(meshId: Int): Unit = {
    meshMap.put(meshId, new MeshSkeleton(meshId))
  }

  def meshAddEdgeByVertex(meshId: Int, v1: Vertex, v2: Vertex): Edge =
    getMesh(meshId).addEdgeByVertex(v1, v2)

  def meshAddFaceByVertex(meshId: Int, ids: Vertex*): Face =
    getMesh(meshId).addFaceByVertex(ids: _*)

  def meshAddFaceByEdge(meshId: Int, ids: Edge*): Face =
    getMesh(meshId).addFaceByEdge(ids: _*)

  def meshAddCellByVertex(meshId: Int, ids: Vertex*): Cell =
    getMesh(meshId).addCellByVertex(ids: _*)

  def meshAddCellByFace(meshId: Int, ids: Face*): Cell =
    getMesh(meshId).addCellByFace(ids: _*)

  def meshSetData[T:Manifest](meshId: Int, name: String, v: Vertex, value: T*) =
    getMesh(meshId).setVertexData(name, v, value : _*)

  def meshSetBoundarySet(meshId: Int, name: String, v: Vertex) =
    getMesh(meshId).setBoundarySetForVertex(name, v)

  def meshBuild(meshId: Int): Rep[ppl.dsl.deliszt.Mesh] = {
    val meshSkeleton = getMesh(meshId)
    Log.log("Building new mesh from MeshSkeleton " + meshSkeleton)
    reflectPure(DeLisztMeshBuild(meshSkeleton.toMesh()))
  }

  def meshGetFromFile(filename: String): Rep[ppl.dsl.deliszt.Mesh] = {
    Log.log("Loading mesh from file  " + filename)
    reflectPure(DeLisztMeshPath(filename))
  }
}

class MeshSkeleton(meshId: Int) {
  object Log extends Log("MeshSkeleton")

  class ListTreeMap {
    var nkeys = 0
    var nvalues = 0
    var map = Map.empty[Int, List[Int]]

    def apply(key: Int) = map.get(key) match {
      case None => List.empty[Int]
      case Some(v) => v
    }

    def intersect(ids: Int*): Option[Int] = {
      val list = intersectSets(ids: _*)
      if (list.isEmpty)
        None
      else
        Option(list.head)
    }

    def intersectSets(ids: Int*): List[Int] = {
      if (ids.length <= 1) {
        List()
      } else {
        var common = this(ids.head)
        for (e <- ids.tail)
          common = common.intersect(this(e))
        common
      }
    }

    def set(keys: Int*)(values: Int*) {
      for (key <- keys; value <- values)
        set_(key)(value)
    }

    def setDifferent(keys: Int*)(values: Int*) {
      for {key <- keys
           value <- values
           if key != value}
        set_(key)(value)
    }

    def set_(key: Int)(value: Int) {
      if (map.contains(key)) {
        val list = map(key)
        if (!list.contains(value)) {
          nvalues += 1
          nkeys = Math.max(nkeys, key + 1)
          map(key) = map(key) ++ List(value)
        }
      } else {
        nvalues += 1
        nkeys = Math.max(nkeys, key + 1)
        map += (key -> List(value))
      }
    }


    def getCRS(): CRSImpl = {
      val rows = Array.ofDim[Int](nkeys + 1)
      val values = Array.ofDim[Int](nvalues)
      var i = 0
      var offset = 0
      while (i < nkeys) {
        rows(i) = offset
        if (map.contains(i)) {
          val list = map(i).zipWithIndex
          for ((v, index) <- list) {
            values(offset + index) = v
          }
          offset += map(i).size
        }
        i += 1
      }
      rows(nkeys) = offset
      new CRSImpl(rows, values)
    }


    def swapKeys(a: Int, b: Int): (List[Int], List[Int]) = {
      val alist = this(a)
      val blist = this(b)
      map(a) = blist
      map(b) = alist
      (alist, blist)
    }

    def swapValues(a: Int, b: Int)(aset: List[Int], bset: List[Int]) {
      var bsetCopy = bset.toSet
      for (ka <- aset) {
        if (!map(ka).contains(b)) {
          map(ka) = map(ka).map(x => if (x == a) b else x)
        } else {
          map(ka) = map(ka).map(x => if (x == a) b else if (x == b) a else x)
          bsetCopy -= ka
        }
      }
      for (kb <- bsetCopy) {
        map(kb) = map(kb).map(x => if (x == b) a else x)
      }
    }
  }

  class MeshElementData {

    val booleanData = Map.empty[String, (Map[Int, Boolean], Int)]
    val intData = Map.empty[String, (Map[Int, Int], Int)]
    val floatData = Map.empty[String, (Map[Int, Float], Int)]
    val doubleData = Map.empty[String, (Map[Int, Double], Int)]
    val longData = Map.empty[String, (Map[Int, Long], Int)]

    def update[T](t : Tuple2[String, Int], value : T*)(implicit man: Manifest[T]) {
      val (name, id) = t
      Log.log("Setting vertex data for " + name + " for id " + id)
      val size = value.size
      val m = (man.toString) match {
        case "int" | "Int" => intData
        case "float" | "Float" => floatData
        case "double" | "Double" => floatData
        case "long" | "Long" => longData
        case "boolean" | "Boolean" => booleanData
        case _ => throw new Exception("Not supported format for mesh element data " + man.toString)
      }
      val map = m.asInstanceOf[Map[String, Tuple2[Map[Int, T], Int]]].getOrElseUpdate(name, (Map.empty[Int, T], size))
      if (map._2 != size) throw new Exception("Wrong number of parameters for mesh element data")
      for ((v,i)<-value.zipWithIndex)
        map._1(id*size + i) = v

    }

    def toArray[T:Manifest](map : Map[Int, T]) : Array[T] = {
      val size = (map.maxBy[Int](x => x._1))._1
      val ret = new Array[T](size + 1)
      for ((k:Int, v:T) <- map)
        ret(k) = v
      ret
    }

    def toList[T:Manifest](m : Map[String, (Map[Int, T], Int)]) : List[(String, Array[T])] = {
      m.map(v => (v._1, toArray[T](v._2._1))).toList
    }


    def toLabelData(): LabelData = {
      LabelData(toList[Boolean](booleanData), toList[Int](intData), toList[Float](floatData), toList[Double](doubleData), toList[Long](longData))
    }

    def swap[T:Manifest](m : Map[String, (Map[Int, T], Int)], a: Int, b: Int) {
      for ((_, (map, size)) <- m) {
        for (i <- 0 until size) {
          val objectA = map(a*size + i)
          val objectB = map(b*size + i)
          map(a*size + i) = objectB
          map(b*size + i) = objectA
        }
      }
    }

    def swap(a : Int, b : Int) {
      swap(intData, a, b)
      swap(floatData, a, b)
    }
  }

  Log.log("Creating new MeshSkeleton with id " + meshId)


  val vtov = new ListTreeMap
  val vtoe = new ListTreeMap
  val vtof = new ListTreeMap
  val vtoc = new ListTreeMap

  val etov = new ListTreeMap
  val etof = new ListTreeMap
  val etoc = new ListTreeMap

  val ftov = new ListTreeMap
  val ftoe = new ListTreeMap
  val ftoc = new ListTreeMap

  val ctov = new ListTreeMap
  val ctoe = new ListTreeMap
  val ctof = new ListTreeMap
  val ctoc = new ListTreeMap

  var vidx = 0
  var eidx = 0
  var fidx = 0
  // wz : for some reason cells index starts from 1
  var cidx = 1
  var nfe = 0
  val vdata = new MeshElementData
  val edata = new MeshElementData
  val fdata = new MeshElementData
  val cdata = new MeshElementData
  val boundarySet = Map.empty[String, Set[Int]]
  val boundaries = Map.empty[String, BoundaryRange]

  def swapVertex(a: Int, b: Int) {
    (vtov.swapValues(a, b) _).tupled(vtov.swapKeys(a, b))
    (etov.swapValues(a, b) _).tupled(vtoe.swapKeys(a, b))
    (ftov.swapValues(a, b) _).tupled(vtof.swapKeys(a, b))
    (ctov.swapValues(a, b) _).tupled(vtoc.swapKeys(a, b))
    vdata.swap(a,b)
  }

  def renumerate() {
    var pos = 0
    var oldPos = 0
    for (name <- boundarySet.keys) {
      val sortedSet = boundarySet(name).toList.sorted
      oldPos = pos
      Log.log("boundary set " + name + " initially is compound of vertices " + sortedSet)
      for (v <- sortedSet) {
        Log.log("swapping vertex " + v + " to " + pos)
        if (pos < v) {
          swapVertex(v, pos)
          for {
            (name2, set2) <- boundarySet
            if name2 != name
            if set2 contains pos
          } {
            boundarySet(name2) = (set2 - pos) + v
            Log.log("updated boundary set " + name2 + " to " + boundarySet(name2))
          }
        } else if (pos > v) {
          throw new Exception("Can't reorder vertices to establish boundary sets")
        }
        pos += 1
      }
      boundaries(name) = BoundaryRange(oldPos, pos)
    }
  }

  import ppl.dsl.deliszt.datastruct.scala._
  def toMesh(): Mesh = {
    renumerate()
    Mesh(MeshSize(vidx, eidx, fidx, Math.max(cidx, 2)), vtov.getCRS(), vtoe.getCRS(), vtof.getCRS(), vtoc.getCRS(), etov.getCRS(), etof.getCRS(), etoc.getCRS(),
      ftov.getCRS(), ftoe.getCRS(), ftoc.getCRS(), ctov.getCRS(), ctoe.getCRS(), ctof.getCRS(), ctoc.getCRS(), vdata.toLabelData, edata.toLabelData,
      fdata.toLabelData, cdata.toLabelData, boundaries.toMap)
  }

  def combine(a2b: ListTreeMap, b2a: ListTreeMap)(aid: Int*)(bid: Int*) {
    a2b.set(aid: _*)(bid: _*)
    b2a.set(bid: _*)(aid: _*)
  }

  def addEdgeByVertex(id1: Vertex, id2: Vertex): Edge = {
    vidx = Math.max(vidx, Math.max(id1, id2) + 1)
    vtoe.intersect(id1, id2) match {
      case Some(e) => new Edge(e, new Vertex(id1), new Vertex(id2))
      case None =>
        combine(vtoe, etov)(id1, id2)(eidx)
        vtov.setDifferent(id1, id2)(id1, id2)
        val e = new Edge(eidx, new Vertex(id1), new Vertex(id2))
        eidx += 1
        Log.log("Added new edge " + e)
        e
    }
  }

  def addFaceByEdge(ids: Edge*): Face = {
    val f = new Face(etof.intersect(ids: _*).getOrElse({
      combine(etof, ftoe)(ids: _*)(fidx)
      val vertices = (for (id <- ids) yield etov(id)).flatten
      combine(vtof, ftov)(vertices: _*)(fidx)
      fidx += 1
      fidx - 1
    }))
    Log.log("Added new face " + f)
    f
  }

  def addFaceByVertex(ids: Vertex*): Face = {
    val shifted = ids.last +: ids.init
    addFaceByEdge(
      (for (
        (a, b) <- ids zip shifted
      )
      yield addEdgeByVertex(a, b)): _*)
  }


  def addCellByFace(ids: Face*): Cell = {
    val c = new Cell(ftoc.intersect(ids: _*).getOrElse({
      combine(ftoc, ctof)(ids: _*)(cidx)
      val edges = (for (id <- ids) yield ftoe(id)).flatten
      val vertices = (for (id <- ids) yield ftov(id)).flatten
      combine(etoc, ctoe)(edges: _*)(cidx)
      combine(vtoc, ctov)(vertices: _*)(cidx)
      cidx += 1
      cidx - 1
    }))
    Log.log("Added new cell " + c)
    c
  }

  //make sense only for tetrahedron
  def addCellByVertex(ids: Vertex*): Cell = {
    Log.log("Adding new cell by vertices " + ids)
    val array = ids.toArray
    if (ids.size != 4) throw new Exception("Only support for adding tetrahedrons, please specify 4 vertices oriented as xyz")
    val faces = for (t <- List((0, 2, 1), (2, 3, 1), (2, 0, 3), (0, 1, 3)))
    yield addFaceByVertex(array(t._1), array(t._2), array(t._3))
    addCellByFace(faces: _*)
  }

  def setBoundarySetForVertex(name: String, id: Int) = {
    Log.log("set boundary set for " + name + " for id " + id)
    boundarySet(name) = boundarySet.getOrElse(name, Set.empty[Int]) + id
  }


  def setVertexData[T:Manifest](name: String, id: Int, value: T*) {
    vdata((name, id)) = value : _*
  }

  override def toString() = {
    "MeshSkeleton: meshId " + meshId + ", nvertices " + vidx + ", nedges " + eidx + ", nfaces " + fidx + ", ncells " + cidx + ", nfe " + nfe
  }
}

trait ScalaGenMeshBuilderOps extends ScalaGenBase {
  val IR: MeshBuilderOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case DeLisztMeshBuild(m) => {
        val filename = m.serialize()
        emitValDef(sym, "MeshLoader.loadMesh(\"" + filename + "\")")
      }
      case DeLisztMeshPath(filename) => emitValDef(sym, "MeshLoader.loadMesh(\"" + filename + "\")")
      case _ => super.emitNode(sym, rhs)
    }
  }
}