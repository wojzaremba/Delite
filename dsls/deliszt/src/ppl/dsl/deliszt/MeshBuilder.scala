package ppl.dsl.deliszt

import reflect.Manifest
import datastruct.scala.LabelData
import ppl.dsl.deliszt.datastruct.scala.Log
import scala.virtualization.lms.internal.{GenericFatCodegen, GenerationFailedException}
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import ppl.delite.framework.ops._

/**
 * dynamic mesh creation
 *
 * User: wojto
 * Date: 12/31/11
 * Time: 1:35 PM
 */


trait MeshBuilderOps extends Base with OverloadHack {

  object Log extends Log("MeshBuilder")

  object FromFileMesh {
    def apply(filename: String): Rep[Mesh] = meshGetFromFile(filename)
  }

  class MeshBuilder() {
    val meshId: Int = MeshBuilder.maxId
    MeshBuilder.maxId += 1

    newMeshBuilder(meshId)

    def addEdge(v1: Vertex, v2: Vertex): Edge = meshAddEdgeByVertex(meshId, v1, v2)

    def addFace(ids: Vertex*): Face = meshAddFaceByVertex(meshId, ids: _*)

    def addFace(ids: Edge*)(implicit o: Overloaded1): Face = meshAddFaceByEdge(meshId, ids: _*)

    def addCell(ids: Vertex*): Cell = meshAddCellByVertex(meshId, ids: _*)

    def addCell(ids: Face*)(implicit o: Overloaded1): Cell = meshAddCellByFace(meshId, ids: _*)

    def setVertexPosition(v: Int, x: Double, y: Double, z: Double) = setData("position", Vertex(v), Array[Double](x, y, z))

    def setData(name: String, v: Vertex, value: Any) = meshSetData(meshId, name, v, value)

    def setBoundarySet(name: String, v: Vertex) = meshSetBoundarySet(meshId, name, v)

    def build(): Rep[Mesh] = meshBuild(meshId)
  }

  private object MeshBuilder {
    var maxId = 0
  }

  def newMeshBuilder(meshId: Int): Unit

  def meshAddEdgeByVertex(meshId: Int, id1: Vertex, id2: Vertex): Edge

  def meshAddFaceByVertex(meshId: Int, ids: Vertex*): Face

  def meshAddFaceByEdge(meshId: Int, ids: Edge*): Face

  def meshAddCellByVertex(meshId: Int, ids: Vertex*): Cell

  def meshAddCellByFace(meshId: Int, ids: Face*): Cell

  def meshSetData(meshId: Int, name: String, id: Vertex, value: Any): Unit

  def meshSetBoundarySet(meshId: Int, name: String, v: Vertex): Unit

  def meshBuild(meshId: Int): Rep[Mesh]

  def meshGetFromFile(filename: String): Rep[Mesh]
}


trait MeshBuilderOpsExp extends MeshBuilderOps with BaseFatExp with EffectExp with DeliteOpsExp {

  import scala.collection.immutable.{TreeSet => Set}
  import scala.collection.mutable.Map
  import ppl.dsl.deliszt.datastruct.scala.{Mesh => MeshStruct}

  case class DeLisztMeshBuild(m: MeshStruct) extends Def[Mesh]

  case class DeLisztMeshPath(filename: String) extends Def[Mesh]

  val meshMap = Map.empty[Int, MeshSkeleton]

  def getMesh(id: Int): MeshSkeleton = {
    if (!meshMap.contains(id)) {
      throw new Exception("mesh not defined for id " + id)
    }
    meshMap(id)
  }

  class MeshSkeleton(meshId: Int) {

    class ListTreeMap {

      import ppl.dsl.deliszt.datastruct.scala.{CRS, CRSImpl}

      var nkeys = 0
      var nvalues = 0
      var map = Map.empty[Int, Set[Int]]

      def apply(key: Int) = map.get(key) match {
        case None => Set.empty[Int]
        case Some(v) => v
      }

      def intersect(ids: Int*): Option[Int] = {
        val set = intersectList(ids : _*)
        if (set.isEmpty)
          None
        else
          Option(set.head)
      }

      def intersectList(ids: Int*): Set[Int] = {
        if (ids.length <= 1) {
          Set()
        } else {
          var common = this(ids.head)
          for (e <- ids.tail)
            common = common.intersect(this(e))
          common
        }
      }

      def attach(keys: Int*)(values: Int*) {
        for (key <- keys; value <- values)
          attachCouple(key)(value)
      }

      def attachDifferent(keys: Int*)(values: Int*) {
        for {key <- keys
             value <- values
             if key != value}
          attachCouple(key)(value)
      }

      def attachCouple(key: Int)(value: Int) {
        if (map.contains(key)) {
          val list = map(key)
          if (!list.contains(value)) {
            nvalues += 1
            nkeys = Math.max(nkeys, key + 1)
            map(key) += value
          }
        } else {
          nvalues += 1
          nkeys = Math.max(nkeys, key + 1)
          map += (key -> Set(value))
        }
      }


      def getCRS(): CRS = {
        val rows = Array.ofDim[Int](nkeys)
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
        new CRSImpl(rows, values)
      }

      def swapKeys(a: Int, b: Int): (Set[Int], Set[Int]) = {
        val alist = this(a)
        val blist = this(b)
        map(a) = blist
        map(b) = alist
        (alist, blist)
      }

      def swapValues(a: Int, b: Int)(aset: Set[Int], bset: Set[Int]) {
        var bsetCopy = bset
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

    val m = new MeshStruct()
    var vidx = 0
    var eidx = 0
    var fidx = 0
    var cidx = 0
    var nfe = 0
    val vertexData = new LabelData
    val data = Map.empty[String, Map[Int, Object]]
    val boundarySet = Map.empty[String, Set[Int]]
    val boundaries = Map.empty[String, (Int, Int)]

    def swapVertex(a: Int, b: Int) {
      (vtov.swapValues(a, b) _).tupled(vtov.swapKeys(a, b))
      (etov.swapValues(a, b) _).tupled(vtoe.swapKeys(a, b))
      (ftov.swapValues(a, b) _).tupled(vtof.swapKeys(a, b))
      (ctov.swapValues(a, b) _).tupled(vtoc.swapKeys(a, b))
      for ((name, map) <- data) {
        val objectA = map(a)
        val objectB = map(b)
        map(a) = objectB
        map(b) = objectA
      }
    }

    def renumerate() {
      var pos = 0
      var oldPos = 0
      for ((name, set) <- boundarySet) {
        oldPos = pos
        for (v <- set) {
          Log.log("swapping vertex " + v + " to " + pos)
          if (pos < v) {
            swapVertex(v, pos)
            pos += 1
          } else if (pos == v) {
            pos += 1
          } else {
            throw new Exception("Can't reorder vertices to establish boundary sets")
          }
        }
        boundaries(name) = (oldPos, pos)
      }
    }

    def toMesh(): MeshStruct = {
      renumerate()
      m.nvertices = vidx
      m.nedges = eidx
      m.nfaces = fidx
      m.ncells = Math.max(2, cidx)
      m.vtov = vtov.getCRS()
      m.vtoe = vtoe.getCRS()
      m.vtof = vtof.getCRS()
      m.vtoc = vtof.getCRS()

      m.etov = etov.getCRS()
      m.etof = etof.getCRS()
      m.etoc = etof.getCRS()

      m.ftov = ftov.getCRS()
      m.ftoe = ftoe.getCRS()
      m.ftoc = ftoe.getCRS()

      m.ctov = ftoe.getCRS()
      m.ctoe = ftoe.getCRS()
      m.ctof = ftoe.getCRS()
      m.ctoc = ftoe.getCRS()
      m.nfe = nfe
      m.boundaries = boundaries
      for ((k, map) <- data) {
        for ((id, value) <- map)
          (m.vertexData.data.getOrElseUpdate(k, new Array[Object](vidx + 1)))(id) = value.asInstanceOf[Object]
      }
      m
    }

    def combine(a2b: ListTreeMap, b2a: ListTreeMap)(aid: Int*)(bid: Int*) {
      a2b.attach(aid: _*)(bid: _*)
      b2a.attach(bid: _*)(aid: _*)
    }

    def addEdgeByVertex(id1: Vertex, id2: Vertex): Edge = {
      vidx = Math.max(vidx, Math.max(id1, id2) + 1)
      val list = vtoe.intersectList(id1, id2)
      list.find(e=>etov.map.get(e) == List(id1, id2)) match {
        case Some(e) => new Edge(e, new Vertex(id1), new Vertex(id2))
        case None =>
          combine(vtoe, etov)(id1, id2)(eidx)
          vtov.attachDifferent(id1, id2)(id1, id2)
          val e = new Edge(eidx, new Vertex(id1), new Vertex(id2))
          eidx += 1
          Log.log("Added new edge " + e)
          e
      }
    }

    def addFaceByEdge(ids: Edge*): Face = {
      val f = new Face(etof.intersect(ids: _*).getOrElse({
        nfe += ids.length
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
        val vertices = (for (id <- ids) yield etov(id)).flatten
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
      val faces =
        for {a <- ids
             b <- ids
             c <- ids
             if (a < b) && (b < c)}
        yield addFaceByVertex(a, b, c)
      addCellByFace(faces: _*)
    }

    def setBoundarySetForVertex(name: String, id: Int) = {
      boundarySet(name) = boundarySet.getOrElse(name, Set.empty[Int]) + id
    }

    def setVertexData(name: String, id: Int, value: Any) {
      Log.log("Setting vertex data for " + name + " for id " + id)
      (data.getOrElseUpdate(name, Map.empty[Int, Object]))(id) = value.asInstanceOf[Object]
    }

    override def toString() = {
      "MeshSkeleton: meshId " + meshId + ", nvertices " + (vidx + 1) + ", nedges " + (eidx + 1) + ", nfaces " + (fidx + 1) + ", ncells " + (cidx + 1)
    }
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

  def meshSetData(meshId: Int, name: String, v: Vertex, value: Any) =
    getMesh(meshId).setVertexData(name, v, value)

  def meshSetBoundarySet(meshId: Int, name: String, v: Vertex) =
    getMesh(meshId).setBoundarySetForVertex(name, v)

  def meshBuild(meshId: Int): Rep[Mesh] = {
    val meshSkeleton = getMesh(meshId)
    Log.log("Building new mesh from MeshSkeleton " + meshSkeleton)
    reflectPure(DeLisztMeshBuild(meshSkeleton.toMesh()))
  }

  def meshGetFromFile(filename: String): Rep[Mesh] = {
    Log.log("Loading mesh from file  " + filename)
    reflectPure(DeLisztMeshPath(filename))
  }
}