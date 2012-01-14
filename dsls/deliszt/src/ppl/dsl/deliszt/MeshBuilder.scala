package ppl.dsl.deliszt

import reflect.Manifest
import datastruct.scala.LabelData
import ppl.dsl.deliszt.datastruct.scala.Log
import scala.virtualization.lms.internal.{GenericFatCodegen, GenerationFailedException}
import scala.virtualization.lms.common._
import ppl.delite.framework.ops._

/**
 * dynamic mesh creation
 *
 * User: wojto
 * Date: 12/31/11
 * Time: 1:35 PM
 */


trait MeshBuilderOps extends Base {

  object Log extends Log("MeshBuilder")

  object FromFileMesh {
    def apply(filename: String): Rep[Mesh] = meshGetFromFile(filename)
  }

  class MeshBuilder() {
    val meshId: Int = MeshBuilder.maxId
    MeshBuilder.maxId += 1

    newMeshBuilder(meshId)

    def addEdgeByVertex(v1: Int, v2: Int): Edge = meshAddEdgeByVertex(meshId, v1, v2)

    def addFaceByVertex(ids: Int*): Face = meshAddFaceByVertex(meshId, ids: _*)

    def addFaceByEdge(ids: Edge*): Face = meshAddFaceByEdge(meshId, ids: _*)

    def addCellByVertex(id0: Int, id1: Int, id2: Int, id3: Int): Cell = meshAddCellByVertex(meshId, id0, id1, id2, id3)

    def addCellByFace(ids: Face*): Cell = meshAddCellByFace(meshId, ids: _*)

    def setVertexPosition(v: Int, x: Double, y: Double, z: Double) = setVertexData("position", v, Array[Double](x, y, z))

    def setVertexData(name: String, v: Int, value: Any) = meshSetVertexData(meshId, name, v, value)

    def build(): Rep[Mesh] = meshBuild(meshId)
  }

  private object MeshBuilder {
    var maxId = 0
  }

  def newMeshBuilder(meshId: Int): Unit

  def meshAddEdgeByVertex(meshId: Int, id1: Int, id2: Int): Edge

  def meshAddFaceByVertex(meshId: Int, ids: Int*): Face

  def meshAddFaceByEdge(meshId: Int, ids: Edge*): Face

  def meshAddCellByVertex(meshId: Int, id0: Int, id1: Int, id2: Int, id3: Int): Cell

  def meshAddCellByFace(meshId: Int, ids: Face*): Cell

  def meshSetVertexData(meshId: Int, name: String, id: Int, value: Any): Unit

  def meshBuild(meshId: Int): Rep[Mesh]

  def meshGetFromFile(filename: String): Rep[Mesh]
}


trait MeshBuilderOpsExp extends MeshBuilderOps with BaseFatExp with EffectExp with DeliteOpsExp {

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
      var map = Map.empty[Int, List[Int]]

      def apply(key: Int) = map(key)

      def intersect(ids: Int*): Option[Int] = {
         intersectList(ids : _*) match {
           case List() => None
           case h::tail => Option(h)
         }
      }

      def intersectList(ids: Int*): List[Int] = {
        if (ids.length <= 1) {
          List()
        } else {
          var common = map.getOrElse(ids.head, List())
          for (e <- ids.tail)
            common = common.intersect(map.getOrElse(e, List()))
          common
        }
      }

      def attach(keys:Int*)(values: Int*) {
        for (key <- keys; value <- values) 
          attachCouple(key)(value)
      }

      def attachDifferent(keys:Int*)(values: Int*) {
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
              map(key) = (value :: list)
            }
          } else {
            nvalues += 1
            nkeys = Math.max(nkeys, key + 1)
            map += (key -> List(value))
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
            offset += map(i).length
          }
          i += 1
        }
        new CRSImpl(rows, values)
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

    def toMesh(): MeshStruct = {
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
      for ((k, map) <- data) {
        for ((id, value) <- map)
          (m.vertexData.data.getOrElseUpdate(k, new Array[Object](vidx + 1)))(id) = value.asInstanceOf[Object]
      }
      m
    }
    
    def combine(a2b : ListTreeMap, b2a : ListTreeMap)(aid : Int*)(bid : Int*) {
      a2b.attach(aid: _*)(bid: _*)
      b2a.attach(bid: _*)(aid: _*)
    }

    def addEdgeByVertex(id1: Int, id2: Int): Edge = {
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
      val f = new Face(etof.intersect(ids:_*).getOrElse({
        nfe += ids.length
        combine(etof, ftoe)(ids:_*)(fidx)
        val vertices = (for (id <- ids) yield etov(id)).flatten
        combine(vtof, ftov)(vertices : _*)(fidx)
        fidx += 1
        fidx - 1
      }))
      Log.log("Added new face " + f)
      f
    }

    def addFaceByVertex(ids: Int*): Face = {
      val shifted = ids.last +: ids.init
      addFaceByEdge(
        (for (
          (a, b) <- ids zip shifted
        )
        yield addEdgeByVertex(a, b)):_*)
    }

    def addCellByFace(ids: Face*): Cell = {
      val c = new Cell(ftoc.intersect(ids:_*).getOrElse({
        combine(ftoc, ctof)(ids:_*)(cidx)
        val edges = (for (id <- ids) yield ftoe(id)).flatten
        val vertices = (for (id <- ids) yield etov(id)).flatten
        combine(etoc, ctoe)(edges : _*)(cidx)
        combine(vtoc, ctov)(vertices : _*)(cidx)
        cidx += 1
        cidx - 1
      }))
      Log.log("Added new cell " + c)
      c
    }

    def addCellByVertex(id0: Int, id1: Int, id2: Int, id3: Int): Cell = {
      Log.log("Adding new cell by vertices " + id0 + " "  + id1 + " " + id2 + " " + id3)
      val vertices = List(id0, id1, id2, id3)
      val faces =
        for {a <- vertices
             b <- vertices
             c <- vertices
             if (a < b) && (b < c)}
          yield addFaceByVertex(a, b, c)
      addCellByFace(faces:_*)
    }

    def setVertexData(name: String, id: Int, value: Any) {
      Log.log("Setting vertex data for " + name + " for id " + id)
      (data.getOrElseUpdate(name, Map.empty[Int, Object]))(id) = value.asInstanceOf[Object]
    }

    override def toString() = {
      "MeshSkeleton: meshId " + meshId + ", nvertices " + (vidx+1) + ", nedges " + (eidx + 1) + ", nfaces " + (fidx + 1) + ", ncells " + (cidx + 1)
    }
  }


  def newMeshBuilder(meshId: Int): Unit = {
    meshMap.put(meshId, new MeshSkeleton(meshId))
  }

  def meshAddEdgeByVertex(meshId: Int, id1: Int, id2: Int): Edge =
    getMesh(meshId).addEdgeByVertex(id1, id2)

  def meshAddFaceByVertex(meshId: Int, ids: Int*): Face =
    getMesh(meshId).addFaceByVertex(ids: _*)

  def meshAddFaceByEdge(meshId: Int, ids: Edge*): Face =
    getMesh(meshId).addFaceByEdge(ids: _*)

  def meshAddCellByVertex(meshId: Int, id0: Int, id1: Int, id2: Int, id3: Int): Cell =
    getMesh(meshId).addCellByVertex(id0, id1, id2, id3)

  def meshAddCellByFace(meshId: Int, ids: Face*): Cell =
    getMesh(meshId).addCellByFace(ids: _*)

  def meshSetVertexData(meshId: Int, name: String, id: Int, value: Any) =
    getMesh(meshId).setVertexData(name, id, value)

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