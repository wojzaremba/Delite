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

  class MeshBuilder(nvertices: Int, nedges: Int, nfaces: Int, ncells: Int) {
    val meshId: Int = MeshBuilder.maxId
    MeshBuilder.maxId += 1

    newMeshBuilder(meshId, nvertices, nedges, nfaces, ncells)

    def addEdgeByVertex(id1: Int, id2: Int): Int = meshAddEdgeByVertex(meshId, id1, id2)

    def addFaceByEdge(ids: Int*): Int = meshAddFaceByEdge(meshId, ids: _*)

    def addCellByFace(ids: Int*): Int = meshAddCellByFace(meshId, ids: _*)

    def setVertexPosition(id: Int, x: Double, y: Double, z: Double) = setVertexData("position", id, Array[Double](x, y, z))

    def setVertexData(name: String, id: Int, value: Any) = meshSetVertexData(meshId, name, id, value)

    def build(): Rep[Mesh] = meshBuild(meshId)
  }

  private object MeshBuilder {
    var maxId = 0
  }

  def newMeshBuilder(meshId: Int, nvertices: Int, nedges: Int, nfaces: Int, ncells: Int): Unit

  def meshAddEdgeByVertex(meshId: Int, id1: Int, id2: Int): Int

  def meshAddFaceByEdge(meshId: Int, ids: Int*): Int

  def meshAddCellByFace(meshId: Int, ids: Int*): Int

  def meshSetVertexData(meshId: Int, name: String, id: Int, value: Any): Unit

  def meshBuild(meshId: Int): Rep[Mesh]

  def meshGetFromFile(filename: String): Rep[Mesh]
}


trait MeshBuilderOpsExp extends MeshBuilderOps with BaseFatExp with EffectExp with DeliteOpsExp {

  import scala.collection.mutable.Map
  import ppl.dsl.deliszt.datastruct.scala.{Mesh => MeshStruct}

  case class DeLisztMeshBuild(m: MeshStruct) extends Def[Mesh]
  case class DeLisztMeshPath(filename : String) extends Def[Mesh]

  val meshMap = Map.empty[Int, MeshSkeleton]

  def getMesh(id: Int): MeshSkeleton = {
    if (!meshMap.contains(id)) {
      throw new Exception("mesh not defined for id " + id)
    }
    meshMap(id)
  }

  class MeshSkeleton(meshId: Int, nvertices: Int, nedges: Int, nfaces: Int, ncells: Int) {

    class ListTreeMap {

      import ppl.dsl.deliszt.datastruct.scala.{CRS, CRSImpl}

      var nkeys = 0
      var nvalues = 0
      var map = Map.empty[Int, List[Int]]

      def attach(key: Int, value: Int) {
        nvalues += 1
        nkeys = Math.max(nkeys, key + 1)
        if (map.contains(key)) {
          val list = map(key)
          map(key) = (value :: list)
        } else {
          map += (key -> List(value))
        }
      }

      def apply(key: Int): List[Int] = map(key)

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

    Log.log("Creating new MeshSkeleton " + this)


    val vtov = new ListTreeMap
    val vtoe = new ListTreeMap
    val vtof = new ListTreeMap

    val etov = new ListTreeMap
    val etof = new ListTreeMap

    val ftov = new ListTreeMap
    val ftoe = new ListTreeMap

    val m = new MeshStruct()
    m.nvertices = nvertices
    m.nedges = nedges
    m.nfaces = nfaces
    m.ncells = ncells
    var vidx = 0
    var eidx = 0
    var fidx = 0
    var cidx = 0
    var nfe = 0

    def toMesh(): MeshStruct = {
      m.vtov = vtov.getCRS()
      m.vtoe = vtoe.getCRS()
      m.vtof = vtof.getCRS()

      m.etov = etov.getCRS()
      m.etof = etof.getCRS()

      m.ftov = ftov.getCRS()
      m.ftoe = ftoe.getCRS()
      m.nfe = nfe
      m
    }

    def addEdgeByVertex(id1: Int, id2: Int): Int = {
      Log.log("Adding new edge, which connects vertexes " + id1 + " " + id2)
      vtov.attach(id1, id2)
      vtov.attach(id2, id1)
      vtoe.attach(id1, eidx)
      vtoe.attach(id2, eidx)
      etov.attach(eidx, id1)
      etov.attach(eidx, id2)
      eidx += 1
      eidx - 1
    }

    def addFaceByEdge(ids: Int*): Int = {
      Log.log("Adding new face composed of edges " + ids)
      nfe += ids.length
      for (e <- ids) {
        ftoe.attach(fidx, e)
        etof.attach(e, fidx)
        val verList = etov(e)
        for (v <- verList) {
          ftov.attach(fidx, v)
          vtof.attach(v, fidx)
        }
      }
      fidx += 1
      fidx - 1
    }

    //not implemented
    def addCellByFace(ids: Int*): Int = {
      Log.log("Adding new cell composed of faces " + ids)
      cidx += 1
      cidx - 1
    }

    val vertexData = new LabelData

    def setVertexData(name: String, id: Int, value: Any) {
      Log.log("Setting vertex data for " + name + " for id " + id)
      (m.vertexData.data.getOrElseUpdate(name, new Array[Object](m.nvertices)))(id) = value.asInstanceOf[Object]
    }

    override def toString() = {
      "MeshSkeleton: meshId " + meshId + ", nvertices " + nvertices + ", nedges " + nedges + ", nfaces " + nfaces + ", ncells " + ncells
    }
  }


  def newMeshBuilder(meshId: Int, nvertices: Int, nedges: Int, nfaces: Int, ncells: Int): Unit = {
    meshMap.put(meshId, new MeshSkeleton(meshId, nvertices, nedges, nfaces, ncells))
  }

  def meshAddEdgeByVertex(meshId: Int, id1: Int, id2: Int): Int = {
    getMesh(meshId).addEdgeByVertex(id1, id2)
  }

  def meshAddFaceByEdge(meshId: Int, ids: Int*): Int = {
    getMesh(meshId).addFaceByEdge(ids: _*)
  }

  def meshAddCellByFace(meshId: Int, ids: Int*): Int = {
    getMesh(meshId).addCellByFace(ids: _*)
  }

  def meshSetVertexData(meshId: Int, name: String, id: Int, value: Any) {
    getMesh(meshId).setVertexData(name, id, value)
  }

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