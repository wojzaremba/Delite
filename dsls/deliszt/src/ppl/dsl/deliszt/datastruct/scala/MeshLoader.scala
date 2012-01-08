package ppl.dsl.deliszt.datastruct.scala

import java.io._
import scala.util.parsing.json._
import scala.io._

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/05/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */


object MeshLoader {

  object Log extends Log("MeshLoader")

  var loadedLibrary = false

  def init(): Unit = {
    Global.wall_start = System.currentTimeMillis
    if (!loadedLibrary) {
      try {
        System.loadLibrary("MeshLoader")
        Mesh.loader = new MeshLoader()
      }
      catch {
        case e: java.lang.UnsatisfiedLinkError => if (e.getMessage.indexOf("already loaded") < 0) throw e
      }
      loadedLibrary = true
    }
  }

  //loads mesh - from path provided in .cfg file or directly from mesh file
  def loadMesh(filepath: String): Mesh = {
    { val file = new File(filepath)
      if (!file.exists())
        None
      else
      if (filepath.endsWith(".cfg")) {
        val json = JSON.parseFull(Source.fromFile(file).mkString).get.asInstanceOf[Map[String, Any]]
        json.get("mesh-file") match {
          case Some(meshFilename: String) =>
            Log.log("Loading mesh file from " + meshFilename)
            val list : List[() => Option[File]] = List(
              () => Some(new File(meshFilename)),
              () => {
                val resource = getClass.getResource(meshFilename)
                if (resource != null)
                  Some(new File(resource.getPath))
                else None
              }, 
              () => Some(new File(file.getParent, meshFilename))
            )
            list.foldLeft[Option[Mesh]](None)((ack : Option[Mesh], elem : () => Option[File])=>
              if (!ack.isDefined)
                elem() match {
                  case Some(f:File) =>  if (f.exists()) Option(Mesh.loader.loadMesh(f.getPath)) else None
                  case _ => None
                }
              else None)
          case None => None
        }
      } else
      Option(Mesh.loader.loadMesh(filepath))
    } match {
      case Some(m:Mesh) => Log.log("ncells: " + m.ncells)
      Log.log("nedges: " + m.nedges)
      Log.log("nfaces: " + m.nfaces)
      Log.log("nvertices: " + m.nvertices)
      m
      case None => throw new RuntimeException("Loading mesh from " + filepath +  " failed!")
    }
  }

}

class MeshLoader {
  @native
  def loadMesh(file: String): Mesh = null

  def loadBoundarySet(mesh : Mesh, name: String, mo_type: Int) = {
    val bs = _loadBoundarySet(mesh, name, mo_type)

    if (bs == null) {
      throw new RuntimeException("Loading boundary set " + name + " of type " + mo_type + " failed!")
    }

    bs
  }

  @native
  def _loadBoundarySet(mesh: Mesh, name: String, mo_type: Int): BoundarySet = null
}
