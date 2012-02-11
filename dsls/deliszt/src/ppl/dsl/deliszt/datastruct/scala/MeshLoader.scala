package ppl.dsl.deliszt.datastruct.scala

import java.io._
import scala.io._
import net.liftweb.json._
import net.liftweb.json.JsonDSL
import net.liftweb.json.Serialization._
import java.lang.System

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
    if (!loadedLibrary) {
      try {
        System.loadLibrary("MeshLoader")
      }
      catch {
        case e: java.lang.UnsatisfiedLinkError => if (e.getMessage.indexOf("already loaded") < 0) throw e
      }
      loadedLibrary = true
    }
  }
  
  
  
  def loadMesh(filepath: String): Mesh= {
    val file = new File(filepath)
    if (!file.exists()) 
      throw new Exception("Can't find file " + filepath)
    if (filepath.endsWith(".cfg") || filepath.endsWith(".xml")) {
      val cfg = new BufferedReader(new FileReader(filepath))
      implicit val formats = ppl.dsl.deliszt.datastruct.scala.Mesh.formats
      val json = JsonParser.parse(cfg)
      if (filepath.endsWith(".cfg")) {
        case class MeshFilename(`mesh-file`: String)
        val meshFilename = json.extract[MeshFilename].`mesh-file`
        loadMesh(meshFilename)
      } else {
        val start = System.currentTimeMillis
        val m = json.extract[ppl.dsl.deliszt.datastruct.scala.Mesh]
        Log.log("Mesh loading time " + (System.currentTimeMillis - start))
        m
      }
    } else {

      Mesh.loader.loadMesh(filepath)
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
