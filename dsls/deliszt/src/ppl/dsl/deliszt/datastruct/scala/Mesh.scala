package ppl.dsl.deliszt.datastruct.scala

import collection.mutable.{ArrayBuffer}
import java.io._
import java.nio.ByteBuffer
import net.liftweb.json._
import net.liftweb.json.Serialization._
import scala.Array

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/05/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Mesh {
  var mesh: Mesh = null

  var maxId = 0

  val loader: MeshLoader = new MeshLoader

  val DMASK = 0x80000000
  val IMASK = 0x7FFFFFFF
  val SHIFT = 31
  val FORWARD = 1
  val REVERSE = -1

  def reversed(id: Int) = {
    (id & DMASK) != 0
  }

  def reverse(id: Int) = {
    id ^ DMASK
  }

  def internal(id: Int) = {
    id & IMASK
  }

  def flip(e: Int): Int = {
    e ^ DMASK
  }

  val OUTSIDE = 0
  val INSIDE = 1
  val HEAD = 0
  val TAIL = 1

  // Todo
  def wall_time() = 0.0

  def processor_time() = 0.0

  val formats = new Formats{
    val dateFormat = DefaultFormats.lossless.dateFormat
    override val typeHints = ShortTypeHints(List(classOf[BoundaryRange], classOf[Map[String, BoundaryRange]], classOf[CRSImpl],
      classOf[LabelData], classOf[MeshSize]))
    override val typeHintFieldName = "type"
  }
}

case class BoundaryRange(start : Int, end : Int)

case class LabelData(intData : Map[String, Array[Int]], doubleData : Map[String, Array[Double]]) {
  def apply[T](name : String)(implicit m: ClassManifest[T]) : Array[T] = {
    (m.toString match {
      case "int" | "Int" => intData(name)
      case "double" | "Double" => doubleData(name)
      //wz: we should pass everywhere fields as arrays of primitives
      case "generated.scala.Vec[Double]" =>
        val arr = doubleData(name)
        val size = arr.size / 3
        val retArray = new Array[Vec[Double]](size)
        for (i <- 0 until size)
          retArray(i) = new VecImpl[Double](Array[Double](arr(3*i), arr(3*i+1), arr(3*i+2)))
        retArray
      case x => throw new Exception("not supported type " + x)
    }).asInstanceOf[Array[T]]
  }
  
  override def toString() = {
    val s = new StringBuilder
    for (m <- List(intData, doubleData))
    for ((k, array) <- m)
      s.append(k + " -> " + array.toList.toString + "\n")
    s.toString()
  }
}

case class MeshSize(nvertices : Int, nedges : Int, nfaces : Int, ncells : Int)

case class Mesh(size : MeshSize, vtov: CRSImpl, vtoe: CRSImpl, vtof: CRSImpl, vtoc: CRSImpl, etov: CRSImpl, etof: CRSImpl,
                          etoc: CRSImpl, ftov: CRSImpl, ftoe: CRSImpl, ftoc: CRSImpl, ctov: CRSImpl, ctoe: CRSImpl, ctof: CRSImpl, ctoc: CRSImpl, vertexData: LabelData, edgeData: LabelData,
                          faceData: LabelData, cellData: LabelData, vboundaries : Map[String, BoundaryRange]) {
  val (nvertices, nedges, nfaces, ncells) = (size.nvertices, size.nedges, size.nfaces, size.ncells)
  def typeName = "Mesh"

  val id = Mesh.maxId
  Mesh.maxId += 1

  def verticesMesh: MeshSet = MeshSetImpl(nvertices)

  def boundarySetCells(name: String): BoundarySet = {
    Mesh.loader.loadBoundarySet(this, name, MeshObj.CELL_TYPE)
  }

  def boundarySetEdges(name: String): BoundarySet = {
    Mesh.loader.loadBoundarySet(this, name, MeshObj.EDGE_TYPE)
  }

  def boundarySetFaces(name: String): BoundarySet = {
    Mesh.loader.loadBoundarySet(this, name, MeshObj.FACE_TYPE)
  }

  def boundarySetVertices(name: String): BoundarySet = {
    if (vboundaries != null) {
      val a = vboundaries(name)
      ppl.dsl.deliszt.datastruct.scala.BoundarySetRangeImpl(a.start, a.end)
    } else {
      Mesh.loader.loadBoundarySet(this, name, MeshObj.VERTEX_TYPE)
    }
  }

  def verticesVertex(e: Int): MeshSet = IndexSetImpl(vtov, e)

  def verticesEdge(e: Int): MeshSet = IndexSetImpl(etov, e)

  def verticesFace(e: Int): MeshSet = IndexSetImpl(ftov, e)

  def verticesCell(e: Int): MeshSet = IndexSetImpl(ctov, e)

  def verticesCCW(e: Int): MeshSet = {
    val c = outside(e)
    if (ftoc.apply(Mesh.IMASK & e, Mesh.OUTSIDE) == (Mesh.IMASK & c)) {
      IndexSetImpl(ftov, e)
    } else {
      CWIndexSetImpl(ftov, e)
    }
  }

  def verticesCW(e: Int): MeshSet = {
    val c = outside(e)
    if (ftoc.apply(Mesh.IMASK & e, Mesh.INSIDE) == (Mesh.IMASK & c)) {
      IndexSetImpl(ftov, e)
    } else {
      CWIndexSetImpl(ftov, e)
    }
  }

  def vertex(e: Int, i: Int): Int = {
    ctov.apply(e, i)
  }

  def cellsMesh: MeshSet = new CellSetImpl(ncells)

  def cellsVertex(e: Int): MeshSet = IndexSetImpl(vtoc, e)

  def cellsEdge(e: Int): MeshSet = IndexSetImpl(etoc, e)

  def cellsFace(e: Int): MeshSet = IndexSetImpl(ftoc, e)

  def cellsCell(e: Int): MeshSet = IndexSetImpl(ctoc, e)

  def cellsCCW(e: Int): MeshSet = {
    val v = head(e)
    if (etov.apply(Mesh.IMASK & e, Mesh.HEAD) == (Mesh.IMASK & v)) {
      IndexSetImpl(etoc, e)
    } else {
      CWIndexSetImpl(etoc, e)
    }
  }

  def cellsCW(e: Int): MeshSet = {
    val v = head(e)
    if (etov.apply(Mesh.IMASK & e, Mesh.TAIL) == (Mesh.IMASK & v)) {
      IndexSetImpl(etoc, e)
    } else {
      CWIndexSetImpl(etoc, e)
    }
  }

  def edgesMesh: MeshSet = MeshSetImpl(nedges)

  def edgesVertex(e: Int): MeshSet = IndexSetImpl(vtoe, e)

  def edgesFace(e: Int): MeshSet = IndexSetImpl(ftoe, e)

  def edgesCell(e: Int): MeshSet = IndexSetImpl(ctoe, e)

  def edgesCCW(e: Int): MeshSet = {
    val c = outside(e)
    if (ftoc.apply(Mesh.IMASK & e, Mesh.OUTSIDE) == (Mesh.IMASK & c)) {
      IndexSetImpl(ftoe, e)
    } else {
      CWIndexSetImpl(ftoe, e)
    }
  }

  def edgesCW(e: Int): MeshSet = {
    val c = outside(e)
    if (ftoc.apply(Mesh.IMASK & e, Mesh.INSIDE) == (Mesh.IMASK & c)) {
      IndexSetImpl(ftoe, e)
    } else {
      CWIndexSetImpl(ftoe, e)
    }
  }

  def facesMesh: MeshSet = MeshSetImpl(nfaces)

  def facesVertex(e: Int): MeshSet = IndexSetImpl(vtof, e)

  def facesEdge(e: Int): MeshSet = IndexSetImpl(etof, e)

  def facesCell(e: Int): MeshSet = IndexSetImpl(ctof, e)

  def facesCCW(e: Int): MeshSet = {
    val v = head(e)
    if (etov.apply(Mesh.IMASK & e, Mesh.HEAD) == (Mesh.IMASK & v)) {
      IndexSetImpl(etof, e)
    } else {
      CWIndexSetImpl(etof, e)
    }
  }

  def facesCW(e: Int): MeshSet = {
    val v = head(e)
    if (etov.apply((Mesh.IMASK & e), Mesh.TAIL) == (Mesh.IMASK & v)) {
      IndexSetImpl(etoc, e)
    } else {
      CWIndexSetImpl(etoc, e)
    }
  }

  def face(e: Int, i: Int): Int = etof.apply(e, i)

  def head(e: Int): Int = etov.apply(Mesh.IMASK & e, e >>> Mesh.SHIFT)

  def tail(e: Int): Int = etov.apply(Mesh.IMASK & e, (e ^ Mesh.DMASK) >>> Mesh.SHIFT)

  def outside(e: Int): Int = ftoc.apply(Mesh.IMASK & e, e >>> Mesh.SHIFT)

  def inside(e: Int): Int = ftoc.apply(Mesh.IMASK & e, (e ^ Mesh.DMASK) >>> Mesh.SHIFT)

  def towardsEdgeVertex(e: Int, v: Int): Int = {
    val facing = (Mesh.IMASK & etov.apply(Mesh.IMASK & e, Mesh.HEAD)) == (Mesh.IMASK & v)
    if (facing) e else Mesh.flip(e)
  }

  def towardsFaceCell(e: Int, c: Int): Int = {
    val facing = (Mesh.IMASK & ftoc.apply(Mesh.IMASK & e, Mesh.HEAD)) == (Mesh.IMASK & c)
    if (facing) e else Mesh.flip(e)
  }

  def labelCell[T: ClassManifest](url: String): Array[T] = cellData[T](url)

  def labelEdge[T: ClassManifest](url: String): Array[T] = edgeData[T](url)

  def labelFace[T: ClassManifest](url: String): Array[T] = faceData[T](url)

  def labelVertex[T: ClassManifest](url: String): Array[T] = vertexData[T](url)

  // Use special CellSetImpl, don't expose 0 cell
  val cells: MeshSet = new CellSetImpl(ncells - 1)
  val edges: MeshSet = new MeshSetImpl(nedges)
  val faces: MeshSet = new MeshSetImpl(nfaces)
  val vertices: MeshSet = new MeshSetImpl(nvertices)


  //Coloring hack: remove this!
  def coloredIndexSet(filename: String): MeshSet = {
    //File READ
    val ab = new ArrayBuffer[Int]()
    val xfs = new BufferedReader(new FileReader(filename))
    var line = xfs.readLine()
    while (line != null) {
      line = line.trim()
      val idx = Integer.parseInt(line)
      ab.append(idx)
      line = xfs.readLine()
    }
    xfs.close()

    val arr = ab.toArray
    new IndexSetImpl(arr, arr.size, 0, arr.size, Mesh.FORWARD)
  }

  def getFileName() = {
    (new File("mesh" + id + ".xml")).getAbsolutePath
  }

  private def using[A <: {def close() : Unit}, B](param: A)(f: A => B): B =
    try {
      f(param)
    } finally {
      param.close()
    }


  //mesh serialization
  def serialize() = {
    implicit val formats = ppl.dsl.deliszt.datastruct.scala.Mesh.formats
    val asJson = writePretty(this)
    using(new FileWriter(getFileName())) {
      fileWriter => fileWriter.write(asJson)
    }
    getFileName()
  }

}

