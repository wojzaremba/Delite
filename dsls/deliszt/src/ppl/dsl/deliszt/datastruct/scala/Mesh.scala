package ppl.dsl.deliszt.datastruct.scala

import collection.mutable.{Map, HashMap, ArrayBuffer}
import java.io._
import java.nio.ByteBuffer

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/05/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Mesh {
  var maxId = 0
  var mesh: Mesh = null

  var loader: MeshLoader = null

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
}

class LabelData {
  val data: Map[String, Array[Object]] = new HashMap[String, Array[Object]]()
  val fns: Map[String, Object => Object] = new HashMap[String, Object => Object]()
}

class Mesh {
  def typeName = "Mesh"

  val id = Mesh.maxId
  Mesh.maxId += 1

  var nvertices: Int = 0
  var nedges: Int = 0
  var nfaces: Int = 0
  var ncells: Int = 0
  var nfe: Int = 0

  var vtov: CRS = null
  var vtoe: CRS = null
  var vtof: CRS = null
  var vtoc: CRS = null

  var etov: CRS = null
  var etof: CRS = null
  var etoc: CRS = null

  var ftov: CRS = null
  var ftoe: CRS = null
  var ftoc: CRS = null

  var ctov: CRS = null
  var ctoe: CRS = null
  var ctof: CRS = null
  var ctoc: CRS = null

  val cellData = new LabelData
  val edgeData = new LabelData
  val faceData = new LabelData
  val vertexData = new LabelData

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
    Mesh.loader.loadBoundarySet(this, name, MeshObj.VERTEX_TYPE)
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

  def label[T: ClassManifest](ld: LabelData, url: String): Array[T] = {
    ld.data.get(url) match {
      case Some(data) => {
        ld.fns.get(url) match {
          // If we find a function, use it to convert
          case Some(fn) => {
            val labelData = new Array[T](data.size)
            var i = 0
            while (i < data.size) {
              labelData(i) = fn(data(i)).asInstanceOf[T]
              i += 1
            }
            labelData
          }

          // Straight up data
          case _ => data.asInstanceOf[Array[T]]
        }
      }
      case _ => null
    }
  }

  def labelCell[T: ClassManifest](url: String): Array[T] = label(cellData, url)

  def labelEdge[T: ClassManifest](url: String): Array[T] = label(edgeData, url)

  def labelFace[T: ClassManifest](url: String): Array[T] = label(faceData, url)

  def labelVertex[T: ClassManifest](url: String): Array[T] = label(vertexData, url)

  // Use special CellSetImpl, don't expose 0 cell
  val cells: MeshSet = new CellSetImpl(ncells - 1)
  val edges: MeshSet = new MeshSetImpl(nedges)
  val faces: MeshSet = new MeshSetImpl(nfaces)
  val vertices: MeshSet = new MeshSetImpl(nvertices)

  def positionToVec(p: Object): Object = {
    val a = p.asInstanceOf[Array[Double]]

    val v = DoubleVecImpl.ofSize(3)
    v(0) = a(0)
    v(1) = a(1)
    v(2) = a(2)

    v
  }

  vertexData.fns("position") = positionToVec

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


  val LISZT_MAGIC_NUMBER = 0x18111022

  /*  struct LisztHeader {
          uint32_t magic_number; //LISZT_MAGIC_NUMBER
          lsize_t nV,nE,nF,nC,nFE, nBoundaries;
          file_ptr position_table;
          file_ptr facet_edge_table;
          file_ptr boundary_set_table;
  } __attribute__((packed));

  struct FileFacetEdge { //
          struct HalfFacet {
                  id_t cell;
                  id_t vert;
          } __attribute__((packed));
          id_t face;
          id_t edge;
          HalfFacet hf[2];
  } __attribute__((packed));
  */

  def toByteArray(i: Int) = {
    val ret = new Array[Byte](4)
    ByteBuffer.wrap(ret).putInt(i);
    ret.reverse
  }

  def toByteArray(d: Double) = {
    val ret = new Array[Byte](8)
    ByteBuffer.wrap(ret).putDouble(d);
    ret.reverse
  }

  object Log extends Log("Mesh")

  val sizeOfLisztHeader = 7 * 4 + 3 * 8

  def generateFile(): String = {
    val zero = new Array[Byte](4)
    val one = new Array[Byte](4)
    one(0) = 1.toByte
    var i = 0
    val name = "out" + id + ".lmesh"
    val f = new FileOutputStream(name)
    f.write(toByteArray(LISZT_MAGIC_NUMBER))
    f.write(toByteArray(nvertices))
    f.write(toByteArray(nedges))
    f.write(toByteArray(nfaces))
    f.write(toByteArray(ncells))
    f.write(toByteArray(nfe))
    f.write(zero) //nBoundaries
    f.write(toByteArray(sizeOfLisztHeader));
    f.write(zero) // position_table
    f.write(toByteArray(sizeOfLisztHeader + 8 * 3 * nvertices));
    f.write(zero) // facet_edge_table
    f.write(zero);
    f.write(zero) // boundary_set_table
    val pos = vertexData.data.get("position")
    pos match {
      case Some(array: Array[Object]) =>
        for (i <- 0 until nvertices) {
          for (j <- 0 until 3) {
              f.write(toByteArray(array(i).asInstanceOf[Array[Double]](j)))
            }
        }
      case None =>
    }
    for (i <- 0 until nfaces) {
      for (j <- 0 until ftoe.len(i)) {
        val edge = ftoe.apply(i, j)
        val v0 = etov(edge, 0)
        val v1 = etov(edge, 1)
        Log.log("method generateFile, fem : face " + i + ", edge " + edge + ", vertex0 " + v0  + ", vertex1 " + v1)
        f.write(toByteArray(i)) //face
        f.write(toByteArray(edge)) //edge
        f.write(zero) //hf[0].cell
        f.write(toByteArray(v0)) //hf[0].vert
        f.write(one) //hf[1].cell
        f.write(toByteArray(v1)) //hf[1].vert
      }
    }

    f.close()
    (new File(name)).getAbsolutePath()
  }

}

