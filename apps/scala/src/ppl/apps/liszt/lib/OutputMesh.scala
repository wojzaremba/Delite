package ppl.apps.liszt.lib

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._
import java.io._
import java.nio.ByteBuffer

/**
 * User: woj.zaremba
 * Date: 01/13/11
 */

trait OutputMesh extends DeLisztApplication{
this: Libs =>

  /*
  ply
format ascii 1.0
comment author: Greg Turk
comment object: another cube
element vertex 8
property float x
property float y
property float z
property uchar red                   { start of vertex color }
property uchar green
property uchar blue
element face 7
property list uchar int vertex_index  { number of vertices for each face }
element edge 5                        { five edges in object }
property int vertex1                  { index to first vertex of edge }
property int vertex2                  { index to second vertex }
property uchar red                    { start of edge color }
property uchar green
property uchar blue
end_header
0 0 0 255 0 0                         { start of vertex list }
0 0 1 255 0 0
0 1 1 255 0 0
0 1 0 255 0 0
1 0 0 0 0 255
1 0 1 0 0 255
1 1 1 0 0 255
1 1 0 0 0 255
3 0 1 2                           { start of face list, begin with a triangle }
3 0 2 3                           { another triangle }
4 7 6 5 4                         { now some quadrilaterals }
4 0 4 5 1
4 1 5 6 2
4 2 6 7 3
4 3 7 4 0
0 1 255 255 255                   { start of edge list, begin with white edge }
1 2 255 255 255
2 3 255 255 255
3 0 255 255 255
2 0 0 0 0                         { end with a single black line }
   */
  
  import scala.collection.immutable.{List => UList}

  object OutputMesh {
    def apply(mesh: Rep[Mesh]) = apply[Int](mesh)
    
    def apply[T : Numeric : Manifest](mesh: Rep[Mesh], fields: Seq[Function1[Rep[Vertex], Rep[T]]]) =
      apply[T](mesh, fields:_*)

    def apply[T : Numeric : Manifest](mesh: Rep[Mesh], fields: Function1[Rep[Vertex], Rep[T]]*)(implicit o: Overloaded6) : Unit = {
      val f = SyncedFile("output.ply")
      f.writeln("ply")
      f.writeln("format ascii 1.0")

      f.writeln("element vertex ", vertices(mesh).size)
      for (cord <- UList("x", "y", "z"))
        f.writeln("property float ", cord)
      for (col <- UList("red", "green", "blue"))
        f.writeln("property uchar ", col)
      f.writeln("element face ", faces(mesh).size)
      f.writeln("property list uchar int vertex_index")      
      f.writeln("element edge ", edges(mesh).size)
      for (i <- UList("1", "2"))
        f.writeln("property int vertex", i)
      f.writeln("end_header")
      for (v <- vertices(mesh)) {
        f.write(v.x, " ", v.y, " ", v.z)
	for (field <- fields) 
          f.write(" ", field(v).toInt)
        for (i <- 0 until (3-fields.size))
          f.write(" 0")
        f.writeln()
      }
      for (face <- faces(mesh)) {
        f.write(vertices(face).size)
        for (v <- vertices(face)) {
          f.write(" ", ID(v))
        }
        f.writeln()        
      }
      for (e <- edges(mesh)) {
        f.writeln(ID(head(e)), " ", ID(tail(e)))
      }
      f.close()
    }
  }

}
