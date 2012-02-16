package ppl.dsl.deliszt.datastruct.scala


object VecSpecialized {

  private def vnseq(size : Int) = (0 until size).map(" v" + _).mkString(",")

  def remap(t : String) : String = {
    
  }

  def getFileType(t : String, size : Int) : String = t + "Vec" + size + "Impl"
  def getContent(t : String, size : Int) : String = """
package ppl.dsl.deliszt.datastruct.scala

import ppl.dsl.optila.datastruct.scala.DenseVector

object """ + t + """Vec""" + size + """Impl {
  def apply(""" + (0 until size).map(" v" + _ + ": " + t).mkString(",") + """) = {
    new """ + t + """Vec""" + size + """Impl[T](""" + vnseq + """)
  }
}

class """ + t + """Vec""" + size + """Impl(""" + (0 until size).map("var v" + _ + ": " + t).mkString(",") + """) extends DenseVector[""" + t + """](""" + size + """, true) 
with Vec[""" + t + """] with Copyable {
 
  override val size = """ + size + """
  _data = Array[""" + t + """](v0, v1, v2)

  override def unsafeSetData(xs: Array[""" + t + """], len: Int) {
    """ + (0 until size).map(x => "update(" + x + ", xs(" + x + "))").mkString(";") + """
  }



  // ! unsafe comments are intentional - used as wildcards when specializing datastructures !

  override def apply(n : Int) = {
     _data(n)
  //  /*unsafe.UnsafeAccessor.unsafe.getT(this, 16 + n*UNSAFE_SIZE)*/ v0
  }
  
  def update(n : Int, v : """ + t + """) = {
     _data(n) = v
  //  /*unsafe.UnsafeAccessor.unsafe.putT(this, 16 + n*UNSAFE_SIZE, v)*/
  }
  
  override def Clone = {
    new """ + t + """Vec""" + size + """Impl(""" + vnseq + """)
  }
  
  def copy() = Clone
  
  override def toString() = {
    """ + t + """\"Vec""" + size + """Impl[" + _length + "](""" + vnseq + """)"
  }
} """

}
