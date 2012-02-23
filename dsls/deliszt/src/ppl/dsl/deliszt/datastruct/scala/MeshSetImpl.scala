package ppl.dsl.deliszt.datastruct.scala

/**
 * Created by IntelliJ IDEA.
 * User: mmwu
 * Date: 6/13/11
 * Time: 12:54 AM
 * To change this template use File | Settings | File Templates.
 */

object MeshSetImpl {
  def apply(size: Int) = new MeshSetImpl(size)
}

class MeshSetImpl(override val size : Int) extends MeshSet {
  def apply(i : Int) = {
    // TODO bounds check here?
    i
  }
}

class MeshSetImpl2(override val size : Int) extends MeshSet {
  var _length = 0
  var _data = new Array[Int](size)
  def apply(i : Int) = {
    _data(i)
  }

  def insert(i : Int, e : Int) = {
    _length = _length max i + 1
    _data(i) = e
  }

  /**
   * These are temporarily needed because they are hard-coded into DeliteOp code gen.
   */
  def unsafeSetData(xs: Array[Int], len: Int) {
    _data = xs
    _length = len
  }
  
  
}

// No zero cell
class CellSetImpl(val ncells : Int) extends MeshSet {
  override val size = ncells - 1

  def apply(i : Int) = {
    i+1
  }
}