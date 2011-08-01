package ppl.delite.runtime.graph

/**
 * Author: Kevin J. Brown
 * Date: Nov 9, 2010
 * Time: 3:17:02 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object TestKernel1a {
  def apply() = println("op1a")
}

object TestKernel1b {
  def apply() = println("op1b")
}

object TestKernel1c {
  def apply() = println("op1c")
}

object TestKernel1d {
  def apply() = println("op1d")
}

object TestKernel2a {
  def apply() = println("op2a")
}

object TestKernel2b {
  def apply() = println("op2b")
}

object TestKernel2c {
  def apply() = println("op2c")
}

object TestKernel2d {
  def apply() = println("op2d")
}

object TestKernel3 {
  def apply() = println("op3")
}

object TestKernelBegin {
  def apply() = {
    val res = new ArrayColl[Int](0, 10)
    for (i <- 0 until 10) res.dcUpdate(i, i)
    res
  }
}

object TestKernelPrint {
  def apply(result: Int) { println(result) }
}

object TestKernelEnd {
  def apply(out: ArrayColl[Int]) = {
    print("[ ")
    for (e <- out) print(e + " ")
    print("]\n")
  }
}

/* trait DeliteCollection[T] {
  def size: Int
  def dcApply(idx: Int): T
  def dcUpdate(idx: Int, x: T)
}

trait DeliteChunkableCollection[T] extends DeliteCollection[T] with Serializable {
  def index(logical: Int): Int = logical - startIndex //logical to physical index mapping
  def startIndex: Int
  def endIndex: Int
  //def size = endIndex - startIndex
  abstract override def dcApply(idx: Int) = super.dcApply(index(idx))
  abstract override def dcUpdate(idx: Int, x: T) = super.dcUpdate(index(idx), x)
}

class ArrayCollBase[T: Manifest](val length: Int) extends DeliteCollection[T] {
  val _data = new Array[T](length)
  def foreach[U](f: T => U) = _data.foreach[U](f)
  def size = length

  def apply(idx: Int): T = dcApply(idx)
  def dcApply(idx: Int) = _data(idx)

  def update(idx: Int, x: T) = dcUpdate(idx, x)
  def dcUpdate(idx: Int, x: T) { _data(idx) = x }
}

class ArrayColl[T: Manifest](val startIndex: Int, val endIndex: Int) extends ArrayCollBase[T](endIndex - startIndex) with DeliteChunkableCollection[T]
*/
abstract class DeliteCollection[T: Manifest] extends Serializable {
  private val _data = new Array[T](size)

  def startIndex: Int
  def endIndex: Int
  def size = endIndex - startIndex
  def mapIndex(logical: Int) = logical - startIndex

  final def dcApply(idx: Int): T = _data(mapIndex(idx))
  final def dcUpdate(idx: Int, x: T) { _data(mapIndex(idx)) = x }
}

class ArrayColl[T: Manifest](val startIndex: Int, val endIndex: Int) extends DeliteCollection[T] {
  def apply(idx: Int) = dcApply(idx)
  def update(idx: Int, x: T) = dcUpdate(idx, x)
}

class Activation[T] {
  var out: T = null.asInstanceOf[T]
}

abstract class DeliteOpMultiLoop[T] {
  def size: Int
  def alloc: Activation[T]
  def init(act: Activation[T], index: Int): Activation[T]
  def process(act: Activation[T], index: Int)
  def combine(act: Activation[T], rhs: Activation[T])

  //def split(numChunks: Int, data: Seq[DeliteCollection[_]]): Seq[Seq[DeliteCollectionChunk[_]]] //inputs //TODO: is there a common type signature?
  //def update(data: Seq[Seq[DeliteCollection[_]]]) //mutated inputs //TODO: Delite can always provide this using dcUpdate?
  //def combine(data: Seq[Seq[DeliteCollectionChunk[_]]]): Seq[DeliteCollection[_]] //outputs
}

object TestKernelMap {
  def apply(in0: ArrayColl[Int]) = {
    new DeliteOpMultiLoop[ArrayColl[Int]] {
      def size = in0.length

      def alloc = {
        val act = new Activation[ArrayColl[Int]]
        act.out = new ArrayColl[Int](in0.startIndex, in0.endIndex)
        act
      }

      def init(act: Activation[ArrayColl[Int]], index: Int) = {
        process(act, index)
        act
      }

      def process(act: Activation[ArrayColl[Int]], index: Int) {
        act.out(index) = in0(index) * 2
      }

      def combine(act: Activation[ArrayColl[Int]], rhs: Activation[ArrayColl[Int]]) { }
    }
  }

  def split_in0(numChunks: Int, in0: ArrayColl[Int]): Seq[ArrayColl[Int]] = {
    for (idx <- 0 until numChunks) yield {
      val start = (in0.size * idx) / numChunks
      val end = (in0.size * (idx+1)) / numChunks
      val chunk = new ArrayColl[Int](start, end)
      for (j <- start until end) {
        chunk(j) = in0(j)
      }
      chunk
    }
  }

  //def split_in1(numChunks: Int, in1: T): Seq[T]

  def combine_out(out: ArrayColl[Int]*): ArrayColl[Int] = {
    val length = out.map(_.size).reduceLeft(_ + _)
    val start = out(0).startIndex
    val full = new ArrayColl[Int](start, start + length)
    for (chunk <- out) {
      for (i <- chunk.startIndex until chunk.endIndex) {
        full(i) = chunk(i)
      }
    }
    full
  }
}

/*
object TestKernelReduce {
  def apply(in0: ArrayColl[Int]): DeliteOPReduce[Int] = {
    new DeliteOPReduce[Int] {
      def in = in0
      def reduce(r1: Int, r2: Int) = r1 + r2
    }
  }
}

object TestKernelImmutableZip {
  def apply(in0: ArrayColl[Int], in1: ArrayColl[Int]): DeliteOPZip[Int,Int,Int, ArrayColl[Int]] = {
    new DeliteOPZip[Int,Int,Int, ArrayColl[Int]] {
      def inA = in0
      def inB = in1
      def alloc = new ArrayColl[Int](in0.length)
      def zip(a: Int, b: Int) = a + b
    }
  }
}

object TestKernelMapReduce {
  def apply(in0: ArrayColl[Int]): DeliteOPMapReduce[Int,Int] = {
    new DeliteOPMapReduce[Int,Int] {
      def in = in0
      def map(elem: Int): Int = elem * elem
      def reduce(acc: Int, elem: Int): Int = acc + elem
    }
  }
}
*/

abstract class DeliteOPForeach[A] {
  def in: DeliteCollection[A]
  def foreach(elem: A)
  def sync(idx: Int): List[Any]
}

object TestKernelForeach {
  def apply(in0: ArrayColl[Int], out: ArrayColl[Int]): DeliteOPForeach[Int] = {
    new DeliteOPForeach[Int] {
      def in = in0
      def foreach(elem: Int) {
        val old = out.dcApply(0)
        Thread.sleep(10) //to magnify the race condition
        val now = old + elem
        out.dcUpdate(0, now)
        //println(old + " + " + elem + " = " + now)
      }
      def sync(idx: Int) = List(in0)
    }
  }
}

object TestKernelOut {
  def apply() = new ArrayColl[Int](0,1)
}

object TestKernelPrint0 {
  def apply(in: ArrayColl[Int]) = println(in.dcApply(0))
}
