package epfl.mdarrays.library.scala

import epfl.mdarrays.library.scala.Conversions._
import epfl.mdarrays.library.scala.Operations._
import epfl.mdarrays.library.scala.SpecificOperations._
import epfl.mdarrays.library.scala.With._

object MDArrayIO {
  def readMDArray[@specialized A: Manifest](fileName: String): MDArray[A] = {
    val source = scala.io.Source.fromFile(fileName)(scala.io.Codec.default)
    val lines: List[String] = source.getLines.toList

    // read dimension
    if (lines.size < 1) sys.error("MDArrayIO.readMDArray: File " + fileName + " doesn't contain dimension!")
    val dimension = lines.head.toInt

    // read shape
    if (lines.size < 1 + dimension) sys.error("MDArrayIO.readMDArray: File " + fileName + " doesn't contain the entire shape!")
    val shape: List[Int] = readList[Int](lines.tail, dimension)

    // read contents
    val size = shape.foldLeft(1) {_*_}
    if (lines.size < 1 + dimension + size) sys.error("MDArrayIO.readMDArray: File " + fileName + " doesn't contain the data!")
    val data: List[A] = readList[A](lines.drop(1 + dimension), size)

    // close file
    source.close()

    reshape(shape, data)
  }

  def readMDArrayShape(fileName: String): MDArray[Int] = {
    val source = scala.io.Source.fromFile(fileName)(scala.io.Codec.default)
    val lines = source.getLines

    // read dimension
    if (!lines.hasNext) sys.error("MDArrayIO.readMDArray: File " + fileName + " doesn't contain dimension!")
    val dimension = lines.next.toInt

    // read shape
    val shape: Array[Int] = new Array(dimension)
    for (i <- Range(0,dimension)) {
      if (!lines.hasNext) sys.error("MDArrayIO.readMDArray: File " + fileName + " doesn't contain the entire shape!")
      shape(i) = lines.next.toInt
    }

    // close file
    source.close()

    reshape(List(dimension), shape)
  }

  def writeMDArray[@specialized A: Manifest](fileName: String, array: MDArray[A]) = {

    val output = new java.io.PrintStream(new java.io.FileOutputStream(fileName))

    // write dimension
    output.println(dim(array))

    // write shape
    for (shapeElt <- shape(array).content) output.println(shapeElt)

    // write data
    for (dataElt <- array.content) output.println(dataElt)

    // close file
    output.close()
  }

  private def readList[@specialized A: Manifest](lines: List[String], count: Int): List[A] = {
    lines.take(count).map(readData[A])
  }

  private def readData[@specialized A](line: String)(implicit mf: Manifest[A]): A = {

    if (mf.erasure == classOf[Int])
      line.toInt.asInstanceOf[A]
    else if (mf.erasure == classOf[String])
      line.asInstanceOf[A]
    else if (mf.erasure == classOf[Double])
      line.toDouble.asInstanceOf[A]
    else if (mf.erasure == classOf[Float])
      line.toFloat.asInstanceOf[A]
    else
      sys.error("MDArrayIO.readData[" + mf.toString + "]: Unable to read this type of data")
  }
}
