package example.profiling.datastruct.scala


import ppl.delite.framework.datastruct.scala.DeliteCollection


class ProfileArray(val numMeasurements: Int) extends DeliteCollection[Double] {
  val _data = new Array[Double](numMeasurements)
  
  def dcSize = _data.length
  def dcApply(i: Int) = _data(i)
  def dcUpdate(i: Int, x: Double) {
    _data(i) = x
  }
}
