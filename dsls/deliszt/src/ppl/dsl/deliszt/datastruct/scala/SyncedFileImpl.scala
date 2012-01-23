package ppl.dsl.deliszt.datastruct.scala

import java.io._
import java.nio.ByteBuffer

class SyncedFile(name : String) {
  val file = new FileWriter(name)
  def write(str : Int) = file.write(str.toString())
  def write(str : Double) = file.write(str.toString())
  def write[T <: {def toString():String }](str : T) = file.write(str.toString())
  def close() = file.close()
}
