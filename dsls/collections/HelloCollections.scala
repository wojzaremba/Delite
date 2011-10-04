



import ppl.delite.framework.collections._
import ppl.delite.framework.collections.datastruct.scala._



trait HelloCollections extends CollectionsApplication {
  def main() {
    val xs = ArraySeq[Int](5)
    println("hello array: " + xs.size)
    println("Now printing in foreach.")
    for (x <- xs) println(x)
    println("Now mapping and printing in foreach.")
    val ys = xs.map(_ + 1)
    for (y <- ys) println(y)
  }
}


object HelloCollectionsRunner extends CollectionsApplicationRunner with HelloCollections





