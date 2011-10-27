package ppl.tests.scalatest.misc



import org.scalatest._
import ppl.delite.framework.datastruct.scala._



class HashMapImplSuite extends Suite {
  
  def testBasics {
    val hm = new HashMapImpl[Int, Int](16, 8)
    
    println(1.##)
    
    hm.put(1, 1)
    println(hm.statistics)
    hm.put(1, 2)
    println(hm.statistics)
    hm.put(257, 3)
    println(hm.statistics)
    hm.put(258, 4)
    println(hm.statistics)
    hm.put(256, 0)
    // println(hm.statistics)
    // println(hm.get(1))
    // println(hm.get(256))
    // println(hm.get(257))
    // println(hm.get(258))
    
    // println(hm)
  }
  
  def testInsertInt {
    val hm = new HashMapImpl[Int, Int]()
    
    for (i <- 0 until 200) {
      hm.put(i, i)
      assert(hm.get(i) == i)
      assert(hm.size == (i + 1))
    }
    
    for (i <- 0 until 200) assert(hm.get(i) == i)
  }
  
  def testInsertString {
    val hm = new HashMapImpl[String, Int]()
    val tot = 500
    def key(i: Int) = "key-no(%x)".format(i)
    
    for (i <- 0 until 819) {
      // println(key(i))
      hm.put(key(i), i)
      // println(hm)
      // println(hm.statistics)
      assert(hm.get(key(i)) == i)
    }
  }
  
}
