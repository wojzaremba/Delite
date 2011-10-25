package ppl.dsl.optiql.datastruct.scala.ordering

import collection.mutable.ArrayBuffer
import ppl.dsl.optiql.datastruct.scala.container.DataTable

class OrderedQueryable[TSource](source: DataTable[TSource], currentComparer: Ordering[TSource]) {
  
  def End = {
    val toBeSorted = new Array(source.size).asInstanceOf[Array[TSource]]
    Array.copy(source._data.getArray, 0, toBeSorted, 0, source.size)    
    scala.util.Sorting.quickSort(toBeSorted)(currentComparer)
    val res = new DataTable[TSource] {
      override def addRecord(arr: Array[String]) {
        throw new RuntimeException("Cannot add Record into a projected DataTable")
      }
    }
    res.unsafeSetData(toBeSorted, source.size)
    res    
  }
 
 def ThenBy[TKey](keySelector: TSource => TKey)(implicit comparer: Ordering[TKey]) = {
  new OrderedQueryable(source, new CompoundComparer(currentComparer, new ProjectionComparer(keySelector)))
  }
    
}