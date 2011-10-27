package ppl.delite.framework.datastruct.scala






trait Traversable[@specialized T] extends DeliteCollection[T] {
  def size: Int
  
  def dcSize = size
}


trait Seq[@specialized T] extends Traversable[T] {
  def length: Int
  def apply(idx: Int): T
  def update(idx: Int, x: T)
  
  def size = length
  def dcApply(idx: Int) = apply(idx)
  def dcUpdate(idx: Int, x: T) = update(idx, x)
}


trait ArraySeq[@specialized T] extends Seq[T] {
  def +=(elem: T): Unit
  
  def unsafeData: Array[T]
  def unsafeLength: Int
  def unsafeSetData(arr: Array[T], len: Int)
}


trait Map[@specialized K, @specialized V] extends Traversable[(K, V)] {
  def get(k: K): V
  def put(k: K, v: V)
}


trait HashMap[@specialized K, @specialized V] extends Map[K, V] {
  def unsafeIndices: Array[Int]
  def unsafeKeys: Array[K]
  def unsafeValues: Array[V]
  def unsafeSize: Int
  def unsafeSetInternal(_ind: Array[Int], _keys: Array[K], _values: Array[V], _sz: Int)
  def unsafeBlockSizes: Array[Int]
  def unsafeSetBlockSizes(_blkszs: Array[Int])
  def unsafeSetKeys(_keys: Array[K])
  def unsafeSetValues(_values: Array[V])
  def unsafeSetSize(_sz: Int)
}
