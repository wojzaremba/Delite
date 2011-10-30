package ppl.delite.framework.datastruct.scala






// specialization bug on multiple ctors: (_indices: Array[Int], _keys: Array[K], _values: Array[V], _sz: Int)
final class HashMapImpl[@specialized K: Manifest, @specialized V: Manifest](indsz: Int, datasz: Int) extends HashMap[K, V] {
  private val loadfactor_d2 = 0.4f / 2
  private var indices = Array.fill[Int](HashMapImpl.nextPow2(indsz))(-1)
  private var keys = new Array[K](datasz)
  private var values = new Array[V](datasz)
  private var blocksizes: Array[Int] = _
  private var sz = 0
  
  import HashMapImpl.nextPow2
  
  // def this(indsz: Int, datasz: Int) = this(
  //   Array.fill[Int](HashMapImpl.nextPow2(indsz))(-1),
  //   new Array[K](datasz),
  //   new Array[V](datasz), 
  //   0)
  def this() = this(128, 52)
  
  @inline private def absolute(hc: Int) = {
    val mask = hc >> 31
    (hc + mask) ^ mask
  }
  
  def size = sz
  
  def get(k: K): V = {
    val hc = k.## * 0x9e3775cd
    val relbits = Integer.numberOfTrailingZeros(indices.length / 2)
    var pos = (hc >>> (32 - relbits)) * 2
    var currelem = indices(pos)
    var currhash = indices(pos + 1)
    
    while (currelem != -1 && (currhash != hc || keys(currelem).asInstanceOf[V] != k)) {
      pos = (pos + 2) % indices.length
      currelem = indices(pos)
      currhash = indices(pos + 1)
    }
    
    if (currelem == -1) null.asInstanceOf[V]
    else values(currelem)
  }
  
  def put(k: K, v: V) {
    val hc = k.## * 0x9e3775cd
    val relbits = Integer.numberOfTrailingZeros(indices.length / 2)
    var pos = (hc >>> (32 - relbits)) * 2
    var currelem = indices(pos)
    var currhash = indices(pos + 1)
    
    while (currelem != -1 && (currhash != hc || keys(currelem).asInstanceOf[K] != k)) {
      pos = (pos + 2) % indices.length
      currelem = indices(pos)
      currhash = indices(pos + 1)
    }
    
    if (currelem == -1) {
      val datapos = sz
      indices(pos) = datapos
      indices(pos + 1) = hc
      keys(datapos) = k
      values(datapos) = v
      sz += 1
      
      grow()
    } else {
      val datapos = currelem
      keys(datapos) = k
      values(datapos) = v
    }
  }
  
  def statistics = {
"""size: %d
indices length: %d
data length: %d
growth threshold: %d
""".format(sz, indices.length, keys.length, (loadfactor_d2 * indices.length).toInt)
  }
  
  private def grow() = if (sz > (loadfactor_d2 * indices.length)) {
    val nindices = Array.fill[Int](indices.length * 2)(-1)
    val nkeys = new Array[K](keys.length * 2)
    val nvalues = new Array[V](values.length * 2)
    
    // copy raw data
    System.arraycopy(keys, 0, nkeys, 0, sz)
    System.arraycopy(values, 0, nvalues, 0, sz)
    
    // copy indices
    var i = 0
    val relbits = Integer.numberOfTrailingZeros(nindices.length / 2)
    while (i < indices.length) {
      val elem = indices(i)
      if (elem != -1) {
        val hash = indices(i + 1)
        var pos = (hash >>> (32 - relbits)) * 2
        
        // insert it into nindices
        var currelem = nindices(pos)
        var currhash = nindices(pos + 1)
        while (currelem != -1) {
          pos = (pos + 2) % nindices.length
          currelem = nindices(pos)
          currhash = nindices(pos + 1)
        }
        nindices(pos) = elem
        nindices(pos + 1) = hash
      }
      i += 2
    }
    
    indices = nindices
    keys = nkeys
    values = nvalues
  }
  
  def dcApply(idx: Int) = {
    val pos = idx
    (keys(pos).asInstanceOf[K], values(pos).asInstanceOf[V])
  }
  
  def dcUpdate(idx: Int, x: (K, V)) = throw new UnsupportedOperationException
  
  override def toString = "HashMapImpl(sz: %d; indices: %s; keys: %s, values: %s)".format(sz, if (indices != null) indices.mkString(", ") else "null", if (keys != null) keys.mkString(", ") else "null", if (values != null) values.mkString(", ") else "null")
  
  def unsafeIndices: Array[Int] = indices
  
  def unsafeKeys: Array[K] = keys
  
  def unsafeValues: Array[V] = values
  
  def unsafeSize = sz
  
  def unsafeBlockSizes = blocksizes
  
  def unsafeSetBlockSizes(_blkszs: Array[Int]) = blocksizes = _blkszs
  
  def unsafeSetKeys(_keys: Array[K]) {
    keys = _keys
  }
  
  def unsafeSetValues(_values: Array[V]) {
    values = _values
  }
  
  def unsafeSetSize(_sz: Int) {
    sz = _sz
  }
  
  def unsafeSetInternal(_ind: Array[Int], _keys: Array[K], _values: Array[V], _sz: Int) {
    indices = _ind
    keys = _keys
    values = _values
    sz = _sz
  }
}


final class Bucket[@specialized T] extends DeliteCollection[T] {
  var array: Array[T] = _
  var size = 0
  //var next: Bucket[T] = _
  
  def dcSize = size
  def dcApply(idx: Int) = array(idx)
  def dcUpdate(idx: Int, x: T) = array(idx) = x
  
  override def toString = "Bucket(size: %d; values: %s)".format(size, array.take(size).mkString(", "))
}


object HashMapImpl {
  def range(n: Int) = {
    val hm = new HashMapImpl[Int, Int](n * 5 + 1, n * 3)
    for (i <- 0 until n) hm.put(i, i)
    hm
  }
  def nextPow2(x: Int) = {
    var c = x - 1;
    c |= c >>>  1;
    c |= c >>>  2;
    c |= c >>>  4;
    c |= c >>>  8;
    c |= c >>> 16;
    c + 1;
  }
}
