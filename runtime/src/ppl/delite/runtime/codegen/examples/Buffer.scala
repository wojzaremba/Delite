package ppl.delite.runtime.codegen.examples

import collection.mutable.ArrayBuffer
import java.util.concurrent.locks.ReentrantLock

/*
* Pervasive Parallelism Laboratory (PPL)
* Stanford University
*
*/

/**
 * @author Kevin J. Brown
 */

final class DynamicMultiBuffer[@specialized T](initialSize: Int) {

  private val buffers = new ArrayBuffer[Buffer[T]](initialSize)
  addBuffers(initialSize)

  private val lock = new ReentrantLock
  private val bufferExists = lock.newCondition()

  private def addBuffers(num: Int) {
    var i = 0
    while (i < num) {
      buffers += new Buffer[T]
      i += 1
    }
  }

  def get(index: Int): T = {
    if (index >= buffers.length) { //we can read 'buffers' asynchronously since we only ever add to it
      val lock = this.lock
      lock.lock()
      try {
        while (index >= buffers.length) {
          bufferExists.await()
        }
      } finally { lock.unlock() }
    }
    buffers(index).get
  }

  def put(numConsumers: Int, value: T) {
    if (numConsumers > buffers.length) {
      val lock = this.lock
      lock.lock()
      try {
        addBuffers(numConsumers - buffers.length) //only ever grow buffer count, reuse with possible over-alloc seems preferable to free and realloc
        bufferExists.signalAll()
      } finally { lock.unlock() }
    }

    var i = 0
    while (i < numConsumers) {
      buffers(i).put(value)
      i += 1
    }
  }

}

final class StaticMultiBuffer[@specialized T](size: Int) {

  private val buffers = new Array[Buffer[T]](size)
  addBuffers(size)

  private def addBuffers(num: Int) {
    var i = 0
    while (i < num) {
      buffers(i) = new Buffer[T]
      i += 1
    }
  }

  def get(index: Int): T = {
    buffers(index).get
  }

  /* could allow a thread to grab its buffer and then call get directly in the future
  def getBuffer(index: Int): Buffer[T] = {
    buffers(index)
  } */

  def put(value: T) {
    var i = 0
    while (i < size) {
      buffers(i).put(value)
      i += 1
    }
  }

}

final class Buffer[@specialized T] {

  private var isEmpty: Boolean = true
  private var _value: T = _

  private val lock = new ReentrantLock
  private val notEmpty = lock.newCondition()
  private val notFull = lock.newCondition()

  def get: T = {
    val lock = this.lock
    lock.lock()
    try {
      while(isEmpty) {
        notEmpty.await()
      }
      val value = _value
      _value = null.asInstanceOf[T]
      isEmpty = true
      notFull.signal()
      value
    }
    finally {
      lock.unlock()
    }
  }

  def put(value: T) {
    val lock = this.lock
    lock.lock()
    try {
      while (!isEmpty) {
        notFull.await()
      }
      _value = value
      isEmpty = false
      notEmpty.signal()
    }
    finally {
      lock.unlock()
    }
  }

}


final class CurrentSMPBuffer[@specialized T](numConsumers: Int) {

  private var count : Int = 0
  private var takeIndices = new Array[Int](numConsumers)
  //private var takeIndex0: Int = 0
  //private var takeIndex1: Int = 0
  //private var takeIndex2: Int = 0

  private var putIndex : Int = 0

  private var _result : T = _

  private val lock = new ReentrantLock
  private val notEmpty = lock.newCondition
  private val notFull = lock.newCondition

  def get(index: Int): T = {
    val takeIndex = takeIndices(index)
    val lock = this.lock
    lock.lock()
    try {
      while (takeIndex == putIndex) {
        notEmpty.await()
      }
      extract(index)
    } finally { lock.unlock() }
  }

  private def extract(index: Int): T = {
    val result = _result
    takeIndices(index) += 1
    count -= 1
    if (count == 0) {
      _result = null.asInstanceOf[T]
      notFull.signal()
    }
    result
  }

  def put(result : T) {
    val lock = this.lock
    lock.lock()
    try {
      while (count != 0) {
        notFull.await()
      }
      insert(result)
    } finally { lock.unlock() }
  }

  private def insert(result: T) {
    _result = result
    count = numConsumers
    putIndex += 1
    notEmpty.signalAll()
  }

}
