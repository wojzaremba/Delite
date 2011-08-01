package ppl.delite.runtime.codegen

import ppl.delite.runtime.graph.ops.DeliteOP
import collection.mutable.HashSet

/*
* Pervasive Parallelism Laboratory (PPL)
* Stanford University
*
*/

/**
 * @author Kevin J. Brown
 */

trait Synchronization extends PeerAddressSpace with Local

trait BaseSynchronization extends ScalaGenerator {

  def await(out: StringBuilder, dependency: DeliteOP, op: DeliteOP, location: Int) {
    sys.error("Don't know how to await " + dependency + " for " + op)
  }

  def get(out: StringBuilder, input: DeliteOP, inputSym: String, op: DeliteOP, location: Int) {
    addGetMethodCall(out, input, inputSym, location, op.splitInput(inputSym))
  }

  def signal(out: StringBuilder, op: DeliteOP, location: Int) {
    sys.error("Don't know how to signal for " + op)
  }

  def set(out: StringBuilder, op: DeliteOP, outputSym: String, location: Int) {
    addSetMethodCall(out, op, outputSym, location, op.combineOutput(outputSym))
  }

  //def update(out: StringBuilder, op: DeliteOP) //TODO: ??

  def addLocalSync(out: StringBuilder)

  def addGlobalSync()

  protected def addGetMethodCall(out: StringBuilder, input: DeliteOP, sym: String, location: Int, chunk: Boolean) {
    out.append("val ")
    out.append(getSym(input, sym, location, chunk))
    out.append(" : ")
    out.append(input.outputType(sym))
    out.append(" = ")
    out.append(getMethodName(input, sym, location, chunk))
    out.append('\n')
  }

  protected def getMethodName(input: DeliteOP, sym: String, location: Int, chunk: Boolean) = "get_" + getSym(input, sym, location, chunk)

  protected def addSetMethodCall(out: StringBuilder, op: DeliteOP, sym: String, location: Int, chunk: Boolean) {
    out.append(setMethodName(op, sym, location, chunk))
    out.append('(')
    out.append(getSym(op, sym, location, chunk))
    out.append(")\n")
  }

  protected def setMethodName(op: DeliteOP, sym: String, location: Int, chunk: Boolean) = "set_" + getSym(op, sym, location, chunk)

}

trait Local extends BaseSynchronization { //TODO: non-local data we've already awaited, gotten
  override def await(out: StringBuilder, dependency: DeliteOP, op: DeliteOP, location: Int) {
    if (dependency.scheduledResources.contains(location)) //TODO: wait for all chunks?
      return
    else
      super.await(out, dependency, op, location)
  }

  override def get(out: StringBuilder, input: DeliteOP, inputSym: String, op: DeliteOP, location: Int) {
    if (!input.combineOutput(inputSym) && !op.splitInput(inputSym) && input.scheduledResources.contains(location))
      return
    else
      super.get(out, input, inputSym, op, location)
  }

  override def set(out: StringBuilder, op: DeliteOP, outputSym: String, location: Int) {
    val consumerSet = op.getConsumers.flatMap(_.scheduledResources).toSet - location //TODO: just data consumers?
    if (!op.combineOutput(outputSym) && consumerSet.isEmpty)
      return
    else
      super.set(out, op, outputSym, location)
  }
}

trait SharedMemory extends BaseSynchronization {

}

trait SlaveAddressSpace extends BaseSynchronization {

}

trait PeerAddressSpace extends BaseSynchronization {

  var localBuffer = new StringBuilder
  var globalBuffer = new StringBuilder

  override def get(out: StringBuilder, input: DeliteOP, inputSym: String, op: DeliteOP, location: Int) {
    addGetHeader(localBuffer, input, inputSym, location, op.splitInput(inputSym))

    if (input.combineOutput(inputSym) && op.splitInput(inputSym)) { //chunk to chunk //TODO: need to detect when split is the same across ops
      sys.error("not yet implemented: chunk to chunk")
    }
    else if (input.combineOutput(inputSym)) { //chunk to full
      for (resource <- input.scheduledResources)
        addGetter(localBuffer, input, inputSym, location, resource, true)
      addCombine(localBuffer, input, inputSym, location)
    }
    else if (op.splitInput(inputSym)) { //full to chunk
      addGetter(localBuffer, input, inputSym, location, location, true)
    }
    else { //full to full
      addGetter(localBuffer, input, inputSym, location, -1, false)
    }

    addGetFooter(localBuffer, input, inputSym, location, op.splitInput(inputSym))

    super.get(out, input, inputSym, op, location)
  }

  def getConsumers(op: DeliteOP, output: String) = {
    op.getConsumers.filter(c => c.getInputs.contains((op, output)))
  }

  override def set(out: StringBuilder, op: DeliteOP, outputSym: String, location: Int) {
    addSetHeader(localBuffer, op, outputSym, location, op.combineOutput(outputSym))

    for (cons <- getConsumers(op, outputSym)) {
      if (cons.splitInput(outputSym)) {
        addSplit(localBuffer, op, outputSym, cons, location)
        for (resource <- cons.scheduledResources) {
          addSetter(localBuffer, op, outputSym, resource, true)
          //TODO: should have a "local sync object"
          addSyncObject(globalBuffer, op, outputSym, resource, true) //TODO: with multiple different splits naming convention breaks
        }
      }
      else {
        val chunk = op.combineOutput(outputSym)
        addSetter(localBuffer, op, outputSym, location, chunk)
        addSyncObject(globalBuffer, op, outputSym, location, chunk) //TODO: don't repeat same sync object for multiple consumers
      }
    }

    addSetFooter(localBuffer)

    super.set(out, op, outputSym, location)
  }

  def addLocalSync(out: StringBuilder) {
    out.append(localBuffer)
    localBuffer = new StringBuilder
  }

  def addGlobalSync() {
    makeSyncObject()
    globalBuffer = new StringBuilder
  }

  protected def addGetter(out: StringBuilder, input: DeliteOP, sym: String, thisLocation: Int, remoteLocation: Int, chunk: Boolean) {
    out.append("val ")
    out.append(getSym(input, sym, remoteLocation, chunk))
    out.append(" : ")
    out.append(input.outputType(sym))
    out.append(" = ")
    //out.append(getSync(input, sym, remoteLocation, chunk))
    //out.append(".get")
    //out.append(thisLocation)
    out.append("ClusterService.getFrom(")
    out.append('0') //TODO: fill this in
    out.append(").asInstanceOf[")
    out.append(input.outputType(sym))
    out.append("]\n")
  }

  protected def addGetHeader(out: StringBuilder, input: DeliteOP, sym: String, location: Int, chunk: Boolean) {
    out.append("def ")
    out.append(getMethodName(input, sym, location, chunk))
    out.append(" : ")
    out.append(input.outputType(sym))
    out.append(" = {\n")
  }

  protected def addGetFooter(out: StringBuilder, input: DeliteOP, sym: String, location: Int, chunk: Boolean) {
    out.append(getSym(input, sym, location, chunk)) //return result
    out.append("\n}\n")
  }

  protected def writeChunkList(out: StringBuilder, op: DeliteOP, sym: String) {
    var first = true
    for (resource <- op.scheduledResources) {
      if (!first) out.append(", ")
      first = false
      out.append(getSym(op, sym, resource, true))
    }
  }


  protected def getSync(op: DeliteOP, name: String, location: Int, chunk: Boolean): String = {
    val begin = "Result_"+name
    if (chunk) begin + "_" + location else begin
  }

  protected def addSetter(out: StringBuilder, op: DeliteOP, sym: String, location: Int, chunk: Boolean) {
    //TODO: need API for setting other nodes
    //out.append(getSync(op, sym, location, chunk))
    //out.append(".set(")
    out.append("ClusterService.putAt(")
    out.append('0')
    out.append(", ")
    out.append(getSym(op, sym, location, chunk))
    out.append(")\n")
  }

  protected def addSetHeader(out: StringBuilder, op: DeliteOP, sym: String, location: Int, chunk: Boolean) {
    out.append("def ")
    out.append(setMethodName(op, sym, location, chunk))
    out.append('(')
    out.append(getSym(op, sym, location, chunk))
    out.append(": ")
    out.append(op.outputType(sym))
    out.append(") {\n")
  }

  protected def addSetFooter(out: StringBuilder) {
    out.append("}\n")
  }

  protected def addCombine(out: StringBuilder, input: DeliteOP, sym: String, location: Int) {
    out.append("val ")
    out.append(getSym(input, sym, location, false))
    out.append(" : ")
    out.append(input.outputType(sym))
    out.append(" = ")
    out.append(input.task(location))
    out.append(".combine_")
    out.append(sym)
    out.append('(')
    writeChunkList(out, input, sym)
    out.append(")\n")
  }

  protected def addSplit(out: StringBuilder, op: DeliteOP, sym: String, consumer: DeliteOP, location: Int) {
    out.append("val ")
    out.append(getSym(op, sym, location, false))
    out.append("_s = ")
    out.append(consumer.task(location))
    out.append(".split_")
    out.append(sym)
    out.append('(')
    out.append(consumer.scheduledResources.length)
    out.append(',')
    out.append(getSym(op, sym, location, false))
    out.append(")\n")

    for (idx <- 0 until consumer.scheduledResources.length) {
      out.append("val ")
      out.append(getSym(op, sym, consumer.scheduledResources(idx), true))
      out.append(" = ")
      out.append(getSym(op, sym, location, false))
      out.append("_s(")
      out.append(idx)
      out.append(")\n")
    }
  }

  protected def addSyncObject(out: StringBuilder, op: DeliteOP, sym: String, location: Int, chunk: Boolean) {
    //the header
    out.append("object ")
    out.append(getSync(op, sym, location, chunk))
    out.append( " {\n")

    def calculateConsumerSet(op: DeliteOP, location: Int) = {
      val consumerSet = HashSet.empty[Int]
      for (cons <- op.getConsumers) consumerSet ++= cons.scheduledResources
      if (!chunk) consumerSet -= location //TODO: make a lock-free sync for this case
      consumerSet
    }

    //the state
    val consumerSet = calculateConsumerSet(op, location)
    val numConsumers = consumerSet.size

    out.append("private var count : Int = 0\n")
    for (cons <- consumerSet) {
      out.append("private var takeIndex")
      out.append(cons)
      out.append(" : Int = 0\n")
    }
    out.append("private var putIndex : Int = 0\n")
    out.append("private var _result : ")
    out.append(op.outputType(sym))
    out.append(" = _\n")

    out.append("private val lock = new ReentrantLock\n")
    out.append("private val notEmpty = lock.newCondition\n")
    out.append("private val notFull = lock.newCondition\n")

    //the getters
    for (cons <- consumerSet) {
      out.append("def get")
      out.append(cons)
      out.append(" : ")
      out.append(op.outputType(sym))
      out.append(" = { val takeIndex = takeIndex")
      out.append(cons)
      out.append("; val lock = this.lock; lock.lock; try { while (takeIndex == putIndex) { notEmpty.await }; extract")
      out.append(cons)
      out.append(" } finally { lock.unlock } }\n")

      out.append("private def extract")
      out.append(cons)
      out.append(" : ")
      out.append(op.outputType(sym))
      out.append(" = { val res = _result; takeIndex")
      out.append(cons)
      out.append("+= 1; count -= 1; if (count == 0) { _result = null.asInstanceOf[")
      out.append(op.outputType(sym))
      out.append("]; notFull.signal }; res }\n")
    }

    //the setter
    out.append("def set(result : ")
    out.append(op.outputType(sym))
    out.append(") { val lock = this.lock; lock.lock; try { while (count != 0) { notFull.await }; insert(result) } finally { lock.unlock } }\n")

    out.append("private def insert(result: ")
    out.append(op.outputType(sym))
    out.append(") { _result = result; count = ")
    out.append(numConsumers)
    out.append("; putIndex += 1; notEmpty.signalAll }\n")

    //the footer
    out.append('}')
    out.append('\n')
  }

  protected def addImports(out: StringBuilder) { //TODO: ??
    out.append("import scacs.ClusterService\n")
  }

  protected def makeSyncObject() {
    val out = new StringBuilder
    ScalaGenerator.writePath(out)
    out.append("import java.util.concurrent.locks._\n") //locking primitives
    out.append(globalBuffer)
    ScalaCompile.addSource(out.toString, executableName + "Sync")
  }

}
