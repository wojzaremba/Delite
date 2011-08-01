package ppl.delite.runtime.codegen.kernels.scala

import ppl.delite.runtime.graph.ops.OP_MultiLoop
import ppl.delite.runtime.codegen.{ScalaGenerator, ScalaCompile}

/**
 * Author: Kevin J. Brown
 * Date: Nov 17, 2010
 * Time: 9:00:34 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * Creates a chunk for OP_MultiLoop and generates an executable kernel for that chunk
 * The generated kernels are designed to run in parallel on multiple threads in an SMP system
 * This implementation of MultiLoop is optimized for a DSL collection that is backed by an Array
 */

object MultiLoop_SMP_Array_Generator {

  //TODO: make all smp chunks at one time?
  def makeKernel(op: OP_MultiLoop, location: Int, numChunks: Int) = {
    val out = new StringBuilder

    //the kernel
    writeKernel(out, op, location, numChunks)

    //the global state
    makeHeader(out, op, location, numChunks)

    ScalaCompile.addSource(out.toString, kernelName(op, location))

    kernelName(op, location)
  }

  private def writeObjectHeader(out: StringBuilder, op: OP_MultiLoop, location: Int) {
    ScalaGenerator.writePath(out)
    out.append("object ")
    out.append(kernelName(op, location))
    out.append(" {\n")
  }

  private def writeInputList(out: StringBuilder, op: OP_MultiLoop, typed: Boolean) {
    var first = true
    for ((input, name) <- op.getInputs) {
      if (!first) out.append(", ")
      first = false
      out.append(getSym(name))
      if (typed) {
        out.append(": ")
        out.append(input.outputType(name))
      }
    }
  }

  private def getSym(name: String) = "x" + name

  private def writeKernel(out: StringBuilder, op: OP_MultiLoop, location: Int, numChunks: Int) {
    writeObjectHeader(out, op, location)

    out.append("def apply(")
    writeInputList(out, op, true)
    out.append("): ")
    out.append(op.outputType)
    out.append(" = {\n")

    //TODO: If master, make header and set, else get header
    out.append("val head = new ")
    out.append(headerName(op, location))
    out.append('(')
    writeInputList(out, op, false)
    out.append(")\n")


    val chunkIdx = location //TODO: global vs. local chunk indices?
    //tree reduction
    //first every chunk performs its primary (map-)reduction
    out.append("val size = head.closure.size\n")
    out.append("val out = head.out\n")
    out.append("var idx = size*")
    out.append(chunkIdx)
    out.append('/')
    out.append(numChunks)
    out.append('\n')
    out.append("val end = size*")
    out.append(chunkIdx+1)
    out.append('/')
    out.append(numChunks)
    out.append('\n')
    out.append("val acc = head.closure.init(out, idx)\n") // copy of out per chunk
		out.append("idx += 1\n")
    out.append("while (idx < end) {\n")
    out.append("head.closure.process(acc, idx)\n")
    out.append("idx += 1\n")
    out.append("}\n")

    if (op.needsCombine) {
      var half = chunkIdx
      var step = 1
      while ((half % 2 == 0) && (chunkIdx + step < numChunks)) { //half the chunks quit each iteration
        half = half / 2
        val neighbor = chunkIdx + step //the index of the chunk to reduce with
        step *= 2

        out.append("head.closure.combine(acc, head.getA"+neighbor+")\n")
      }
      if (chunkIdx != 0) { //other chunks store result
        out.append("head.setA"+chunkIdx+"(acc)\n")
      }
    }
    if (op.needsPostProcess) {
      if (chunkIdx != 0) {
        val neighbor = chunkIdx - 1
        out.append("head.closure.postCombine(acc, head.getB"+neighbor+")\n")
      }
      if (chunkIdx == numChunks-1) {
        out.append("head.closure.postProcInit(acc)\n")
      }

      if (numChunks > 1) out.append("head.setB"+chunkIdx+"(acc)\n") // kick off others
      if (chunkIdx != numChunks-1) out.append("head.getB"+(numChunks-1)+"\n") // wait for last one
      out.append("head.closure.postProcess(acc)\n")
    }
    /*if (chunkIdx == 0)*/ out.append("acc\n") //TODO: need to check if chunkIdx equals a local master
    out.append("}\n") //end apply

    //data chunking methods
    writeSplits(out, op)
    writeCombines(out, op)

    out.append("}\n") //end object
  }

  private def writeSplits(out: StringBuilder, op: OP_MultiLoop) {
    for ((input, name) <- op.getInputs if op.splitInput(name)) {
      val funcName = "split_" + name

      out.append("def ")
      out.append(funcName)
      out.append("(numChunks: Int, ")
      out.append(name)
      out.append(": ")
      out.append(input.outputType(name))
      out.append("): Seq[")
      out.append(input.outputType(name))
      out.append("] = ")
      out.append(op.function)
      out.append('.')
      out.append(funcName)
      out.append("(numChunks, ")
      out.append(name)
      out.append(")\n")
    }
  }

  private def writeCombines(out: StringBuilder, op: OP_MultiLoop) {
    for (name <- op.getOutputs if op.combineOutput(name)) {
      val funcName = "combine_" + name

      out.append("def ")
      out.append(funcName)
      out.append('(')
      out.append(name)
      out.append(": ")
      out.append(op.outputType(name))
      out.append("*): ")
      out.append(op.outputType(name))
      out.append(" = ")
      out.append(op.function)
      out.append('.')
      out.append(funcName)
      out.append('(')
      out.append(name)
      out.append(":_*)\n")
    }
  }

  private def kernelName(op: OP_MultiLoop, location: Int) = {
    "MultiLoop_SMP_Array_" + op.id + "_Chunk_" + location
  }

  private def makeHeader(out: StringBuilder, op: OP_MultiLoop, location: Int, numChunks: Int) {
    //the kernel
    writeClass(out, op, location)

    if (op.needsCombine) {
      //the sync state
      for (i <- 1 until numChunks) //sync for all chunks except 0
        writeSync(out, "A"+i, op.outputType)
    }
    if (op.needsPostProcess && numChunks > 1) { //all chunks need to sync
      for (i <- 0 until numChunks)
        writeSync(out, "B"+i, op.outputType)
    }
    
    //the footer
    out.append("}\n")
  }

  private def writeClass(out: StringBuilder, op: OP_MultiLoop, location: Int) {
    out.append("final class ")
    out.append(headerName(op, location))
    out.append('(')
    writeInputList(out, op, true)
    out.append(") {\n")

    out.append("val closure = ")
    out.append(op.function)
    out.append('(')
    writeInputList(out, op, false)
    out.append(")\n")

    out.append("val out: ")
    out.append(op.outputType)
    out.append(" = closure.alloc\n")
  }

  private def writeSync(out: StringBuilder, key: String, outputType: String) {
    out.append("@volatile private var notReady")
    out.append(key)
    out.append(": Boolean = true\n")

    out.append("private var _result")
    out.append(key)
    out.append(" : ")
    out.append(outputType)
    out.append(" = _\n")

    out.append("def get")
    out.append(key)
    out.append(": ")
    out.append(outputType)
    out.append(" = { while (notReady")
    out.append(key)
    out.append(") { }; _result")
    out.append(key)
    out.append(" }\n")

    out.append("def set")
    out.append(key)
    out.append("(result: ")
    out.append(outputType)
    out.append(") { _result")
    out.append(key)
    out.append(" = result; notReady")
    out.append(key)
    out.append(" = false }\n")
  }

  private def headerName(op: OP_MultiLoop, location: Int) = {
    "MultiLoop_SMP_Array_" + op.id + "_Header_" + location
  }

}
