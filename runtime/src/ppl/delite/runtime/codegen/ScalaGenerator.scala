package ppl.delite.runtime.codegen

import ppl.delite.runtime.graph.ops.DeliteOP
import javax.xml.stream.Location

/*
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */
 
/**
 * @author Kevin J. Brown
 */
 
trait ScalaGenerator { //TODO: extends SourceGenerator?

  protected def executableName: String

  protected def addObjectHeader(out: StringBuilder, location: Int) {
    out.append("object ")
    out.append(executableName)
    out.append(location)
    out.append(" extends DeliteExecutable {\n")
  }

  protected def addObjectFooter(out: StringBuilder) {
    out.append("}\n")
  }

  protected def addMethodHeader(out: StringBuilder) {
    out.append("def run() {\n")
  }

  protected def addMethodFooter(out: StringBuilder) {
    out.append("}\n")
  }

  protected def addFunctionCall(op: DeliteOP, location: Int, out: StringBuilder) {
    val outSym = op.getOutputs.head
    def returnsResult = op.outputType(outSym) == op.outputType
    def resultName = if (returnsResult) getSym(op, outSym, location, op.combineOutput(outSym)) else "op_" + getSym(op, op.id, location, false)

    if (op.task(location) == null) return //dummy op
    out.append("val ")
    out.append(resultName)
    out.append(" : ")
    out.append(op.outputType)
    out.append(" = ")
    out.append(op.task(location))
    out.append('(')
    var first = true
    for ((input, name) <- op.getInputs) {
      if (!first) out.append(',') //no comma before first argument
      first = false
      out.append(getSym(input, name, location, op.splitInput(name)))
    }
    out.append(")\n")

    if (!returnsResult) {
      for (name <- op.getOutputs) {
        out.append("val ")
        out.append(getSym(op, name, location, op.combineOutput(name)))
        out.append(" : ")
        out.append(op.outputType(name))
        out.append(" = ")
        out.append(resultName)
        out.append('.')
        out.append(name)
        out.append('\n')
      }
    }
  }

  /*protected def chunkOrFull(op: DeliteOP, name: String, consumer: DeliteOP) {
    case getter => if (op.splitInputs)
    case setter => if (op.combineOutputs)
    case kernelInput => if (op.splitInputs)
    case kernelOutput => if (op.combineOutputs)
  } */

  protected def getSym(op: DeliteOP, name: String, location: Int, chunk: Boolean): String = {
    val begin = "x"+name
    if (chunk) begin + "_" + location else begin
  }

  protected def getSym(op: DeliteOP, name: String): String = sys.error("using old getSym")

  protected def getSync(op: DeliteOP, name: String): String = sys.error("using old getSync")

}

object ScalaGenerator {
  private[codegen] def classPath = "generated.scala"

  private[codegen] def writePath(out: StringBuilder) = {
    out.append("package ")
    out.append(classPath)
    out.append('\n')
  }

}
