package ppl.delite.runtime.codegen

import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.scheduler.PartialSchedule
import java.util.ArrayDeque
import collection.mutable.ArrayBuffer

/**
 * Author: Kevin J. Brown
 * Date: Oct 26, 2010
 * Time: 8:19:19 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * Generates optimized DeliteExecutable for a given schedule
 * This generator creates a single executable function for each resource based on the provided schedule
 * The resulting executable should have minimal indirection and overhead, as well as increase compiler optimization opportunities
 *
 * This generator makes the following synchronization optimizations:
 * 1) it generates a synchronized getter for dependencies from other resources for the first use only
 *    outputs created in the current resource or already retrieved from another resource are accessed through local variables
 * 2) it generates synchronized result publication only for outputs that other resources will need to consume
 *    outputs that the scheduler has restricted to this resource are kept local 
 */

trait ExecutableGenerator extends ScalaGenerator with Synchronization {

  def makeExecutables(schedule: PartialSchedule) {
    for (i <- schedule.startLocation until schedule.endLocation) {
      val src = makeExecutable(schedule(i), i)
      ScalaCompile.addSource(src, executableName + i)
    }
    addGlobalSync() //TODO: this is a weird place for this
  }

  protected def makeExecutable(resource: ArrayDeque[DeliteOP], location: Int) = {
    val out = new StringBuilder //the output string

    //the header
    addImports(out)
    addObjectHeader(out, location)

    //the run method
    addMethodHeader(out)
    addKernelCalls(resource, location, out)
    addMethodFooter(out)

    //the sync methods/objects
    addLocalSync(out)

    //an accessor method for the object
    addAccessor(out)

    //the footer
    addObjectFooter(out)

    out.toString
  }

  protected def addKernelCalls(resource: ArrayDeque[DeliteOP], location: Int, out: StringBuilder) {
    val iter = resource.iterator
    while (iter.hasNext) { //foreach op
      val op = iter.next

      if (op.isInstanceOf[OP_Nested]) makeNestedFunction(op, location)

      //get dependencies
      for (dep <- op.getDependencies) {
        for (sym <- dep.getOutputs)
          get(out, dep, sym, op, location)
      }
      //TODO: separate this into control and input dependencies and only get outputs that are needed
      /* for (dep <- op.getDependencies)
        await(out, dep, op, location)

      for ((in, name) <- op.getInputs) {
        get(out, in, sym, op, location)
      } */

      //write the function call:
      addFunctionCall(op, location, out)

      //write the setter:
      for (sym <- op.getOutputs)
        set(out, op, sym, location)
    }
  }

  protected def makeNestedFunction(op: DeliteOP, location: Int) {
    op match {
      case c: OP_Condition => new ConditionGenerator(c, location).makeExecutable()
      case w: OP_While => new WhileGenerator(w, location).makeExecutable()
      case v: OP_Variant => new VariantGenerator(v, location).makeExecutable()
      case err => sys.error("Unrecognized nested OP type: " + err.getClass.getSimpleName)
    }
  }

  protected override def addImports(out: StringBuilder) {
    ScalaGenerator.writePath(out)
    out.append("import ppl.delite.runtime.codegen.DeliteExecutable\n") //base trait
    super.addImports(out)
  }

  protected def addAccessor(out: StringBuilder) {
    out.append("def self = this\n")
  }

}
