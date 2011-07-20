package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets

/**
 * Author: Kevin J. Brown
 * Date: Oct 20, 2010
 * Time: 2:23:30 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class TestOP(kernel: String)(deps: DeliteOP*) extends OP_Executable {

  def task = kernel

  def id = System.identityHashCode(this).toString

  private[graph] val outputTypesMap = Map(Targets.Scala -> Map(id -> "Unit", "functionReturn" -> "Unit"))

  //initialize
  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

  def cost = 0
  def size = 0
  def isDataParallel = false

}

class TestSingle[T: Manifest](kernel: String)(deps: DeliteOP*)(inputs: DeliteOP*)
        extends OP_Single("", kernel, null) {

  override val id = System.identityHashCode(this).toString
  override private[graph] val outputTypesMap = Map(Targets.Scala -> Map(id -> manifest[T].toString, "functionReturn" -> manifest[T].toString))

  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

  for (input <- inputs.reverse) { //need a reverse to preserve order (addInput prepends)
    this.addInput(input, input.getOutputs.head)
  }

}

class TestMap[T: Manifest](func: String)(deps: DeliteOP*)(inputs: DeliteOP*)
        extends OP_MultiLoop("", "", false, func, null, false, false) {

  override val id = System.identityHashCode(this).toString
  override private[graph] val outputTypesMap = Map(Targets.Scala -> Map("out" -> manifest[T].toString, "functionReturn" -> ("ppl.delite.runtime.graph.Activation[" + manifest[T].toString + "]")))

  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

  for (input <- inputs.reverse) { //need a reverse to preserve order (addInput prepends)
    this.addInput(input, input.getOutputs.head)
  }

}

class TestForeach(func: String)(deps: DeliteOP*)(input: DeliteOP, free: DeliteOP*)
        extends OP_Foreach("", func, null) {

  override val id = System.identityHashCode(this).toString
  override private[graph] val outputTypesMap = Map(Targets.Scala -> Map(id -> "Unit", "functionReturn" -> "Unit"))

  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

  for (f <- free.reverse) { //need a reverse to preserve order (addInput prepends)
    this.addInput(f, f.getOutputs.head)
  }
  this.addInput(input, input.getOutputs.head)

}
