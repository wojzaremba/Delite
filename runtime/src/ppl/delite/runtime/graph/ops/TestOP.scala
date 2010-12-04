package ppl.delite.runtime.graph.ops

/**
 * Author: Kevin J. Brown
 * Date: Oct 20, 2010
 * Time: 2:23:30 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class TestOP(kernel: String)(deps: DeliteOP*)
        extends DeliteOP {

  def task = kernel

  def outputType = "Unit"

  //initialize
  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

  def nested = null
  def cost = 0
  def size = 0
  def isDataParallel = false

}

class TestMap(func: String)(deps: DeliteOP*)(output: DeliteOP, input: DeliteOP, free: DeliteOP*)
        extends OP_Map(func) {

  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

  for (f <- free.reverse) { //need a reverse to preserve order (addInput prepends)
    this.addInput(f)
  }
  this.addInput(input)
  this.addInput(output)

}

class TestReduce[T: Manifest](func: String)(deps: DeliteOP*)(input: DeliteOP, free: DeliteOP*)
        extends OP_Reduce(func, manifest[T].toString) {

  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

  for (f <- free.reverse) {
    this.addInput(f)
  }
  this.addInput(input)

}

class TestMapReduce[T: Manifest](mapFunc: String, reduceFunc: String)(deps: DeliteOP*)(input: DeliteOP)(mapFree: DeliteOP*)(reduceFree: DeliteOP*)
        extends OP_MapReduce(mapFunc, reduceFunc, manifest[T].toString) {

  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

  for (f <- mapFree.reverse) {
    this.Map.addInput(f)
  }
  //this.Map.addInput(input)

  for (f <- reduceFree.reverse) {
    this.Reduce.addInput(f)
  }
  //this.Reduce.addInput(this.Map)

  inputList = input :: this.Map.getInputs.toList ::: this.Reduce.getInputs.toList

}

class TestZip(func: String)(deps: DeliteOP*)(output: DeliteOP, input1: DeliteOP, input2: DeliteOP, free: DeliteOP*)
        extends OP_Zip(func) {

  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

  for (f <- free.reverse) {
    this.addInput(f)
  }
  this.addInput(input2)
  this.addInput(input1)
  this.addInput(output)

}

class TestSingle[T: Manifest](kernel: String)(deps: DeliteOP*)(inputs: DeliteOP*)
        extends OP_Single(kernel, manifest[T].toString) {

  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

  for (input <- inputs.reverse) { //need a reverse to preserve order (addInput prepends)
    this.addInput(input)
  }

}