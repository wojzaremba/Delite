/*
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.codegen.kernels.scala.MultiLoop_SMP_Array_Generator

class OP_MultiLoop(val id: String, val size: String, val sizeIsConst: Boolean, func: String, private[graph] val outputTypesMap: Map[Targets.Value,Map[String,String]], val needsCombine: Boolean, val needsPostProcess: Boolean) extends OP_Executable {

  final def isDataParallel = true

  def task(location: Int) = kernelNames.get(location) match {
    case Some(kernelName) => kernelName
    case None => {
      val kernelName = MultiLoop_SMP_Array_Generator.makeKernel(this, location, scheduledResources.length)
      kernelNames += Pair(location, kernelName)
      kernelName
    }
  }

  private var kernelNames = Map[Int, String]()

  def function = func

  override def splitInput(name: String) = { //TODO: mark inputs uniquely
    if (scheduledResources.length > 1) true
    else false
  }

  override def combineOutput(name: String) = { //TODO: mark outputs uniquely
    if (scheduledResources.length > 1 && !needsCombine) true
    else false
  }

}
