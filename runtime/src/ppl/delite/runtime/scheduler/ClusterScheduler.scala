package ppl.delite.runtime.scheduler

import ppl.delite.runtime.cost.ParallelUtilizationCostModel
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.ops.{OP_Nested, OP_MultiLoop, DeliteOP}
import java.util.ArrayDeque

/*
* Pervasive Parallelism Laboratory (PPL)
* Stanford University
*
*/

/**
 * @author Kevin J. Brown
 */

class ClusterScheduler extends StaticScheduler with ParallelUtilizationCostModel {

  private val numNodes = Config.numNodes

  def schedule(graph: DeliteTaskGraph) {
    scheduleFlat(graph)
  }

  protected def scheduleSequential(graph: DeliteTaskGraph) = scheduleFlat(graph, true)

  protected def scheduleFlat(graph: DeliteTaskGraph) = scheduleFlat(graph, false)

  protected def scheduleFlat(graph: DeliteTaskGraph, sequential: Boolean) {
    val opQueue = new ArrayDeque[DeliteOP]
    val schedule = PartialSchedule(Config.numNodes * Config.numThreads)
    enqueueRoots(graph, opQueue)
    while (!opQueue.isEmpty) {
      val op = opQueue.remove
      if (sequential)
        addSequential(op, graph, schedule, 0)
      else
        scheduleOne(op, graph, schedule)
      enqueueRoots(graph, opQueue)
    }
    ensureScheduled(graph)
    graph.schedule = schedule
  }

  protected def scheduleOne(op: DeliteOP, graph: DeliteTaskGraph, schedule: PartialSchedule) {
    op match {
      case c: OP_Nested => addNested(c, graph, schedule, Range(0, numNodes))
      case l: OP_MultiLoop =>
        if (shouldParallelize(l, Map[String,Int]())){
          split(op, graph, schedule, Range(0, numNodes))
        }
        else {
          addSequential(op, graph, schedule, 0)
        }
      case _ => {
        addSequential(op, graph, schedule, 0)
      }
    }
  }
}
