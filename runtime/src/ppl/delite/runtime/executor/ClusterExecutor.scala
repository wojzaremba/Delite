package ppl.delite.runtime.executor

import scacs.MasterService
import ppl.delite.runtime.scheduler.StaticSchedule
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.profiler.PerformanceTimer
import ppl.delite.runtime.graph.ops.EOP
import ppl.delite.runtime.codegen.Compilers
import javax.xml.stream.Location
import ppl.delite.runtime.{Delite, Config}

/*
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

/**
 * @author Kevin J. Brown
 */

class ClusterExecutor extends Executor {

  val numNodes = Config.numNodes

  def init() {
    MasterService.config("localhost", 8000, numNodes, 100) //TODO: numNodes
  }

  override def run(graph: DeliteTaskGraph) {

    //MasterService.submitAll(schedule, deliteSlave)
    val ends = for (i <- 0 until numNodes) yield {
      val fun = (location: Int) => Delite.execute(graph, "SMP", i, i+1)
      MasterService.submitAt(i, i, fun)
    }

    for (i <- 0 until numNodes)
      MasterService.retrieveFrom(i, ends(i))
  }

  def run(schedule: StaticSchedule) {
    assert(schedule.resources.length == 1)
    val iter = schedule.resources(0).iterator()
    while (iter.hasNext) {
      val work = iter.next
      work.run()
    }
  }

  def shutdown() {
    MasterService.shutdown()
  }

}
