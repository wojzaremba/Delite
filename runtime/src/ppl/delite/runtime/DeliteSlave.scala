package ppl.delite.runtime

import scacs.ClusterService

/*
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */
 
/**
 * @author Kevin J. Brown
 */
 
object DeliteSlave {

  //TODO: share execution with Delite object
  def main(args: Array[String]) {
    ClusterService.main(args)
  }

}
