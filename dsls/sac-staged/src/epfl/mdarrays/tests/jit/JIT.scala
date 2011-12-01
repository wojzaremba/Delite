package epfl.mdarrays.tests.jit

import epfl.mdarrays.library.scala.Conversions._
import epfl.mdarrays.library.scala._

import epfl.mdarrays.staged._
import scala.util.Random


object JITStagedRunner extends StagedSACApplicationRunner with JITStaged
trait JITStaged extends StagedSACApplication with JITStagedFakeReadMDArray {

  def main() = {
    /*
      Need to do the following:
      1. read a multidimensional array from an argument
      2. run testJIT(1000, <array>)
      3. print the result? -- we don't have any effects, is there another way to do this?
     */
    val matrix = fakeReadMDArray()
    println(matrix)
    writeMDArray("/home/sun/sac-test/jit-test.txt", matrix)
    //println(testJIT(1000, matrix).getString)
  }

  def testJIT(iter: Int, start: Rep[MDArray[Int]]): Rep[MDArray[Int]] = {
    var alive: Rep[MDArray[Int]] = start
    for (i <- List.range(0, iter))
      alive = gameOfLife(alive)
    alive
  }

  def computeIfDead(neigh: Rep[MDArray[Int]], alive: Rep[MDArray[Int]]): Rep[MDArray[Int]] = {

    if (alive === 1) {
      if ((neigh - alive) < 2)
        1 // Rule1: Any live cell with fewer than two live neighbours dies, as if caused by under-population.
      else if ((neigh - alive) < 4)
        0 // Rule2: Any live cell with two or three live neighbours lives on to the next generation.
      else
        1 // Rule3: Any live cell with more than three live neighbours dies, as if by overcrowding.
    }
    else
      0
  }

  def computeIfReborn(neigh: Rep[MDArray[Int]], dead: Rep[MDArray[Int]]): Rep[MDArray[Int]] = {

    if (dead === 1) {
      if (neigh === 3)
        1 // Rule 4: Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
      else
        0
    } else
      0
  }

  def gameOfLife(alive: Rep[MDArray[Int]]) = {

    val dead = With(lbStrict = true, ubStrict = true, function =
      iv => computeIfDead(sum(tile(values(dim(alive), 3), iv-1, alive)), alive(iv))).GenArray(shape(alive))

    val reborn = With(lbStrict = true, ubStrict = true, function =
      iv => computeIfReborn(sum(tile(values(dim(alive), 3), iv-1, alive)) -
                            sum(tile(values(dim(alive), 3), iv-1, dead)), dead(iv))).GenArray(shape(alive))

    val result = alive - dead + reborn
    result
  }
}

trait JITStagedFakeReadMDArray {
  def fakeReadMDArray(): MDArray[Int] = {
    // We want deterministic random
    Random.setSeed(0);

    import epfl.mdarrays.library.scala.Operations._
    import epfl.mdarrays.library.scala.Conversions._
    import epfl.mdarrays.library.scala._

    // Create the matrix
    val size: Int = 10
    val arr2: Array[Int] = new Array[Int](size * size)
    val rnd2: Random = new Random(1) // We need to have a fixed seed
    for (i <- arr2.indices)
      arr2(i) = rnd2.nextInt(2)
    val gameOfLifeMatrix: MDArray[Int] = reshape(size :: size :: Nil, arr2)

    gameOfLifeMatrix
  }
}
