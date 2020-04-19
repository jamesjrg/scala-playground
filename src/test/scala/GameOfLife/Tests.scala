package GameOfLife

import org.scalatest._
import GameOfLife.{candidateCells, neighbourCount, neighboursEverybodyNeeds, shouldLive, tick}

// mostly copied from https://github.com/chrisphelps/funConway/blob/master/src/test/scala/FunConway.scala
class GameOfLifeUnitSpec extends FlatSpec {

  "FunctionalConway" should "generate neighbors for a cell" in {
    val initialCell = (1,1)
    val expectedCells = List((0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2))
    assert(neighboursEverybodyNeeds(initialCell).toSet === expectedCells.toSet)

  }

  it should "generate a list of all potential cells" in {
    val initialCells = List((1,1), (1,2))
    val expectedCells = List((0,0),(0,1),(0,2),(0,3),
      (1,0),(1,1),(1,2),(1,3),
      (2,0),(2,1),(2,2), (2,3))
    assert(candidateCells(initialCells).toSet === expectedCells.toSet)
  }

  it should "count live neighbors" in {
    val liveCells = List((1,1), (1,2))
    val cellExists:((Int, Int)) => Boolean = liveCells.contains(_)
    assert(neighbourCount((0,1), cellExists) === 2)
    assert(neighbourCount((1,1), cellExists) === 1)
    assert(neighbourCount((5,5), cellExists) === 0)
  }

  it should "kill underpopulated cells" in {
    assert(shouldLive(true, 1) === false)
  }

  it should "kill overpopulated cells" in {
    assert(shouldLive(true, 4) === false)
  }

  it should "allow normal cells to live" in {
    assert(shouldLive(true, 2) === true)
  }

  it should "reproduce cells" in {
    assert(shouldLive(false, 3) === true)
  }

  it should "maintain the block" in {
    val liveCells = List((1,1), (1,2), (2,1), (2,2))
    assert(tick(liveCells).toSet === liveCells.toSet)
  }

  it should "blink the blinker" in {
    val liveCells = List((1,0), (1,1), (1,2))
    val expectedCells = List((0,1), (1,1), (2,1))
    assert(tick(liveCells).toSet === expectedCells.toSet)
  }

  it should "glide the glider" in {
    val liveCells = List((0,2),(1,0),(1,2),(2,1),(2,2))
    val expectedCells = List((0,2),(1,3),(2,1),(2,2),(2,3))
    assert(tick(tick(liveCells)).toSet === expectedCells.toSet)
  }
}

// idea and some code copied from http://rosettacode.org/wiki/Conway%27s_Game_of_Life/Scala
class GameOfLifeIntegrationSpec extends FlatSpec {

  implicit def coordsFromPattern(pattern: String) = for {
    (xs, y) <- pattern.stripMargin.split('\n').map(_.zipWithIndex).zipWithIndex.iterator
    (c, x) <- xs.iterator
    if c != ' '
  } yield (x, y)
}