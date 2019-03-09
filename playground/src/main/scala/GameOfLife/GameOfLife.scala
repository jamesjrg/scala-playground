package GameOfLife

import scala.collection.mutable

// based on an F# version which was in turn based on https://github.com/chrisphelps/funConway/blob/master/src/test/scala/FunConway.scala
object GameOfLife {
  type Cell = (Int, Int)
  type Grid = List[Cell]

  def shouldLive(alive: Boolean, neighbourCount: Int) = (alive, neighbourCount) match {
    case (true, n) if n >= 2 && n <= 3 => true
    case (false, 3) => true
    case _ => false
  }

  def neighboursEverybodyNeeds(cell: Cell): Seq[Cell] = cell match {
    case (cellRow, cellCol) =>
      for (
        row <- cellRow - 1 to cellRow + 1;
        col <- cellCol - 1 to cellCol + 1
        if !(row == cellRow && col == cellCol)
      ) yield (row, col)
  }

  def neighbourCount(cell: Cell, cellExists: Cell => Boolean): Int = {
    neighboursEverybodyNeeds(cell) count cellExists
  }

  def candidateCells(grid: Grid): List[Cell] = {
    (grid flatMap ((cell: Cell) => neighboursEverybodyNeeds(cell))).distinct ++ grid
  }

  def memoize[A, B](f: A => B): A => B = {
    val cache = new mutable.HashMap[A, B]()
    x =>
      cache.get(x) match {
        case Some(found) => found
        case None => {
          val res = f(x)
          cache += (x -> res)
          res
        }
      }
  }

  def tick(grid: Grid): Grid = {
    val cellExists = memoize((cell: Cell) => grid.contains(cell))
    candidateCells(grid)
      .map(cell => (cell, cellExists(cell), neighbourCount(cell, cellExists)))
      .filter { case (_, alive: Boolean, neighbourCount: Int) => shouldLive(alive, neighbourCount) }
      .map { case (cell, _, _) => cell }
  }
}
