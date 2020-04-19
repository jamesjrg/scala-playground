package warmup.jumpingontheclouds // because HackerRank insists all objects are called Solution

import io.StdIn.readLine

object Solution {

  def solve(clouds: List[Boolean]) = {
    @annotation.tailrec
    def inner(clouds: List[Boolean], jumpCount: Int): Int =
      clouds match {
        case Nil => jumpCount
        case _ :: false :: t => inner(t, jumpCount + 1)
        case _ :: t => inner(t, jumpCount + 1)
      }

    inner(clouds.tail, 0)
  }

  def main(args: Array[String]) = {
    val nClouds = readLine.toInt
    val clouds = readLine.trim.split(' ').map(x => if (x == "1") true else false).toList

    val result = solve(clouds)

    println(result)
  }

}
