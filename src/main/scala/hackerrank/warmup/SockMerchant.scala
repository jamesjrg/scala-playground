package warmup.sockmerchant // because HackerRank insists all objects are called Solution

import io.StdIn.readLine

// https://www.hackerrank.com/challenges/sock-merchant/
object Solution {

  def solve(socks: Seq[Int]) = {
    socks
      .groupBy(x => x)
      .map { case (_, sockGroup) => sockGroup.length }
      .foldLeft(0)((acc, groupCount) => acc + groupCount / 2)
  }

  def main(args: Array[String]) = {
    val nSocks = readLine.toInt
    val socks = readLine.split(' ').map(_.toInt)

    val result = solve(socks)

    println(result)
  }

}
