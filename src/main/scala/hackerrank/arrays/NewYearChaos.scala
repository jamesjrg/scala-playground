package arrays.newyearchaos // because HackerRank insists all objects are called Solution

import io.StdIn.readLine

// https://www.hackerrank.com/challenges/new-year-chaos/
object Solution {

  def minimumBribes(arr: Array[Int]): Option[Int] = {
    ((Some(0): Option[Int]) /: arr.view.zipWithIndex) {
      case (maybeState, (item, index)) =>
        maybeState.flatMap(state => {
          val oneIndexed = index + 1

          if (item - oneIndexed > 2) None else {
            val numberOfPassingPeople = arr.view.take(index).count(_ > item)
            Some(numberOfPassingPeople + state)
          }
        })
    }
  }

  def mapInputToOutput(input: String) = {
    val q = input.split(" ").map(_.trim.toInt)

    minimumBribes(q) match {
      case None => "Too chaotic"
      case Some(x) => x.toString
    }
  }


  def main(args: Array[String]) {
    val t = readLine.trim.toInt

    for (tItr <- 1 to t) {
      val n = readLine.trim.toInt

      val text = mapInputToOutput(readLine())
      println(text)
    }
  }

}
