package warmup.countingvalleys // because HackerRank insists all objects are called Solution

import io.StdIn.readLine

object Solution {

  def solve(path: Seq[Char]) = {
    val result =
      ((0, false, 0) /: path)
      {
        case ((height, alreadyInValley, valleyCount), step) =>
          val newHeight = if (step == 'U') height + 1 else height - 1

          alreadyInValley match  {
            case false if newHeight < 0 => (newHeight, true, valleyCount + 1)
            case true if newHeight == 0 => (newHeight, false, valleyCount)
            case _ => (newHeight, alreadyInValley, valleyCount)
          }
      }

    result._3
  }

  def main(args: Array[String]) {
    val nSteps = readLine.toInt
    val path = readLine.trim.toArray

    val result = solve(path)

    println(result)
  }

}
