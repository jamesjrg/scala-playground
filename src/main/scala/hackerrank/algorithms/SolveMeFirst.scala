package algorithms.solvemefirst // because HackerRank insists all objects are called Solution

object Solution {

  def main(args: Array[String]) = {
    println(io.Source.stdin.getLines().take(2).map(_.toInt).sum)
  }

}
