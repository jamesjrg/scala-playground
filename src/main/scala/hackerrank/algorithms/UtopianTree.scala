package algorithms.utopiantree // because HackerRank insists all objects are called Solution

// https://www.hackerrank.com/challenges/utopian-tree/
object Solution {

  def main(args: Array[String]) = {
    val inputs = io.Source.stdin.getLines().drop(1).map(_.toInt).toList
    val outputs = inputs.map(input => {
      (1 /: (1 to input)) ((total, idx) =>
        if (idx % 2 == 0) total + 1 else total * 2)
    })
    outputs.foreach(println)
  }
}
