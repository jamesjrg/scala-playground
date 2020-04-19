package arrays.two2darray // because HackerRank insists all objects are called Solution

// https://www.hackerrank.com/challenges/2d-array/
object Solution {

  def hourGlassSum(arr: Array[Array[Int]], rowCenter: Int, colCenter: Int) = {
    (for (
      row <- rowCenter -1 to rowCenter + 1;
      col <- colCenter -1 to colCenter + 1
      if row != rowCenter || col == colCenter
    ) yield arr(row)(col))
    .sum
  }

  def solve(arr: Array[Array[Int]]) = {
    //Range(1, arr.length - 1).zipWithIndex()

    (for (
      row <- 1 to arr.length - 2;
      col <- 1 to arr.length - 2
    ) yield hourGlassSum(arr, row, col))
    .max
  }

  def main(args: Array[String]) {
    val arr = io.Source.stdin.getLines().map(_.split(' ').map(_.toInt).toArray).toArray

    val result = solve(arr)

    println(result)
  }

}
