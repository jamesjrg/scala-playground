package hackerrank.arrays

import org.scalatest.FlatSpec
import arrays.two2darray.Solution

class TwoDArraySpec extends FlatSpec {

  it should "handle simple grid of 1s" in {
    val grid = Array.fill(16, 16)(1)
    assert(Solution.solve(grid) == 7)
  }
}