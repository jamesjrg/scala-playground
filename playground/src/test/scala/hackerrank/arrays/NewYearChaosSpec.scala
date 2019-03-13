package hackerrank.arrays

import org.scalatest.FlatSpec
import arrays.newyearchaos.Solution

class NewYearChaosSpec extends FlatSpec {

  it should "handle unchanged lines" in {
    val input = "1 2 3 4 5"
    assert(Solution.mapInputToOutput(input) == "0")
  }

  it should "handle input from example 1" in {
    val input = "2 1 5 3 4"
    assert(Solution.mapInputToOutput(input) == "3")
  }

  it should "handle input from example 2" in {
    val input = "2 5 1 3 4"
    assert(Solution.mapInputToOutput(input) == "Too chaotic")
  }

  it should "handle awkward things" in {
    val input = "1 2 5 3 7 8 6 4"
    assert(Solution.mapInputToOutput(input) == "7")
  }
}

//1 2 3 4 5 6 7 8
//1 2 3 5.4 6 7 8 => 1
//1 2 5.3 4 6 7 8 => 2
//1 2 5 3 4 7.6 8 => 3
//1 2 5 3 7.4 6 8 => 4
//1 2 5 3 7 6.4 8 => 5
//1 2 5 3 7 6 8.4 => 6
//1 2 5 3 7 8.6 4 => 7