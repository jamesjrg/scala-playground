package hackerrank.arrays

import org.scalatest.FlatSpec
import arrays.newyearchaos.Solution

import scala.io.Source
import scala.io.StdIn.readLine


object FileUtil {
  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try {
      f(resource)
    } finally {
      resource.close()
    }

  def fileToArray(filename: String) = {
    using (Source.fromFile(filename)) {
      bufferedSource =>
        bufferedSource.getLines.toArray
    }
  }

  def fileToReader(filename: String) = {
    Source.fromFile(filename).bufferedReader()
  }
}

object PerformanceTestUtil {
  def runWithTimeout[A](getResult: => A, milliseconds: Int) = {

    class WorkAroundScalaNotHavingDefaultKeyword {
      var a: A = _
    }

    var result = new WorkAroundScalaNotHavingDefaultKeyword().a

    val thread = new Thread {
      override def run() {
        result = getResult
      }
    }

    thread.start()
    thread.join(milliseconds)

    result
  }

}

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

  // could be simplified
  it should "handle someone bribing after they have themselves been bribed" in {
    val input = "1 2 5 3 7 8 6 4"
    assert(Solution.mapInputToOutput(input) == "7")
  }

  it should "run large difficult input in a reasonable span of time" in {
    import java.nio.file.Paths
    val path = Paths.get(System.getProperty("user.home"), "/oss/hackerrank_test_cases/new_year_chaos.txt")
    val input = FileUtil.fileToReader(path.toString)
    val numberOfTestsInFile = input.readLine.trim.toInt
    val n = input.readLine.trim.toInt
    var result = ""

    PerformanceTestUtil.runWithTimeout(Solution.mapInputToOutput(input.readLine), 5000)

    assert(result == "119847")
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