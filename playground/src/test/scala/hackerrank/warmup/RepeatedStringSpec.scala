package warmup

import org.scalatest.FlatSpec
import warmup.repeatedstring.Solution

class RepeatedStringSpec extends FlatSpec {

  it should "handle simple repetition" in {
    assert(Solution.solve("aa", 4) == 4)
  }

  it should "handle needing only part of substring" in {
    assert(Solution.solve("aa", 5) == 5)
  }

  it should "handle more than max int characters" in {
    assert(Solution.solve("aaaaaa", Int.MaxValue.toLong * 2) == Int.MaxValue.toLong * 2)
  }
}