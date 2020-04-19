package warmup.repeatedstring // because HackerRank insists all objects are called Solution

import io.StdIn.readLine

object Solution {

  def solve(input: String, totalCount: Long) = {
    val asPerRepeat = input.count(_ == 'a')
    val wholeRepeats = totalCount / input.length
    val countAfterWholeRepeats = input.length * wholeRepeats
    val extraFromNonTotalRepeat = input.substring(0, (totalCount - countAfterWholeRepeats).toInt).count(_ == 'a')

    asPerRepeat * wholeRepeats + extraFromNonTotalRepeat

  }

  def main(args: Array[String]) = {
    val input = readLine
    val charCount = readLine.trim.toLong

    val result = solve(input, charCount)

    println(result)
  }
}
