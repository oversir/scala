package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = Array.fill(length)('(')
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    val (left, right) = ('(', ')')
    val parens = chars.filter(ch => ch == left || ch == right)
    val length = parens.length
    @scala.annotation.tailrec
    def balance(index: Int, open: Int): Boolean = {
      if(index >= length) open == 0
      else
        if(parens(index) == left) balance(index + 1, open + 1)
        else open > 0 && balance(index + 1, open - 1)
    }
    balance(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    val (left, right) = ('(', ')')
    val parens = chars.filter(ch => ch == left || ch == right)

    def traverse(from: Int, until: Int) /*: ???*/ = {
      var (open, close) = (0, 0)
      var index = from
      while(index < until) {
        if(parens(index) == left)
          open += 1
        else if(open > 0)
          open -= 1
        else
          close += 1
        index += 1
      }
      (open, close)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      val step = until - from
      if(step <= threshold || step <= 1)
        traverse(from, until)
      else {
        val mid = from + step / 2
        val ((lo, lc), (ro, rc)) =
          parallel(reduce(from, mid), reduce(mid, until))
        if(lo > rc)
          (ro + lo - rc, lc)
        else
          (ro, rc - lo + lc)
      }
    }

    reduce(0, parens.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
