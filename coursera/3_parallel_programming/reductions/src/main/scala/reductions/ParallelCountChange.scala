package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    (money, coins) match {
      case (0, _) => 1
      case (money, _) if money < 0 => 0
      case (_, coins)  if coins.isEmpty => 0
      case (money, coins) => countChange(money - coins.head, coins) +
                             countChange(money, coins.tail)
    }
  }

  type Threshold = (Int, List[Int]) => Boolean
  type Coins = List[Int]

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: Coins, threshold: Threshold): Int = {
    val overflow = money <= 0 || coins.size < 1
    if (threshold(money, coins) || overflow)
      countChange(money, coins)
    else {
      val (withCoin, withoutCoin) =
        parallel(parCountChange(money - coins.head, coins, threshold),
                 parCountChange(money, coins.tail, threshold))
      withCoin + withoutCoin
    }
  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(total: Int): Threshold =
    (money: Int, coins: Coins) => money <= (2 * total / 3)

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(size: Int): Threshold =
    (money: Int, coins: Coins) => coins.size <= (2 * size / 3)

  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(total: Int, initial: Coins): Threshold =
    (money: Int, coins: Coins) =>
      money * coins.size <= (total * initial.size) / 2

}
