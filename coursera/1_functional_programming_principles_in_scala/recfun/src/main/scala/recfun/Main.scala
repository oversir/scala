package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  def choose(n: Int, k: Int): Int = {
    if (k > n) return 0
    if (k == 0 || k == n) return 1
    else {
      var result = n
      for (i <- 2 to (if (k * 2 > n) n - k else k)) {
        result *= (n - i + 1)
        result /= i
      }
      return result
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = choose(r, c)
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val (left, right) = ('(', ')')
    val parens = chars.filter(ch => ch == left || ch == right)
    @scala.annotation.tailrec
    def balance(parens: List[Char], open: Int): Boolean = {
      if(parens.isEmpty) open == 0
      else
        if(parens.head == left) balance(parens.tail, open + 1)
        else open > 0 && balance(parens.tail, open - 1)
    }
    balance(parens, 0)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    (money, coins) match {
      case (0, _) => 1
      case (money, _) if money < 0 => 0
      case (_, coins)  if coins.isEmpty => 0
      case (money, coins) => countChange(money - coins.head, coins) +
                             countChange(money, coins.tail)
    }

}
