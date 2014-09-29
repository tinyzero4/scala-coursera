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

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if (c == 0 || r == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def doAnalysis(opened: Int, chars: List[Char]): Int = {
        if (chars.isEmpty) opened
        else {
          chars.head match {
            case ')' => doAnalysis(if (opened > 0) opened -1 else opened - 1000, chars.tail)
            case '(' => doAnalysis(opened + 1, chars.tail)
            case _ => doAnalysis(opened, chars.tail)
          }
        }
    }
    doAnalysis(0, chars) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val nominalCoins = coins.sorted
    def find(count: Int, amount: Int, node: Int): Int = {
      nominalCoins.filter(p => p >= node).foldLeft(count)((sum, p) => {
        val newAmount = amount + p
        if (newAmount < money) sum + find(count, newAmount, p)
        else {
          if (newAmount == money) sum + 1
          else sum
        }
      })
    }
    find(0, 0, nominalCoins.head)
  }

  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }
}
