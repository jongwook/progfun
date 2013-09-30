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
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (0, rr) => 1
    case (cc, 0) => 1
    case (cc, rr) => if (cc == rr) 1 else pascal(cc - 1, rr - 1) + pascal(cc, rr - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def test(chars: List[Char], depth: Int): Boolean = chars match {
      case Nil => depth == 0
      case '(' :: rest => test(rest, depth + 1)
      case ')' :: rest => depth > 0 && test(rest, depth - 1)
      case _ :: rest => test(rest, depth)
    }
    test(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = coins match {
    case Nil => if (money == 0) 1 else 0
    case x :: xs => {
      def cases(money: Int, coin: Int): Int = {
        if (money >= coin) countChange(money - coin, xs) + cases(money - coin, coin)
        else 0
      }
      countChange(money, xs) + cases(money, x)
    }
  }
}
