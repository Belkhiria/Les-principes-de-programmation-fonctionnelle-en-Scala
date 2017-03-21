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
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def rec(chars: List[Char], numOpensBrackets: Int): Boolean = {
        if (chars.isEmpty){
          numOpensBrackets == 0
        } else {
          val h = chars.head
          val n =
            if (h == '(') numOpensBrackets +1
            else if (h == ')' ) numOpensBrackets - 1
            else numOpensBrackets
          if (n >= 0) rec(chars.tail, n)
          else false
        }
      }
      rec(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) 0
      else if (money - coins.head == 0) 1
      else if (money - coins.head < 0) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
