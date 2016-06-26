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
    if ((c < 1) || (c >= r)) return 1
    else return pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def isOpenChar(char: Char): Boolean = char == '('

    def isCloseChar(char: Char): Boolean = char == ')'

    def balanceIter(openedCount: Int, chars: List[Char]): Boolean = {

      if (chars.isEmpty) return openedCount == 0

      val currentChar: Char = chars.head
      val remainingChars: List[Char] = chars.tail

      if (isOpenChar(currentChar)) {
        return balanceIter(openedCount + 1, remainingChars)
      }

      if (isCloseChar(currentChar)) {

        if (openedCount > 0) return balanceIter(openedCount - 1, remainingChars)

        return false
      }

      return balanceIter(openedCount, remainingChars)
    }

    return balanceIter(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countExchangesUsingFirstCoin(moneyToExchange: Int, allCoins: List[Int], totalCount: Int): Int = {

      val firstCoin: Int = allCoins.head

      countChangeIter(moneyToExchange - firstCoin, allCoins, totalCount)
    }

    def countExchangesUsingAllCoinsButFirst(moneyToExchange: Int, allCoins: List[Int], totalCount: Int): Int = {

      val followingCoins: List[Int] = allCoins.tail

      countChangeIter(moneyToExchange, followingCoins, totalCount)
    }

    def countChangeIter(moneyToExchange: Int, allCoins: List[Int], totalCount: Int): Int = {

      if (allCoins.isEmpty || moneyToExchange < 0) return totalCount

      if (moneyToExchange == 0) return totalCount + 1

      return countExchangesUsingFirstCoin(moneyToExchange, allCoins, totalCount) +
        countExchangesUsingAllCoinsButFirst(moneyToExchange, allCoins, totalCount)
    }

    return countChangeIter(money, coins.sorted, 0)
  }
}