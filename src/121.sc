/*
 * 121 Best Time to Buy and Sell Stock
 */

def maxProfit(prices: Array[Int]) = {
  if (prices.isEmpty) 0
  else {
    val xs = prices.scanLeft(prices(0))(math.min)
    val ys = prices.scanRight(prices.last)(math.max)

    xs.zip(ys).foldLeft(0) { case (profit, (x, y)) =>
      math.max(profit, y - x)
    }
  }
}

maxProfit(Array(5, 1))