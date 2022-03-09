package CodeWars

import scala.annotation.tailrec

object SeriesSum {
  def seriesSum(n: Int): String = {
    @tailrec
    def calculateNum(n: Int, denom: Int, result: Float): Float = {
      if (n == 0)
        result
      else
        calculateNum(n - 1, denom + 3, (result + 1.0 / denom).toFloat)
    }
    f"${calculateNum(n, 1, 0)}%.2f"
  }
}
