package CodeWars

import scala.annotation.tailrec

object Tribonacci {
  @tailrec
  def tribonacci[T: Numeric](signature: List[T], n: Int): List[T] = {
    if (n <= signature.size)
      signature.take(n)
    else
      tribonacci(signature :+ signature.takeRight(3).sum, n)
  }
}
