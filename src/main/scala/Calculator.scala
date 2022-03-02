import scala.annotation.tailrec

object Calculator {
  def lcm(a: Int, b: Int): Int = {
    a / gcd(a, b) * b
  }

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    if (a == 0)
      b
    else if (b == 0)
      a
    else
      gcd(b, a % b)
  }
}
