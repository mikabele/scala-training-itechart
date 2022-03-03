import scala.annotation.tailrec

object Calculator {
  def lcm(a: Int, b: Int): Option[Int] = {
    gcd(a, b) match {
      case 0   => None
      case gcd => Some(Math.abs(a / gcd * b))
    }
  }

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }
}
