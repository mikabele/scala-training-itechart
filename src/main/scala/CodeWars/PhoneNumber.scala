package CodeWars

object PhoneNumber {
  def createPhoneNumber(numbers: Seq[Int]): String = {
    val firstPart  = numbers.slice(0, 3).mkString("")
    val secondPart = numbers.slice(3, 6).mkString("")
    val thirdPart  = numbers.slice(6, 10).mkString("")
    s"($firstPart) $secondPart-$thirdPart"
  }
}
