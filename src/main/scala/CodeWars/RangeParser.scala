package CodeWars

object RangeParser {
  private def parseRes(
    strVal: String
  )(
    comaFunc:  String => String,
    colonFunc: String => String,
    dashFunc:  String => String
  ): String = strVal.takeRight(1) match {
    case "," => comaFunc(strVal)
    case ":" => colonFunc(strVal)
    case "-" => dashFunc(strVal)
  }

  private def addNumberToRangeStr(left: (String, Int), right: (String, Int)): (String, Int) = {
    val (leftStrVal, leftNumValue) = left
    val (_, rightNumValue)         = right
    val newStr = rightNumValue - leftNumValue match {
      case 1 =>
        parseRes(leftStrVal)(x => x.dropRight(1) + ":", x => x.dropRight(1) + "-", x => x)
      case _ =>
        parseRes(leftStrVal)(
          x => x + rightNumValue.toString + ",",
          x => x.dropRight(1) + "," + leftNumValue.toString + "," + rightNumValue.toString + ",",
          x => x + leftNumValue.toString + "," + rightNumValue.toString + ","
        )
    }
    (newStr, rightNumValue)
  }

  private def solutionNonEmpty(xs: List[Int]): String = {
    val (res, lastNum) = xs.tail
      .map(x => (x.toString, x))
      .foldLeft((xs.head.toString + ",", xs.head))(addNumberToRangeStr)
    parseRes(res)(x => x.dropRight(1), x => x.dropRight(1) + "," + lastNum.toString, x => x + lastNum.toString)
  }

  def solution(xs: List[Int]): String = xs match {
    case Nil => ""
    case _   => solutionNonEmpty(xs)
  }
}
