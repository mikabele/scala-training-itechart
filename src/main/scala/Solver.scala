import TableParser._

object Solver {
  def process(line: String): String = {
    line.split("\\s+").toList match {
      case "table" :: "--input" :: inputFilePath :: _ =>
        calculateTableValues(inputFilePath)
      case "gcd" :: a :: b :: Nil =>
        Calculator.gcd(a.toInt, b.toInt).toString
      case "lcm" :: a :: b :: Nil =>
        Calculator.lcm(a.toInt, b.toInt).getOrElse(-1).toString
      case _ =>
        "Error: incorrect input parameters"
    }
  }
}
