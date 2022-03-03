import java.io.PrintWriter

object TableParser {
  case class CellParser(rowCnt: Int, colCnt: Int) {
    def parseCellNum(cellNum: String): (Int, Int) = {
      val (word, num) = cellNum.split()
    }
  }
  class Cell(rowCnt: Int, colCnt: Int, cellNum: String, val expr: String) {
    val (row, col) = parseCellNum
  }

  def calculateTableValues(inputFile: String): List[String] = {}

  def process(line: String): Unit = {
    line.split("\\s+").toList match {
      case "--input" :: inputFilePath :: "--output" :: outputFilePath => {
        val out = new PrintWriter(outputFilePath, "UTF-8")
        calculateTableValues(inputFilePath).foreach()
      }
    }
  }
}
