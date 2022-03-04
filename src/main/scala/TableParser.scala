import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try, Using}

object TableParser {
  object CellParser {
    val exprPattern:    Regex = "=(.*)".r
    val strPattern:     Regex = "'(.*)".r
    val cellNumPattern: Regex = "(\\w)+(\\d+)[+\\-*/]?".r
    val numPattern:     Regex = "(\\d)+[+\\-*/]?".r

    def parseCellNum(rowStr: String, colStr: String)(implicit size: Size): Int = {
      val colLetters = colStr.split("")
      val col = colLetters
        .map((_, 0))
        .foldRight(("0", -1))((leftValue, rightValue) => {
          (
            (rightValue._1.toInt + (leftValue._1.head - 'A') * Math.pow(26, rightValue._2 + 1).toInt).toString,
            rightValue._2 + 1
          )
        })
        ._1
        .toInt
      (rowStr.toInt - 1) * size.colCnt + col

    }

    def parseCellExpr(table: Seq[String], value: String)(implicit size: Size): Cell = {
      value match {
        case exprPattern(expr) => {
          val cellNumExprs = expr.split("(?<=[+\\-*/])")
          val cellValue = cellNumExprs.foldLeft("0+")((leftValue, rightValue) => {
            val res2 = rightValue match {
              case numPattern(num) => num.toInt
              case cellNumPattern(colStr, rowStr) => {
                val index2 = parseCellNum(rowStr, colStr)
                parseCellExpr(table, table(index2)).result.toInt
              }
            }
            val nextOperator = if (rightValue.takeRight(1).matches("[+\\-*/]")) rightValue.takeRight(1) else ""
            (leftValue.takeRight(1) match {
              case "+" => leftValue.dropRight(1).toInt + res2
              case "-" => leftValue.dropRight(1).toInt - res2
              case "*" => leftValue.dropRight(1).toInt * res2
              case "/" => leftValue.dropRight(1).toInt / res2
            }).toString + nextOperator
          })
          Cell(cellValue)
        }
        case strPattern(strValue) => Cell(strValue)
        case _                    => Cell(value)
      }
    }
  }

  case class Cell(result: String)

  case class Size(rowCnt: Int, colCnt: Int)

  def calculateTableValues(inputFile: String): Seq[String] = {
    readFile(inputFile) match {
      case Success(content) =>
        val rows                  = content.split("\\r\\n")
        val Array(rowNum, colNum) = rows.head.split("\\t").map(_.toInt)
        implicit val size: Size = Size(rowNum, colNum)
        val table = rows.tail.flatMap(row => row.split("\\t"))
        table.map(cell => CellParser.parseCellExpr(table, cell).result).grouped(colNum).map(_.mkString("\t")).toSeq
      case Failure(exception) =>
        println(exception.getMessage)
        Nil
    }
  }

  def readFile(inputFile: String): Try[String] = {
    Using(Source.fromFile(new File(inputFile))) { source => source.mkString }
  }

  def writeFile(filename: String, lines: Seq[String]): Unit = {
    val file = new File(filename)
    val fw   = new FileWriter(file)
    for (line <- lines) {
      fw.write(line + "\r\n")
    }
    fw.close()
  }

  def process(line: String): Try[Unit] = {
    Try(line.split("\\s+").toList match {
      case "--input" :: inputFilePath :: "--output" :: outputFilePath :: Nil =>
        val table = calculateTableValues(inputFilePath)
        writeFile(outputFilePath, table)
      case "--input" :: inputFilePath :: Nil =>
        calculateTableValues(inputFilePath).foreach(println)
      case _ =>
        new IllegalArgumentException("Error: incorrect input parameters")
    })
  }
}
