import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try, Using}

object TableParser {
  object CellParser {
    val exprPattern:    Regex = "=(.*)".r
    val strPattern:     Regex = "'(.*)".r
    val cellNumPattern: Regex = "(\\w)+(\\d+)".r

    def parseCellNum(rowCnt: Int, colCnt: Int)(cellNum: String): Int = {
      cellNum match {
        case cellNumPattern(colStr, rowStr) => {
          val colLetters = colStr.split("")
          val col = colLetters
            .map((_, 0))
            .reduceRight((val1, val2) => {
              ((val1._1.toInt + val2._1.toInt * Math.pow(26, val1._2 + 1).toInt).toString, val1._2 + 1)
            })
            ._1
            .toInt
          rowStr.toInt * colCnt + col
        }
      }
    }

    def parseCellExpr(rowCnt: Int, colCnt: Int)(table: Seq[String], value: String): Cell = {
      value match {
        case exprPattern(expr) => {
          val cellNumExprs = expr.split("(?=\\w+\\d+[\\+\\-\\*\\/]?)")
          val cellValue = cellNumExprs.reduceLeft((val1, val2) => {
            val index1 = parseCellNum(rowCnt, colCnt)(val1.dropRight(1))
            val index2 =
              if (val2.takeRight(1).matches("[+-*/]")) parseCellNum(rowCnt, colCnt)(val2.dropRight(1))
              else parseCellNum(rowCnt, colCnt)(val2)
            val res1 = parseCellExpr(rowCnt, colCnt)(table, table(index1)).result.toInt
            val res2 = parseCellExpr(rowCnt, colCnt)(table, table(index2)).result.toInt
            (val1.takeRight(1) match {
              case "+" => res1 + res2
              case "-" => res1 - res2
              case "*" => res1 * res2
              case "/" => res1 / res2
            }).toString
          })
          Cell(cellValue)
        }
        case strPattern(strValue) => Cell(strValue)
        case _                    => Cell(value)
      }
    }
  }

  case class Cell(result: String)

  def calculateTableValues(inputFile: String): List[String] = {
    readFile(inputFile) match {
      case Success(content) =>
        val rows                  = content.split("\\r\\n")
        val Array(rowNum, colNum) = rows.head.split("\\t").map(_.toInt)
        val table                 = rows.tail.flatMap(row => row.split("\\t"))
        val tableCells            = table.map(cell => CellParser.parseCellExpr(rowNum, colNum)(table, cell))
      case Failure(exception) => println(exception.getMessage)
    }
    Nil
  }

  def readFile(inputFile: String): Try[String] = {
    Using(Source.fromFile(new File(inputFile))) { source => source.mkString }
  }

  def writeFile(filename: String, lines: Seq[String]): Unit = {
    val file = new File(filename)
    val bw   = new BufferedWriter(new FileWriter(file))
    for (line <- lines) {
      bw.write(line)
    }
    bw.close()
  }

  def process(line: String): Try[Unit] = {
    Try(line.split("\\s+").toList match {
      case "--input" :: inputFilePath :: "--output" :: outputFilePath :: Nil =>
        writeFile(outputFilePath, calculateTableValues(inputFilePath))
      case "--input" :: inputFilePath :: Nil =>
        calculateTableValues(inputFilePath).foreach(println)
      case _ =>
        new IllegalArgumentException("Error: incorrect input parameters")
    })
  }
}
