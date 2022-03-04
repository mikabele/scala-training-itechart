import java.io.File
import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try, Using}

object TestTaskHomework {

  def parseInt(x: String): Try[Int] = Try(x.toInt)

  def from10to26(x: Int): String = {}

  type ErrorMessage = String
  final case class RawSpreadsheet(size: Size, table: Seq[String])
  final case class ProcessedSpreadsheet(size: Size, cells: Seq[Cell])
  final case class Size(rowCnt: Int, colCnt: Int)
  final case class Cell(result: String)

  def validateReadPath(readPath:  String): Either[ErrorMessage, String] = ???
  def validateWritePath(readPath: String): Either[ErrorMessage, String] = ???

  trait SpreadsheetParser {
    def parse(): Either[ErrorMessage, RawSpreadsheet]
  }

  def readFile(inputFile: String): Try[String] = {
    Using(Source.fromFile(new File(inputFile))) { source => source.mkString }
  }

  class LocalSpreadsheetParser(path: String) extends SpreadsheetParser {
    override def parse(): Either[ErrorMessage, RawSpreadsheet] = {
      readFile(path) match {
        case Success(content) =>
          val rows = content.split("\\r\\n")
          rows match {
            case Nil =>
              Left("Input file is empty")
            case sizes :: content =>
              Try(sizes.toString.split("\\t").map(_.toInt)) match {
                case Success(Array(rowNum, colNum)) =>
                  val size  = Size(rowNum, colNum)
                  val table = content.flatMap(row => row.toString.split("\\t"))
                  if (content.size == size.rowCnt && table.size == size.rowCnt * size.colCnt)
                    Right(RawSpreadsheet(size, table))
                  else
                    Left("Table size doesn't correspond given sizes")
                case Failure(exception) =>
                  Left(exception.getStackTrace.mkString("\r\n"))
              }
          }
        case Failure(exception) =>
          Left(exception.getStackTrace.mkString("\r\n"))
      }
    }
  }

  trait SpreadsheetProcessor {
    def process(spreadsheet:          RawSpreadsheet): ProcessedSpreadsheet
    def parseCell(spreadsheet:        RawSpreadsheet, rawCell: String): Cell
    def getCellIndex(spreadsheet:     RawSpreadsheet, cell:   String): String
    def getIndexFromCell(spreadsheet: RawSpreadsheet, rowStr: String, colStr: String): Int
    def parseCellExpr(spreadsheet:    RawSpreadsheet, cell:   String): String
  }

  class SimpleSpreadsheetProcessor extends SpreadsheetProcessor {

    val exprPattern:    Regex = "=(.*)".r
    val strPattern:     Regex = "'(.*)".r
    val numPattern:     Regex = "(\\d+)".r
    val errorPattern:   Regex = "#(.*)".r
    val cellNumPattern: Regex = "(\\w)+(\\d+)[+\\-*/]?".r
    val numExprPattern: Regex = "(\\d)+[+\\-*/]?".r

    override def getCellIndex(spreadsheet: RawSpreadsheet, cell: String): String = {
      val index = spreadsheet.table.indexOf(cell)
      val row   = index / spreadsheet.size.rowCnt
      val col   = index % spreadsheet.size.colCnt
      val colLetter = col.toString
        .map(char => (char.toString, 0))
        .foldRight(("", 0))((leftValue, rightValue) => {
          val (leftChar, leftPower)   = leftValue
          val (rightChar, rightPower) = rightValue
          rightChar +
        })
      colLetter
    }

    override def getIndexFromCell(spreadsheet: RawSpreadsheet, rowStr: String, colStr: String): Int = {
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
      (rowStr.toInt - 1) * spreadsheet.size.colCnt + col
    }

    override def parseCellExpr(spreadsheet: RawSpreadsheet, expr: String): String = {
      val splitExpressions = expr.split("(?<=[+\\-*/])")
      val cellValue = splitExpressions.foldLeft("0+")((leftValue, rightValue) => {
        val res2 = rightValue match {
          case numExprPattern(num) => num.toInt
          case cellNumPattern(colStr, rowStr) =>
            val index2 = getIndexFromCell(spreadsheet, rowStr, colStr)
            parseCell(spreadsheet, spreadsheet.table(index2)).result.toInt
        }
        val nextOperator = if (rightValue.takeRight(1).matches("[+\\-*/]")) rightValue.takeRight(1) else ""
        (leftValue.takeRight(1) match {
          case "+" => leftValue.dropRight(1).toInt + res2
          case "-" => leftValue.dropRight(1).toInt - res2
          case "*" => leftValue.dropRight(1).toInt * res2
          case "/" => leftValue.dropRight(1).toInt / res2
        }).toString + nextOperator
      })
      cellValue
    }

    override def parseCell(spreadsheet: RawSpreadsheet, rawCell: String): Cell = {
      rawCell match {
        case exprPattern(expr)     => Cell(parseCellExpr(spreadsheet, expr))
        case strPattern(strValue)  => Cell(strValue)
        case numPattern(value)     => Cell(value)
        case errorPattern(message) => Cell(message)
        case _ =>
          val cellIndex = getCellIndex(spreadsheet, rawCell)
          Cell(s"#Incorrect input in cell $cellIndex")
      }
    }

    override def process(spreadsheet: RawSpreadsheet): ProcessedSpreadsheet = {
      ProcessedSpreadsheet(
        spreadsheet.size,
        spreadsheet.table
          .map(cell => parseCell(spreadsheet, cell))
      )
    }
  }

  trait SpreadsheetWriter {
    def write(writePath: String, processedSpreadsheet: ProcessedSpreadsheet): Either[ErrorMessage, RawSpreadsheet]
  }

  class LocalSpreadsheetWriter() extends SpreadsheetWriter {
    override def write(
      writePath:            String,
      processedSpreadsheet: ProcessedSpreadsheet
    ): Either[ErrorMessage, RawSpreadsheet] = ???
  }

  def main(args: Array[String]): Unit = {
    val readPath  = "get from args - command line args"
    val writePath = "get from args - command line args"

    for {
      validReadPath  <- validateReadPath(readPath)
      validWritePath <- validateWritePath(writePath)

      spreadsheetParser  = new LocalSpreadsheetParser(validReadPath)
      parsedSpreadsheet <- spreadsheetParser.parse()

      spreadsheetProcessor = new SimpleSpreadsheetProcessor
      processedSpreadsheet = spreadsheetProcessor.process(parsedSpreadsheet)

      spreadsheetWriter = new LocalSpreadsheetWriter
      _                <- spreadsheetWriter.write(validWritePath, processedSpreadsheet)
    } yield ()
  }
}
