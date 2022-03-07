import java.io.{File, FileWriter}
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try, Using}

object TestTaskHomework {

  type ErrorMessage = String
  val errorPrefix      = "Error: "
  val cellErrorMessage = "#Incorrect message"
  final case class RawSpreadsheet(size: Size, table: Seq[String])
  final case class ProcessedSpreadsheet(size: Size, cells: Seq[Cell])
  final case class Size(rowCnt: Int, colCnt: Int)
  final case class Cell(result: String)

  def validateReadPath(readPath: Option[String]): Either[ErrorMessage, String] = {
    readPath match {
      case None => Left(errorPrefix + "Input file doesn't exist")
      case Some(path) =>
        if (Files.exists(Paths.get(path))) Right(path) else Left(errorPrefix + "Input file doesn't exist")
    }

  }

  def validateWritePath(readPath: Option[String]): Either[ErrorMessage, String] = {
    readPath match {
      case None => Right("console")
      case Some(path) =>
        val readDir = path.split("(?>/)").dropRight(1).mkString("")
        if (Files.exists(Paths.get(readDir))) Right(path) else Left(errorPrefix + "Output file doesn't exist")
    }
  }

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
          val rows = content.split("\\s+").toList
          rows match {
            case "" :: Nil =>
              Left(errorPrefix + "Input file is empty")
            case rowNumStr :: colNumStr :: content =>
              val rowNum = Try(rowNumStr.toInt)
              val colNum = Try(colNumStr.toInt)
              if (rowNum.isSuccess && colNum.isSuccess) {
                val size = Size(rowNum.getOrElse(0), colNum.getOrElse(0))
                if (content.size == size.rowCnt * size.colCnt)
                  Right(RawSpreadsheet(size, content))
                else
                  Left(errorPrefix + "Table size doesn't correspond given sizes")
              } else
                Left(errorPrefix + "Incorrect table sizes")
          }
        case Failure(exception) =>
          Left(errorPrefix + exception.getStackTrace.mkString("\r\n"))
      }
    }
  }

  trait SpreadsheetProcessor {
    def process(spreadsheet:          RawSpreadsheet): ProcessedSpreadsheet
    def parseCell(spreadsheet:        RawSpreadsheet, rawCell: String): Cell
    def getIndexFromCell(spreadsheet: RawSpreadsheet, rowStr: String, colStr: String): Int
    def parseCellExpr(spreadsheet:    RawSpreadsheet, cell:   String): String
  }

  class SimpleSpreadsheetProcessor extends SpreadsheetProcessor {

    val exprPattern:    Regex = "=(.*)".r
    val strPattern:     Regex = "'(.*)".r
    val numPattern:     Regex = "(\\d+)".r
    val errorPattern:   Regex = "(#.*)".r
    val cellNumPattern: Regex = "(\\w)+(\\d+)[+\\-*/]?".r
    val numExprPattern: Regex = "(\\d)+[+\\-*/]?".r

    override def getIndexFromCell(spreadsheet: RawSpreadsheet, rowStr: String, colStr: String): Int = {
      val colLetters = colStr.split("")
      val (col, _) = colLetters
        .map((_, 0))
        .foldRight(("0", -1))((leftValue, rightValue) => {
          val (nonCalcExpr, _)       = leftValue
          val (calcExpr, rightPower) = rightValue
          ((calcExpr.toInt + (nonCalcExpr.head - 'A') * Math.pow(26, rightPower + 1).toInt).toString, rightPower + 1)
        })
      (rowStr.toInt - 1) * spreadsheet.size.colCnt + col.toInt
    }

    def checkError(x: String)(successfulFunc: String => String): String = {
      x match {
        case errorPattern(error) => error
        case _                   => successfulFunc(x)
      }
    }

    def computeValue(x: String, nextOperator: String)(y: String): String = {
      val calcValue = x.takeRight(1) match {
        case "+" => x.dropRight(1).toInt + y.toInt
        case "-" => x.dropRight(1).toInt - y.toInt
        case "*" => x.dropRight(1).toInt * y.toInt
        case "/" => x.dropRight(1).toInt / y.toInt
      }
      calcValue.toString + nextOperator
    }

    override def parseCellExpr(spreadsheet: RawSpreadsheet, expr: String): String = {
      val splitExpressions = expr.split("(?<=[+\\-*/])")
      val cellValue = splitExpressions.foldLeft("0+")((leftValue, rightValue) => {
        checkError(leftValue)(x => {
          rightValue match {
            case numExprPattern(_) | cellNumPattern(_, _) =>
              val res2 = rightValue match {
                case numExprPattern(num) => num
                case cellNumPattern(colStr, rowStr) =>
                  val index2 = getIndexFromCell(spreadsheet, rowStr, colStr)
                  parseCell(spreadsheet, spreadsheet.table(index2)).result
              }
              val nextOperator = if (rightValue.takeRight(1).matches("[+\\-*/]")) rightValue.takeRight(1) else ""
              checkError(res2)(computeValue(x, nextOperator))
            case _ => "#Incorrect input"
          }
        })
      })
      cellValue
    }

    override def parseCell(spreadsheet: RawSpreadsheet, rawCell: String): Cell = {
      rawCell match {
        case exprPattern(expr)     => Cell(parseCellExpr(spreadsheet, expr))
        case strPattern(strValue)  => Cell(strValue)
        case numPattern(value)     => Cell(value)
        case errorPattern(message) => Cell(message)
        case _                     => Cell(s"#Incorrect input")
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

    def writeFile(filename: String, lines: Seq[String], delimiter: String): Unit = {
      val file = new File(filename)
      val fw   = new FileWriter(file)
      for (line <- lines) {
        fw.write(line + delimiter)
      }
      fw.close()
    }

    override def write(
      writePath:            String,
      processedSpreadsheet: ProcessedSpreadsheet
    ): Either[ErrorMessage, RawSpreadsheet] = {
      Try(
        processedSpreadsheet.cells
          .map(_.result)
          .grouped(processedSpreadsheet.size.colCnt)
          .map(_.mkString("\t"))
      ) match {
        case Success(value) =>
          writePath match {
            case "console" => value.foreach(println)
            case filePath  => writeFile(filePath, value.toSeq, "\r\n")
          }
          Right(RawSpreadsheet(processedSpreadsheet.size, processedSpreadsheet.cells.map(_.result)))
        case Failure(exception) =>
          Left(exception.getStackTrace.mkString("\r\n"))
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val readPath = args.indexOf("--input") match {
      case -1    => None
      case index => Some(args(index + 1))
    }
    val writePath = args.indexOf("--output") match {
      case -1    => None
      case index => Some(args(index + 1))
    }

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
