import scopt.OParser

import java.io.{File, FileWriter}
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try, Using}

object TestTaskHomework {

  type ErrorMessage = String
  val errorPrefix                = "Error: "
  val cellErrorMessage           = "#Incorrect input"
  val englishAlphabetLetterCount = 26

  final case class RawSpreadsheet(size: Size, table: Seq[String])
  final case class ProcessedSpreadsheet(size: Size, cells: Seq[Cell])
  final case class Size(rowCnt: Int, colCnt: Int)
  final case class Cell(result: String)

  def validateReadPath(readPath: String): Either[ErrorMessage, String] = {
    if (Files.exists(Paths.get(readPath)))
      Right(readPath)
    else
      Left(errorPrefix + "Input file doesn't exist")
  }

  def validateWritePath(readPath: String): Either[ErrorMessage, String] = {
    val readDir = readPath.split("(?>/)").dropRight(1).mkString("")
    if (Files.exists(Paths.get(readDir)))
      Right(readPath)
    else
      Left(errorPrefix + "Output file doesn't exist")
  }

  trait SpreadsheetParser {
    def parse(): Either[ErrorMessage, RawSpreadsheet]
  }

  def readFile(inputFile: String): Try[String] = {
    Using(Source.fromFile(new File(inputFile))) { source => source.mkString }
  }

  class LocalSpreadsheetParser(path: String) extends SpreadsheetParser {

    private def validateTableSize(content: Seq[String], rowNum: Int, colNum: Int): Either[String, RawSpreadsheet] = {
      val size = Size(rowNum, colNum)
      if (content.size == size.rowCnt * size.colCnt)
        Right(RawSpreadsheet(size, content))
      else
        Left(errorPrefix + "Table size doesn't correspond given sizes")
    }

    private def parseRawContent(rows: List[String]): Either[ErrorMessage, RawSpreadsheet] = rows match {
      case "" :: Nil =>
        Left(errorPrefix + "Input file is empty")
      case rowNumStr :: colNumStr :: content =>
        val rowNum = Try(rowNumStr.toInt)
        val colNum = Try(colNumStr.toInt)
        if (rowNum.isSuccess && colNum.isSuccess)
          validateTableSize(content, rowNum.getOrElse(0), colNum.getOrElse(0))
        else
          Left(errorPrefix + "Incorrect table sizes")
    }

    override def parse(): Either[ErrorMessage, RawSpreadsheet] = readFile(path) match {
      case Success(content) =>
        val rows = content.split("\\s+").toList
        parseRawContent(rows)
      case Failure(exception) =>
        Left(errorPrefix + exception.getStackTrace.mkString("\r\n"))
    }

  }

  trait SpreadsheetProcessor {
    def process(spreadsheet: RawSpreadsheet): ProcessedSpreadsheet
  }

  class SimpleSpreadsheetProcessor extends SpreadsheetProcessor {

    val exprPattern:    Regex = "=(.*)".r
    val strPattern:     Regex = "'(.*)".r
    val numPattern:     Regex = "(\\d+)".r
    val errorPattern:   Regex = "(#.*)".r
    val cellNumPattern: Regex = "(\\w)+(\\d+)[+\\-*/]?".r
    val numExprPattern: Regex = "(\\d)+[+\\-*/]?".r

    private def addLetterToIndex(leftValue: (String, Int), rightValue: (String, Int)): (String, Int) = {
      val (nonCalcExpr, _)       = leftValue
      val (calcExpr, rightPower) = rightValue
      (
        (calcExpr.toInt + (nonCalcExpr.head - 'A') * Math
          .pow(englishAlphabetLetterCount, rightPower + 1)
          .toInt).toString,
        rightPower + 1
      )
    }

    private def getIndexFromCell(spreadsheet: RawSpreadsheet, rowStr: String, colStr: String): Int = {
      val colLetters = colStr.split("")
      val (col, _) = colLetters
        .map((_, 0))
        .foldRight(("0", -1))(addLetterToIndex)
      (rowStr.toInt - 1) * spreadsheet.size.colCnt + col.toInt
    }

    private def checkError(x: String)(successfulFunc: String => String): String = {
      x match {
        case errorPattern(error) => error
        case _                   => successfulFunc(x)
      }
    }

    private def computeValue(x: String, nextOperator: String)(y: String): String = {
      val calcValue = x.takeRight(1) match {
        case "+" => x.dropRight(1).toInt + y.toInt
        case "-" => x.dropRight(1).toInt - y.toInt
        case "*" => x.dropRight(1).toInt * y.toInt
        case "/" => x.dropRight(1).toInt / y.toInt
      }
      calcValue.toString + nextOperator
    }

    private def parseNonCalcExpression(rightValue: String, spreadsheet: RawSpreadsheet): String = rightValue match {
      case numExprPattern(num) => num
      case cellNumPattern(colStr, rowStr) =>
        val index2 = getIndexFromCell(spreadsheet, rowStr, colStr)
        parseCell(spreadsheet, spreadsheet.table(index2)).result
    }

    private def mergeTwoExpressions(rightValue: String, spreadsheet: RawSpreadsheet)(leftValue: String): String =
      rightValue match {
        case numExprPattern(_) | cellNumPattern(_, _) =>
          val res2         = parseNonCalcExpression(rightValue, spreadsheet)
          val nextOperator = if (rightValue.takeRight(1).matches("[+\\-*/]")) rightValue.takeRight(1) else ""
          checkError(res2)(computeValue(leftValue, nextOperator))
        case _ => cellErrorMessage
      }

    private def parseCellExpr(spreadsheet: RawSpreadsheet, expr: String): String = {
      val splitExpressions = expr.split("(?<=[+\\-*/])")
      val cellValue = splitExpressions.foldLeft("0+")((leftValue, rightValue) => {
        checkError(leftValue)(mergeTwoExpressions(rightValue, spreadsheet))
      })
      cellValue
    }

    private def parseCell(spreadsheet: RawSpreadsheet, rawCell: String): Cell = {
      rawCell match {
        case exprPattern(expr)     => Cell(parseCellExpr(spreadsheet, expr))
        case strPattern(strValue)  => Cell(strValue)
        case numPattern(value)     => Cell(value)
        case errorPattern(message) => Cell(message)
        case _                     => Cell(cellErrorMessage)
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
          writeFile(writePath, value.toSeq, "\r\n")
          Right(RawSpreadsheet(processedSpreadsheet.size, processedSpreadsheet.cells.map(_.result)))
        case Failure(exception) =>
          Left(exception.getStackTrace.mkString("\r\n"))
      }
    }
  }

  case class Config(
    input:  String = "",
    output: String = "",
  )

  def parseArguments(args: Array[String]): Either[ErrorMessage, Config] = {
    val builder = OParser.builder[Config]
    val parser1 = {
      import builder._
      OParser.sequence(
        programName("scopt"),
        head("scopt", "4.x"),
        // option -i, --input
        opt[String]('i', "input")
          .action((x, c) => c.copy(input = x))
          .required()
          .text("input is the path for input file"),
        // option -o, --output
        opt[String]('o', "output")
          .action((x, c) => c.copy(output = x))
          .required()
          .text("output is the path for output file"),
      )
    }
    OParser.parse(parser1, args, Config()) match {
      case Some(value) => Right(value)
      case _           => Left(errorPrefix + "Error during parsing arguments")
    }
  }

  def main(args: Array[String]): Unit = {
    for {
      parsedArgs     <- parseArguments(args)
      readPath        = parsedArgs.input
      writePath       = parsedArgs.output
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
