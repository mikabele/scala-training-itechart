import TestTaskHomework.{validateReadPath, LocalSpreadsheetParser}
import org.scalatest.funsuite.AnyFunSuite

class TableParserTest extends AnyFunSuite {
  test("Test successful path") {
    val expectedSpreadsheet = Array("12	-4	3	Sample", "4	-16	-4	Spread", "Test	1	5	Sheet")
    TestTaskHomework.main("--input input.txt --output output.txt".split("\\s+"))
    val actualSpreadsheet = TestTaskHomework.readFile("output.txt").getOrElse("").split("\\r\\n")
    assert(expectedSpreadsheet.sameElements(actualSpreadsheet))
  }

  test("Test error message in cells") {
    val expectedSpreadsheet = Array("#Incorrect input	3", "4	#Incorrect input")
    TestTaskHomework.main(
      "--input src/test/testFiles/TestTask/errorInExpressionCell.txt --output output1.txt".split("\\s+")
    )
    val actualSpreadsheet = TestTaskHomework.readFile("output1.txt").getOrElse("").split("\\r\\n")
    assert(expectedSpreadsheet.sameElements(actualSpreadsheet))
  }

  test("Test raw spreadsheets") {
    val actualSpreadsheet = for {
      validReadPath     <- validateReadPath(Some("input.txt"))
      spreadsheetParser  = new LocalSpreadsheetParser(validReadPath)
      parsedSpreadsheet <- spreadsheetParser.parse()
    } yield parsedSpreadsheet.table
    val expectedSpreadsheet =
      Array("12", "=C2", "3", "'Sample", "=A1+B1*C1/5", "=A2*B1", "=B3-C3", "'Spread", "'Test", "=4-3", "5", "'Sheet")
    assert(expectedSpreadsheet.sameElements(actualSpreadsheet.getOrElse(Nil)))
  }

  test("Test error in expression") {
    val expectedSpreadsheet = Array("#Incorrect input	3", "4	#Incorrect input")
    TestTaskHomework.main(
      "--input src/test/testFiles/TestTask/errorInExpressionCellWithArithmeticOperations.txt --output output2.txt"
        .split("\\s+")
    )
    val actualSpreadsheet = TestTaskHomework.readFile("output2.txt").getOrElse("").split("\\r\\n")
    assert(expectedSpreadsheet.sameElements(actualSpreadsheet))
  }

  test("Test empty input file") {
    val actualSpreadsheet = for {
      validReadPath     <- validateReadPath(Some("src/test/testFiles/TestTask/emptyTest.txt"))
      spreadsheetParser  = new LocalSpreadsheetParser(validReadPath)
      parsedSpreadsheet <- spreadsheetParser.parse()
    } yield parsedSpreadsheet
    assert(actualSpreadsheet.left.getOrElse("") == "Error: Input file is empty")
  }

  test("Test incorrect corresponding of table size and given sizes") {
    val actualSpreadsheet = for {
      validReadPath <- validateReadPath(
        Some("src/test/testFiles/TestTask/miscorrespondingOfTableSizeAndGivenSizes.txt")
      )
      spreadsheetParser  = new LocalSpreadsheetParser(validReadPath)
      parsedSpreadsheet <- spreadsheetParser.parse()
    } yield parsedSpreadsheet
    assert(actualSpreadsheet.left.getOrElse("") == "Error: Table size doesn't correspond given sizes")
  }

  test("Test incorrect given sizes") {
    val actualSpreadsheet = for {
      validReadPath     <- validateReadPath(Some("src/test/testFiles/TestTask/incorrectGivenSizes.txt"))
      spreadsheetParser  = new LocalSpreadsheetParser(validReadPath)
      parsedSpreadsheet <- spreadsheetParser.parse()
    } yield parsedSpreadsheet
    assert(actualSpreadsheet.left.getOrElse("") == "Error: Incorrect table sizes")
  }
}
