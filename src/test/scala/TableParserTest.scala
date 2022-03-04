import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success}

class TableParserTest extends AnyFunSuite {
  test("Test input params parsing") {
    val successfulTest = TableParser.readFile("src/test/testFiles/task2SuccessfulPath.txt") match {
      case Success(value) => value
      case Failure(_)     => ""
    }
    assert(Solver.process("table --input input.txt --output output.txt") == successfulTest)
  }
}
