import org.scalatest.funsuite.AnyFunSuite

class TableParserTest extends AnyFunSuite {
  test("Test input params parsing") {
    assert(TableParser.process("--input input.txt --output output.txt").isSuccess)
  }
}
