import org.scalatest.funsuite.AnyFunSuite

class CalculatorTest extends AnyFunSuite {
  test("Test gcd when a greater than b - successful path") {
    assert(Calculator.gcd(105, 252) == 21)
  }
  test("Test gcd when a or b equals to 0") {
    assert(Calculator.gcd(252, 0) == 252)
  }
  test("Test gcd with one negative number") {
    assert(Calculator.gcd(20, -8) == 4)
  }
  test("Test lcm when a greater than b - successful path") {
    assert(Calculator.lcm(10, 25).getOrElse(0) == 50)
  }
  test("Test lcm when b or a equals to 0") {
    assert(Calculator.lcm(25, 0).getOrElse(-1) == 0)
  }
  test("Test lcm when a and b equal to 0") {
    assert(Calculator.lcm(0, 0).isEmpty)
  }
  test("Test lcm with one negative number") {
    assert(Calculator.lcm(20, -8).getOrElse(0) == 40)
  }
}
