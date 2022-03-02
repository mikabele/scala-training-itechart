import org.scalatest.funsuite.AnyFunSuite

class CalculatorTest extends AnyFunSuite{
  test("Test gcd when a greater than b"){
    assert(Calculator.gcd(105,252)==21)
  }
  test("Test gcd when b greater than a"){
    assert(Calculator.gcd(252,105)==21)
  }
  test("Test lcm when a greater than b"){
    assert(Calculator.lcm(10,25)==50)
  }
  test("Test lcm when b greater than a"){
    assert(Calculator.lcm(25,10)==50)
  }
}
