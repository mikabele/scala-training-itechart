import ErrorHandlingHomework.AccountValidationError.PersonIsNotACardHolder
import ErrorHandlingHomework.{AccountValidator, PaymentCard, Person}
import cats.data.Validated.Invalid
import org.scalatest.funspec.AnyFunSpec
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._
import cats.data.ValidatedNec
import cats.syntax.all._

class ErrorHandlingHomeworkTest extends AnyFunSpec {
  describe("Person") {
    it("should throw type error if age is negative number") {
      assertTypeError("Person(\"MIKHAIL BIALEVICH\", -20, \"2002-01-24\", \"HB1234567\") ")
    }

    it("should throw type error if birthdate doesn't correspond pattern yyyy-MM-dd") {
      assertTypeError("Person(\"MIKHAIL BIALEVICH\", 20, \"200-01-24\", \"HB1234567\") ")
    }

    it("should throw type error if passport number doesn't correspond pattern [A-Z]{2}\\d{7}") {
      assertTypeError("Person(\"MIKHAIL BIALEVICH\", 20, \"2002-01-24\", \"Hf1243567\") ")
    }

    it("should compile successfully otherwise") {
      assertCompiles("Person(\"MIKHAIL BIALEVICH\", 20, \"2002-01-24\", \"HB1234567\")")
    }
  }

  describe("PaymentCard") {
    it("should throw type error if card number doesn't correspond pattern \\d{4} \\d{4} \\d{4} \\d{4}") {
      assertTypeError("PaymentCard(\"12341234 1234 1234\", \"07/23\", \"MIKHAIL BIALEVICH\", \"111\")")
    }

    it("should throw type error if expiration date doesn't correspond pattern MM/yy") {
      assertTypeError("PaymentCard(\"1234 1234 1234 1234\", \"07/232\", \"MIKHAIL BIALEVICH\", \"111\")")
    }

    it("should throw type error if card holder value doesn't correspond pattern \"\\w+ \\w+\"") {
      assertTypeError("PaymentCard(\"1234 1234 1234 1234\", \"07/23\", \"MIKHAIL+ BIALEVICH\", \"111\")")
    }

    it("should throw type error if security code doesn't correspond pattern \\d{3}") {
      assertTypeError("PaymentCard(\"1234 1234 1234 1234\", \"07/23\", \"MIKHAIL BIALEVICH\", \"1112\")")
    }

    it("should compiles successfully otherwise") {
      assertCompiles("PaymentCard(\"1234 1234 1234 1234\", \"07/23\", \"MIKHAIL BIALEVICH\", \"111\")")
    }
  }

  describe("Account") {
    it("should throw PersonIsNotACardHolder if card holder name doesn't correspond person name") {
      val person = Person("MIKHAI BIALEVICH", 20, "2002-01-24", "HB1234567")
      val card   = PaymentCard("1234 1234 1234 1234", "07/23", "MIKHAIL BIALEVICH", "111")
      val acc    = AccountValidator.validate(person, card)
      assert(acc.isInvalid)
      val errChain = acc match {
        case Invalid(e) => e.toChain
      }
      assert(errChain.headOption.contains(PersonIsNotACardHolder))
    }
  }
}
