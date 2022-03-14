import ErrorHandlingHomework.AccountValidationError._
import ErrorHandlingHomework.{AccountValidator, PaymentCard, Person}
import cats.data.Validated.Invalid
import cats.syntax.all._
import eu.timepit.refined.auto._
import org.scalatest.funspec.AnyFunSpec

class ErrorHandlingHomeworkTest extends AnyFunSpec {
  describe("Person") {
    it("should throw type error if age is negative number") {
      assertTypeError("Person(\"MIKHAIL BIALEVICH\", -20, \"2002-01-24\", \"HB1234567\") ")
    }

    it("should throw type error if birthdate doesn't correspond pattern yyyy-MM-dd") {
      assertTypeError("Person(\"MIKHAIL BIALEVICH\", 20, \"200-01-24\", \"HB1234567\") ")
    }
  }

  describe("PaymentCard") {
    it("should throw type error if card number doesn't correspond pattern \\d{4} \\d{4} \\d{4} \\d{4}") {
      assertTypeError("PaymentCard(\"12341234 1234 1234\", \"07/23\", \"MIKHAIL BIALEVICH\", \"111\")")
    }

    it("should throw type error if expiration date doesn't correspond pattern MM/yy") {
      assertTypeError("PaymentCard(\"1234 1234 1234 1234\", \"07/232\", \"MIKHAIL BIALEVICH\", \"111\")")
    }

    it("should throw type error if security code doesn't correspond pattern \\d{3}") {
      assertTypeError("PaymentCard(\"1234 1234 1234 1234\", \"07/23\", \"MIKHAIL BIALEVICH\", \"1112\")")
    }
  }

  describe("Account") {
    it("should throw PersonIsNotACardHolder if card holder name doesn't correspond person name") {
      val person = Person("MIKHAI BIALEVICH", 20, "2002-01-24", "HB1234567")
      val card   = PaymentCard("1234 1234 1234 1234", "07/23", "MIKHAIL BIALEVICH", "111")
      val acc    = AccountValidator.validate(person, card)
      assert(acc.isInvalid)
      val errChain = acc match {
        case Invalid(e) => e.toList
      }
      assert(errChain.contains(PersonIsNotACardHolder))
    }

    it("should throw BirthdateFromFuture error if birthdate greater than Date.now") {
      val person = Person("MIKHAI BIALEVICH", 20, "2023-01-24", "HB1234567")
      val card   = PaymentCard("1234 1234 1234 1234", "07/23", "MIKHAIL BIALEVICH", "111")
      val acc    = AccountValidator.validate(person, card)
      assert(acc.isInvalid)
      val errChain = acc match {
        case Invalid(e) => e.toList
      }
      assert(errChain.contains(BirthdateFromFuture))
    }

    it("should throw ZeroSecurityCode if security code equals to 000") {
      val person = Person("MIKHAI BIALEVICH", 20, "2023-01-24", "HB1234567")
      val card   = PaymentCard("1234 1234 1234 1234", "07/23", "MIKHAIL BIALEVICH", "000")
      val acc    = AccountValidator.validate(person, card)
      assert(acc.isInvalid)
      val errChain = acc match {
        case Invalid(e) => e.toList
      }
      assert(errChain.contains(ZeroSecurityCode))
    }

    it("should throw ExpirationDateIsOver error if expiration date is over") {
      val person = Person("MIKHAI BIALEVICH", 20, "2023-01-24", "HB1234567")
      val card   = PaymentCard("1234 1234 1234 1234", "07/21", "MIKHAIL BIALEVICH", "111")
      val acc    = AccountValidator.validate(person, card)
      assert(acc.isInvalid)
      val errChain = acc match {
        case Invalid(e) => e.toList
      }
      assert(errChain.contains(ExpirationDateIsOver))
    }

    it("should throw MismatchOfAgeAndBirthdate error if age doesn't correspond birthdate") {
      val person = Person("MIKHAI BIALEVICH", 20, "2023-01-24", "HB1234567")
      val card   = PaymentCard("1234 1234 1234 1234", "07/23", "MIKHAIL BIALEVICH", "111")
      val acc    = AccountValidator.validate(person, card)
      assert(acc.isInvalid)
      val errChain = acc match {
        case Invalid(e) => e.toList
      }
      assert(errChain.contains(MismatchOfAgeAndBirthdate))
    }
  }
}
