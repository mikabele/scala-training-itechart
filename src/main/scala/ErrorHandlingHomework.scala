import ErrorHandlingHomework.AccountValidationError._
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.syntax.all._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._

import java.text.SimpleDateFormat
import java.time.{LocalDate, Period, ZoneId}
import java.util.{Calendar, Date}

object ErrorHandlingHomework {

  // think about adding Refined integration here to provide type level validation
  final case class Account(person: Person, card: PaymentCard)
  final case class Person(
    name:           String,
    age:            Int Refined Positive,
    birthDay:       String Refined MatchesRegex["\\d{4}-\\d{2}-\\d{2}"],
    passportNumber: String Refined MatchesRegex["[A-Z]{2}\\d{7}"]
  ) // name, age, birthDay, passportNumber
  final case class PaymentCard(
    cardNumber:     String Refined MatchesRegex["\\d{4} \\d{4} \\d{4} \\d{4}"],
    expirationDate: String Refined MatchesRegex["\\d\\d/\\d\\d"],
    cardHolder:     String Refined MatchesRegex["\\w+ \\w+"],
    securityCode:   String Refined MatchesRegex["\\d{3}"]
  ) // card number, expirationDate, securityCode etc

  sealed trait AccountValidationError
  object AccountValidationError {
    final case object PersonIsNotACardHolder extends AccountValidationError {
      override def toString: String = "Person name doesn't correspond name of card holder."
    }
    final case object ExpirationDateIsOver extends AccountValidationError {
      override def toString: String = "Expiration date is over."
    }
    final case object MismatchOfAgeAndBirthdate extends AccountValidationError {
      override def toString: String = "Given age doesn't correspond birthdate."
    }
  }

  object AccountValidator {

    type AllErrorsOr[A] = ValidatedNec[AccountValidationError, A]

    private val expirationDateFormat = new SimpleDateFormat("MM/yyyy")
    private val birthDateFormat      = new SimpleDateFormat("yyyy-MM-dd")

    private val dateNow = Calendar.getInstance().getTime

    private def toLocalDate(date: Date): LocalDate = {
      date.toInstant.atZone(ZoneId.systemDefault()).toLocalDate
    }

    private def calcDateDiff(first: LocalDate, second: LocalDate): Int = {
      val period = Period.between(first, second)
      Math.abs(period.getYears)
    }

    private def validateAge(p: Person): Boolean =
      calcDateDiff(toLocalDate(dateNow), toLocalDate(birthDateFormat.parse(p.birthDay.value))) != p.age.value

    def validate(person: Person, card: PaymentCard): AllErrorsOr[Account] = (person, card) match {
      case (p, c) if p.name.compareToIgnoreCase(c.cardHolder.value) != 0         => PersonIsNotACardHolder.invalidNec
      case (_, c) if expirationDateFormat.parse(c.expirationDate).after(dateNow) => ExpirationDateIsOver.invalidNec
      case (p, _) if validateAge(p)                                              => MismatchOfAgeAndBirthdate.invalidNec
      case _                                                                     => Account(person, card).validNec
    }
  }

  def main(args: Array[String]): Unit = {
    val person = Person("MIKHAIL BIALEVICH", 20, "2002-01-24", "HB1234567")
    val card   = PaymentCard("1234 1234 1234 1234", "07/23", "MIKHAIL BIALEVICH", "111")
    AccountValidator.validate(person, card) match {
      case Valid(a)   => println(a)
      case Invalid(e) => println(e)
    }
  }
}
