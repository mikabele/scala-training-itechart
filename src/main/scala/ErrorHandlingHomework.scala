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

  type PositiveInt    = Int Refined Positive
  type BirthDay       = String Refined MatchesRegex["\\d{4}-\\d{2}-\\d{2}"]
  type CardNumber     = String Refined MatchesRegex["\\d{4} \\d{4} \\d{4} \\d{4}"]
  type ExpirationDate = String Refined MatchesRegex["\\d\\d/\\d\\d"]
  type SecurityCode   = String Refined MatchesRegex["\\d{3}"]

  // think about adding Refined integration here to provide type level validation
  final case class Account(person: Person, card: PaymentCard)
  final case class Person(
    name:           String,
    age:            PositiveInt,
    birthDay:       BirthDay,
    passportNumber: String
  ) // name, age, birthDay, passportNumber
  final case class PaymentCard(
    cardNumber:     CardNumber,
    expirationDate: ExpirationDate,
    cardHolder:     String,
    securityCode:   SecurityCode
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
    final case object BirthdateFromFuture extends AccountValidationError {
      override def toString: String = "You can't specify birthdate greater than Date.Now"
    }
    final case object InvalidPersonName extends AccountValidationError {
      override def toString: String = "You should enter name according to pattern \"<NAME> <SURNAME>\" ignore case"
    }
    final case object InvalidPassportNumber extends AccountValidationError {
      override def toString: String = "You should enter passport number according to pattern \"AA0000000\""
    }
    final case object ZeroSecurityCode extends AccountValidationError {
      override def toString: String = "Invalid security code 000"
    }
  }

  object AccountValidator {

    type AllErrorsOr[A] = ValidatedNec[AccountValidationError, A]

    private val expirationDateFormat = new SimpleDateFormat("MM/yyyy")
    private val birthDateFormat      = new SimpleDateFormat("yyyy-MM-dd")

    private val personNamePattern:     scala.util.matching.Regex = "(\\w+ \\w+)".r
    private val passportNumberPattern: scala.util.matching.Regex = "([A-Z]{2}\\d{7})".r

    private val dateNow = Calendar.getInstance().getTime

    private def toLocalDate(date: Date): LocalDate = {
      date.toInstant.atZone(ZoneId.systemDefault()).toLocalDate
    }

    private def calcDateDiff(first: LocalDate, second: LocalDate): Int = {
      val period = Period.between(first, second)
      Math.abs(period.getYears)
    }

    private def validateCardNumber(number: CardNumber): AllErrorsOr[CardNumber] = number.validNec

    private def validateSecurityCode(code: SecurityCode): AllErrorsOr[SecurityCode] = if (code.value != "000")
      code.validNec
    else
      ZeroSecurityCode.invalidNec

    private def validateCardHolder(name: String, cardHolder: String): AllErrorsOr[String] =
      if (name.compareToIgnoreCase(cardHolder) == 0)
        cardHolder.validNec
      else
        PersonIsNotACardHolder.invalidNec

    private def validateExpirationDate(expirationDate: ExpirationDate): AllErrorsOr[ExpirationDate] =
      if (expirationDateFormat.parse(expirationDate).before(dateNow))
        ExpirationDateIsOver.invalidNec
      else
        expirationDate.validNec

    private def validateAge(age: PositiveInt, birthDay: BirthDay): AllErrorsOr[PositiveInt] = {
      if (calcDateDiff(toLocalDate(dateNow), toLocalDate(birthDateFormat.parse(birthDay.value))) == age.value)
        age.validNec
      else
        MismatchOfAgeAndBirthdate.invalidNec
    }

    private def validatePersonName(name: String): AllErrorsOr[String] = name match {
      case personNamePattern(_) => name.validNec
      case _                    => InvalidPersonName.invalidNec
    }

    private def validatePassportNumber(number: String): AllErrorsOr[String] = number match {
      case passportNumberPattern(_) => number.validNec
      case _                        => InvalidPassportNumber.invalidNec
    }

    private def validateBirthDate(
      birthDay: BirthDay
    ): AllErrorsOr[BirthDay] = if (birthDateFormat.parse(birthDay.value).before(dateNow))
      birthDay.validNec
    else
      BirthdateFromFuture.invalidNec

    private def validatePerson(p: Person): AllErrorsOr[Person] = {
      (
        validatePersonName(p.name),
        validateAge(p.age, p.birthDay),
        validateBirthDate(p.birthDay),
        validatePassportNumber(p.passportNumber)
      )
        .mapN(Person)
    }

    private def validatePaymentCard(p: Person, c: PaymentCard): AllErrorsOr[PaymentCard] = (
      validateCardNumber(c.cardNumber),
      validateExpirationDate(c.expirationDate),
      validateCardHolder(p.name, c.cardHolder),
      validateSecurityCode(c.securityCode)
    ).mapN(PaymentCard)

    def validate(person: Person, card: PaymentCard): AllErrorsOr[Account] =
      (validatePerson(person), validatePaymentCard(person, card)).mapN(Account)
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
