import ErrorHandlingHomework.AccountValidationError._
import cats.data.ValidatedNec
import cats.syntax.all._
import eu.timepit.refined._
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._

import java.text.SimpleDateFormat
import java.util.Calendar

object ErrorHandlingHomework {

  type Age            = Int Refined Interval.Open[0, 130]
  type BirthDay       = String Refined MatchesRegex["\\d{4}-\\d{2}-\\d{2}"]
  type CardNumber     = String Refined MatchesRegex["\\d{4} \\d{4} \\d{4} \\d{4}"]
  type ExpirationDate = String Refined MatchesRegex["\\d\\d/\\d\\d"]
  type SecurityCode   = String Refined MatchesRegex["\\d{3}"]
  type PersonName     = String Refined MatchesRegex["[A-Z]+ [A-Z]+"]
  type PassportNumber = String Refined MatchesRegex["[A-Z]{2}\\d{7}"]

  // think about adding Refined integration here to provide type level validation
  //domain classes
  final case class Account(person: Person, card: PaymentCard)
  final case class Person(
    name:           PersonName,
    age:            Age,
    birthDay:       BirthDay,
    passportNumber: PassportNumber
  ) // name, age, birthDay, passportNumber
  final case class PaymentCard(
    cardNumber:     CardNumber,
    expirationDate: ExpirationDate,
    cardHolder:     PersonName,
    securityCode:   SecurityCode
  ) // card number, expirationDate, securityCode etc

  //dto classes
  final case class PersonDto(name: String, age: Int, birthDay: String, passportNumber: String)
  final case class PaymentCardDto(cardNumber: String, expirationDate: String, cardHolder: String, securityCode: String)

  sealed trait AccountValidationError
  object AccountValidationError {
    final case object InvalidExpirationDate extends AccountValidationError {
      override def toString: String =
        "You should enter expiration date in format MM/yy and this date shouldn't be over."
    }
    final case object InvalidAge extends AccountValidationError {
      override def toString: String = "You should select age in diapason from 0 to 130."
    }
    final case object InvalidBirthdate extends AccountValidationError {
      override def toString: String =
        "You should enter birthdate in format yyyy-MM-dd"
    }
    final case object BirthdateFromFuture extends AccountValidationError {
      override def toString: String = "You can't enter birthdate after Date.Now"
    }
    final case object InvalidPersonName extends AccountValidationError {
      override def toString: String = "You should enter name according to pattern \"<NAME> <SURNAME>\" ignore case"
    }
    final case object InvalidPassportNumber extends AccountValidationError {
      override def toString: String = "You should enter passport number according to pattern \"AA0000000\""
    }
    final case object InvalidSecurityCode extends AccountValidationError {
      override def toString: String = "Security code should have format \"XXX\""
    }
    final case object InvalidCardNumber extends AccountValidationError {
      override def toString: String = "You should enter card number according to pattern \"XXXX XXXX XXXX XXXX\""
    }
    final case object ExpirationDateIsOver extends AccountValidationError {
      override def toString: String = "Expiration date is over"
    }
    final case object MismatchAgeAndBirthdate extends AccountValidationError {
      override def toString: String = "Mismatch of age and years from your birthdate"
    }
  }

  object AccountValidator {

    type AllErrorsOr[A] = ValidatedNec[AccountValidationError, A]

    private val expirationDateFormat = new SimpleDateFormat("MM/yy")
    private val birthDateFormat      = new SimpleDateFormat("yyyy-MM-dd")

    private val dateNow = Calendar.getInstance().getTime

    private def refinedValidation[V, R](
      value: V,
      error: AccountValidationError
    )(
      implicit validator: Validate[V, R]
    ): AllErrorsOr[V Refined R] =
      refineV(value)(validator).left.map(_ => error).toValidatedNec

    private def validateCardNumber(number: String): AllErrorsOr[CardNumber] =
      refinedValidation(number, InvalidCardNumber)

    private def validateSecurityCode(code: String): AllErrorsOr[SecurityCode] =
      refinedValidation(code, InvalidSecurityCode)

    private def validateCardHolder(cardHolder: String): AllErrorsOr[PersonName] =
      refinedValidation(cardHolder, InvalidPersonName)

    private def validateExpirationDate(expirationDate: String): AllErrorsOr[ExpirationDate] = {
      def checkExpirationDateIsOver(validatedExpirationDate: ExpirationDate): AllErrorsOr[ExpirationDate] =
        if (expirationDateFormat.parse(validatedExpirationDate.value).before(dateNow))
          ExpirationDateIsOver.invalidNec
        else
          validatedExpirationDate.validNec

      val refinedRes: AllErrorsOr[ExpirationDate] = refinedValidation(expirationDate, InvalidExpirationDate)
      refinedRes.andThen(checkExpirationDateIsOver)
    }

    private def validateAge(age: Int): AllErrorsOr[Age] =
      refinedValidation(age, InvalidAge)

    private def validatePersonName(name: String): AllErrorsOr[PersonName] =
      refinedValidation(name, InvalidPersonName)

    private def validatePassportNumber(number: String): AllErrorsOr[PassportNumber] =
      refinedValidation(number, InvalidPassportNumber)

    private def validateBirthDate(birthDay: String): AllErrorsOr[BirthDay] = {
      def checkBirthDateIsNotFromFuture(validatedBirthDay: BirthDay): AllErrorsOr[BirthDay] =
        if (birthDateFormat.parse(validatedBirthDay.value).before(dateNow))
          validatedBirthDay.validNec
        else
          BirthdateFromFuture.invalidNec

      val refinedRes: AllErrorsOr[BirthDay] = refinedValidation(birthDay, InvalidBirthdate)
      refinedRes.andThen(checkBirthDateIsNotFromFuture)
    }

    def validatePerson(p: PersonDto): AllErrorsOr[Person] = (
      validatePersonName(p.name),
      validateAge(p.age),
      validateBirthDate(p.birthDay),
      validatePassportNumber(p.passportNumber)
    ).mapN(Person)

    def validatePaymentCard(c: PaymentCardDto): AllErrorsOr[PaymentCard] = (
      validateCardNumber(c.cardNumber),
      validateExpirationDate(c.expirationDate),
      validateCardHolder(c.cardHolder),
      validateSecurityCode(c.securityCode)
    ).mapN(PaymentCard)

    def validate(personDto: PersonDto, cardDto: PaymentCardDto): AllErrorsOr[Account] =
      (validatePerson(personDto), validatePaymentCard(cardDto)).mapN(Account)
  }

  def main(args: Array[String]): Unit = {
    val person = PersonDto("MIKHAIL BIALEVICH", 20, "2002-01-24", "HB1234567")
    val card   = PaymentCardDto("1234 1234 1234 1234", "07/23", "MIKHAIL BIALEVICH", "111")
    println(AccountValidator.validate(person, card))
  }
}
