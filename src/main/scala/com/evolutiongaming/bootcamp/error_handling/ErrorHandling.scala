package com.evolutiongaming.bootcamp.error_handling

import cats.data.{Validated, ValidatedNec}
import cats.syntax.all._

import java.time.YearMonth
import java.time.format.DateTimeFormatter
import scala.util.Try

object ErrorHandling {
  // Homework. Place the solution under `error_handling` package in your homework repository.
  //
  // 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
  // 2. Add `ValidationError` cases (at least 5, may be more).
  // 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.

  // Note. Supporting only Visa and MasterCard, so Amex and other cards are mostly invalid
  object CardData {
    final case class HolderName(name: String) extends AnyVal
    final case class Number(number: String) extends AnyVal
    final case class ExpirationDate(date: YearMonth) extends AnyVal
    final case class SecurityCode(code: String) extends AnyVal
  }

  import CardData._
  final case class PaymentCard(
    holderName:     HolderName,
    number:         Number,
    expirationDate: ExpirationDate,
    securityCode:   SecurityCode
  )

  sealed trait ValidationError
  object ValidationError {
    // E.g. Elon Musk's son X Æ A-12 can use XAshA - the name shouldn't be accurate for Visa and MasterCard
    // https://en.wikipedia.org/wiki/Address_verification_service
    final case object HolderNameContentIsInvalid extends ValidationError {
      override def toString: String = "Cardholder's name must contain from 3 to 30 alphabetic characters"
    }
    final case object CardNumberContentIsInvalid extends ValidationError {
      override def toString: String = "Card number must contain exactly 16 digits"
    }
    final case object CardNumberChecksumIsInvalid extends ValidationError {
      override def toString: String = "Card number must have the correct checksum"
    }
    final case object ExpirationDateIsInvalid extends ValidationError {
      override def toString: String = "Expiration date format must be in MM/yy"
    }
    final case object SecurityCodeContentIsInvalid extends ValidationError {
      override def toString: String = "Security code must contain exactly 3 digits"
    }
  }

  private object ValidationConstant {
    val HolderRegex       = "^[a-zA-Z]{3,30}$"
    val NumberRegex       = "^[0-9]{16}$"
    val SecurityCodeRegex = "^[0-9]{3}$"
    val YearMonthFormat   = "MM/yy"
  }

  object PaymentCardValidator {
    import ValidationError._
    import ValidationConstant._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    private def validateName(name: String): AllErrorsOr[HolderName] = {
      def validateNameContent: AllErrorsOr[HolderName] =
        Validated.condNec(name.matches(HolderRegex), HolderName(name), HolderNameContentIsInvalid)

      validateNameContent
    }

    private def validateNumber(number: String): AllErrorsOr[Number] = {
      def validateNumberContent: AllErrorsOr[Number] =
        Validated.condNec(number.matches(NumberRegex), Number(number), CardNumberContentIsInvalid)

      def validateNumberChecksum: AllErrorsOr[Number] =
        Validated.condNec(LunhChecksum.check(number), Number(number), CardNumberChecksumIsInvalid)

      validateNumberContent productR validateNumberChecksum
    }

    private def validateExpirationDate(date: String): AllErrorsOr[ExpirationDate] = {
      def validateDateFormat: AllErrorsOr[ExpirationDate] =
        YearMonthParser.parse(date, YearMonthFormat).toValidNec(ExpirationDateIsInvalid).map(ExpirationDate)

      // I don’t check if the date has expired as don't want to deal with side effects here.
      validateDateFormat
    }

    private def validateSecurityCode(code: String): AllErrorsOr[SecurityCode] = {
      def validateSecurityCodeContent: AllErrorsOr[SecurityCode] =
        Validated.condNec(code.matches(SecurityCodeRegex), SecurityCode(code), SecurityCodeContentIsInvalid)

      validateSecurityCodeContent
    }

    def validate(
      name:           String,
      number:         String,
      expirationDate: String,
      securityCode:   String,
    ): AllErrorsOr[PaymentCard] = (
      validateName(name),
      validateNumber(number),
      validateExpirationDate(expirationDate),
      validateSecurityCode(securityCode)
    ).mapN(PaymentCard)
  }

  private object YearMonthParser {
    def parse(date: String, yearMonthFormat: String): Option[YearMonth] = {
      Try(YearMonth.parse(date, DateTimeFormatter.ofPattern(yearMonthFormat))).toOption
    }
  }

  private object LunhChecksum {
    // Just for fun implementation
    // https://en.wikipedia.org/wiki/Luhn_algorithm
    def check(number: String): Boolean = {
      val doubleDigit = (digit: Int) => (digit * 2) % 10 + (digit * 2) / 10

      number.reverse
        .map(_.asDigit)
        .toList
        .mapWithIndex((d, i) => if (i % 2 != 0) doubleDigit(d) else d)
        .sum % 10 == 0
    }
  }
}
