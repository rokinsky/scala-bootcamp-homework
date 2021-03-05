package com.evolutiongaming.bootcamp.error_handling

import cats.implicits.catsSyntaxValidatedId
import cats.syntax.all._
import com.evolutiongaming.bootcamp.error_handling.ErrorHandling._
import org.scalatest.Assertion
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ErrorHandlingSpec extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  import ValidationError._

  def checkInvalid(
    name:           String,
    number:         String,
    expirationDate: String,
    securityCode:   String,
    errors:         Set[ValidationError]
  ): Assertion =
    PaymentCardValidator
      .validate(name, number, expirationDate, securityCode)
      .leftMap(_.toList.toSet) shouldBe errors.invalid

  def checkValid(
    name:           String,
    number:         String,
    expirationDate: String,
    securityCode:   String,
  ): Assertion =
    PaymentCardValidator
      .validate(name, number, expirationDate, securityCode)
      .isValid shouldBe true

  "ValidationError" - {
    "when converting to string" in {
      HolderNameContentIsInvalid.toString shouldBe "Cardholder's name must contain from 3 to 30 alphabetic characters"
      CardNumberContentIsInvalid.toString shouldBe "Card number must contain exactly 16 digits"
      CardNumberChecksumIsInvalid.toString shouldBe "Card number must have the correct checksum"
      ExpirationDateIsInvalid.toString shouldBe "Expiration date format must be in MM/yy"
      SecurityCodeContentIsInvalid.toString shouldBe "Security code must contain exactly 3 digits"
    }
  }

  "PaymentCardValidator" - {
    "when all fields are valid" in {
      checkValid("JOHNDOE", "4111111111111111", "12/99", "123") // Visa
      checkValid("JOHNDOE", "5500000000000004", "11/37", "987") // MasterCard
    }

    "when all fields are empty" in {
      checkInvalid(
        "",
        "",
        "",
        "",
        Set(
          HolderNameContentIsInvalid,
          CardNumberContentIsInvalid,
          ExpirationDateIsInvalid,
          SecurityCodeContentIsInvalid
        )
      )
    }

    "when all fields are incorrect due format" in {
      checkInvalid(
        "#@",
        "q",
        "9999",
        "h4x",
        Set(
          HolderNameContentIsInvalid,
          CardNumberContentIsInvalid,
          CardNumberChecksumIsInvalid,
          ExpirationDateIsInvalid,
          SecurityCodeContentIsInvalid
        )
      )
    }

    "when some fields are incorrect due logic" in {
      checkInvalid(
        "JOHNDOE",
        "1234567812345678",
        "20/99",
        "321",
        Set(
          CardNumberChecksumIsInvalid,
          ExpirationDateIsInvalid,
        )
      )
    }
  }
}
