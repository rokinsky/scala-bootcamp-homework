package com.evolutiongaming.bootcamp.http.models

import cats.implicits._
import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder}

final case class GameSettings(min: Int, max: Int, attempts: Int)

object GameSettings {
  // FIXME possible improvements
  //  - make a sensible limit for the attempts value
  //  - implement validation errors instead of hardcoded strings
  def of(min: Int, max: Int, attempts: Int): Either[String, GameSettings] =
    if (attempts <= 0) "Attempts must be a positive value".asLeft
    else if (max < min) "The max must be larger than min".asLeft
    else GameSettings(min, max, attempts).asRight

  implicit val gameSettingsEncoder: Encoder[GameSettings] = deriveEncoder[GameSettings]
  implicit val gameSettingsDecoder: Decoder[GameSettings] =
    Decoder
      .forProduct3("min", "max", "attempts")((min: Int, max: Int, attempts: Int) => (min, max, attempts))
      .emap((GameSettings.of _).tupled)
}
