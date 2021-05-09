package com.evolutiongaming.bootcamp.http.models

import io.circe.Codec
import io.circe.generic.extras.semiauto.deriveEnumerationCodec

sealed trait GuessStatus
object GuessStatus {
  case object Lower extends GuessStatus
  case object Greater extends GuessStatus
  case object Win extends GuessStatus
  case object Defeat extends GuessStatus

  def of(game: GameState): GuessStatus = game match {
    case GameState(number, 0, guess) if number != guess => GuessStatus.Defeat
    case GameState(number, _, guess) if number > guess  => GuessStatus.Greater
    case GameState(number, _, guess) if number < guess  => GuessStatus.Lower
    case GameState(_, _, _)                             => GuessStatus.Win
  }

  implicit val gameStatusCodec: Codec[GuessStatus] = deriveEnumerationCodec[GuessStatus]
}
