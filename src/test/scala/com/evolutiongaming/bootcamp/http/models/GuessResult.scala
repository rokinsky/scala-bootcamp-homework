package com.evolutiongaming.bootcamp.http.models

import io.circe.generic.JsonCodec

@JsonCodec
final case class GuessResult(attemptsLeft: Int, status: GuessStatus)

object GuessResult {
  def of(game: GameState): GuessResult =
    GuessResult(game.attemptsLeft, GuessStatus.of(game))
}
