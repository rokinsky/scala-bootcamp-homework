package com.evolutiongaming.bootcamp.http.models

import io.circe.generic.JsonCodec

@JsonCodec
final case class GuessNumber(guess: Int)
