package com.evolutiongaming.bootcamp.http.models

import io.circe.generic.JsonCodec

import java.util.UUID

@JsonCodec
final case class GameSession(gameId: UUID) extends AnyVal
