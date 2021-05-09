package com.evolutiongaming.bootcamp.http.config

import io.circe.generic.JsonCodec

@JsonCodec
final case class GuessConfig(client: ClientConfig, server: ServerConfig)
