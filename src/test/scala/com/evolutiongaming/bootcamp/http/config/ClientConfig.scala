package com.evolutiongaming.bootcamp.http.config

import io.circe.generic.JsonCodec

@JsonCodec
final case class ClientConfig(uri: String)
