package com.evolutiongaming.bootcamp.http.config

import io.circe.generic.JsonCodec

@JsonCodec
final case class ServerConfig(host: String, port: Int)
