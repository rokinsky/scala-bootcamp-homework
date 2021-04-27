package com.evolutiongaming.bootcamp.http.server

import cats.effect._
import com.evolutiongaming.bootcamp.http.models.GameState
import org.http4s.HttpApp
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.{Router, Server}
import org.http4s.syntax.kleisli._

import java.util.UUID
import scala.concurrent.ExecutionContext.global

object GameServer {
  def httpApp[F[_]: Sync](ctx: GameModule[F]): HttpApp[F] =
    Router(
      "/games" -> ctx.gameHttpEndpoint,
    ).orNotFound

  def resource[F[_]: Sync: ConcurrentEffect: Timer]: Resource[F, Server[F]] =
    for {
      repository <- Repository.of[F, UUID, GameState]
      ctx        <- Resource.pure(GameModule.of(repository))
      server <- BlazeServerBuilder[F](global)
        .bindHttp(9001)
        .withHttpApp(httpApp(ctx))
        .resource
    } yield server
}
