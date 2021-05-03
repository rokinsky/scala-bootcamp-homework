package com.evolutiongaming.bootcamp.http.server

import cats.effect._
import com.evolutiongaming.bootcamp.http.config.GuessConfig
import com.evolutiongaming.bootcamp.http.models.GameState
import io.circe.config.parser
import org.http4s.HttpApp
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.{Router, Server}
import org.http4s.syntax.kleisli._

import java.util.UUID
import scala.concurrent.ExecutionContext.global

object GameServer {
  private def httpApp[F[_]: Sync](ctx: GameModule[F]): HttpApp[F] =
    Router(
      "/games" -> ctx.gameHttpEndpoint,
      "/games" -> ctx.gameWsEndpoint
    ).orNotFound

  def resource[F[_]: Sync: ConcurrentEffect: Timer]: Resource[F, Server[F]] =
    for {
      conf       <- Resource.eval(parser.decodePathF[F, GuessConfig]("guess"))
      repository <- Repository.of[F, UUID, GameState]
      ctx        <- Resource.pure(GameModule.of(repository))
      server <- BlazeServerBuilder[F](global)
        .bindHttp(conf.server.port, conf.server.host)
        .withHttpApp(httpApp(ctx))
        .resource
    } yield server
}
