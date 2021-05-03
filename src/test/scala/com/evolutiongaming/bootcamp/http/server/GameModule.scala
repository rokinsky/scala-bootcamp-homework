package com.evolutiongaming.bootcamp.http.server

import cats.effect._
import com.evolutiongaming.bootcamp.http.models.GameState
import org.http4s.HttpRoutes

import java.util.UUID

trait GameModule[F[_]] {
  def gameHttpEndpoint: HttpRoutes[F]
  def gameWsEndpoint:   HttpRoutes[F]
}

object GameModule {
  def of[F[_]: Sync: Concurrent](repository: Repository[F, UUID, GameState]): GameModule[F] = new GameModule[F] {
    override def gameHttpEndpoint: HttpRoutes[F] =
      GameHttpEndpoint[F](GuessService.of(repository)).endpoint

    override def gameWsEndpoint: HttpRoutes[F] =
      GameWsEndpoint[F](GuessService.of(repository)).endpoint
  }
}
