package com.evolutiongaming.bootcamp.http.server

import cats.effect.Sync
import cats.implicits._
import com.evolutiongaming.bootcamp.http.models.GameSettings.gameSettingsDecoder
import com.evolutiongaming.bootcamp.http.models.{GameSettings, GuessNumber}
import org.http4s.circe.CirceEntityCodec.{circeEntityDecoder, circeEntityEncoder}
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, Method, Request, Response, _}

import java.util.UUID

sealed abstract class GameHttpEndpoint[F[_]: Sync](guessService: GuessService[F]) extends Http4sDsl[F] {
  def endpoint: HttpRoutes[F] =
    HttpRoutes.of[F] {
      // curl -XPOST "localhost:9001/games" -d '{"min": 0, "max": 2, "attempts": 2 }' -H "Content-Type: application/json"
      case req @ Method.POST -> Root =>
        createGame(req).handleErrorWith(intercept)

      // curl -XPOST "localhost:9001/games/17901497-72ef-460a-9acc-456d2eb5a371" -d '{"guess": 2 }' -H "Content-Type: application/json"
      case req @ Method.POST -> Root / UUIDVar(uuid) =>
        tryGuess(req, uuid).handleErrorWith(intercept)
    }

  private def createGame(request: Request[F]): F[Response[F]] =
    for {
      settings <- request.as[GameSettings]
      uuid     <- guessService.createSession(settings)
      response <- Created(uuid)
    } yield response

  private def tryGuess(request: Request[F], uuid: UUID): F[Response[F]] =
    for {
      guess    <- request.as[GuessNumber]
      result   <- guessService.tryGuess(uuid, guess.guess)
      response <- result.fold(NotFound())(Ok(_))
    } yield response

  private def intercept: Throwable => F[Response[F]] = {
    case InvalidMessageBodyFailure(r, _) => BadRequest(r)
    case err                             => InternalServerError(err.toString)
  }
}

object GameHttpEndpoint {
  def apply[F[_]: Sync](guessService: GuessService[F]): GameHttpEndpoint[F] =
    new GameHttpEndpoint[F](guessService) {}
}
