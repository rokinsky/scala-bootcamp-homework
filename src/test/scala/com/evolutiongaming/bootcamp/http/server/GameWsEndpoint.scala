package com.evolutiongaming.bootcamp.http.server

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import com.evolutiongaming.bootcamp.http.models.{GameSettings, GuessNumber}
import fs2.concurrent.Queue
import fs2.{Pipe, Pull}
import io.circe.parser._
import io.circe.syntax.EncoderOps
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame

sealed abstract class GameWsEndpoint[F[_]: Sync: Concurrent](guessService: GuessService[F]) extends Http4sDsl[F] {
  def endpoint: HttpRoutes[F] = HttpRoutes.of[F] { case Method.GET -> Root =>
    val gamePipe: Pipe[F, WebSocketFrame, WebSocketFrame] = {
      _.collect { case WebSocketFrame.Text(message, _) => message.trim }.pull.uncons1
        .flatMap {
          case Some((settingsMessage, guessMessages)) =>
            val sessionF = for {
              settings <- Sync[F].fromEither(decode[GameSettings](settingsMessage))
              session  <- guessService.createSession(settings)
            } yield session

            Pull.eval(sessionF).flatMap { session =>
              guessMessages
                .evalMap { guessMessage =>
                  for {
                    guess  <- Sync[F].fromEither(decode[GuessNumber](guessMessage))
                    result <- guessService.tryGuess(session.gameId, guess.guess)
                  } yield result
                }
                .pull
                .echo
            }
          case None => Pull.done
        }
        .stream
        .map {
          case Some(result) => WebSocketFrame.Text(result.asJson.toString)
          case None         => WebSocketFrame.Close()
        }
    }

    for {
      queue <- Queue.bounded[F, WebSocketFrame](2048)
      response <- WebSocketBuilder[F].build(
        receive = queue.enqueue,
        send    = queue.dequeue.through(gamePipe),
      )
    } yield response
  }
}

object GameWsEndpoint {
  def apply[F[_]: Sync: Concurrent](guessService: GuessService[F]): GameWsEndpoint[F] =
    new GameWsEndpoint[F](guessService) {}
}
