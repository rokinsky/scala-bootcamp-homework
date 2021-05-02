package com.evolutiongaming.bootcamp.http.client

import cats.effect.{ConcurrentEffect, Resource, Sync}
import cats.implicits._
import com.evolutiongaming.bootcamp.http.config.GuessConfig
import com.evolutiongaming.bootcamp.http.effects.{Console, GenRandom}
import com.evolutiongaming.bootcamp.http.models.GuessStatus.{Defeat, Greater, Lower, Win}
import com.evolutiongaming.bootcamp.http.models.{GameSession, GameSettings, GuessNumber, GuessResult}
import io.circe.config.parser
import org.http4s._
import org.http4s.circe.CirceEntityCodec.{circeEntityDecoder, circeEntityEncoder}
import org.http4s.client._
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.Http4sClientDsl

import java.util.UUID
import scala.concurrent.ExecutionContext

final private class GuessHttpClient[F[_]: Sync](client: Client[F], uri: Uri) extends Http4sClientDsl[F] {

  private def guess(id: UUID, min: Int, max: Int): F[String] = {
    val number = min + (max - min) / 2
    client.expect[GuessResult](Method.POST(GuessNumber(number), uri / "games" / id.toString)).flatMap {
      case GuessResult(_, Win)     => s"Win. Number is $number.".pure[F]
      case GuessResult(_, Lower)   => guess(id, min, number)
      case GuessResult(_, Greater) => guess(id, number, max)
      case GuessResult(_, Defeat)  => "Defeat. Try again.".pure[F]
    }
  }

  // FIXME: possible improvements:
  //  - add settings reading from the console instead of selecting random ones
  //  - add proper logging
  //  - handle server-side error responses (the current flow represents the optimistic path)
  def run: F[Unit] = for {
    min      <- GenRandom[F].between(Int.MinValue, Int.MaxValue)
    max      <- GenRandom[F].between(min, Int.MaxValue)
    attempts <- GenRandom[F].between(1, 100)
    _        <- Console[F].writeLn(s"Game has been started with: min ($min), max ($max), attempts ($attempts)")
    session  <- client.expect[GameSession](Method.POST(GameSettings(min, max, attempts), uri / "games"))
    _        <- Console[F].writeLn(s"Your game id is ${session.gameId}")
    result   <- guess(session.gameId, min, max)
    _        <- Console[F].writeLn(result)
  } yield ()
}

object GuessHttpClient {
  def resource[F[_]: Sync: ConcurrentEffect]: Resource[F, Client[F]] = for {
    conf   <- Resource.liftF(parser.decodePathF[F, GuessConfig]("guess"))
    uri    <- Resource.liftF(Sync[F].fromEither(Uri.fromString(conf.client.uri)))
    client <- BlazeClientBuilder[F](ExecutionContext.global).resource
    _      <- Resource.liftF(new GuessHttpClient(client, uri).run)
  } yield client
}
