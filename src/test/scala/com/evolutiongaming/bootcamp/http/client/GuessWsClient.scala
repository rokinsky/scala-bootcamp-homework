package com.evolutiongaming.bootcamp.http.client

import cats.effect.{ConcurrentEffect, ContextShift, Resource, Sync}
import cats.implicits._
import com.evolutiongaming.bootcamp.http.config.GuessConfig
import com.evolutiongaming.bootcamp.http.effects.{Console, GenRandom}
import com.evolutiongaming.bootcamp.http.models.GuessStatus.{Defeat, Greater, Lower, Win}
import com.evolutiongaming.bootcamp.http.models.{GameSettings, GuessNumber, GuessResult}
import io.circe.config.parser
import io.circe.parser.decode
import io.circe.syntax.EncoderOps
import org.http4s._
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.client.jdkhttpclient.{JdkWSClient, WSConnectionHighLevel, WSFrame, WSRequest}

import java.net.http.HttpClient

final private class GuessWsClient[F[_]: Sync](client: WSConnectionHighLevel[F]) extends Http4sClientDsl[F] {

  private def guess(min: Int, max: Int): F[String] = {
    val number = min + (max - min) / 2
    client.send(WSFrame.Text(GuessNumber(number).asJson.toString)) >>
      client.receiveStream
        .collectFirst { case WSFrame.Text(message, _) => message }
        .evalMap(resultMessage => Sync[F].fromEither(decode[GuessResult](resultMessage)))
        .evalMap {
          case GuessResult(_, Win)     => s"Win. Number is $number.".pure[F]
          case GuessResult(_, Lower)   => guess(min, number)
          case GuessResult(_, Greater) => guess(number, max)
          case GuessResult(_, Defeat)  => "Defeat. Try again.".pure[F]
        }
        .compile
        .lastOrError
  }

  def run: F[Unit] = for {
    min      <- GenRandom[F].between(Int.MinValue, Int.MaxValue)
    max      <- GenRandom[F].between(min, Int.MaxValue)
    attempts <- GenRandom[F].between(1, 100)
    _        <- Console[F].writeLn(s"Game has been started with: min ($min), max ($max), attempts ($attempts)")
    _        <- client.send(WSFrame.Text(GameSettings(min, max, attempts).asJson.toString))
    result   <- guess(min, max)
    _        <- Console[F].writeLn(result)
  } yield ()
}

object GuessWsClient {
  def resource[F[_]: Sync: ConcurrentEffect: ContextShift]: Resource[F, WSConnectionHighLevel[F]] = for {
    conf       <- Resource.eval(parser.decodePathF[F, GuessConfig]("guess"))
    uri        <- Resource.eval(Sync[F].fromEither(Uri.fromString(conf.client.wsUri)))
    httpClient <- Resource.eval(Sync[F].delay(HttpClient.newHttpClient()))
    wsClient   <- JdkWSClient[F](httpClient).connectHighLevel(WSRequest(uri / "games"))
    _          <- Resource.eval(new GuessWsClient(wsClient).run)
  } yield wsClient

}
