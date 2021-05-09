package com.evolutiongaming.bootcamp.http.server

import cats.data.OptionT
import cats.effect.Sync
import cats.syntax.all._
import com.evolutiongaming.bootcamp.http.effects.{GenRandom, GenUUID}
import com.evolutiongaming.bootcamp.http.models.{GameSession, GameSettings, GameState, GuessResult}

import java.util.UUID

trait GuessService[F[_]] {
  def createSession(settings: GameSettings): F[GameSession]

  def tryGuess(uuid: UUID, guess: Int): F[Option[GuessResult]]
}

object GuessService {
  def of[F[_]: Sync](repository: Repository[F, UUID, GameState]): GuessService[F] = new GuessService[F] {
    override def createSession(settings: GameSettings): F[GameSession] = for {
      uuid  <- GenUUID[F].random
      guess <- GenRandom[F].between(settings.min, settings.max + 1)
      _     <- repository.put(uuid, GameState(guess, settings.attempts, guess))
    } yield GameSession(uuid)

    override def tryGuess(uuid: UUID, guess: Int): F[Option[GuessResult]] = {
      // We should fetch, update and get the game state atomically
      OptionT(repository.updateWith(uuid)(_.flatMap {
        case GameState(_, 0, _)             => None
        case GameState(number, attempts, _) => Some(GameState(number, attempts - 1, guess))
      })).map(GuessResult.of).value
    }
  }
}
