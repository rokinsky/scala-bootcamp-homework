package com.evolutiongaming.bootcamp.effects.minhash.service

import cats.MonadError
import cats.effect.{Blocker, ContextShift, Resource, Sync}
import cats.implicits._
import com.evolutiongaming.bootcamp.effects.minhash.domain.{Console, Parse}

final case class ParseService[F[_]: MonadError[*[_], Throwable]](console: Console[F]) {
  def parse[T: Parse](message: String): F[T] = {
    val read = for {
      str   <- console.write(message) *> console.read
      entry <- Parse[T].parse(str).liftTo[F]
    } yield entry

    read.handleErrorWith(_ => console.writeln("Invalid input") *> parse(message))
  }
}

object ParseService {
  def of[F[_]: Sync: ContextShift](blocker: Blocker): Resource[F, ParseService[F]] =
    Resource.pure[F, ParseService[F]](ParseService(Console.of[F](blocker)))
}
