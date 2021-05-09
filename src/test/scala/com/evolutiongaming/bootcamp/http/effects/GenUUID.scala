package com.evolutiongaming.bootcamp.http.effects

import cats.effect.Sync

import java.util.UUID

trait GenUUID[F[_]] {
  def random: F[UUID]
}

object GenUUID {
  def apply[F[_]: GenUUID]: GenUUID[F] = implicitly

  implicit def syncGenUUID[F[_]: Sync]: GenUUID[F] = new GenUUID[F] {
    def random: F[UUID] =
      Sync[F].delay(UUID.randomUUID())
  }
}
