package com.evolutiongaming.bootcamp.http.effects

import cats.effect.Sync

import scala.util.Random

trait GenRandom[F[_]] {
  def between[N: Numeric](minInclusive: N, maxExclusive: N): F[Int]
}

object GenRandom {
  def apply[F[_]: GenRandom]: GenRandom[F] = implicitly

  implicit def syncGenRandom[F[_]: Sync]: GenRandom[F] = new GenRandom[F] {
    override def between[N: Numeric](minInclusive: N, maxExclusive: N): F[Int] =
      Sync[F].delay(Random.between(Numeric[N].toInt(minInclusive), Numeric[N].toInt(maxExclusive)))
  }
}
