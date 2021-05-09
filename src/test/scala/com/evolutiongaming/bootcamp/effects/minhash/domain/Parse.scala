package com.evolutiongaming.bootcamp.effects.minhash.domain

trait Parse[T] {
  def parse(entity: String): Either[Throwable, T]
}

object Parse {
  def apply[F: Parse]: Parse[F] = implicitly[Parse[F]]
}
