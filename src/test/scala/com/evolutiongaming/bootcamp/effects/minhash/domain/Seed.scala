package com.evolutiongaming.bootcamp.effects.minhash.domain

import com.evolutiongaming.bootcamp.effects.minhash.error.{InvalidSeed, ValidationError}

sealed abstract case class Seed private (value: Int)
object Seed {
  def from(value: String): Either[ValidationError, Seed] = {
    value.toIntOption.map(n => new Seed(n) {}).toRight(InvalidSeed)
  }

  implicit val parseSeed: Parse[Seed] = from
}
