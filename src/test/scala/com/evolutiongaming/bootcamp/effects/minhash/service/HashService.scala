package com.evolutiongaming.bootcamp.effects.minhash.service

import cats.Applicative
import com.evolutiongaming.bootcamp.effects.minhash.domain.{Hash, Seed}
import cats.implicits._

object HashService {
  def knuthHash(word: String, constant: Int): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
    hash % constant
  }

  def javaHash(word: String, seed: Int = 0): Int = {
    var hash = 0

    for (ch <- word.toCharArray)
      hash = 31 * hash + ch.toInt

    hash = hash ^ (hash >> 20) ^ (hash >> 12)
    hash ^ (hash >> 7) ^ (hash >> 4)
  }

  def hashes[F[_]: Applicative](seed: Seed): F[List[Hash]] = {
    // TODO: I would generate n different hash functions here.
    List(word => javaHash(word, seed.value), word => knuthHash(word, seed.value)).map(Hash).pure[F]
  }
}
