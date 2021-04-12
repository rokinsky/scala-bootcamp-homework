package com.evolutiongaming.bootcamp.effects.minhash.service

import cats.Parallel
import cats.effect.implicits._
import cats.effect.{Concurrent, Resource}
import cats.implicits._
import com.evolutiongaming.bootcamp.effects.minhash.domain.{File, Hash, Signature, SignatureRepositoryAlgebra}

class SignatureService[F[_]: Concurrent: Parallel](repository: SignatureRepositoryAlgebra[F]) {
  def sign(file: File, words: List[String], hashes: List[Hash]): F[Signature] = for {
    value     <- hashes.parTraverseN(hashes.length)(h => words.map(h.value).min.pure[F])
    signature <- repository.create(Signature(file, value))
  } yield signature
}

object SignatureService {
  def of[F[_]: Concurrent: Parallel](
    repository: SignatureRepositoryAlgebra[F]
  ): Resource[F, SignatureService[F]] = {
    Resource.pure[F, SignatureService[F]](new SignatureService[F](repository))
  }
}
