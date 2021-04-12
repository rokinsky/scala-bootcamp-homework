package com.evolutiongaming.bootcamp.effects.minhash.repository

import cats.Applicative
import cats.effect.Resource
import cats.implicits.catsSyntaxApplicativeId
import com.evolutiongaming.bootcamp.effects.minhash.domain.{Signature, SignatureRepositoryAlgebra}

class SignatureFakeRepository[F[_]: Applicative] extends SignatureRepositoryAlgebra[F] {
  def create(signature: Signature): F[Signature] = signature.pure[F]
}

object SignatureFakeRepository {
  def of[F[_]: Applicative]: Resource[F, SignatureRepositoryAlgebra[F]] =
    Resource.pure[F, SignatureRepositoryAlgebra[F]](new SignatureFakeRepository[F])
}
