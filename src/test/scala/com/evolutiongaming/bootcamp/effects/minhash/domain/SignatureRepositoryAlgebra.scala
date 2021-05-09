package com.evolutiongaming.bootcamp.effects.minhash.domain

trait SignatureRepositoryAlgebra[F[_]] {
  def create(signature: Signature): F[Signature]
}
