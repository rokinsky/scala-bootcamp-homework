package com.evolutiongaming.bootcamp.http.server

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{Resource, Sync}
import cats.syntax.all._

trait Repository[F[_], K, V] {
  def get(key: K): F[Option[V]]

  def put(key: K, value: V): F[Unit]

  def remove(key: K): F[Unit]

  def updateWith(key: K)(update: Option[V] => Option[V]): F[Option[V]]
}

final private class InMemoryRepository[F[_]: Monad, K, V](
  state: Ref[F, Map[K, V]],
) extends Repository[F, K, V] {
  override def get(key: K): F[Option[V]] =
    state.get.map(_.get(key))

  override def put(key: K, value: V): F[Unit] =
    state.update(_.updated(key, value))

  override def remove(key: K): F[Unit] =
    state.update(_.removed(key))

  override def updateWith(key: K)(update: Option[V] => Option[V]): F[Option[V]] =
    state.modifyMaybe(map => {
      val updatedMap = map.updatedWith(key)(update)
      updatedMap.get(key).map((updatedMap, _))
    })
}

object Repository {
  def of[F[_]: Sync, K, V]: Resource[F, Repository[F, K, V]] =
    Resource[F, Repository[F, K, V]](
      for {
        state <- Ref.of[F, Map[K, V]](Map.empty)
        cache <- new InMemoryRepository(state).pure[F]
      } yield (cache, Monad[F].unit)
    )
}
