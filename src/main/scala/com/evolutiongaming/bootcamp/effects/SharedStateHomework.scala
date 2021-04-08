package com.evolutiongaming.bootcamp.effects

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{Clock, Concurrent, Timer}
import cats.implicits._

import scala.concurrent.duration._

/*
 * Please implement a Cache which allows concurrent access.
 *
 * Tip: checking expiration could be represented as some infinite process somewhere in background
 *
 * Cached items should have an expiration timestamp after which they are evicted.
 */
object SharedStateHomework {
  type Timestamp              = Long
  type CacheMap[K, V]         = Map[K, (Timestamp, V)]
  type CacheState[F[_], K, V] = Ref[F, CacheMap[K, V]]

  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]

    def put(key: K, value: V): F[Unit]
  }

  class RefCache[F[_]: Clock: Monad, K, V](
    state:     CacheState[F, K, V],
    expiresIn: FiniteDuration
  ) extends Cache[F, K, V] {

    def get(key: K): F[Option[V]] = for {
      map   <- state.get
      value <- map.get(key).traverse { case (_, v) => Monad[F].pure(v) }
    } yield value

    def put(key: K, value: V): F[Unit] = for {
      now <- Clock[F].realTime(MILLISECONDS)
      _   <- state.update(_.updated(key, (now, value)))
    } yield ()

    def expire: F[Unit] = for {
      now <- Clock[F].realTime(MILLISECONDS)
      _   <- state.update(_.filter { case (_, (timestamp, _)) => now - timestamp < expiresIn.toMillis })
    } yield ()

    def expireAfter(time: FiniteDuration)(implicit T: Timer[F]): F[Unit] =
      T.sleep(time) *> expire
  }

  object Cache {
    def of[F[_]: Concurrent: Timer: Clock, K, V](
      expiresIn:               FiniteDuration,
      checkOnExpirationsEvery: FiniteDuration
    ): F[Cache[F, K, V]] = for {
      state <- Ref.of[F, CacheMap[K, V]](Map.empty)
      cache <- Monad[F].pure(new RefCache(state, expiresIn))
      _     <- Concurrent[F].start(cache.expireAfter(checkOnExpirationsEvery).foreverM.void)
    } yield cache
  }
}
