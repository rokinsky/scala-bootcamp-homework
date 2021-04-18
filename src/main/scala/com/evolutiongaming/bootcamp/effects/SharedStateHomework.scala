package com.evolutiongaming.bootcamp.effects

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.implicits._
import cats.effect.{Clock, Concurrent, Resource, Timer}
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

  private class RefCache[F[_]: Monad: Timer, K, V](
    state:     CacheState[F, K, V],
    expiresIn: FiniteDuration
  ) extends Cache[F, K, V] {

    def get(key: K): F[Option[V]] = for {
      map   <- state.get
      value <- map.get(key).traverse { case (_, v) => v.pure[F] }
    } yield value

    def put(key: K, value: V): F[Unit] = for {
      now <- Clock[F].realTime(MILLISECONDS)
      _   <- state.update(_.updated(key, (now + expiresIn.toMillis, value)))
    } yield ()

    def expire: F[Unit] = for {
      now <- Clock[F].realTime(MILLISECONDS)
      _   <- state.update(_.filter { case (_, (expiration, _)) => expiration > now })
    } yield ()

    def expireAfter(time: FiniteDuration): F[Unit] =
      Timer[F].sleep(time) *> expire
  }

  object Cache {
    def of[F[_]: Concurrent: Timer, K, V](
      expiresIn:               FiniteDuration,
      checkOnExpirationsEvery: FiniteDuration
    ): Resource[F, Cache[F, K, V]] = Resource[F, Cache[F, K, V]](
      for {
        state <- Ref.of[F, CacheMap[K, V]](Map.empty)
        cache <- new RefCache(state, expiresIn).pure[F]
        fiber <- cache.expireAfter(checkOnExpirationsEvery).foreverM.void.start
      } yield (cache, fiber.cancel)
    )
  }
}
