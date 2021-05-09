package com.evolutiongaming.bootcamp.effects.minhash.domain

import cats.effect.{Blocker, ContextShift, Sync}

import scala.io.StdIn.readLine

trait Console[F[_]] {
  def read: F[String]
  def write(s:   String): F[Unit]
  def writeln(s: String): F[Unit]
}

object Console {
  def of[F[_]: Sync: ContextShift](blocker: Blocker): Console[F] = new Console[F] {
    def read: F[String] = blocker.delay(readLine())
    def write(s:   String): F[Unit] = blocker.delay(print(s))
    def writeln(s: String): F[Unit] = blocker.delay(println(s))
  }
}
