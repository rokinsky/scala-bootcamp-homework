package com.evolutiongaming.bootcamp.http.effects

import cats.effect.Sync

import scala.io.StdIn

trait Console[F[_]] {
  def readLn: F[String]

  def writeLn(value: String): F[Unit]
}

object Console {
  def apply[F[_]: Console]: Console[F] = implicitly

  implicit def console[F[_]: Sync]: Console[F] = new Console[F] {
    override def readLn: F[String] =
      Sync[F].delay(StdIn.readLine())

    override def writeLn(value: String): F[Unit] =
      Sync[F].delay(println(value))
  }

}
