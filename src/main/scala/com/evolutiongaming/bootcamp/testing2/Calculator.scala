package com.evolutiongaming.bootcamp.testing2

import cats.implicits._
import cats.{Monad, MonoidK}

object Calculator {

  trait Expression[F[_], A] {
    def const(x: A): F[A]

    def add(left: F[A], right: F[A]): F[A]

    def sub(left: F[A], right: F[A]): F[A]

    def mul(left: F[A], right: F[A]): F[A]

    def div(left: F[A], right: F[A]): F[A]
  }

  object Expression {
    def intInterpreter[F[_]: Monad: MonoidK]: Expression[F, Int] = new Expression[F, Int] {
      override def const(x: Int): F[Int] =
        x.pure[F]

      override def add(left: F[Int], right: F[Int]): F[Int] =
        (left, right).mapN(_ + _)

      override def sub(left: F[Int], right: F[Int]): F[Int] =
        (left, right).mapN(_ - _)

      override def mul(left: F[Int], right: F[Int]): F[Int] =
        (left, right).mapN(_ * _)

      override def div(left: F[Int], right: F[Int]): F[Int] =
        (left, right).tupled.flatMap {
          case (_, 0) => MonoidK[F].empty
          case (l, r) => (l / r).pure[F]
        }
    }
  }
}
