package com.evolutiongaming.bootcamp.cats

import cats.Functor

object TypeHierarchy {
  trait Applicative[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]

    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] // = map2(fa, fab)((a, f) => f(a))

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      apply(unit(f))(fa)

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(map(fa)(f.curried))(fb)

    def sequence[A](fas: List[F[A]]): F[List[A]] =
      traverse(fas)(identity)

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List.empty[B]))((a, acc) => map2(f(a), acc)(_ :: _))
  }

  trait Monad[M[_]] extends Functor[M] {
    def unit[A](a: => A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] // = join(map(ma)(f))

    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => unit(f(a)))

    def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))

    def join[A](mma: M[M[A]]): M[A] =
      flatMap(mma)(identity)
  }
}
