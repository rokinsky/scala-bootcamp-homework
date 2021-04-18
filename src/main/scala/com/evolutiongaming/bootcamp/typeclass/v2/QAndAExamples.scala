package com.evolutiongaming.bootcamp.typeclass.v2

object QAndAExamples {
  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit class FunctorOps[F[_]: Functor, A](fa: F[A]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
  }

  object Functor {
    def apply[F[_]: Functor]: Functor[F] = implicitly
  }

  trait Semigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  object Semigroupal {
    def apply[F[_]: Semigroupal]: Semigroupal[F] = implicitly
  }

  implicit class SemigroupalOps[F[_]: Semigroupal, A](fa: F[A]) {
    def product[B](fb: F[B]): F[(A, B)] = Semigroupal[F].product(fa, fb)
  }

  implicit class MapNOps[F[_]: Functor: Semigroupal, A, B](x: (F[A], F[B])) {
    def mapN[R](f: (A, B) => R): F[R] = x match {
      case (fa, fb) => fa.product(fb).fmap(f.tupled)
    }
  }

  // 4.4. Implement Semigroupal for Map
  implicit def functorMap[T]: Functor[Map[T, *]] = new Functor[Map[T, *]] {
    override def fmap[A, B](fa: Map[T, A])(f: A => B): Map[T, B] =
      fa.transform((_, a) => f(a))
  }

  implicit def semigroupalMap[T]: Semigroupal[Map[T, *]] = new Semigroupal[Map[T, *]] {
    override def product[A, B](fa: Map[T, A], fb: Map[T, B]): Map[T, (A, B)] =
      for {
        (t, a) <- fa
        b      <- fb.get(t)
      } yield t -> (a, b)
  }

  // 5. Applicative
  trait Applicative[F[_]] extends Semigroupal[F] with Functor[F] {
    def pure[A](a: A): F[A]
  }

  object Applicative {
    def apply[F[_]: Applicative]: Applicative[F] = implicitly
  }

  // 5.1. Implement Applicative for Option, Either
  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def pure[A](a: A): Option[A] = Some(a)

    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
      case (Some(a), Some(b)) => Some((a, b))
      case _                  => None
    }

    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit def eitherApplicative[T]: Applicative[Either[T, *]] = new Applicative[Either[T, *]] {
    override def pure[A](a: A): Either[T, A] = Right(a)

    override def product[A, B](fa: Either[T, A], fb: Either[T, B]): Either[T, (A, B)] = (fa, fb) match {
      case (Right(a), Right(b)) => Right((a, b))
      case (Left(t), _)         => Left(t)
      case (_, Left(t))         => Left(t)
    }

    override def fmap[A, B](fa: Either[T, A])(f: A => B): Either[T, B] = fa.map(f)
  }

  // 5.2. Implement `traverse` for all Applicatives instead of Option
  def traverse[F[_]: Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(Applicative[F].pure(List.empty[B]))((a, acc) => (f(a), acc).mapN((a, b) => a :: b))

  // 6. Foldable
  // 6.1. Implement Foldable with `foldLeft`
  trait Foldable[F[_]] {
    def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B
  }

  object Foldable {
    def apply[F[_]: Foldable]: Foldable[F] = implicitly
  }

  implicit class FoldableOps[F[_]: Foldable, A](fa: F[A]) {
    def foldLeft[B](b: B)(f: (B, A) => B): B = Foldable[F].foldLeft(fa, b)(f)
  }

  // 6.2. Implement Foldable for List
  // Note: we can use here foldLeft from standard library
  implicit val listFoldable: Foldable[List] = new Foldable[List] {
    override def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
  }

  // 6.3. Implement `traverse` for all Foldables instead of List
  implicit class TraverseOps[F[_]: Foldable, A](fa: F[A]) {
    def traverse[G[_]: Applicative, B](f: A => G[B]): G[Unit] = {
      fa.foldLeft(Applicative[G].pure(()))((acc, a) => (f(a), acc).mapN((_, _) => ()))
    }
  }
}
