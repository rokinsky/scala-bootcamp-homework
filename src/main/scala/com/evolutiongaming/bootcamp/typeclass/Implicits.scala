package com.evolutiongaming.bootcamp.typeclass

object Implicits {

  object Task1 {
    final case class Money(amount: BigDecimal)

    object MoneyImplicits {
      implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)
    }
  }

  object Task2 {
    final case class User(id: String, name: String)

    trait Show[T] {
      def show(entity: T): String
    }

    object Show {
      def apply[F: Show]: Show[F] = implicitly[Show[F]]
    }

    object ShowSyntax {
      implicit class ShowOps[A: Show](inner: A) {
        def show: String = Show[A].show(inner)
      }
    }

    object ShowInstances {
      implicit val showUser: Show[User] = _.toString
    }
  }

  object Task3 {
    type Error = String
    final case class User(id: String, name: String)

    trait Parse[T] {
      def parse(entity: String): Either[Error, T]
    }

    object Parse {
      def apply[F: Parse]: Parse[F] = implicitly[Parse[F]]
    }

    object ParseSyntax {
      implicit class ParseOps(inner: String) {
        def parse[A: Parse]: Either[Error, A] = Parse[A].parse(inner)
      }
    }

    object ParseInstances {
      implicit val parseUser: Parse[User] = (entity: String) =>
        entity.split(";").toList match {
          case id :: name :: Nil => Right(User(id, name))
          case _                 => Left(s"Cannot parse '$entity' as User")
        }
    }
  }

  object Task4 {
    trait Eq[T] {
      def eqv(lhs: T, rhs: T): Boolean
    }

    object Eq {
      def apply[F: Eq]: Eq[F] = implicitly[Eq[F]]
    }

    object EqSyntax {
      implicit class EqOps[A: Eq](lhs: A) {
        def ===(rhs: A): Boolean = Eq[A].eqv(lhs, rhs)
        def eqv(rhs: A): Boolean = Eq[A].eqv(lhs, rhs)
      }
    }

    object EqInstances {
      implicit def anyEq[A]: Eq[A] = _ == _
    }
  }

  object TypeclassTask {
    trait HashCode[T] {
      def hash(entity: T): Int
    }

    object HashCode {
      def apply[F: HashCode]: HashCode[F] = implicitly[HashCode[F]]
    }

    object HashCodeSyntax {
      implicit class HashCodeOps[A: HashCode](x: A) {
        def hash: Int = HashCode[A].hash(x)
      }
    }

    object HashCodeInstances {
      implicit val stringHashCode: HashCode[String] = _.hashCode
    }
  }

  object AdvancedHomework {
    trait FlatMap[F[_]] {
      def flatMap[A, B](x: F[A])(f: A => F[B]): F[B]
    }

    object FlatMap {
      def apply[F[_]: FlatMap]: FlatMap[F] = implicitly[FlatMap[F]]
    }

    object FlatMapSyntax {
      implicit class FlatMapOps[F[_]: FlatMap, A](x: F[A]) {
        def >>=[B](f: A => F[B]): F[B] = FlatMap[F].flatMap(x)(f)
      }
    }

    object FlatMapInstances {
      implicit val optionFlatMap: FlatMap[Option] = new FlatMap[Option] {
        def flatMap[A, B](x: Option[A])(f: A => Option[B]): Option[B] = x.flatMap(f)
      }
    }
  }
}
