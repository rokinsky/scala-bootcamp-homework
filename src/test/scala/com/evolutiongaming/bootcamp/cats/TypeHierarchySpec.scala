package com.evolutiongaming.bootcamp.cats

import com.evolutiongaming.bootcamp.cats.TypeHierarchy.{Applicative, Monad}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class TypeHierarchySpec extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  "Applicative" - {
    val optionApplicative: Applicative[Option] = new Applicative[Option] {
      override def unit[A](a: => A): Option[A] = Some(a)

      override def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] =
        for {
          a <- fa
          f <- fab
        } yield f(a)
    }

    "when all optional ints must be mapped correctly" in {
      val fn: Int => String = i => i.toString
      forAll { (o: Option[Int]) =>
        optionApplicative.map(o)(fn) shouldBe o.map(fn)
      }
    }

    "when all optional ints and optional chars must be mapped correctly together" in {
      import cats.implicits.catsSyntaxApply

      val fn: (Int, Char) => String = (i, c) => s"$i$c"
      forAll { (a: Option[Int], b: Option[Char]) =>
        optionApplicative.map2(a, b)(fn) shouldBe a.map2(b)(fn)
      }
    }

    "when all lists with optional ints must be sequenced correctly" in {
      import cats.implicits.toTraverseOps

      forAll { (loi: List[Option[Int]]) =>
        optionApplicative.sequence(loi) shouldBe loi.sequence
      }
    }

    "when all lists with ints must be traversed correctly" in {
      import cats.implicits.toTraverseOps

      val fn: Int => Option[String] = {
        case 0 => None
        case i => Some(i.toString)
      }
      forAll { (li: List[Int]) =>
        optionApplicative.traverse(li)(fn) shouldBe li.traverse(fn)
      }
    }
  }

  "Monad" - {
    val optionMonad = new Monad[Option] {
      override def unit[A](a: => A): Option[A] = Some(a)

      override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
    }

    "when all optional ints must be mapped correctly" in {
      val fn: Int => String = i => i.toString
      forAll { (o: Option[Int]) =>
        optionMonad.map(o)(fn) shouldBe o.map(fn)
      }
    }

    "when all optional ints and optional chars must be mapped correctly together" in {
      import cats.implicits.catsSyntaxApply

      val fn: (Int, Char) => String = (i, c) => s"$i$c"
      forAll { (a: Option[Int], b: Option[Char]) =>
        optionMonad.map2(a, b)(fn) shouldBe a.map2(b)(fn)
      }
    }

    "when all optional ints must be flatten correctly" in {
      forAll { (o: Option[Option[Int]]) =>
        optionMonad.join(o) shouldBe o.flatten
      }
    }
  }
}
