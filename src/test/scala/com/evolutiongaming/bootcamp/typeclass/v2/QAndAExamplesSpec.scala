package com.evolutiongaming.bootcamp.typeclass.v2

import com.evolutiongaming.bootcamp.typeclass.v2.QAndAExamples._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class QAndAExamplesSpec extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {

  "Semigroupal" - {
    "when intersects map" in {
      (Map(1 -> "a", 2 -> "b"), Map(2 -> "c")).mapN(_ + _) shouldBe Map(2 -> "bc")
    }

    "when second either is left" in {
      type ErrorOr[A] = Either[Vector[String], A]

      Semigroupal[ErrorOr].product(
        Right(1),
        Left(Vector("error 2"))
      ) shouldBe Left(Vector("error 2"))
    }
  }

  "Applicative" - {
    "when traverse with none" in {
      traverse(List(1, 2, 3)) { i =>
        Option.when(i % 2 == 1)(i)
      } shouldBe None
    }

    "when traverse without none" in {
      traverse(List(1, 2, 3)) { i =>
        Some(i + 1): Option[Int]
      } shouldBe Some(List(2, 3, 4))
    }
  }

  "Foldable" - {
    "when traverse list using option" in {
      List("1", "2", "3").traverse(_.toIntOption) shouldBe Option(())
      List("a", "b", "c").traverse(_.toIntOption) shouldBe None
    }

    "when traverse list using either" in {
      List("1", "2", "3").traverse(_.toIntOption.toRight("Error")) shouldBe Right(())
      List("a", "b", "c").traverse(_.toIntOption.toRight("Error")) shouldBe Left("Error")
    }
  }
}
