package com.evolutiongaming.bootcamp.adt

import com.evolutiongaming.bootcamp.adt.AlgebraicDataTypes._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class AlgebraicDataTypesSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  behavior of "Board"

  it should "be correct" in {
    Board.of("5c6dAcAsQs").isRight shouldBe true
    Board.of("2h5c8sAsKc").isRight shouldBe true
    Board.of("3d4s5dJsQd").isRight shouldBe true
    Board.of("3d3s4d6hJc").isRight shouldBe true
  }

  it should "protects against invalid data" in {
    Board.of("").isLeft shouldBe true
    Board.of(" ").isLeft shouldBe true
    Board.of("2q5c8sAsKc").isLeft shouldBe true
    Board.of("3d4s5dPsQd").isLeft shouldBe true
    Board.of("3d3s4d6hJ").isLeft shouldBe true
  }

  behavior of "Hand"

  it should "be correct for Texas Holdem" in {
    Hand.of("Qs9h").isRight shouldBe true
    Hand.of("KdQh").isRight shouldBe true
    Hand.of("3cKh").isRight shouldBe true
    Hand.of("Jc6s").isRight shouldBe true
  }

  it should "be correct for Omaha Holdem" in {
    Hand.of("Js2dKd8c").isRight shouldBe true
    Hand.of("KsAsTcTs").isRight shouldBe true
    Hand.of("Jh2h3c9c").isRight shouldBe true
    Hand.of("Qc8dAd6c").isRight shouldBe true
    Hand.of("7dQsAc5d").isRight shouldBe true
  }

  it should "protects against invalid data" in {
    Hand.of("").isLeft shouldBe true
    Hand.of(" ").isLeft shouldBe true
    Hand.of("KdQ").isLeft shouldBe true
    Hand.of("3zKh").isLeft shouldBe true
    Hand.of("Zc6s").isLeft shouldBe true
  }
}
