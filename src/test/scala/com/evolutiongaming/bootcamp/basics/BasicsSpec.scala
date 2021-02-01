package com.evolutiongaming.bootcamp.basics

import com.evolutiongaming.bootcamp.basics.Basics._
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.math.{cbrt, sqrt}

class BasicsSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  def boundIntegers(x: Int): Gen[Int] = for (n <- Gen.choose(-x, x)) yield n

  val squareBoundedIntegers: Gen[Int] = boundIntegers(sqrt(Int.MaxValue).toInt)
  val cubeBoundedIntegers:   Gen[Int] = boundIntegers(cbrt(Int.MaxValue).toInt)

  behavior of "gcd"

  it should "be correct for random generated values" in {
    forAll(squareBoundedIntegers, squareBoundedIntegers) { (a: Int, b: Int) =>
      gcd(a, b) shouldEqual BigInt(a).gcd(BigInt(b))
    }
  }

  it should "follow the commutative law" in {
    forAll(squareBoundedIntegers, squareBoundedIntegers) { (a: Int, b: Int) =>
      gcd(a, b) shouldEqual gcd(b, a)
    }
  }

  it should "follow the associative law" in {
    forAll(squareBoundedIntegers, squareBoundedIntegers, squareBoundedIntegers) { (a: Int, b: Int, c: Int) =>
      gcd(a, gcd(b, c)) shouldEqual gcd(gcd(a, b), c)
    }
  }

  it should "follow the absorption law" in {
    forAll(squareBoundedIntegers, squareBoundedIntegers) { (a: Int, b: Int) =>
      gcd(a, lcm(a, b)) shouldEqual a.abs
    }
  }

  it should "follow the idempotent law" in {
    forAll(squareBoundedIntegers) { (a: Int) =>
      gcd(a, a) shouldEqual a.abs
    }
  }

  it should "be distributed over lcm" in {
    forAll(squareBoundedIntegers, squareBoundedIntegers, squareBoundedIntegers) { (a: Int, b: Int, c: Int) =>
      gcd(a, lcm(b, c)) shouldEqual lcm(gcd(a, b), gcd(a, c))
    }
  }

  behavior of "lcm"

  it should "be correct for non-zero random generated values" in {
    forAll(squareBoundedIntegers, squareBoundedIntegers) { (a: Int, b: Int) =>
      whenever(a != 0 && b != 0) {
        lcm(a, b) shouldEqual a.abs / BigInt(a).gcd(BigInt(b)).toInt * b.abs
      }
    }
  }

  it should "be correct for zero values" in {
    lcm(0, 0) shouldEqual 0
  }

  it should "follow the commutative law" in {
    forAll(squareBoundedIntegers, squareBoundedIntegers) { (a: Int, b: Int) =>
      lcm(a, b) shouldEqual lcm(b, a)
    }
  }

  it should "follow the associative law" in {
    forAll(cubeBoundedIntegers, cubeBoundedIntegers, cubeBoundedIntegers) { (a: Int, b: Int, c: Int) =>
      lcm(a, lcm(b, c)) shouldEqual lcm(lcm(a, b), c)
    }
  }

  it should "follow the absorption law" in {
    forAll(squareBoundedIntegers, squareBoundedIntegers) { (a: Int, b: Int) =>
      lcm(a, gcd(a, b)) shouldEqual a.abs
    }
  }

  it should "follow the idempotent law" in {
    forAll(squareBoundedIntegers) { (a: Int) =>
      lcm(a, a) shouldEqual a.abs
    }
  }

  it should "be distributed over gcd" in {
    forAll(squareBoundedIntegers, squareBoundedIntegers, squareBoundedIntegers) { (a: Int, b: Int, c: Int) =>
      lcm(a, gcd(b, c)) shouldEqual gcd(lcm(a, b), lcm(a, c))
    }
  }
}
