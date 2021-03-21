package com.evolutiongaming.bootcamp.testing2

import com.evolutiongaming.bootcamp.testing2.Calculator.Expression
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.whenever
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll

class CalculatorSpec extends AnyPropSpec with Matchers {
  val dsl: Expression[Option, Int] = Expression.intInterpreter[Option]
  import dsl._
  type BinaryOp[A] = (A, A) => A

  val checkCommutative: BinaryOp[Option[Int]] => Assertion = prop => {
    forAll { (x: Int, y: Int) =>
      prop(const(x), const(y)) shouldBe prop(const(y), const(x))
    }
  }

  val checkAssociative: BinaryOp[Option[Int]] => Assertion = prop => {
    forAll { (x: Int, y: Int, z: Int) =>
      prop(const(x), prop(const(y), const(z))) shouldBe prop(prop(const(x), const(y)), const(z))
    }
  }

  val checkLeftIdentity: (BinaryOp[Option[Int]], Int) => Assertion = (prop, id) => {
    forAll { (x: Int) =>
      prop(const(id), const(x)) shouldBe const(x)
    }
  }

  val checkRightIdentity: (BinaryOp[Option[Int]], Int) => Assertion = (prop, id) => {
    forAll { (x: Int) =>
      prop(const(x), const(id)) shouldBe const(x)
    }
  }

  property("add should be correct") {
    forAll { (x: Int, y: Int) =>
      add(const(x), const(y)) shouldBe const(x + y)
    }
  }

  property("add should be commutative") {
    checkCommutative(add)
  }

  property("add should be associative") {
    checkAssociative(add)
  }

  property("add should follow identity law") {
    checkLeftIdentity(add, 0)
    checkRightIdentity(add, 0)
  }

  property("multiply should be correct") {
    forAll { (x: Int, y: Int) =>
      mul(const(x), const(y)) shouldBe const(x * y)
    }
  }

  property("multiply should be commutative") {
    checkCommutative(mul)
  }

  property("multiply should be associative") {
    forAll { (x: Int, y: Int, z: Int) =>
      mul(const(x), mul(const(y), const(z))) shouldBe mul(mul(const(x), const(y)), const(z))
    }
  }

  property("multiply should follow identity law") {
    checkLeftIdentity(mul, 1)
    checkRightIdentity(mul, 1)
  }

  property("divide should be correct") {
    forAll { (x: Int, y: Int) =>
      whenever(y != 0) {
        div(const(x), const(y)) shouldBe const(x / y)
      }
    }
  }

  property("divide should be correct for zero dividend") {
    forAll { (x: Int) =>
      whenever(x != 0) {
        div(const(0), const(x)) shouldBe const(0)
      }
    }
  }

  property("divide should follow division by itself") {
    forAll { (x: Int) =>
      whenever(x != 0) {
        div(const(x), const(x)) shouldBe const(1)
      }
    }
  }

  property("divide should follow right identity law") {
    checkRightIdentity(div, 1)
  }

  property("subtract should be correct") {
    forAll { (x: Int, y: Int) =>
      sub(const(x), const(y)) shouldBe const(x - y)
    }
  }

  property("subtract should follow right identity law") {
    checkRightIdentity(sub, 0)
  }

  property("subtract should follow subtraction with itself") {
    forAll { (x: Int) =>
      sub(const(x), const(x)) shouldBe const(0)
    }
  }
}
