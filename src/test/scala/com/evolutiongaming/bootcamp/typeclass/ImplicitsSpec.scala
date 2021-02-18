package com.evolutiongaming.bootcamp.typeclass

import com.evolutiongaming.bootcamp.typeclass.Implicits._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ImplicitsSpec extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {

  "Task1" - {
    import Task1._
    import MoneyImplicits._
    "when all big decimals are sorted correctly" in {
      forAll { (amounts: List[BigDecimal]) =>
        amounts.map(Money).sorted.map(_.amount) shouldEqual amounts.sorted
      }
    }
  }

  "Task2" - {
    import Task2._
    import ShowSyntax._
    import ShowInstances._
    "when all strings are shown correctly" in {
      forAll { (id: String, name: String) =>
        {
          val user = User(id, name)
          user.show shouldEqual user.toString
        }
      }
    }
  }

  "Task3" - {
    import Task3._
    import ParseSyntax._
    import ParseInstances._
    "when to handle invalid parsing data" in {
      "lalala".parse[User].isLeft shouldBe true
    }

    "when the correct data needs to be parsed correctly" in {
      "1;Oleg".parse[User] shouldEqual Right(User("1", "Oleg"))
    }
  }

  "Task4" - {
    import Task4._
    import EqSyntax._
    import EqInstances._
    "when comparing the same type correctly" in {
      forAll { (a: Int, b: Int) =>
        {
          a eqv b shouldEqual a == b
          // FIXME: for some reasons the === syntax doesn't work here
          implicitly[EqOps[Int]](a) === b shouldEqual a == b
        }
      }
    }
  }

  "TypeclassTask" - {
    import TypeclassTask._
    import HashCodeSyntax._
    import HashCodeInstances._
    "when all strings should be hash-coded" in {
      forAll { (s: String) =>
        s.hash shouldEqual s.hashCode
      }
    }
  }

  "AdvancedHomework" - {
    import AdvancedHomework._
    import FlatMapSyntax._
    import FlatMapInstances._
    "when all optional ints must be flat-mapped correctly" in {
      val fn: Int => Option[String] = value => Some(value.toString)
      forAll { (o: Option[Int]) =>
        (o >>= fn) shouldBe o.flatMap(fn)
      }
    }
  }
}
