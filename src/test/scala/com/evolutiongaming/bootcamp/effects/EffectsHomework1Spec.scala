package com.evolutiongaming.bootcamp.effects

import com.evolutiongaming.bootcamp.effects.EffectsHomework1._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll

import java.io.ByteArrayOutputStream
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.util.{Failure, Success, Try}

class EffectsHomework1Spec extends AnyFlatSpec with Matchers {
  "map" should "transform values" in {
    forAll { (x: String, y: String) =>
      val a = IO(x)
      val c = a.map(_ => y)
      c.unsafeRunSync() shouldEqual y
    }
  }

  "flatMap" should "work correctly" in {
    forAll { (x: String, y: String) =>
      val a = IO(x)
      val b = IO(y)
      val c = a.flatMap(_ => b)
      c.unsafeRunSync() shouldEqual y
    }
  }

  "flatMap" should "sequence the side effects" in {
    forAll { (x: String, y: String) =>
      val baos = new ByteArrayOutputStream
      Console.withOut(baos) {
        val out = for {
          _ <- IO(print(x))
          _ <- IO(print(y))
        } yield ()
        baos.toString shouldEqual ""
        out.unsafeRunSync()
        baos.toString shouldEqual x + y
      }
    }
  }

  "*>" should "ignore the first action" in {
    forAll { (x: String, y: String) =>
      val a = IO(x)
      val b = IO(y)
      val c = a *> b
      c.unsafeRunSync() shouldEqual y
    }
  }

  "*>" should "not run if source fails" in {
    forAll { (x: String, ex: Exception) =>
      val a = IO.raiseError(ex)
      val b = IO(x)
      val c = a *> b
      an[Exception] should be thrownBy c.unsafeRunSync()
    }
  }

  "as" should "replace the result of IO" in {
    forAll { (x: String, y: String) =>
      val a = IO(x)
      val b = IO(y)
      val c = a as b
      c.unsafeRunSync() shouldEqual b
    }
  }

  "void" should "return unit" in {
    forAll { (x: String) =>
      val input = IO(x).void
      input.unsafeRunSync() shouldEqual ()
    }
  }

  "attempt" should "materialize an exception" in {
    forAll { (ex: Exception) =>
      val b = IO.raiseError(ex).attempt
      b.unsafeRunSync() shouldEqual Left(ex)
    }
  }

  it should "work with valid value" in {
    forAll { (x: Int) =>
      val b = IO(x).attempt
      b.unsafeRunSync() shouldEqual Right(x)
    }
  }

  "option" should "work with valid data" in {
    forAll { (x: Int) =>
      val input = IO(x).option
      input.unsafeRunSync() shouldEqual Some(x)
    }
  }

  it should "return None when data is invalid" in {
    val input = IO("not-int".toInt).option
    input.unsafeRunSync() shouldEqual None
  }

  "handleErrorWith" should "handle errors" in {
    forAll { (x: Int) =>
      val input = IO(x / 0).handleErrorWith(_ => IO(-1))
      input.unsafeRunSync() shouldEqual -1
    }
  }

  it should "handle valid values" in {
    forAll { (x: Int) =>
      val input = IO(x / 1).handleErrorWith(_ => IO(-1))
      input.unsafeRunSync() shouldEqual x
    }
  }

  "redeem" should "handle errors" in {
    forAll { (x: Int) =>
      val input = IO(x / 0).redeem(_ => None, Some(_))
      input.unsafeRunSync() shouldEqual None
    }
  }

  it should "handle valid values" in {
    forAll { (x: Int) =>
      val input = IO(x / 1).redeem(_ => None, Some(_))
      input.unsafeRunSync() shouldEqual Some(x)
    }
  }

  "redeemWith" should "handle errors" in {
    forAll { (x: Int) =>
      val input = IO(x / 0).redeemWith(_ => IO[Option[Int]](None), v => IO[Option[Int]](Some(v)))
      input.unsafeRunSync() shouldEqual None
    }
  }

  it should "handle valid values" in {
    forAll { (x: Int) =>
      val input = IO(x / 1).redeemWith(_ => IO[Option[Int]](None), v => IO[Option[Int]](Some(v)))
      input.unsafeRunSync() shouldEqual Some(x)
    }
  }

  "unsafeToFuture" should "handle valid values" in {
    implicit val ec: ExecutionContextExecutor = ExecutionContext.global
    forAll { (x: Int, y: Int) =>
      IO(x + y).unsafeToFuture().onComplete {
        case Success(value)     => value shouldEqual x + y
        case Failure(exception) => fail(s"${x + y} expected, but $exception found")
      }
    }
  }

  it should "handle errors" in {
    implicit val ec: ExecutionContextExecutor = ExecutionContext.global
    forAll { (ex: Exception) =>
      IO.raiseError(ex).unsafeToFuture().onComplete {
        case Success(value)     => fail(s"$ex expected, but $value found")
        case Failure(exception) => exception shouldBe ex
      }
    }
  }

  "suspend" should "suspend the side effects" in {
    forAll { (x: String) =>
      val baos = new ByteArrayOutputStream
      Console.withOut(baos) {
        val input = IO.suspend { IO(print(x)) }
        baos.toString shouldEqual ""
        input.unsafeRunSync()
        baos.toString shouldEqual x
      }
    }
  }

  "delay" should "suspend the side effects" in {
    forAll { (x: String) =>
      val baos = new ByteArrayOutputStream
      Console.withOut(baos) {
        val input = IO.delay { print(x) }
        baos.toString shouldEqual ""
        input.unsafeRunSync()
        baos.toString shouldEqual x
      }
    }
  }

  "pure" should "suspend pure values" in {
    forAll { (x: Int) =>
      IO.pure(x).unsafeRunSync() shouldEqual x
    }
  }

  "fromEither" should "convert from Right" in {
    forAll { (x: Int) =>
      val input = IO.fromEither(Right(x)).unsafeRunSync()
      input shouldEqual x
    }
  }

  it should "convert from Left" in {
    forAll { (ex: Exception) =>
      val input = IO.fromEither(Left(ex))
      a[Exception] should be thrownBy input.unsafeRunSync()
    }
  }

  "fromOption" should "convert from Some" in {
    forAll { (x: Int) =>
      val ex    = new NumberFormatException
      val input = IO.fromOption(Some(x))(ex).unsafeRunSync()
      input shouldEqual x
    }
  }

  it should "throw exceptions in case of None" in {
    forAll { (ex: Exception) =>
      val input = IO.fromOption(None)(ex)
      a[Exception] should be thrownBy input.unsafeRunSync()
    }
  }

  "fromTry" should "convert from Success" in {
    forAll { (x: Int) =>
      val input = IO.fromTry(Try(x / 1)).unsafeRunSync()
      input shouldEqual x
    }
  }

  it should "throw exceptions in case of Failure" in {
    forAll { (x: Int) =>
      val input = IO.fromTry(Try(x / 0))
      a[ArithmeticException] should be thrownBy input.unsafeRunSync()
    }
  }

  "none" should "return None" in {
    val input = IO.none
    input.unsafeRunSync() shouldEqual None
  }

  "raiseError" should "throw exceptions" in {
    forAll { (ex: Exception) =>
      val b = IO.raiseError(ex)
      a[Exception] should be thrownBy b.unsafeRunSync()
    }
  }

  "raiseUnless" should "handle true condition" in {
    forAll { (ex: Exception) =>
      val input = IO.raiseUnless(cond = true)(ex)
      input.unsafeRunSync() shouldEqual ()
    }
  }

  it should "handle false condition" in {
    forAll { (ex: Exception) =>
      val input = IO.raiseUnless(cond = false)(ex)
      a[Exception] should be thrownBy input.unsafeRunSync()
    }
  }

  "raiseWhen" should "handle true condition" in {
    forAll { (ex: Exception) =>
      val input = IO.raiseWhen(cond = true)(ex)
      a[Exception] should be thrownBy input.unsafeRunSync()
    }
  }

  it should "handle false condition" in {
    forAll { (ex: Exception) =>
      val input = IO.raiseWhen(cond = false)(ex)
      input.unsafeRunSync() shouldEqual ()
    }
  }

  "unlessA" should "handle true condition" in {
    forAll { (x: String) =>
      val baos = new ByteArrayOutputStream()
      Console.withOut(baos) {
        val input = IO.unlessA(cond = true) { IO(print(x)) }
        baos.toString shouldEqual ""
        input.unsafeRunSync() shouldEqual ()
      }
    }
  }

  it should "handle false condition" in {
    forAll { (x: String) =>
      val baos = new ByteArrayOutputStream()
      Console.withOut(baos) {
        IO.unlessA(cond = false) {
          IO(print(x))
        }.unsafeRunSync()
        baos.toString shouldEqual x
      }
    }
  }

  "whenA" should "handle true condition" in {
    forAll { (x: String) =>
      val baos = new ByteArrayOutputStream()
      Console.withOut(baos) {
        val input = IO.unlessA(cond = true) {
          IO(print(x))
        }
        input.unsafeRunSync() shouldEqual ()
      }
    }
  }

  it should "handle false condition" in {
    forAll { (x: String) =>
      val baos = new ByteArrayOutputStream()
      Console.withOut(baos) {
        IO.unlessA(cond = false) {
          IO(print(x))
        }.unsafeRunSync()
        baos.toString shouldEqual x
      }
    }
  }
}
