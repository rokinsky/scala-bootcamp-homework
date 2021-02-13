package com.evolutiongaming.bootcamp.basics

import com.evolutiongaming.bootcamp.basics.ControlStructures._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ControlStructuresSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  behavior of "process"

  it should "show errors when incorrect command type" in {
    process("") shouldEqual "Error: unrecognized command"
    process("boom 4 0") shouldEqual "Error: unrecognized command"
    process("foo 5 6") shouldEqual "Error: unrecognized command"
  }

  behavior of "Divide"

  it should "be correctly processed" in {
    process("divide 4 5") shouldEqual "4 divided by 5 is 0.8"
    process("   divide 4  5") shouldEqual "4 divided by 5 is 0.8"
    process("divide 36 4") shouldEqual "36 divided by 4 is 9"
    process("divide 2 3") shouldBe "2 divided by 3 is 0.6666666666666666"
  }

  it should "show errors when incorrect input" in {
    process("divide chicken egg") shouldEqual "Error: non-number detected (chicken)"
    process("divide") shouldEqual "Error: command requires precisely 2 arguments"
    process("divide 36 0") shouldEqual "Error: division by zero is not defined"
    process("divide 36") shouldEqual "Error: command requires precisely 2 arguments"
  }

  it should "be right parsed" in {
    forAll { (dividend: Double, divisor: Double) =>
      {
        parseCommand(s"divide $dividend $divisor") shouldBe
          Right(Command.Divide(dividend, divisor))
      }
    }
  }

  it should "be right calculated" in {
    forAll { (dividend: Double, divisor: Double) =>
      {
        whenever(divisor != 0) {
          val command = Command.Divide(dividend, divisor)
          calculate(command) shouldBe Right(Result(command, dividend / divisor))
        }
      }
    }
  }

  behavior of "Sum"

  it should "be correctly processed" in {
    process("sum 5 5 6 8.5") shouldEqual "the sum of 5 5 6 8.5 is 24.5"
    process("sum     5 5    6 8.5   ") shouldEqual "the sum of 5 5 6 8.5 is 24.5"
    process("sum 5") shouldEqual "the sum of 5 is 5"
    process("sum 4 5") shouldEqual "the sum of 4 5 is 9"
    process("sum 4  5      8") shouldEqual "the sum of 4 5 8 is 17"
  }

  it should "show errors when incorrect input" in {
    process("sum") shouldEqual "Error: command isn't defined for an empty list of arguments"
    process("sum  ") shouldEqual "Error: command isn't defined for an empty list of arguments"
    process("sum 4 foo 5 bar 1") shouldEqual "Error: non-number detected (foo)"
  }

  it should "be right parsed" in {
    forAll { (numbers: List[Double]) =>
      {
        parseCommand(s"sum ${numbers.mkString(" ")}") shouldBe
          Right(Command.Sum(numbers))
      }
    }
  }

  it should "be right calculated" in {
    forAll { (numbers: List[Double]) =>
      {
        whenever(numbers.nonEmpty) {
          val command = Command.Sum(numbers)
          calculate(command) shouldBe
            Right(Result(command, numbers.sum))
        }
      }
    }
  }

  behavior of "Average"

  it should "be correctly processed" in {
    process("average 4.45") shouldBe "the average of 4.45 is 4.45"
    process("average 2 2 3") shouldBe "the average of 2 2 3 is 2.3333333333333335"
    process("average 4 3 8.5 4") shouldEqual "the average of 4 3 8.5 4 is 4.875"
    process("average 4") shouldEqual "the average of 4 is 4"
    process("average 4 3    8.5 4") shouldEqual "the average of 4 3 8.5 4 is 4.875"
    process("average 3 8 1") shouldEqual "the average of 3 8 1 is 4"
  }

  it should "show errors when incorrect input" in {
    process("average") shouldEqual "Error: command isn't defined for an empty list of arguments"
    process("average 4 $# !@ 4") shouldEqual "Error: non-number detected ($#)"
  }

  it should "be right parsed" in {
    forAll { (numbers: List[Double]) =>
      {
        parseCommand(s"average ${numbers.mkString(" ")}") shouldBe
          Right(Command.Average(numbers))
      }
    }
  }

  it should "be right calculated" in {
    forAll { (numbers: List[Double]) =>
      {
        whenever(numbers.nonEmpty) {
          val command = Command.Average(numbers)
          calculate(command) shouldBe
            Right(Result(command, numbers.sum / numbers.length))
        }
      }
    }
  }

  behavior of "Min"

  it should "be correctly processed" in {
    process("min 4") shouldEqual "the minimum of 4 is 4"
    process("min    4 -3      -17   ") shouldEqual "the minimum of 4 -3 -17 is -17"
    process("min -0.999") shouldBe "the minimum of -0.999 is -0.999"
    process("min 4 -3 -17") shouldEqual "the minimum of 4 -3 -17 is -17"
    process("min 3 8 1 6") shouldEqual "the minimum of 3 8 1 6 is 1"
    process("min -3 -8 -1 -6") shouldEqual "the minimum of -3 -8 -1 -6 is -8"
  }

  it should "show errors when incorrect input" in {
    process("min") shouldEqual "Error: command isn't defined for an empty list of arguments"
    process("min 4 hello -17") shouldEqual "Error: non-number detected (hello)"
  }

  it should "be right parsed" in {
    forAll { (numbers: List[Double]) =>
      {
        parseCommand(s"min ${numbers.mkString(" ")}") shouldBe
          Right(Command.Min(numbers))
      }
    }
  }

  it should "be right calculated" in {
    forAll { (numbers: List[Double]) =>
      {
        whenever(numbers.nonEmpty) {
          val command = Command.Min(numbers)
          calculate(command) shouldBe
            Right(Result(command, numbers.min))
        }
      }
    }
  }

  behavior of "Max"

  it should "be correctly processed" in {
    process("max 4.45005 -3.75 -17.875") shouldBe "the maximum of 4.45005 -3.75 -17.875 is 4.45005"
    process("max 4 -3 -17") shouldEqual "the maximum of 4 -3 -17 is 4"
    process("max") shouldEqual "Error: command isn't defined for an empty list of arguments"
    process("max 4") shouldEqual "the maximum of 4 is 4"
    process("   max 4    -3 -17 ") shouldEqual "the maximum of 4 -3 -17 is 4"
    process("max 1020.220") shouldBe "the maximum of 1020.22 is 1020.22"
    process("max -3 -8 -1 -6") shouldEqual "the maximum of -3 -8 -1 -6 is -1"
    process("max 3 8 1 6") shouldEqual "the maximum of 3 8 1 6 is 8"
  }

  it should "show errors when incorrect input" in {
    process("max 4 -3 3qwe") shouldEqual "Error: non-number detected (3qwe)"
    process("max0") shouldBe "Error: unrecognized command"
  }

  it should "be right parsed" in {
    forAll { (numbers: List[Double]) =>
      {
        parseCommand(s"max ${numbers.mkString(" ")}") shouldBe
          Right(Command.Max(numbers))
      }
    }
  }

  it should "be right calculated" in {
    forAll { (numbers: List[Double]) =>
      {
        whenever(numbers.nonEmpty) {
          val command = Command.Max(numbers)
          calculate(command) shouldBe
            Right(Result(command, numbers.max))
        }
      }
    }
  }
}
