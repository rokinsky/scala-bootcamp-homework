package com.evolutiongaming.bootcamp.basics

import scala.io.Source

object ControlStructures {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  object NumberParser {
    def parse(number: String): Either[ParseError, Double] =
      number.toDoubleOption.toRight(Error.NumberFormat(number))

    def parse(numbers: List[String]): Either[ParseError, List[Double]] =
      numbers.partitionMap(parse) match {
        case (Nil, parsedNumbers) => Right(parsedNumbers)
        case (error :: _, _)      => Left(error)
      }
  }

  object NumberRenderer {
    // Strip all trailing zeros if applicable
    def render(number: Double): String =
      if (number.isWhole) f"$number%.0f" else number.toString

    def render(numbers: List[Double]): String =
      numbers.map(render).mkString(" ")
  }

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command

    import NumberParser._

    object Divide {
      def apply(dividend: String, divisor: String): Either[ParseError, Divide] =
        for {
          parsedDividend <- parse(dividend)
          parsedDivisor  <- parse(divisor)
        } yield Divide(parsedDividend, parsedDivisor)
    }

    object Sum {
      def apply(numbers: List[String]): Either[ParseError, Sum] =
        for {
          parsedNumbers <- parse(numbers)
        } yield Sum(parsedNumbers)
    }

    object Average {
      def apply(numbers: List[String]): Either[ParseError, Average] =
        for {
          parsedNumbers <- parse(numbers)
        } yield Average(parsedNumbers)
    }

    object Min {
      def apply(numbers: List[String]): Either[ParseError, Min] =
        for {
          parsedNumbers <- parse(numbers)
        } yield Min(parsedNumbers)
    }

    object Max {
      def apply(numbers: List[String]): Either[ParseError, Max] =
        for {
          parsedNumbers <- parse(numbers)
        } yield Max(parsedNumbers)
    }
  }

  final case class Result(command: Command, result: Double)

  sealed trait Error
  sealed trait ParseError extends Error
  sealed trait CalculationError extends Error
  object Error {
    final case class NumberFormat(nonNumber: String) extends ParseError
    final case class NumberOfArguments(nArguments: Int) extends ParseError
    final case object UnrecognizedCommand extends ParseError
    final case object DivisionByZero extends CalculationError
    final case object NilCalculation extends CalculationError
  }

  def parseCommand(input: String): Either[ParseError, Command] = {
    import Command._

    input.strip.split("\\s+").toList match {
      case "divide" :: dividend :: divisor :: Nil => Divide(dividend, divisor)
      case "divide" :: _                          => Left(Error.NumberOfArguments(2))
      case "sum" :: numbers                       => Sum(numbers)
      case "average" :: numbers                   => Average(numbers)
      case "min" :: numbers                       => Min(numbers)
      case "max" :: numbers                       => Max(numbers)
      case _                                      => Left(Error.UnrecognizedCommand)
    }
  }

  def calculate(command: Command): Either[CalculationError, Result] = {
    import Command._

    command match {
      case Divide(_, 0)              => Left(Error.DivisionByZero)
      case Divide(dividend, divisor) => Right(Result(command, dividend / divisor))
      case Sum(Nil)                  => Left(Error.NilCalculation)
      case Sum(numbers)              => Right(Result(command, numbers.sum))
      case Average(Nil)              => Left(Error.NilCalculation)
      case Average(numbers)          => Right(Result(command, numbers.sum / numbers.length))
      case Min(Nil)                  => Left(Error.NilCalculation)
      case Min(numbers)              => Right(Result(command, numbers.min))
      case Max(Nil)                  => Left(Error.NilCalculation)
      case Max(numbers)              => Right(Result(command, numbers.max))
    }
  }

  def renderResult(result: Result): String = {
    import Command._
    import NumberRenderer._

    result match {
      case Result(Divide(dividend, divisor), result) =>
        s"${render(dividend)} divided by ${render(divisor)} is ${render(result)}"
      case Result(Sum(numbers), result) => s"the sum of ${render(numbers)} is ${render(result)}"
      case Result(Average(numbers), result) =>
        s"the average of ${render(numbers)} is ${render(result)}"
      case Result(Min(numbers), result) => s"the minimum of ${render(numbers)} is ${render(result)}"
      case Result(Max(numbers), result) => s"the maximum of ${render(numbers)} is ${render(result)}"
    }
  }

  def renderError(error: Error): String = {
    import Error._

    val message = error match {
      case NumberFormat(value)      => s"non-number detected ($value)"
      case UnrecognizedCommand      => "unrecognized command"
      case NumberOfArguments(value) => s"command requires precisely $value arguments"
      case DivisionByZero           => "division by zero is not defined"
      case NilCalculation           => "command isn't defined for an empty list of arguments"
    }

    s"Error: $message"
  }

  def process(input: String): String = {
    val output = for {
      command <- parseCommand(input)
      result  <- calculate(command)
    } yield result

    output.fold(renderError, renderResult)
  }

  def main(args: Array[String]): Unit = Source.stdin.getLines().map(process).foreach(println)
}
