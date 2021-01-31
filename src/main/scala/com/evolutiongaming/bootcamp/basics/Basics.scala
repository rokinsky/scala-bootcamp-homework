package com.evolutiongaming.bootcamp.basics

import scala.annotation.tailrec
import scala.math.abs

object Basics {
  // Homework. Implement functions that calculate
  // https://en.wikipedia.org/wiki/Least_common_multiple and
  // https://en.wikipedia.org/wiki/Greatest_common_divisor for integers.

  // NOTE: Assuming that `lcm` and `gcd` won’t be invoked with arguments
  // that will cause an overflow as in general case it is possible, e.g.
  // lcm(895342101, 1592538308) shouldEqual 1425866594607705108
  // gcd(-2147483648,-2147483648) shouldEqual 2147483648 // max int is 2147483647

  def lcm(a: Int, b: Int): Int = (a, b) match {
    case (_, 0) => 0
    case (_, _) => abs(a) / gcd(a, b) * abs(b)
  }

  @tailrec
  def gcd(a: Int, b: Int): Int = (a, b) match {
    case (_, 0) => abs(a)
    case (_, _) => gcd(b, a % b)
  }
}
