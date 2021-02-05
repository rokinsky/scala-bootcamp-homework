package com.evolutiongaming.bootcamp.basics

import com.evolutiongaming.bootcamp.basics.Collections._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random

class CollectionsSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  behavior of "findGap"

  it should "find gap" in {
    findGap(List(1, 2, 3, 5, 6)) shouldEqual Some(3, 5)
  }

  it should "work correctly on empty" in {
    findGap(List.empty) shouldEqual None
  }

  it should "work correctly on no gaps" in {
    findGap((1 to 100).toList) shouldEqual None
  }

  behavior of "min"

  it should "work correctly on empty" in {
    min(Nil) shouldEqual None
  }

  it should "work correctly on non empty" in {
    min(Random.shuffle(1 to 100).toList) shouldEqual Some(1)
  }

  behavior of "scanLeft"

  it should "work correctly on numbers" in {
    val numbers = (1 to 100).toList
    scanLeft(0)(numbers)(_ + _) shouldEqual numbers.scanLeft(0)(_ + _)
  }

  it should "work correctly on letters" in {
    val letters = ('a' to 'z').toList.map(_.toString)
    scanLeft("")(letters)(_ + _) shouldEqual letters.scanLeft("")(_ + _)
  }

  behavior of "count"

  it should "pass" in {
    count("aaaabbbcca") shouldEqual List(('a', 4), ('b', 3), ('c', 2), ('a', 1))
  }

  behavior of "runningSum"

  it should "work correctly on example 1" in {
    runningSum(Array(1, 2, 3, 4)) shouldEqual Array(1, 3, 6, 10)
  }

  it should "work correctly on example 2" in {
    runningSum(Array(1, 1, 1, 1, 1)) shouldEqual Array(1, 2, 3, 4, 5)
  }

  it should "work correctly on example 3" in {
    runningSum(Array(3, 1, 2, 10, 1)) shouldEqual Array(3, 4, 6, 16, 17)
  }

  behavior of "shuffle"

  it should "work correctly on example 1" in {
    shuffle(Array(2, 5, 1, 3, 4, 7), 3) shouldEqual Array(2, 3, 5, 4, 1, 7)
  }

  it should "work correctly on example 2" in {
    shuffle(Array(1, 2, 3, 4, 4, 3, 2, 1), 4) shouldEqual Array(1, 4, 2, 3, 3, 2, 4, 1)
  }

  it should "work correctly on example 3" in {
    shuffle(Array(1, 1, 2, 2), 2) shouldEqual Array(1, 2, 1, 2)
  }

  behavior of "maximumWealth"

  it should "work correctly on example 1" in {
    maximumWealth(Array(Array(1, 2, 3), Array(3, 2, 1))) shouldEqual 6
  }

  it should "work correctly on example 2" in {
    maximumWealth(Array(Array(1, 5), Array(7, 3), Array(3, 5))) shouldEqual 10
  }

  it should "work correctly on example 3" in {
    maximumWealth(Array(Array(2, 8, 7), Array(7, 1, 3), Array(1, 9, 5))) shouldEqual 17
  }

  behavior of "kidsWithCandies"

  it should "work correctly on example 1" in {
    kidsWithCandies(Array(2, 3, 5, 1, 3), 3) shouldEqual Array(true, true, true, false, true)
  }

  it should "work correctly on example 2" in {
    kidsWithCandies(Array(4, 2, 1, 1, 2), 1) shouldEqual Array(true, false, false, false, false)
  }

  it should "work correctly on example 3" in {
    kidsWithCandies(Array(12, 1, 12), 10) shouldEqual Array(true, false, true)
  }

  behavior of "maxWidthOfVerticalArea"

  it should "work correctly on example 1" in {
    maxWidthOfVerticalArea(Array(
      Array(8, 7),
      Array(9, 9),
      Array(7, 4),
      Array(9, 7)
    )) shouldEqual 1
  }

  it should "work correctly on example 2" in {
    maxWidthOfVerticalArea(Array(
      Array(3, 1),
      Array(9, 0),
      Array(1, 0),
      Array(1, 4),
      Array(5, 3),
      Array(8, 8)
    )) shouldEqual 3
  }

  behavior of "maxDepth"

  it should "work correctly on example 1" in {
    maxDepth("(1+(2*3)+((8)/4))+1") shouldEqual 3
  }

  it should "work correctly on example 2" in {
    maxDepth("(1)+((2))+(((3)))") shouldEqual 3
  }

  it should "work correctly on example 3" in {
    maxDepth("1+(2*3)/(2-1)") shouldEqual 1
  }

  it should "work correctly on example 4" in {
    maxDepth("1") shouldEqual 0
  }
}
