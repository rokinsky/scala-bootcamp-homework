package com.evolutiongaming.bootcamp.basics

object Collections {

  // In a sorted list find two numbers which have a gap between
  //    None for List(1, 2, 3, 4)
  //    Some((2, 8)) for List(1, 2, 8)
  def findGap(l: List[Int]): Option[(Int, Int)] = l match {
    case Nil => None
    case xs => xs.zip(xs.tail).find { case (left, right) => right - left > 1 }
  }

  // try to implement min different ways (fold, reduce, recursion)
  def min(list: List[Int]): Option[Int] = {
    list.minOption
    // list.reduceOption(math.min)

    // list match {
    //   case Nil  => None
    //   case _    => Some(list.foldLeft(list.head)(math.min))
    // }
    //
    // list match {
    //   case Nil  => None
    //   case _    => Some(list.reduce(math.min))
    // }
  }

  // Implement scanLeft (not using scans ofc)
  def scanLeft[T](zero: T)(list: List[T])(f: (T, T) => T): List[T] = {
    list.foldLeft(List(zero))((acc, cur) => f(acc.head, cur) :: acc).reverse
  }

  // https://twitter.com/allenholub/status/1357115515672555520/photo/1
  // pass the interview
  def count(s: String): List[(Char, Int)] = {
    s.foldRight(List.empty[(Char, Int)]) {
      case (cur, (prev, n) :: tail) if cur == prev => (prev, n + 1) :: tail
      case (cur, acc)                              => (cur, 1) :: acc
    }
  }

  // hometask:
  // https://leetcode.com/problems/running-sum-of-1d-array/
  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.scanLeft(0)(_ + _).drop(1)
  }

  // https://leetcode.com/problems/shuffle-the-array
  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    // (nums.take(n) zip nums.takeRight(n)).flatMap { case (a, b) => Array(a, b) }
    Array.range(0, n).flatMap(x => Array(nums(x), nums(x + n)))
  }

  // https://leetcode.com/problems/richest-customer-wealth
  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.map(_.sum).max
  }

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    val max = candies.max
    candies.map(_ + extraCandies >= max)
  }

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points
  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
     val sortedXs = points.map { case Array(x, _) => x }.sorted
     sortedXs.zip(sortedXs.tail).map { case (cur, next) => next - cur }.max
  }

  // optional hometask:
  //
  // https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
  def maxDepth(s: String): Int = {
    s.foldLeft((0, 0)) {
      case ((max, acc), '(') => (max, acc + 1)
      case ((max, acc), ')') => (math.max(max, acc), acc - 1)
      case (acc, _)          => acc
    } match {
      case (max, _) => max
    }
  }

  // https://leetcode.com/problems/split-a-string-in-balanced-strings
  def balancedStringSplit(s: String): Int = {
    s.scanLeft(0) {
      case (balance, 'L') => balance + 1
      case (balance, 'R') => balance - 1
      case (balance, _)   => balance
    }.count(_ == 0) - 1
  }

  // https://leetcode.com/problems/matrix-block-sum/
  // TODO
}
