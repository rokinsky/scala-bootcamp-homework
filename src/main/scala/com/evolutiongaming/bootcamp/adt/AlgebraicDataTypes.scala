package com.evolutiongaming.bootcamp.adt

object AlgebraicDataTypes {
  // Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
  // task you completed to join the bootcamp. Use your best judgement about particular data types to include
  // in the solution, you can model concepts like:
  //
  // 1. Suit
  // 2. Rank
  // 3. Card
  // 4. Hand (Texas or Omaha)
  // 5. Board
  // 6. Poker Combination (High Card, Pair, etc.)
  // 7. Test Case (Board & Hands to rank)
  // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
  //
  // Make sure the defined model protects against invalid data. Use value classes and smart constructors as
  // appropriate. Place the solution under `adt` package in your homework repository.
  final case class ErrorMessage(value: String) extends AnyVal

  sealed trait Suit
  object Suit {
    case object Clubs extends Suit
    case object Diamonds extends Suit
    case object Hearts extends Suit
    case object Spades extends Suit

    private val suits = Map(
      'c' -> Clubs,
      'd' -> Diamonds,
      'h' -> Hearts,
      's' -> Spades
    )

    def of(suit: Char): Either[ErrorMessage, Suit] =
      suits.get(suit).toRight(ErrorMessage(s"Invalid Suit '$suit'"))
  }

  sealed abstract case class Rank private (worth: Int)
  object Rank {
    object Two extends Rank(0)
    object Three extends Rank(1)
    object Four extends Rank(2)
    object Five extends Rank(3)
    object Six extends Rank(4)
    object Seven extends Rank(5)
    object Eight extends Rank(6)
    object Nine extends Rank(7)
    object Ten extends Rank(8)
    object Jack extends Rank(9)
    object Queen extends Rank(10)
    object King extends Rank(11)
    object Ace extends Rank(12)

    private val ranks = Map(
      '2' -> Two,
      '3' -> Three,
      '4' -> Four,
      '5' -> Five,
      '6' -> Six,
      '7' -> Seven,
      '8' -> Eight,
      '9' -> Nine,
      'T' -> Ten,
      'J' -> Jack,
      'Q' -> Queen,
      'K' -> King,
      'A' -> Ace
    )

    def of(rank: Char): Either[ErrorMessage, Rank] =
      ranks.get(rank).toRight(ErrorMessage(s"Invalid Rank '$rank'"))
  }

  sealed abstract case class Card private (rank: Rank, suit: Suit)
  object Card {
    def of(card: String): Either[ErrorMessage, Card] = card.toList match {
      case rank :: suit :: Nil => of(rank, suit)
      case _                   => Left(ErrorMessage(s"Invalid Card '$card'"))
    }

    def of(rank: Char, suit: Char): Either[ErrorMessage, Card] = for {
      verifiedRank <- Rank.of(rank)
      verifiedSuit <- Suit.of(suit)
    } yield new Card(verifiedRank, verifiedSuit) {}
  }

  sealed trait Hand {
    def cards: List[Card]
  }
  object Hand {
    sealed abstract case class HoldemHand(cards: List[Card]) extends Hand
    sealed abstract case class OmahaHand(cards: List[Card]) extends Hand
    def of(cards: String): Either[ErrorMessage, Hand] = of(cards.grouped(2).toList)

    def of(cards: List[String]): Either[ErrorMessage, Hand] = cards.partitionMap(Card.of) match {
      case (Nil, verifiedCards) => of(verifiedCards)
      case _                    => Left(ErrorMessage(s"Invalid Hand '$cards'"))
    }

    def of(cards: => List[Card]): Either[ErrorMessage, Hand] = cards.size match {
      case 2 => Right(new HoldemHand(cards) {})
      case 4 => Right(new OmahaHand(cards) {})
      case _ => Left(ErrorMessage(s"Invalid Hand Size '$cards.size"))
    }
  }

  sealed abstract case class Board private (cards: List[Card])
  object Board {
    def of(cards: String): Either[ErrorMessage, Board] = of(cards.grouped(2).toList)

    def of(cards: List[String]): Either[ErrorMessage, Board] = cards.partitionMap(Card.of) match {
      case (Nil, verifiedCards) => of(verifiedCards)
      case _                    => Left(ErrorMessage(s"Invalid Board '$cards'"))
    }

    def of(cards: => List[Card]): Either[ErrorMessage, Board] =
      Either.cond(cards.size == 5, new Board(cards) {}, ErrorMessage(s"Invalid Board Size '${cards.size}'"))
  }

  sealed trait PokerCombination {
    def worth: Int
  }
  object PokerCombination {
    sealed abstract case class HighCard(worth: Int = 0, kickers: List[Rank]) extends PokerCombination
    sealed abstract case class Pair(worth: Int = 1, pair: Rank, kickers: List[Rank]) extends PokerCombination
    sealed abstract case class TwoPairs(worth: Int = 2, pairs: List[Rank], kicker: Rank) extends PokerCombination
    sealed abstract case class ThreeOfAKind(worth: Int = 3, triplet: Rank, kickers: List[Rank]) extends PokerCombination
    sealed abstract case class Straight(worth: Int = 4, highest: Rank) extends PokerCombination
    sealed abstract case class Flush(worth: Int = 5, ranks: List[Rank]) extends PokerCombination
    sealed abstract case class FullHouse(worth: Int = 6, triplet: Rank, pair: Rank) extends PokerCombination
    sealed abstract case class FourOfAKind(worth: Int = 7, quartet: Rank, kicker: Rank) extends PokerCombination
    sealed abstract case class StraightFlush(worth: Int = 8, highest: Rank) extends PokerCombination
  }

  sealed trait TestCase
  object TestCase {
    sealed abstract case class TexasCase(board: Board, hands: List[Hand]) extends TestCase
    sealed abstract case class OmahaCase(board: Board, hands: List[Hand]) extends TestCase
  }

  sealed abstract case class TestResult private (sortedHands: List[(Hand, PokerCombination)])
}
