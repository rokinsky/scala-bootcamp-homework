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
  sealed abstract class ErrorMessage private (val value: String)
  object ErrorMessage {
    final case class InvalidSuit(suit: Char) extends ErrorMessage(s"Invalid Suit '$suit'")
    final case class InvalidRank(rank: Char) extends ErrorMessage(s"Invalid Rank '$rank'")
    final case class InvalidCard(card: String) extends ErrorMessage(s"Invalid Card '$card'")
    final case class InvalidBoard(board: List[String]) extends ErrorMessage(s"Invalid Board '${board.mkString}'")
    final case class InvalidBoardSize(size: Int) extends ErrorMessage(s"Invalid Board Size '$size'")
    final case class InvalidHand(hand: List[String]) extends ErrorMessage(s"Invalid Hand '${hand.mkString}'")
    final case class InvalidHandSize(size: Int) extends ErrorMessage(s"Invalid Card '$size'")
  }

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
      suits.get(suit).toRight(ErrorMessage.InvalidSuit(suit))
  }

  sealed abstract class Rank private (val worth: Int)
  object Rank {
    case object Two extends Rank(0)
    case object Three extends Rank(1)
    case object Four extends Rank(2)
    case object Five extends Rank(3)
    case object Six extends Rank(4)
    case object Seven extends Rank(5)
    case object Eight extends Rank(6)
    case object Nine extends Rank(7)
    case object Ten extends Rank(8)
    case object Jack extends Rank(9)
    case object Queen extends Rank(10)
    case object King extends Rank(11)
    case object Ace extends Rank(12)

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
      ranks.get(rank).toRight(ErrorMessage.InvalidRank(rank))
  }

  sealed abstract case class Card private (rank: Rank, suit: Suit)
  object Card {
    def of(card: String): Either[ErrorMessage, Card] = card.toList match {
      case rank :: suit :: Nil => of(rank, suit)
      case _                   => Left(ErrorMessage.InvalidCard(card))
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
    sealed abstract case class HoldemHand protected (cards: List[Card]) extends Hand
    sealed abstract case class OmahaHand protected (cards: List[Card]) extends Hand
    def of(cards: String): Either[ErrorMessage, Hand] = of(cards.grouped(2).toList)

    def of(cards: List[String]): Either[ErrorMessage, Hand] = cards.partitionMap(Card.of) match {
      case (Nil, verifiedCards) => of(verifiedCards)
      case _                    => Left(ErrorMessage.InvalidHand(cards))
    }

    def of(cards: => List[Card]): Either[ErrorMessage, Hand] = cards.size match {
      case 2 => Right(new HoldemHand(cards) {})
      case 4 => Right(new OmahaHand(cards) {})
      case _ => Left(ErrorMessage.InvalidHandSize(cards.size))
    }
  }

  sealed abstract case class Board private (cards: List[Card])
  object Board {
    def of(cards: String): Either[ErrorMessage, Board] = of(cards.grouped(2).toList)

    def of(cards: List[String]): Either[ErrorMessage, Board] = cards.partitionMap(Card.of) match {
      case (Nil, verifiedCards) => of(verifiedCards)
      case _                    => Left(ErrorMessage.InvalidBoard(cards))
    }

    def of(cards: => List[Card]): Either[ErrorMessage, Board] =
      Either.cond(cards.size == 5, new Board(cards) {}, ErrorMessage.InvalidBoardSize(cards.size))
  }

  sealed abstract class PokerCombination(val worth: Int)
  object PokerCombination {
    sealed abstract case class HighCard protected (kickers: List[Rank]) extends PokerCombination(0)
    sealed abstract case class Pair protected (pair: Rank, kickers: List[Rank]) extends PokerCombination(1)
    sealed abstract case class TwoPairs protected (pairs: List[Rank], kicker: Rank) extends PokerCombination(2)
    sealed abstract case class ThreeOfAKind protected (triplet: Rank, kickers: List[Rank]) extends PokerCombination(3)
    sealed abstract case class Straight protected (highest: Rank) extends PokerCombination(4)
    sealed abstract case class Flush protected (ranks: List[Rank]) extends PokerCombination(5)
    sealed abstract case class FullHouse protected (triplet: Rank, pair: Rank) extends PokerCombination(6)
    sealed abstract case class FourOfAKind protected (quartet: Rank, kicker: Rank) extends PokerCombination(7)
    sealed abstract case class StraightFlush protected (highest: Rank) extends PokerCombination(8)
  }

  sealed abstract case class TestCase private (board: Board, hands: List[Hand])

  sealed abstract case class TestResult private (sortedHands: List[(Hand, PokerCombination)])
}
