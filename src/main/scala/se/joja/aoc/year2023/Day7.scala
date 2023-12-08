package se.joja.aoc.year2023

import se.joja.aoc.getInput

object Day7 extends App {

  val input = getInput("2023/day7.txt")

  val cardNums = "23456789TJQKA".toCharArray
  val cardNums2 = "J23456789TQKA".toCharArray

  sealed abstract class HandType(val strength: Int)
  case object HighCard extends HandType(0)
  case object OnePair extends HandType(1)
  case object TwoPair extends HandType(2)
  case object ThreeOfAKind extends HandType(3)
  case object FullHouse extends HandType(4)
  case object FourOfAKind extends HandType(5)
  case object FiveOfAKind extends HandType(6)

  case class Hand(cards: String, bid: Int, typ: HandType, strength: Long) {}

  object Hand {
    def init(cardStr: String, bid: Int): Hand = {
      val counts = cardStr.toList.groupBy(identity).map(c => c._1 -> c._2.size).toList.sortBy(_._2).reverse
      println(s"Counts: $counts")
      counts match {
        case (_, 5) :: Nil => new Hand(cardStr, bid, FiveOfAKind, handStrength(FiveOfAKind, cardStr))
        case (_, 4) :: _ => new Hand(cardStr, bid, FourOfAKind, handStrength(FourOfAKind, cardStr))
        case (_, 3) :: (_, 2) :: _ => new Hand(cardStr, bid, FullHouse, handStrength(FullHouse, cardStr))
        case (_, 3) :: _ => new Hand(cardStr, bid, ThreeOfAKind, handStrength(ThreeOfAKind, cardStr))
        case (_, 2) :: (_, 2) :: _ => new Hand(cardStr, bid, TwoPair, handStrength(TwoPair, cardStr))
        case (_, 2) :: _ => new Hand(cardStr, bid, OnePair, handStrength(OnePair, cardStr))
        case _ => new Hand(cardStr, bid, HighCard, handStrength(HighCard, cardStr))
      }
    }

    def init2(cardStr: String, bid: Int): Hand = {
      val counts = cardStr.toList.groupBy(identity).map(c => c._1 -> c._2.size).toList.sortBy(_._2).reverse
      val (nonJokers, joker) = counts.partition(_._1 != 'J')
      println(s"Non jokers: $nonJokers, Jokers: $joker")

      val (maxChar, maxCount) = nonJokers.headOption.getOrElse(('A', 0))
      val jokerCount = joker.headOption.map(_._2).getOrElse(0)
      val maxedCounts = maxChar -> (maxCount + jokerCount) :: nonJokers.drop(1)
      println(s"Maxed counts: $maxedCounts")
      maxedCounts match {
        case (_, 5) :: Nil => new Hand(cardStr, bid, FiveOfAKind, handStrength2(FiveOfAKind, cardStr))
        case (_, 4) :: _ => new Hand(cardStr, bid, FourOfAKind, handStrength2(FourOfAKind, cardStr))
        case (_, 3) :: (_, 2) :: _ => new Hand(cardStr, bid, FullHouse, handStrength2(FullHouse, cardStr))
        case (_, 3) :: _ => new Hand(cardStr, bid, ThreeOfAKind, handStrength2(ThreeOfAKind, cardStr))
        case (_, 2) :: (_, 2) :: _ => new Hand(cardStr, bid, TwoPair, handStrength2(TwoPair, cardStr))
        case (_, 2) :: _ => new Hand(cardStr, bid, OnePair, handStrength2(OnePair, cardStr))
        case _ => new Hand(cardStr, bid, HighCard, handStrength2(HighCard, cardStr))
      }

    }

    private def handStrength(typ: HandType, cards: String): Long = {
      val nums = cards.reverse.zipWithIndex.map { case (c, i) =>
        cardNums.indexOf(c) * math.pow(10, i * 2).toLong
      }
      println(s"Cards: $cards, Nums: $nums")
      nums.sum + typ.strength * 10000000000L
    }

    private def handStrength2(typ: HandType, cards: String): Long = {
      val nums = cards.reverse.zipWithIndex.map { case (c, i) =>
        cardNums2.indexOf(c) * math.pow(10, i * 2).toLong
      }
      println(s"Cards: $cards, Nums: $nums")
      nums.sum + typ.strength * 10000000000L
    }
  }

  val hands = input.map { s =>
    val handRaw :: bidRaw :: Nil = s.split(' ').toList
    Hand.init(handRaw, bidRaw.toInt)
  }

  val result1 = hands.sortBy(_.strength).zipWithIndex.map { case (hand, i) =>
    hand.bid * (i + 1)
  }.sum

  println(s"Result part 1: ${result1}")

  // Part 2

  val hands2 = input.map { s =>
    val handRaw :: bidRaw :: Nil = s.split(' ').toList
    Hand.init2(handRaw, bidRaw.toInt)
  }

  val result2 = hands2.sortBy(_.strength).zipWithIndex.map { case (hand, i) =>
    hand.bid * (i + 1)
  }.sum

  println(s"Result part 2: ${result2}")
}

