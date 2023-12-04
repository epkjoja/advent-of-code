package se.joja.aoc.year2023

import se.joja.aoc.getInput

object Day4 extends App {

  val input = getInput("2023/day4.txt")

  case class Card(num: Int, winning: List[Int], having: List[Int]) {
    def numMatching: Int = having.count(winning.contains)

    def points: Int = {
      val count = numMatching
      if (count == 0) 0 else scala.math.pow(2, count - 1).toInt
    }
  }

  val CardRegex = """Card +(\d+):([\d ]+)\|([\d ]+)""".r

  val cards = input.map {
    case CardRegex(cardNum, winning, own) =>
      Card(
        cardNum.toInt,
        winning.split(' ').filter(_.nonEmpty).map(_.toInt).toList,
        own.split(' ').filter(_.nonEmpty).map(_.toInt).toList
      )
  }

  val result1 = cards.map(_.points).sum
  println(s"Result part 1: ${result1}")

  // Part 2

  val cardMap = cards.map(c => c.num -> c).toMap
  val cardMax = cards.map(_.num).max

  def processCards(stack: Map[Int, Int]): Map[Int, Int] = {
    if (stack.isEmpty) Map.empty else {
      val currCardNum = stack.keys.min
      val currCardCount = stack(currCardNum)
      println(s"Processing: ${currCardNum}, $currCardCount")
      val p = cardMap.get(currCardNum).map(_.numMatching).getOrElse(0)
      val newCards = (currCardNum + 1).until(currCardNum + 1 + p).flatMap {
        case c if c <= cardMax => Some(c -> (stack(c) + currCardCount))
        case _ => None
      }.toMap
      println(s"p: $p, adding: $newCards")
      val newStack = (stack - currCardNum) ++ newCards
      Map(currCardNum -> currCardCount) ++ processCards(newStack)
    }
  }

  val result2 = processCards(cardMap.keys.map(c => c -> 1).toMap).values.sum

  println(s"Result part 2: ${result2}")
}

