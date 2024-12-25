package se.joja.aoc.year2024

import se.joja.aoc.util.Point
import se.joja.aoc.{readInput, splitOnEmptyLine}

object Day13 extends App {

  val ButtonPattern = """Button ([AB]): X\+(\d+), Y\+(\d+)""".r
  val PrizePattern = """Prize: X=(\d+), Y=(\d+)""".r

  val empty = Point(-1, -1)
  case class Machine(buttA: Point = empty, buttB: Point = empty, prize: Point = empty) {
//    def tokens(): Option[Int] = {
//      (prize.x % )
//    }
  }

  val input = splitOnEmptyLine(readInput(2024, 13, true)).map { case part =>
    part.foldLeft(Machine()) {
      case (acc, ButtonPattern("A", x, y)) => acc.copy(buttA = Point(x.toInt, y.toInt))
      case (acc, ButtonPattern("B", x, y)) => acc.copy(buttB = Point(x.toInt, y.toInt))
      case (acc, PrizePattern(x, y)) => acc.copy(prize = Point(x.toInt, y.toInt))
    }
  }

  val result1 = input

  println(s"Result part 1: $result1")

  // Part 2

  val result2 = ""

  println(s"Result part 2: $result2")
}

