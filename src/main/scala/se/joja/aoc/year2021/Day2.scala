package se.joja.aoc.year2021

import se.joja.aoc.getInput

object Day2 extends App {

  case class Move(dir: String, amount: Int)

  val input = getInput("2021/day2.txt").map { line =>
    line.split(' ') match {
      case Array(first, second) => Move(first, second.toInt)
    }
  }

  println(input)

  // Part 1

  val sums = input.groupMapReduce(_.dir)(_.amount)(_ + _)

  val result1 = (sums("down") - sums("up")) * sums("forward")

  println(s"Result part 1: $result1")

  // Part 2

  case class Position(horisontal: Int = 0, aim: Int = 0, depth: Int = 0) {
    def move(move: Move): Position = {
      move match {
        case Move("forward", amount) => this.copy(horisontal = horisontal + amount, depth = depth + (aim * amount))
        case Move("down", amount) => this.copy(aim = aim + amount)
        case Move("up", amount) => this.copy(aim = aim - amount)
      }
    }
  }

  val finalPos = input.foldLeft(Position()) { case (acc, move) =>
    acc.move(move)
  }

  val result2 = finalPos.horisontal * finalPos.depth
  println(s"Result part 2: $result2")

}
