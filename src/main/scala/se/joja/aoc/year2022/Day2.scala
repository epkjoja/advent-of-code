package se.joja.aoc.year2022

import se.joja.aoc.getInput

object Day2 extends App {

  val Lose = 0
  val Draw = 3
  val Win = 6

  val RegEx = "([ABC]) ([XYZ])".r
  val input = getInput("2022/day2.txt").map {
    case RegEx(a, b) => a -> b
  }
  println(input)

  val drawScore = input.map {
    case ("A", "X") => 1 + Draw
    case ("A", "Y") => 2 + Win
    case ("A", "Z") => 3 + Lose
    case ("B", "X") => 1 + Lose
    case ("B", "Y") => 2 + Draw
    case ("B", "Z") => 3 + Win
    case ("C", "X") => 1 + Win
    case ("C", "Y") => 2 + Lose
    case ("C", "Z") => 3 + Draw
  }

  val result1 = drawScore.sum

  println(s"Result part 1: $result1")

  // Part 2

  val drawScore2 = input.map {
    case ("A", "X") => 3 + Lose
    case ("A", "Y") => 1 + Draw
    case ("A", "Z") => 2 + Win
    case ("B", "X") => 1 + Lose
    case ("B", "Y") => 2 + Draw
    case ("B", "Z") => 3 + Win
    case ("C", "X") => 2 + Lose
    case ("C", "Y") => 3 + Draw
    case ("C", "Z") => 1 + Win
  }

  val result2 = drawScore2.sum

  println(s"Result part 2: $result2")
}
