package se.joja.aoc.year2021

import se.joja.joja.getInput

object Day1 extends App {

  val input = getInput("2021/day1.txt").map(_.toInt)

  println(input)

  val (result1, _) = input.foldLeft((0, Option.empty[Int])) {
    case ((acc, maybePrev), curr) =>
      maybePrev match {
        case Some(prev) if curr > prev => (acc + 1, Some(curr))
        case _                         => (acc, Some(curr))
      }
  }

  println(s"Result part 1: $result1")

  // Part 2

  val (result2, _) = input.sliding(3).map(_.sum).foldLeft((0, Option.empty[Int])) {
    case ((acc, maybePrev), curr) =>
      maybePrev match {
        case Some(prev) if curr > prev => (acc + 1, Some(curr))
        case _                         => (acc, Some(curr))
      }
  }

  println(s"Result part 2: $result2")
}
