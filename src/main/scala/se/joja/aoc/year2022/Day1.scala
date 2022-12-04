package se.joja.aoc.year2022

import se.joja.aoc.{getInput, splitOnEmptyLine}

object Day1 extends App {

  val input = getInput("2022/day1.txt")
  val elvFood = splitOnEmptyLine(input).map(_.map(_.toLong))

  val elvCalories = elvFood.map(_.sum)
  val result1 = elvCalories.max

  println(s"Result part 1: $result1")

  // Part 2

  val result2 = elvCalories.sorted.reverse.take(3).sum

  println(s"Result part 2: $result2")
}
