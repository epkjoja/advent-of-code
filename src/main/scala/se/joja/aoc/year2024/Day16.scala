package se.joja.aoc.year2024

import se.joja.aoc.readInput
import se.joja.aoc.util.Area

object Day16 extends App {

  val input = Area(readInput(2024, 16, true).map(_.toVector).toVector)

  val start = input.filter(_ == 'S').head._1 -> '>'
  val result1 = ""

  println(s"Result part 1: $result1")

  // Part 2

  val result2 = ""

  println(s"Result part 2: $result2")
}

