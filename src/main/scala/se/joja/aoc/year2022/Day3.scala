package se.joja.aoc.year2022

import se.joja.aoc.getInput

object Day3 extends App {

  val input = getInput("2022/day3.txt").map(_.map(convertChar).toList)

  def convertChar(char: Char): Int =
    char match {
      case c if c >= 'a' && c <= 'z' => c.toInt - 96
      case c if c >= 'A' && c <= 'Z' => c.toInt - 38
      case other => throw new RuntimeException(s"Char not in valid range: $other")
    }

  val result1 = input.flatMap { rugsack =>
    val (a, b) = rugsack.splitAt(rugsack.length / 2)
    a.find(b.contains)
  }.sum

  println(s"Result part 1: $result1")

  // Part 2

  val result2 = input.grouped(3).flatMap { case a :: b :: c :: Nil =>
    a.find(x => b.contains(x) && c.contains(x))
  }.sum

  println(s"Result part 2: $result2")
}
