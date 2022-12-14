package se.joja.aoc.year2022

import se.joja.aoc.{getInput, splitOnEmptyLine}

object Day14 extends App {

  val input = getInput("2022/day14.txt").map(line => line.split(" -> ").map { point =>
    val x :: y :: Nil = point.split(',').toList
    Point(x.toInt, y.toInt)
  }.toList)

  case class Point(x: Int, y: Int)

  input.map { line =>
    line.scanLeft()
  }

  println(input)

  //println(s"Result part 1: $result1")

  // Part 2

  //println(s"Result part 2: $result2")
}
