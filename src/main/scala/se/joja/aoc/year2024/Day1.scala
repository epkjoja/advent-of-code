package se.joja.aoc.year2024

import se.joja.aoc.readInput

object Day1 extends App {

  val Pattern = """(\d+) +(\d+)""".r

  val input = readInput(2024, 1).map {
    case Pattern(l, r) => l.toInt -> r.toInt
  }

  val (left, right) = input.unzip
  val sorted = left.sorted.zip(right.sorted)

  val dists = sorted.map { case (l, r) =>
    Math.abs(l - r)
  }

  val result1 = dists.sum

  println(s"Result part 1: $result1")

  // Part 2

  val rCounts = right.groupMapReduce(identity)(_ => 1)(_ + _)

  val scores = left.map(l => l.toLong * rCounts.getOrElse(l, 0))

  val result2 = scores.sum

  println(s"Result part 2: $result2")
}

