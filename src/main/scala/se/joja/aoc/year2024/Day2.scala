package se.joja.aoc.year2024

import se.joja.aoc.readInput

case class Report(levels: List[Int]) {
  def isSafe: Boolean = {
    val diffs = levels.sliding(2).map { case a :: b :: Nil => a - b }.toList
    val numPos = diffs.count(d => d > 0)
    val steps = diffs.forall(d => Math.abs(d) >= 1 && Math.abs(d) <= 3)
    steps && (numPos == 0 || numPos == diffs.size)
  }

  def isSafe2: Boolean = {
    if (isSafe) true else {
      val alts = levels.indices.map { i =>
        Report(levels.take(i) ++ levels.takeRight(levels.size - 1 - i))
      }.toList
      alts.exists(_.isSafe)
    }
  }
}

object Day2 extends App {

  val input = readInput(2024, 2)

  val reports = input.map(l => Report(l.split(' ').map(_.toInt).toList))
  val safeReports = reports.map(_.isSafe)

  val result1 = safeReports.count(identity)

  println(s"Result part 1: $result1")

  // Part 2

  val result2 = reports.map(_.isSafe2).count(identity)

  println(s"Result part 2: $result2")
}

