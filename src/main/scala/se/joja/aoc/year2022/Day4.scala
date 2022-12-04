package se.joja.aoc.year2022

import se.joja.aoc.getInput

object Day4 extends App {

  val input = getInput("2022/day4.txt")

  case class Area(min: Int, max: Int) {
    def overlapAllOf(other: Area): Boolean =
      min <= other.min && max >= other.max

    def overlapNoneOf(other: Area): Boolean =
      max < other.min || min > other.max

    def overlapSomeOf(other: Area): Boolean =
      !overlapNoneOf(other)
  }

  val areaPairs = input.map { l =>
    val first :: second :: Nil = l.split(',').toList
    val aMin :: aMax :: Nil = first.split('-').toList
    val bMin :: bMax :: Nil = second.split('-').toList
    Area(aMin.toInt, aMax.toInt) -> Area(bMin.toInt, bMax.toInt)
  }

  val result1 = areaPairs.count { case (a1, a2) =>
    a1.overlapAllOf(a2) || a2.overlapAllOf(a1)
  }

  println(s"Result part 1: $result1")

  // Part 2

  val result2 = areaPairs.count(a => a._1.overlapSomeOf(a._2))

  println(s"Result part 2: $result2")
}
