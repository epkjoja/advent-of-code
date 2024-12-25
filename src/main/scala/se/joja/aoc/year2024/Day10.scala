package se.joja.aoc.year2024

import se.joja.aoc.readInput
import se.joja.aoc.util.{Area, Point}

object Day10 extends App {

  val input = readInput(2024, 10).map(_.map(_.asDigit).toVector).toVector
  val area = Area(input)

  val theads = area.filter(_ == 0).map(_._1)

  def findEnds(point: Point): List[Point] = {
    val height = area(point)
    if (height == 9) List(point) else  {
      val nexts = point.neighbours4.filter(p => area.isInside(p) && area(p) == height + 1)
      nexts.flatMap(findEnds)
    }
  }

  val allEnds = theads.map(findEnds)

  val result1 = allEnds.map(_.distinct.size).sum

  println(s"Result part 1: $result1")

  // Part 2

  val result2 = allEnds.map(_.size).sum

  println(s"Result part 2: $result2")
}

