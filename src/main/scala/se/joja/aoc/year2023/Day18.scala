package se.joja.aoc.year2023

import se.joja.aoc.readInput
import se.joja.aoc.util.{Area, Point}

object Day18 extends App {

  val input = readInput(2023, 18, true)

  val CmdRegex = """(\w) (\d+) \(#([0-9a-f]+)\)""".r

  val digPoints = input.foldLeft(List(Point(0, 0))) { case (acc, l) =>
    val p@Point(x, y) = acc.last
    val newDig = l match {
      case CmdRegex("U", dist, color) => p.line(Point(x - dist.toInt, y))
      case CmdRegex("D", dist, color) => p.line(Point(x + dist.toInt, y))
      case CmdRegex("L", dist, color) => p.line(Point(x, y - dist.toInt))
      case CmdRegex("R", dist, color) => p.line(Point(x, y + dist.toInt))
    }
    acc.init ++ newDig
  }

  val result1 = Area(digPoints.map(_ -> '#'), '.').fillInside(_ == '#', '#') //.count(_ == '#')

  println(s"Result part 1: $result1")

  // Part 2

  val result2 = ""

  println(s"Result part 2: $result2")
}

