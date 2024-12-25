package se.joja.aoc.year2023

import se.joja.aoc.readInput
import se.joja.aoc.util.{Area, Point}

object Day22 extends App {

  val input = readInput(2023, 22, true)

  case class Brick(id: Int, a: Area[Char], bottom: Int, h: Int)
  val BrickRowRegex = """(\d+),(\d+),(\d+)~(\d+),(\d+),(\d+)""".r

  val bricks = input.zipWithIndex.map {
    case (BrickRowRegex(x1, y1, z1, x2, y2, z2), id) =>
      val (p1, p2) = (Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
      val height = z2.toInt - z1.toInt + 1
      Brick(id, Area(p1, p2, 'B', ' '), z1.toInt, height)
  }

  val result1 = bricks

  println(s"Result part 1: $result1")

  // Part 2

  val result2 = ""

  println(s"Result part 2: $result2")
}

