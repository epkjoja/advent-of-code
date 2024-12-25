package se.joja.aoc.year2023

import se.joja.aoc.readInput
import se.joja.aoc.util.{Area, Point}

import scala.annotation.tailrec

object Day21 extends App {

  val input = readInput(2023, 21)

  case class Tile(p: Point, char: Char)

  val tiles = input.zipWithIndex.flatMap { case (l, x) =>
    l.zipWithIndex.map { case (c, y) => Tile(Point(x, y), c)}
  }
  val start = tiles.filter(_.char == 'S').head.p
  val map = Area(tiles.map(t => t.p -> (if (t.char == 'S') '.' else t.char)), '-')

  @tailrec
  def takeSteps(places: List[Point], stepsLeft: Int): List[Point] = {
    if (stepsLeft == 0) places else {
      val newPlaces = places.flatMap { p =>
        p.neighbours4.filter(n => map.isValid(n, _ == '.'))
      }.distinct
      takeSteps(newPlaces, stepsLeft - 1)
    }
  }
  val result1 = takeSteps(List(start), 64).length

  println(s"Result part 1: $result1")

  // Part 2

  val result2 = ""

  println(s"Result part 2: $result2")
}

