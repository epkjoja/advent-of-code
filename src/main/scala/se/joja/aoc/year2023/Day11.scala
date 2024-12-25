package se.joja.aoc.year2023

import se.joja.aoc.getInput
import se.joja.aoc.util.Point

import scala.annotation.tailrec

object Day11 extends App {

  val input = getInput("2023/day11.txt")

  case class Point(x: Long, y: Long) {
    def dist(that: Point): Long =
      (x - that.x).abs + (y - that.y).abs
  }

  val image = input.zipWithIndex.flatMap { case (line, x) =>
    line.zipWithIndex.flatMap { case (c, y) => if (c == '#') Some(Point(x, y)) else None }
  }

  val maxX: Long = input.length - 1
  val maxY: Long = input.head.length - 1

  val filled = image.foldLeft((Set.empty[Long], Set.empty[Long])) { case ((xSet, ySet), p) => (xSet + p.x, ySet + p.y)}
  val missingX = (0L.to(maxX).toSet -- filled._1).toList.sorted
  val missingY = (0L.to(maxY).toSet -- filled._2).toList.sorted

  val transXMap = missingX.prepended(0L).appended(maxX).sliding(2).zipWithIndex.flatMap { case (a :: b :: _, i) =>
    a.to(b).map(x => x -> i)
  }.toMap
  val transYMap = missingY.prepended(0L).appended(maxY).sliding(2).zipWithIndex.flatMap { case (a :: b :: _, i) =>
    a.to(b).map(y => y -> i)
  }.toMap

  val expanded = image.map(p => Point(p.x + transXMap(p.x), p.y + transYMap(p.y)))

  val dists = expanded.combinations(2).map { case (p1 :: p2 :: _) => p1.dist(p2) }.toList

  val result1 = dists.sum

  println(s"Result part 1: $result1")

  // Part 2

  val expanded2 = image.map { p =>
    Point(p.x + transXMap(p.x) * 1000000 - transXMap(p.x), p.y + transYMap(p.y) * 1000000 - transYMap(p.y))
  }

  val dists2 = expanded2.combinations(2).map { case (p1 :: p2 :: _) => p1.dist(p2) }.toList

  val result2 = dists2.sum

  println(s"Result part 2: $result2")
}

