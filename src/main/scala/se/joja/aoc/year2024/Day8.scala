package se.joja.aoc.year2024

import se.joja.aoc.readInput
import se.joja.aoc.util.{Area, Point}

object Day8 extends App {

  val input = readInput(2024, 8).map(_.toVector).toVector
  val area = Area(input)

  val antMap = area.filter(_ != '.').groupMap(_._2)(_._1)

  val j = antMap.flatMap { case (_, points) =>
    points.combinations(2).flatMap { case p1 :: p2 :: Nil =>
      calcAntinodes(p1, p2)
    }
  }.toSet

  def calcAntinodes(p1: Point, p2: Point): Set[Point] = {
    val p1a = Point(p2.x - (p1.x - p2.x), p2.y - (p1.y - p2.y))
    val p2a = Point(p1.x - (p2.x - p1.x), p1.y - (p2.y - p1.y))
    Set(p1a, p2a).filter(area.isInside)
  }

  val result1 = j.size

  println(s"Result part 1: $result1")

  // Part 2

  val j2 = antMap.flatMap { case (_, points) =>
    points.combinations(2).flatMap { case p1 :: p2 :: Nil =>
      calcAntinodes2(p1, p2)
    }
  }.toSet

  def calcAntinodes2(p1: Point, p2: Point): Set[Point] = {
    val p1a = oneRec(p2, p1.diff(p2))
    val p2a = oneRec(p1, p2.diff(p1))
    Set(p1a, p2a).flatten
  }

  def oneRec(p: Point, diff: (Int, Int)): Set[Point] = {
    oneAntinode(p, diff) match {
      case None => Set.empty
      case Some(p2) => Set(p2) ++ oneRec(p2, diff)
    }
  }

  def oneAntinode(p: Point, diff: (Int, Int)): Option[Point] = {
    val newPoint = Point(p.x - diff._1, p.y - diff._2)
    Some(newPoint).filter(area.isInside)
  }

  val result2 = (j2 ++ antMap.flatMap(_._2).toSet).size

  println(s"Result part 2: $result2")
}

