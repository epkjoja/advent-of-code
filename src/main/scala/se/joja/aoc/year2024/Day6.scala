package se.joja.aoc.year2024

import se.joja.aoc.readInput
import se.joja.aoc.util.{Area, Point}

import scala.annotation.tailrec

object Day6 extends App {

  val input = readInput(2024, 6).map(_.toCharArray.toVector).toVector

  val area = Area(input)

  val start = area.filter(_ == '^').head._1

  val turn = Map('N' -> 'E', 'E' -> 'S', 'S' -> 'W', 'W' -> 'N')

  @tailrec
  def visitedPoints(p: Point, dir: Char, acc: Set[Point]): Set[Point] = {
    val tmp = p.move(dir)

    val newPoint = if (!area.isInside(tmp)) None
    else if (area(tmp) == '#') Some(p.move(turn(dir)) -> turn(dir))
    else Some(tmp -> dir)

    newPoint match {
      case None => acc
      case Some((point, dir)) => visitedPoints(point, dir, acc + point)
    }
  }

  val visited = visitedPoints(start, 'N', Set(start))

  val result1 = visited.size

  println(s"Result part 1: $result1")

  // Part 2

  case class DirPoint(p: Point, dir: Char)

//  @tailrec
//  def visitedPoints2(dp: DirPoint, acc: Set[DirPoint]): Set[DirPoint] = {
//    val tmp = p.move(dir)
//
//    val newPoint = if (!area.isInside(tmp)) None
//    else if (area(tmp) == '#') Some(p.move(turn(dir)) -> turn(dir))
//    else Some(tmp -> dir)
//
//    newPoint match {
//      case None => acc
//      case Some((point, dir)) => visitedPoints(point, dir, acc + point)
//    }
//  }

  val result2 = ""

  println(s"Result part 2: $result2")
}

