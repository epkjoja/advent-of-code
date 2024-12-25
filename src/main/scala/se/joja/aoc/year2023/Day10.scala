package se.joja.aoc.year2023

import se.joja.aoc.getInput
import se.joja.aoc.util.Point

import scala.annotation.tailrec

object Day10 extends App {

  val input = getInput("2023/day10.txt")

  val ConnRight = Seq('-', 'L', 'F')
  val ConnLeft = Seq('-', 'J', '7')
  val ConnUp = Seq('|', 'L', 'J')
  val ConnDown = Seq('|', '7', 'F')

  case class P(p: Point, c: Char) {
    def isConnected(other: P): Boolean = {
      (other.p.x - p.x, other.p.y - p.y) match {
        case (0, 1) if ConnRight.appended('S').contains(c) && ConnLeft.contains(other.c) => true
        case (0, -1) if ConnLeft.appended('S').contains(c) && ConnRight.contains(other.c) => true
        case (1, 0) if ConnDown.appended('S').contains(c) && ConnUp.contains(other.c) => true
        case (-1, 0) if ConnUp.appended('S').contains(c) && ConnDown.contains(other.c) => true
        case _ => false
      }
    }
  }

  val pipes = input.zipWithIndex.map { case (line, x) =>
    line.zipWithIndex.map { case (c, y) => P(Point(x, y), c) }.toList
  }

  val maxX = pipes.length - 1
  val maxY = pipes.head.length - 1

  val start = pipes.flatMap(l => l.find(_.c == 'S')).head

  def getP(point: Point): Option[P] = {
    if (point.x < 0 || point.x > maxX || point.y < 0 || point.y > maxY) None else {
      val p = pipes(point.x)(point.y)
      if (p.c == '.') None else Some(p)
    }
  }

  def findConnected(p: P): List[P] =
    p.p.neighbours4.flatMap(getP).filter(p.isConnected)

  @tailrec
  def followPipe(path: List[P]): List[P] = {
    val next = findConnected(path.last).filterNot(path.contains)
    if (next.isEmpty) path else {
      followPipe(path ++ next)
    }
  }

  val pipe = followPipe(List(start, findConnected(start).head))
  val result1 = pipe.length / 2

  println(s"Result part 1: ${result1}")

  // Part 2

  def enclosedOnRow(row: List[P]): Int = {
//    println(row)
    val (sum, _, _) = 0.to(maxY).foldLeft(0, false, Option.empty[Char]) { case ((sum, doCount, startChar), y) =>
      val p = row.find(_.p.y == y)
//      println(s"p: $p, doCount: $doCount, startChar: $startChar")
      (p, doCount, startChar) match {
        case (Some(P(_, '|')), _, _) => (sum, !doCount, None)
        case (Some(P(_, c)), _, _) if Seq('L', 'F').contains(c) => (sum, doCount, Some(c))
        case (Some(P(_, '-')), _, s)  => (sum, doCount, s)
        case (Some(P(_, 'J')), _, Some('F'))  => (sum, !doCount, None)
        case (Some(P(_, '7')), _, Some('L'))  => (sum, !doCount, None)
        case (None, true, _) => (sum + 1, doCount, None)
        case _ => (sum, doCount, None)
      }
    }
    sum
  }

  val enclosed = 0.to(maxX).map { rowNum =>
    enclosedOnRow(pipe.filter(_.p.x == rowNum))
  }.toList

  val result2 = enclosed.sum

  println(s"Result part 2: ${result2}")
}

