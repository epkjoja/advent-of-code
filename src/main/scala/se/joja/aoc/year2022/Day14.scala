package se.joja.aoc.year2022

import se.joja.aoc.getInput

import scala.annotation.tailrec

object Day14 extends App {

  val input = getInput("2022/day14.txt").map(
    line =>
      line
        .split(" -> ")
        .map { point =>
          val x :: y :: Nil = point.split(',').toList
          Point(x.toInt, y.toInt)
        }
        .toList)

  case class Point(x: Int, y: Int) {
    def line(to: Point): List[Point] = {
      val (xDir, yDir) = (to.x - this.x, to.y - this.y)

      val l = (xDir, yDir) match {
        case (x, _) if x > 0 => this.x.to(to.x).map(i => this.copy(x = i))
        case (x, _) if x < 0 => to.x.to(this.x).map(i => this.copy(x = i))
        case (_, y) if y > 0 => this.y.to(to.y).map(i => this.copy(y = i))
        case (_, y) if y < 0 => to.y.to(this.y).map(i => this.copy(y = i))
      }
      l.toList
    }
  }

  val rocks = input.flatMap { line =>
    line.sliding(2).foldLeft(List.empty[Point]) {
      case (acc, p1 :: p2 :: Nil) =>
        acc ++ p1.line(p2)
    }
  }

  val xMin = rocks.minBy(_.x).x
  val xMax = rocks.maxBy(_.x).x
  val yMax = rocks.maxBy(_.y).y

  println(s"xMin: $xMin, xMax: $xMax, yMax: $yMax")

  def contains(list: List[Point])(p: Point): Boolean = list.contains(p)

  val rockExists = contains(rocks)(_)

  for {
    y <- 0 to yMax
    x <- xMin - 1 to xMax + 1
    p = Point(x, y)
  } yield {
    val sign = if (rockExists(p)) "#" else "."
    print(sign)
    if (x > xMax) println()
  }

  def tryPour(checker: Point => Boolean)(p: Point): Option[Point] = {
    val alts = List(Point(p.x, p.y + 1), Point(p.x - 1, p.y + 1), Point(p.x + 1, p.y + 1))
    val pp   = alts.find(p => !checker(p))
    //println(s"Curr: $p, found: $pp")
    pp
  }

  def pourOneGrain(sandList: List[Point], start: Point = Point(500, 0)): Point = {
    val stuff       = rocks ++ sandList
    val stuffExists = contains(stuff)(_)
    val pourer      = tryPour(stuffExists)(_)

    @tailrec
    def pour(p: Point): Point = {
      pourer(p) match {
        case Some(newP) if newP.y > yMax => newP
        case Some(newP)                  => pour(newP)
        case None                        => p
      }
    }

    pour(start)
  }

  var sand = List.empty[Point]

  while (sand.lastOption.isEmpty || sand.lastOption.get.y <= yMax) {
    sand = sand :+ pourOneGrain(sand)
    //println(sand)
  }

  val result1 = sand.size - 1

  println(s"Result part 1: $result1")

  // Part 2

  val rocksBottom = rocks ++ 1.to(1000).map(i => Point(i, yMax + 2))

  def pourOneGrain2(sandList: List[Point], start: Point = Point(500, 0)): Point = {
    val stuff = rocksBottom ++ sandList
    val stuffExists = contains(stuff)(_)
    val pourer = tryPour(stuffExists)(_)

    @tailrec
    def pour(p: Point): Point = {
      pourer(p) match {
        case Some(newP) if newP.y > yMax + 2 => newP
        case Some(newP) => pour(newP)
        case None => p
      }
    }

    pour(start)
  }

  var sand2 = List.empty[Point]

  while (sand2.lastOption.isEmpty || sand2.last != Point(500, 0)) {
    sand2 = sand2 :+ pourOneGrain2(sand2)
  }

  val result2 = sand2.size

  println(s"Result part 2: $result2")
}
