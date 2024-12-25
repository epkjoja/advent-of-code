package se.joja.aoc.year2024

import se.joja.aoc.readInput
import se.joja.aoc.util.Point

import scala.annotation.tailrec

object Day4 extends App {

  val allDirs = List("N", "NE", "E", "SE", "S", "SW", "W", "NW")

  val input = readInput(2024, 4).map(_.toCharArray).toArray
  val (xMax, yMax) = (input.head.length - 1, input.length - 1)

  def charAt(p: Point): Char = input(p.y)(p.x)

  def wordsAtPoint(p: Point, word: String = "XMAS"): Int = {
    def pointValid(p: Point): Boolean = p.x >= 0 && p.x <= xMax && p.y >= 0 && p.y <= yMax

    @tailrec
    def isWord(p: Point, dir: String, word: String): Boolean = {
      if (!pointValid(p) || word.isEmpty || charAt(p) != word.head)
        false
      else if (word.length == 1)
        true
      else
        isWord(p.move(dir), dir, word.tail)
    }

    if (charAt(p) != word.head) 0 else allDirs.count(dir => isWord(p.move(dir), dir, word.tail))
  }

  val allPoints = for {
    x <- 0.to(xMax)
    y <- 0.to(yMax)
  } yield Point(x, y)

  val found = allPoints.toList.map(wordsAtPoint(_))

  val result1 = found.sum

  println(s"Result part 1: $result1")

  // Part 2

  def xmasAtPoint(p: Point): Boolean = {
    if (charAt(p) != 'A')
      false
    else {
      val a = List(charAt(p.move("NW")), charAt(p.move("SE")))
      val b = List(charAt(p.move("NE")), charAt(p.move("SW")))
      a.contains('M') && a.contains('S') && b.contains('M') && b.contains('S')
    }
  }

  val allPoints2 = for {
    x <- 1.until(xMax)
    y <- 1.until(yMax)
  } yield Point(x, y)

  val result2 = allPoints2.toList.count(xmasAtPoint)

  println(s"Result part 2: $result2")
}

