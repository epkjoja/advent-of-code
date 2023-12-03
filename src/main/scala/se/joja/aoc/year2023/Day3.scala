package se.joja.aoc.year2023

import se.joja.aoc.getInput
import se.joja.aoc.util.Point

object Day3 extends App {

  val input = getInput("2023/day3.txt")

  case class Symbol(sym: Char, p: Point)

  case class Number(n: Int, p: Point) {
    def isAdjecent(pAdj: Point): Boolean = {
      val points = 0.until(n.toString.length).map(i => p.copy(y = p.y + i))
      points.exists(pAdj.isAdjecent)
    }
  }

  val SymRegex = """[^\d.]""".r

  val symbols = input.zipWithIndex.flatMap { case (line, x) =>
    line.zipWithIndex.flatMap {
      case (sym, y) if SymRegex.matches(sym.toString) => Some(Symbol(sym, Point(x, y)))
      case _ => None
    }.toList
  }

  def posToOption(pos: Int): Option[Int] = if (pos < 0) None else Some(pos)

  def takeNums(s: String, basePos: Int = 0): List[(Int, Int)] = {
    posToOption(s.indexWhere(_.isDigit)).map(pos => pos -> s.drop(pos).takeWhile(_.isDigit).toInt) match {
      case Some((pos, num)) =>
        val newBasePos = pos + num.toString.length
        List((pos + basePos, num)) ++ takeNums(s.drop(newBasePos), newBasePos + basePos)
      case None => List.empty
    }
  }

  val numbers = input.zipWithIndex.flatMap { case (line, x) =>
    takeNums(line).map { case (y, num) => Number(num, Point(x, y)) }
  }

  val result1 = numbers.filter(num => symbols.exists(sym => num.isAdjecent(sym.p))).map(_.n.toLong).sum

  println(s"Result part 1: ${result1}")

  // Part 2

  val gearNumbers = symbols.filter(_.sym == '*').map { s =>
    numbers.filter(_.isAdjecent(s.p))
  }.filter(_.length == 2)

  val result2 = gearNumbers.map(_.map(_.n.toLong).product).sum

  println(s"Result part 2: ${result2}")
}

