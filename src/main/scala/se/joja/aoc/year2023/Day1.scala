package se.joja.aoc.year2023

import se.joja.aoc.getInput

object Day1 extends App {

  val input = getInput("2023/day1.txt")

  val digits = input.map(_.filter(_.isDigit))
  val ints = digits.map(s => s"${s.head}${s.last}".toInt)
  val result1 = ints.sum

  println(s"Result part 1: ${result1}")

  // Part 2

  val words = List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

  def findFirst(s: String): Int = {
    val digitPos = s.indexWhere(_.isDigit)
    val (wordPos, word) = words.foldLeft((-1, "")) { case ((acc1, acc2), w) =>
      (acc1, s.indexOf(w)) match {
        case (-1, pos) => (pos, w)
        case (oldPos, pos) if pos >= 0 && pos < oldPos => (pos, w)
        case _ => (acc1, acc2)
      }
    }

    (digitPos, wordPos, word) match {
      case (-1, _, w) => words.indexOf(w) + 1
      case (posD, -1, _) => s(posD).asDigit
      case (posD, posW, _) if posD < posW => s(posD).asDigit
      case (_, _, w) => words.indexOf(w) + 1
    }
  }

  def findLast(s: String): Int = {
    val digitPos = s.lastIndexWhere(_.isDigit)
    val (wordPos, word) = words.foldLeft((-1, "")) { case ((acc1, acc2), w) =>
      (acc1, s.lastIndexOf(w)) match {
        case (-1, pos) => (pos, w)
        case (oldPos, pos) if pos >= 0 && pos > oldPos => (pos, w)
        case _ => (acc1, acc2)
      }
    }

    (digitPos, wordPos, word) match {
      case (-1, _, w) => words.indexOf(w) + 1
      case (posD, -1, _) => s(posD).asDigit
      case (posD, posW, _) if posD > posW => s(posD).asDigit
      case (_, _, w) => words.indexOf(w) + 1
    }
  }

  def findFirstAlt(s: String): Option[Int] = {
    val realDigit = s.headOption.filter(_.isDigit).map(_.asDigit)
    val textDigit = words.find(s.startsWith).map(w => words.indexOf(w) + 1)

    realDigit.orElse(textDigit).orElse(if (s.tail.nonEmpty) findFirstAlt(s.tail) else None)
  }

  def findLastAlt(s: String): Option[Int] = {
    val realDigit = s.lastOption.filter(_.isDigit).map(_.asDigit)
    val textDigit = words.find(s.endsWith).map(w => words.indexOf(w) + 1)

    realDigit.orElse(textDigit).orElse(if (s.init.nonEmpty) findLastAlt(s.init) else None)
  }

  val result2 = input.map(s => s"${findFirst(s)}${findLast(s)}".toInt).sum
  val result2Alt = input.map(s => s"${findFirstAlt(s).get}${findLastAlt(s).get}".toInt).sum

  println(s"Result part 2: ${result2}")
  println(s"Result part 2: ${result2Alt}")
}

