package se.joja.aoc.year2024

import se.joja.aoc.readInput

import scala.annotation.tailrec

object Day11 extends App {

  val input = readInput(2024, 11).head.split(' ').map(_.toLong).toList

  @tailrec
  def blink(line: List[Long], acc: List[Long] = List.empty): List[Long] = {
    line match {
      case Nil => acc
      case 0 :: rest => blink(rest, acc :+ 1L)
      case s :: rest if s"$s".length % 2 == 0 =>
        val text = s"$s"
        val half = text.length / 2
        blink(rest, acc ++ List(text.take(half), text.drop(half)).map(_.toLong))
      case s :: rest => blink(rest, acc :+ s * 2024)
    }
  }

  val res = 1.to(25).foldLeft(input) { case (acc, i) =>
    val b = blink(acc)
    println(s"After $i")
//    println(s"($i) $b")
    b
  }

  val result1 = res.size

  println(s"Result part 1: $result1")

  // Part 2

  val result2 = ""

  println(s"Result part 2: $result2")
}
