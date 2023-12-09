package se.joja.aoc.year2023

import se.joja.aoc.getInput

object Day9 extends App {

  val input = getInput("2023/day9.txt")

  val lines = input.map(s => s.split(' ').map(_.toLong).toList)

  def nextNum(in: List[Long]): Long = {
    if (in.forall(_ == 0)) 0 else {
      val deltas = in.sliding(2).map { case a :: b :: Nil => b - a }.toList
      in.last + nextNum(deltas)
    }
  }

  val result1 = lines.map(nextNum).sum

  println(s"Result part 1: ${result1}")

  // Part 2

  def prevNum(in: List[Long]): Long = {
    if (in.forall(_ == 0)) 0 else {
      val deltas = in.sliding(2).map { case a :: b :: Nil => b - a }.toList
      in.head - prevNum(deltas)
    }
  }

  val result2 = lines.map(prevNum).sum

  println(s"Result part 2: ${result2}")
}

