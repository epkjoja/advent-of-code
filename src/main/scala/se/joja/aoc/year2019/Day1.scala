package se.joja.aoc.year2019

import scala.io.Source

import se.joja.aoc.getInput

object Day1 extends App {
  val input = getInput("2019/day1.txt").map(_.toInt)

  val result = input.map(i => i / 3 - 2).sum
  println(s"Result: $result")

  // Part two

  def countFuel(in: Int): Int = {
    val a = in / 3 - 2
    if (a <= 0) 0 else a + countFuel(a)
  }

  val result2 = input.map(countFuel).sum
  println(s"Result2: $result2")
}
