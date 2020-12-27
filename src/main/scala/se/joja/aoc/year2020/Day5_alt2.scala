package se.joja.aoc.year2020

import scala.io.Source

object Day5_alt2 extends App {

  val input = Source.fromResource("2020/day5.txt").getLines().toList

  val taken = input.map { str =>
    val binStr = str.replace('F', '0').replace('B', '1').replace('L', '0').replace('R', '1')
    Integer.parseInt(binStr, 2)
  }.sorted

  println(s"Result1: ${taken.max}")

  val missing = taken.min.until(taken.max).toList.diff(taken).head

  println(s"Result2: $missing")
}
