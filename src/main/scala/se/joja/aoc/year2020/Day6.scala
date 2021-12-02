package se.joja.aoc.year2020

import scala.io.Source

import se.joja.aoc._

object Day6 extends App {

  val input = Source.fromResource("2020/day6.txt").getLines().toList

  val sets = splitOnEmptyLine(input).map { strings =>
    strings.flatMap(s => s.toList).toSet
  }

  val result = sets.map(_.size).sum

  println(s"Result: $result")

  // Part two

  val sets2 = splitOnEmptyLine(input).map { strings =>
    val res = strings.map(s => s.toList).reduceLeft((acc, a) => acc.intersect(a)).mkString("")
    println(s"$strings => $res")
    res
  }

  val result2 = sets2.map(_.length).sum

  println(s"Result: $result2")

}
