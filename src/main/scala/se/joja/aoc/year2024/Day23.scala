package se.joja.aoc.year2024

import se.joja.aoc.readInput

object Day23 extends App {

  val Pattern = """([a-z]{2})-([a-z]{2})""".r

  val input = readInput(2024, 23, true).flatMap {
    case Pattern(a, b) => List(a -> b, b -> a)
  }.groupMap(_._1)(_._2)

  case class Loop(comps: Set[String])

//  def findLoops(start: String, acc: Set[String] = Set.empty): List[Loop] = {
//    if (acc.)
//    input(start)
//  }

  val result1 = "" //j.map(e => e._1 -> e._2.sorted).mkString("\n")

  println(s"Result part 1: $result1")

  // Part 2

  val result2 = ""

  println(s"Result part 2: $result2")
}

