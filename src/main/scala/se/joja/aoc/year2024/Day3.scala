package se.joja.aoc.year2024

import se.joja.aoc.readInput

object Day3 extends App {

  val input = readInput(2024, 3).head

  val Pattern = """mul\(\d{1,3},\d{1,3}\)""".r
  val Mul = """mul\((\d+),(\d+)\)""".r

  val muls = Pattern.findAllIn(input).map {
    case Mul(a, b) =>
      //println(s"mul($a,$b)")
      a.toLong * b.toLong
  }

  val result1 = muls.sum

  println(s"Result part 1: $result1")

  // Part 2

  val Pattern2 = """(mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\))""".r
  val Do = """do\(\)""".r
  val Dont = """don't\(\)""".r

  val (_, sum) = Pattern2.findAllIn(input).foldLeft((true, 0L)) {
    case ((true, sum), Mul(a, b)) =>
      //println(s"mul($a,$b)")
      (true, sum + a.toLong * b.toLong)

    case ((_, sum), Do()) =>
      //println("Do")
      (true, sum)

    case ((_, sum), Dont()) =>
      //println("Don't")
      (false, sum)

    case ((enabled, sum), _) =>
      //println("Disabled!")
      (enabled, sum)
  }

  val result2 = sum

  println(s"Result part 2: $result2")
}

