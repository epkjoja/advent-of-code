package se.joja.aoc.year2021

import se.joja.aoc.getInput

object Day3 extends App {

  val input = getInput("2021/day3.txt").map(l => l.map(_ - 48))

  println(input)

  def getBitValue(data: List[Seq[Int]], bit: Int): List[Int] =
    data.map(l => l(bit))

  def findMostCommon(i: List[Int]): Int = {
    val (zeros, ones) = i.partition(_ == 0)
    (zeros.size, ones.size) match {
      case (z, o) if z == o => -1
      case (z, o) if z < o  => 1
      case _                => 0
    }
  }

  // Part 1

  val bits = input.head.length

  val gammaString = (0 until bits).map(i => findMostCommon(getBitValue(input, i))).mkString
  val gamma       = Integer.parseInt(gammaString, 2)

  val epsilonString = gammaString.map(c => if (c == '0') '1' else '0')
  val epsilon       = Integer.parseInt(epsilonString, 2)

  val result1 = gamma * epsilon

  println(s"Result part 1: $result1")

  // Part 2

  def findRating(data: List[Seq[Int]])(crit: (List[Seq[Int]], Int) => List[Seq[Int]]) = {
    (0 until (bits)).foldLeft(data) {
      case (acc, i) =>
        println(s"i: $i (${acc.size}) - ${acc.map(_.mkString)}")
        if (acc.size == 1) acc
        else crit(acc, i)
    }.head.mkString
  }

  val oxy = findRating(input) { case (data, i) =>
      findMostCommon(getBitValue(data, i)) match {
        case -1 =>
          println(s"Crit equal => 1")
          data.filter(l => l(i) == 1)
        case a  =>
          println(s"Crit said: $a")
          data.filter(l => l(i) == a)
      }
  }

  val co2 = findRating(input) { case (data, i) =>
    findMostCommon(getBitValue(data, i)) match {
      case -1 =>
        println(s"Crit equal => 0")
        data.filter(l => l(i) == 0)
      case a  =>
        println(s"Crit said: ${1-a}")
        data.filter(l => l(i) == 1 - a)
    }
  }

  val result2 = Integer.parseInt(oxy, 2) * Integer.parseInt(co2, 2)

  println(s"Result part 2: $result2")
}
