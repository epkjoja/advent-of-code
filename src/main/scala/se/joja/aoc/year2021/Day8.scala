package se.joja.aoc.year2021

import se.joja.aoc.{getInput, invertMap}

object Day8 extends App {

  val input = getInput("2021/day8.txt").map(_.split('|')).map { case Array(a, b) => a.trim.split(' ').toList -> b.trim.split(' ').toList}
  println(input.mkString("\n"))

  // Part 1

  val input1 = input.map(_._2)

  def count1478(line: List[String]): Int = {
    line.count {
      case p if p.length == 2 => true // 1
      case p if p.length == 4 => true // 4
      case p if p.length == 3 => true // 7
      case p if p.length == 7 => true // 8
      case _ => false
    }
  }

  val result1 = input1.map(l => count1478(l)).sum
  println(s"Result part 1: $result1")

  // Part 2

  def findMapping(lineIn: List[String]): Map[String, Int] = {
    val line = lineIn.map(_.sorted)
    val one = line.find(_.length == 2).get
    val seven = line.find(_.length == 3).get
    val four = line.find(_.length == 4).get
    val eight = line.find(_.length == 7).get
    val zeroSixNine = line.filter(_.length == 6)
    val nine = zeroSixNine.find(s => four.forall(c => s.contains(c))).get
    val zero = zeroSixNine.find(s => s != nine && seven.forall(c => s.contains(c))).get
    val six = zeroSixNine.find(s => s != nine && s != zero).get
    val twoThreeFive = line.filter(_.length == 5)
    val three = twoThreeFive.find(s => one.forall(c => s.contains(c))).get
    val missingInSix = one.diff(six)
    val five = twoThreeFive.find(s => !s.contains(missingInSix)).get
    val two = twoThreeFive.find(s => s != three && s != five).get

    Map(zero -> 0, one -> 1, two -> 2, three -> 3, four -> 4, five -> 5, six -> 6, seven -> 7, eight -> 8, nine -> 9)
  }

  val allDigits = input.map { line =>
    val mapping = findMapping(line._1)
    line._2.map(d => mapping(d.sorted).toString).mkString("").toInt
  }
  println(allDigits.mkString("\n"))

  val result2 = allDigits.sum
  println(s"Result part 2: $result2")

}
