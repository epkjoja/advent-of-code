package se.joja.aoc.year2024

import se.joja.aoc.readInput

object Day7 extends App {

  val LinePat = """(\d+): ([\d ]+)""".r
  case class Line(res: Long, nums: List[Long])

  val input = readInput(2024, 7).map {
    case LinePat(res, numString) => Line(res.toLong, numString.split(' ').map(_.toLong).toList)
  }

  def isLineValid(line: Line): Boolean = {
    def recCheck(l: List[Long], acc: Long = 0): Boolean = {
      l match {
        case _ if acc > line.res => false
        case Nil => acc == line.res
        case num :: rest if recCheck(rest, acc + num) => true
        case num :: rest if recCheck(rest, acc * num) => true
        case _ => false
      }
    }

    recCheck(line.nums)
  }

  val valids = input.filter(isLineValid)

  val result1 = valids.map(_.res).sum

  println(s"Result part 1: $result1")

  // Part 2

  def isLineValid2(line: Line): Boolean = {
    def recCheck(l: List[Long], acc: Long = 0): Boolean = {
//      print(s"Testing $l, $acc ")
      val t = l match {
        case _ if acc > line.res => false
        case Nil => acc == line.res
        case num :: rest if recCheck(rest, acc + num) => true
        case num :: rest if recCheck(rest, if (acc == 0) num else acc * num) => true
        case num :: rest if recCheck(rest, s"$acc$num".toLong) => true
        case _ => false
      }
//      println(s" => $t")
      t
    }

    recCheck(line.nums)
  }

  val valids2 = input.filter(isLineValid2)

  val result2 = valids2.map(_.res).sum

  println(s"Result part 2: $result2")
}
