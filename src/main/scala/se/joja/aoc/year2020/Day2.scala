package se.joja.aoc.year2020

import scala.io.Source

object Day2 extends App {

  val input = Source.fromResource("2020/day2.txt").getLines().toSeq

  val regex = """(\d+)-(\d+) (\w): (\w+)""".r

  case class Data(from: Int, to: Int, char: Char, password: String) {
    def isValidOne: Boolean = {
      val nbrOf = password.count(_ == char)
      nbrOf >= from && nbrOf <= to
    }

    def isValidTwo: Boolean = {
      (password(from-1) == char, password(to-1) == char) match {
        case (true, false) => true
        case (false, true) => true
        case _ => false
      }
    }
  }

  val data = input.map {
    case regex(from, to, char, password) => Data(from.toInt, to.toInt, char.charAt(0), password)
  }

  val (validOne, invalidOne) = data.partition(_.isValidOne)
  println(s"ONE - Valid: ${validOne.size}, Invalid: ${invalidOne.size}")

  val (validTwo, invalidTwo) = data.partition(_.isValidTwo)
  println(s"TWO - Valid: ${validTwo.size}, Invalid: ${invalidTwo.size}")
}
