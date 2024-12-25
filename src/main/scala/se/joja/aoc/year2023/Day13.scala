package se.joja.aoc.year2023

import se.joja.aoc.{getInput, splitOnEmptyLine}

import scala.annotation.tailrec
import scala.sys.exit

object Day13 extends App {

  val input = splitOnEmptyLine(getInput("2023/day13.txt"))

  type Mat2D = Vector[Vector[Char]]
  def show(m: Mat2D) = m.map(_.mkString("")).mkString("\n")

  val patterns = input.map(_.map(_.toVector).toVector)

  def reflection(p: Mat2D): Option[Int] = {
    val max = p.length

    val cand = p.toList.sliding(2).zipWithIndex.flatMap { case (l1 :: l2 :: Nil, i) =>
      if (l1 == l2) Some(i) else None
    }.toList

    println(s"Kandidater: $cand")

    val j = cand.filter { pos =>
      @tailrec
      def checkRefl(i: Int = 0): Boolean = {
        val (low, high) = (pos - i, pos + i + 1)
        println(s"low: $low, high: $high")
        if (low < 0 || high >= max) return true

        if (p(low) == p(high)) checkRefl(i + 1) else false
      }

      checkRefl()
    }.map(_ + 1)

    if (j.length <= 1) j.headOption else {
      println(s"Hittade flera reflektioner!!, $j")
      exit(1)
    }
  }

  val result1 = patterns.map(p => reflection(p).map(_ * 100).getOrElse(reflection(p.transpose).getOrElse(0))).sum

  println(s"Result part 1: \n$result1")

  // Part 2

  val result2 = ""

  println(s"Result part 2: $result2")
}

