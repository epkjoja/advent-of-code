package se.joja.aoc.year2024

import se.joja.aoc.readInput

import scala.annotation.tailrec

object Day22 extends App {

  val input = readInput(2024, 22).map(_.toLong)

  def calcNext(rand: Long): Long = {
    val one = ((rand * 64) ^ rand) % 16777216
    val two = ((one / 32) ^ one) % 16777216
    ((two * 2048) ^ two) % 16777216
  }

  @tailrec
  def calcNum(rand: Long, num: Int): Long = {
    if (num == 0) rand else {
      val next = calcNext(rand)
      calcNum(next, num - 1)
    }
  }

  val res = input.map(calcNum(_, 2000))

  val result1 = res.sum

  println(s"Result part 1: $result1")

  // Part 2

  val result2 = ""

  println(s"Result part 2: $result2")
}

