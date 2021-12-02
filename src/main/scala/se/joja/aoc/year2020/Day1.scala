package se.joja.aoc.year2020

import se.joja.aoc.getInput

object Day1 extends App {

  val input = getInput("2020/day1.txt").map(_.toInt)

  println(input)
  val size = input.size

  for {
    x <- 0.until(size)
    y <- x.until(size)
    z <- y.until(size)
  } yield {
    val a = input(x)
    val b = input(y)
    val c = input(z)
    if ((a + b + c) == 2020) {
      println(s"$a - $b - $c -> ${a * b * c}")
    }
  }
}
