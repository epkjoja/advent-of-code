package se.joja.aoc.year2021

import scala.annotation.tailrec

import se.joja.aoc.getInput

object Day9 extends App {

  val input = getInput("2021/day9.txt").map(_.map(_.toInt - 48).toVector)
  println(input)

  // Part 1

  val maxX = input.size
  val maxY = input.head.size

  case class P(x: Int, y: Int) {
    def up: P               = P(x + 1, y)
    def down: P             = P(x - 1, y)
    def left: P             = P(x, y - 1)
    def right: P            = P(x, y + 1)
    def neighbours: List[P] = List(up, down, left, right).filter(isValid)
  }

  def isValid: P => Boolean = p => if (p.x < 0 || p.x >= maxX || p.y < 0 || p.y >= maxY) false else true

  def pos(p: P): Option[Int] = if (isValid(p)) Some(input(p.x)(p.y)) else None

  def neighbourValues(p: P): List[Int] = p.neighbours.flatMap(pos)

  def allPoints = for { x <- 0.until(maxX); y <- 0.until(maxY) } yield P(x, y)

  val lowPoints = allPoints.filter(p => neighbourValues(p).forall(_ > pos(p).get))
  println(s"Low points: $lowPoints")

  val result1 = lowPoints.flatMap(pos).map(_ + 1).sum

  println(s"Result part 1: $result1")

  // Part 2

  def findBasin(p: P): List[P] = {
    @tailrec
    def oneStep(in: List[P]): List[P] = {
      val newPoints = in.flatMap(_.neighbours).distinct.filter(p => !in.contains(p) && pos(p).get != 9)
      if (newPoints.isEmpty) in else oneStep(in ++ newPoints)
    }
    oneStep(List(p))
  }

  val basins = lowPoints.map(findBasin).map(_.size)
  println(basins)

  val result2 = basins.sorted.takeRight(3).product
  println(s"Result part 2: $result2")

}
