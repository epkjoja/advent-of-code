package se.joja.aoc.year2021

import se.joja.aoc.getInput

import scala.annotation.tailrec

object Day11 extends App {

  type Mat[A] = List[List[A]]

  val input = getInput("2021/day11.txt").map(_.map(_.toInt - 48).toList)
  println(input)

  // Part 1

  case class P(x: Int, y: Int) {
    def n: P                     = P(x - 1, y)
    def ne: P                    = P(x - 1, y + 1)
    def e: P                     = P(x, y + 1)
    def se: P                    = P(x + 1, y + 1)
    def s: P                     = P(x + 1, y)
    def sw: P                    = P(x + 1, y - 1)
    def w: P                     = P(x, y - 1)
    def nw: P                    = P(x - 1, y - 1)
    def neighbourPoints: List[P] = List(n, ne, e, se, s, sw, w, nw).filter(isValid)
  }

  val maxX = input.size
  val maxY = input.head.size
  val allPoints = for(x <- 0.until(maxX); y <- 0.until(maxY)) yield P(x, y)

  def isValid: P => Boolean = p => if (p.x < 0 || p.x >= maxX || p.y < 0 || p.y >= maxY) false else true

  def value(in: Mat[Int])(p: P): Int = in(p.x)(p.y)
  def incValue(in: Mat[Int], p: P): Mat[Int] = in.updated(p.x, in(p.x).updated(p.y, value(in)(p) + 1))
  def setValue(in: Mat[Int], p: P, value: Int): Mat[Int] = in.updated(p.x, in(p.x).updated(p.y, value))
  def print(in: Mat[Int]): String = in.map(_.mkString("")).mkString("\n")

  def step(in: Mat[Int]): Mat[Int] = {
    @tailrec
    def internalStep(mat: Mat[Int]): Mat[Int] = {
      val flashPoints = allPoints.filter(p => value(mat)(p) > 9)

      if (flashPoints.isEmpty) mat else {
        val updatedMat = flashPoints.foldLeft(mat) { case (acc, p) =>
          setValue(acc, p, 0)
        }
        val newNeighbours = flashPoints.flatMap(_.neighbourPoints).filter(p => value(updatedMat)(p) > 0)
        val updatedMat2 = newNeighbours.foldLeft(updatedMat) { case (acc, p) =>
          incValue(acc, p)
        }
        internalStep(updatedMat2)
      }
    }

    internalStep(in.map(_.map(_ + 1)))
  }

  println("Before any step:\n" + print(input))

  val (finalMat, totalFlashes) = 1.to(100).foldLeft(input, 0) { case ((mat, flashes), num) =>
    val newMat = step(mat)
    val flashNum = newMat.map(_.count(_ == 0)).sum
    println(s"After step $num\n" + print(newMat) + s" - New flashes: $flashNum")
    newMat -> (flashes + flashNum)
  }

  println(s"Result part 1: $totalFlashes")

  // Part 2

  val (_, allFlash) = 1.to(500).foldLeft(input, Option.empty[Int]) { case ((mat, allFlash), num) =>
    val newMat = step(mat)
    val newAllFlash = if (newMat.forall(_.forall(_ == 0))) Some(num) else None
    println(s"After step $num\n" + print(newMat))
    (newMat, allFlash.orElse(newAllFlash))
  }

  println(s"Result part 2: $allFlash")
}
