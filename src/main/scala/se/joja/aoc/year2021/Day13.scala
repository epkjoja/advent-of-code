package se.joja.aoc.year2021

import se.joja.aoc.year2021.Day11.isValid
import se.joja.aoc.{getInput, splitOnEmptyLine}

object Day13 extends App {

  val input                  = getInput("2021/day13.txt")
  val coords :: folds :: Nil = splitOnEmptyLine(input)
  println(coords)
  println(folds)

  // Part 1

  case class Matrix(mat: Map[Int, Vector[String]]) {
    def addPoint(x: Int, y: Int): Matrix = {
      Matrix(mat.updatedWith(y) {
        case Some(value) => Some(value.updated(x, "#"))
        case None        => Some(Vector.empty.updated(x, "#"))
      })
    }

    override def toString: String = {
      for {
        x <- 0.until(mat.map(_._2.size).max)
        y <- 0.until(mat.keys.max)
      } yield {
        mat.get(y) match {
          case Some(value) => ???
          case None => ???
        }
      }
    }
  }

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
  //println(s"Result part 1: $result1")

  // Part 2

  //println(s"Result part 2: $result2")

}
