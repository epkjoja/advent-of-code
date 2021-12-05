package se.joja.aoc.year2021

import se.joja.aoc.getInput

import java.util.regex.Pattern

object Day5 extends App {

  val input = getInput("2021/day5.txt")

  println(input)

  // Part 1

  case class Line(from: (Int, Int), to: (Int, Int)) {
    def max: Int           = List(from._1, from._2, to._1, to._2).max
    def isHorVert: Boolean = from._1 == to._1 || from._2 == to._2
    def points: Seq[(Int, Int)] =
      (from, to) match {
        case ((f1, f2), (t1, t2)) if f1 == t1 =>
          f2.to(t2, if (f2 <= t2) 1 else -1).map(f1 -> _)
        case ((f1, f2), (t1, t2)) if f2 == t2 =>
          f1.to(t1, if (f1 <= t1) 1 else -1).map(_ -> f2)
        case ((f1, f2), (t1, t2)) => {
          val steps = Math.abs(f1 - t1)
          val (signx, signy) = ((t1 - f1).sign, (t2 - f2).sign)
          val p = 0.to(steps).map(i => (f1 + i * signx) -> (f2 + i * signy))
          //println(s"Point: $this, Steps: $steps ($signx, $signy) - $p")
          p
        }
      }
  }

  case class Grid(size: Int, list: List[Int] = List.fill(size * size)(0)) {
    def addLine(line: Line): Grid = {
      println(s"Adding line: $line")
      line.points.foldLeft(this) { case (g, p) => g.addPoint(p) }
    }

    def addPoint(p: (Int, Int)): Grid = {
      val pos      = p._2 * size + p._1
      val newValue = list(pos) + 1
      this.copy(list = list.updated(pos, newValue))
    }

    def overlaps: Int = list.count(_ > 1)

    override def toString: String =
      list.grouped(size).map(_.mkString("")).mkString("\n")
  }

  val LineRegex = """(\d+),(\d+) -> (\d+),(\d+)""".r
  val lines = input.map {
    case LineRegex(f1, f2, t1, t2) => Line(f1.toInt -> f2.toInt, t1.toInt -> t2.toInt)
  }

  val size = lines.map(_.max).max + 1
  println(s"Size: $size")

  val linesToUse = lines.filter(_.isHorVert)

  val finalGrid = linesToUse.foldLeft(Grid(size)) { case (grid, line) => grid.addLine(line) }
  println(finalGrid)

  val result1 = finalGrid.overlaps

  println(s"Result part 1: $result1")

  // Part 2

  val finalGrid2 = lines.foldLeft(Grid(size)) { case (grid, line) => grid.addLine(line)}
  println(finalGrid2)

  val result2 = finalGrid2.overlaps

  println(s"Result part 2: $result2")

}
