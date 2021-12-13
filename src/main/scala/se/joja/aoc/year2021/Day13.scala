package se.joja.aoc.year2021

import se.joja.aoc.{getInput, splitOnEmptyLine}

object Day13 extends App {

  val input                = getInput("2021/day13.txt")
  val coords :: folds :: _ = splitOnEmptyLine(input)
  println(coords)
  println(folds)

  // Part 1

  case class Matrix(sizeX: Int = 0, sizeY: Int = 0, mat: Map[Int, List[Int]] = Map.empty) {
    val dots: Int = mat.map(_._2.size).sum

    def addPoint(x: Int, y: Int): Matrix =
      this.copy(sizeX = List(x + 1, sizeX).max, sizeY = List(y + 1, sizeY).max, mat = mat.updatedWith(y) {
        case Some(value) => Some(value :+ x)
        case None        => Some(List(x))
      })

    def splitHor(at: Int): (Matrix, Matrix) = (
      this.copy(mat = mat.filter(_._1 < at), sizeY = at),
      this.copy(mat = mat.filter(_._1 > at).map { case (y, row) => (y - at - 1) -> row }, sizeY = sizeY - at - 1)
    )

    def splitVert(at: Int): (Matrix, Matrix) = (
      this.copy(mat = mat.map { case (y, row) => y -> row.filter(_ < at) }, sizeX = at),
      this.copy(mat = mat.map { case (y, row) => y -> row.filter(_ > at).map(_ - at - 1) }, sizeX = sizeX - at - 1)
    )

    def flipHor: Matrix  = this.copy(mat = mat.map { case (y, row) => (sizeY - 1 - y) -> row })
    def flipVert: Matrix = this.copy(mat = mat.map { case (y, row) => y               -> row.map(sizeX - 1 - _) })

    def overlay(other: Matrix): Matrix = {
      println(s"Overlay - this: (${this.sizeX}, ${this.sizeY}), other: (${other.sizeX}, ${other.sizeY})")
      this.copy(mat = other.mat.foldLeft(mat) {
        case (acc, (y, row)) =>
          acc.updatedWith(y) {
            case Some(value) => Some((value ++ row).distinct)
            case None        => Some(row)
          }
      })
    }

    override def toString: String =
      0.until(sizeY)
        .map { y =>
          mat.get(y) match {
            case Some(row) =>
              0.until(sizeX).map(x => if (row.contains(x)) "#" else ".").mkString("")
            case None => ".".repeat(sizeX)
          }
        }
        .mkString("\n")
  }

  val CoordRegex = """(\d+),(\d+)""".r
  val FoldRegex  = """fold along ([xy])=(\d+)""".r

  val mat = coords.foldLeft(Matrix(1311, 895)) {
    case (acc, p) =>
      p match {
        case CoordRegex(x, y) => acc.addPoint(x.toInt, y.toInt)
      }
  }
  println(mat)

  val finalMat = folds.foldLeft(mat) {
    case (acc, fold) =>
      //println(s"Before '$fold'\nDots: ${acc.dots}, x: ${acc.sizeX}, y: ${acc.sizeY}")
      //println(acc)
      val newMat = fold match {
        case FoldRegex("x", at) =>
          val (mat1, mat2) = acc.splitVert(at.toInt)
          mat1.overlay(mat2.flipVert)

        case FoldRegex("y", at) =>
          val (mat1, mat2) = acc.splitHor(at.toInt)
          mat1.overlay(mat2.flipHor)
      }
      newMat
  }

  println(finalMat)

  // Part 2

  //println(s"Result part 2: $result2")

}
