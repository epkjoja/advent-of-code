package se.joja.aoc.year2020


import se.joja.joja.getInput

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object Day11 extends App {

  val input = getInput("2020/day11.txt")

  type Point = (Int, Int)
  type Row = Array[Char]
  type Matrix = Array[Row]

  val numRows = input.size
  val numCols = input.head.length

  val m = input.map { row =>
    row.toCharArray
  }.toArray

  /**
   * @return true if the given point is a valid index of the matrix
   */
  def valid (mat: Matrix, p: Point): Boolean = {
    val (x,y) = p
    (x >= 0) && (x < mat.length) && (y >= 0) && (y < mat.head.length) && mat(x)(y) != '.'
  }

  /**
   * @return the neighbors of a point in a 8-connected grid
   */
  def neighbours8(mat: Matrix, p: Point): Iterator[Point] = {
    val (x,y) = p
    val pts =  for (i <- x-1 to x+1; j <- y-1 to y+1) yield (i,j)
    pts.filter(pt => valid(mat, pt) && pt != p).iterator
  }

  def pointContains(mat: Matrix, p: Point, c: Char): Boolean = mat(p._1)(p._2) == c

  def printM(mat: Matrix): String = mat.map(_.mkString("")).mkString("\n")

  def pointsContaining(mat: Matrix, c: Char): Set[Point] = {
    mat.zipWithIndex.flatMap { case (row, i) =>
      row.zipWithIndex.filter(_._1 == c).map(i -> _._2)
    }.toSet
  }

  def setPointsTo(mat: Matrix, pts: Set[Point], c: Char): Matrix = {
    pts.foldLeft(mat) { case (acc, p) =>
      val newRow = acc(p._1).updated(p._2, c)
      acc.updated(p._1, newRow)
    }
  }

  def isSame(mat1: Matrix, mat2: Matrix): Boolean = {
    mat1.zip(mat2).forall(rows => rows._1 sameElements rows._2)
  }

  def mutateOnce(mat: Matrix): Matrix = {
    val changeToOcc = pointsContaining(mat, 'L').flatMap { p =>
      val occupiedNeighbours = neighbours8(mat, p).filter(p => pointContains(mat, p, '#'))
      if (occupiedNeighbours.isEmpty) Some(p) else None
    }
    val changeToFree = pointsContaining(mat, '#').flatMap { p =>
      val occupiedNeighbours = neighbours8(mat, p).filter(p => pointContains(mat, p, '#'))
      if (occupiedNeighbours.size >= 4) Some(p) else None
    }
    val mat1 = setPointsTo(mat, changeToOcc, '#')
    val mat2 = setPointsTo(mat1, changeToFree, 'L')
    mat2
  }

  @tailrec
  def mutate(mat: Matrix): Matrix = {
    println(s"Starting to mutate:\n${printM(mat)}\n")
    val newMat = mutateOnce(mat)
    if (printM(mat) == printM(newMat)) mat else mutate(newMat)
  }

  val fin = mutate(m)
  println(s"Final mutation:\n${printM(fin)}")
  println(s"Occupied: ${pointsContaining(fin, '#').size}")
  println("=".repeat(30))

  // Part two

  def valid2(mat: Matrix, p: Point): Boolean = {
    val (x,y) = p
    (x >= 0) && (x < mat.length) && (y >= 0) && (y < mat.head.length)
  }

  def findFirstInDir(mat: Matrix, p: Point, dir: (Int, Int)): Option[Char] = {
    val newPoint = (p._1 + dir._1, p._2 + dir._2)
    if (!valid2(mat, newPoint))
      None
    else {
      val c = mat(newPoint._1)(newPoint._2)
      if (c == '.') findFirstInDir(mat, newPoint, dir) else Some(c)
    }
  }

  def neighbourignSeats(mat: Matrix, p: Point): (Int, Int) = {
    val (x,y) = p
    val allDirSteps = (for {
      dx <- (-1 to 1).toList
      dy <- (-1 to 1).toList
    } yield (dx -> dy)).filterNot(p => p._1 == 0 && p._2 == 0)

    val ns = allDirSteps.flatMap(dir => findFirstInDir(mat, p, dir))
    (ns.count(_ == '#'), ns.count(_ == 'L'))
  }

  def mutateOnceNew(mat: Matrix): Matrix = {
    val changeToOcc = pointsContaining(mat, 'L').flatMap { p =>
      val (occ, _) = neighbourignSeats(mat, p)
      if (occ == 0) Some(p) else None
    }
    val changeToFree = pointsContaining(mat, '#').flatMap { p =>
      val (occ, _) = neighbourignSeats(mat, p)
      if (occ >= 5) Some(p) else None
    }
    val mat1 = setPointsTo(mat, changeToOcc, '#')
    val mat2 = setPointsTo(mat1, changeToFree, 'L')
    mat2
  }

  @tailrec
  def mutateNew(mat: Matrix): Matrix = {
    println(s"Starting to mutate:\n${printM(mat)}\n")
    val newMat = mutateOnceNew(mat)
    if (printM(mat) == printM(newMat)) mat else mutateNew(newMat)
  }

  val fin2 = mutateNew(m)
  println(s"Final2 mutation:\n${printM(fin2)}")
  println(s"Occupied2: ${pointsContaining(fin2, '#').size}")

}
